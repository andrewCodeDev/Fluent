const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;

//////////////////////////////////
// Public Access Point ///////////

pub fn init(slice: anytype) FluentInterface(DeepChild(@TypeOf(slice)), isConst(@TypeOf(slice))) {
    return .{ .items = slice };
}

fn FluentInterface(comptime T: type, comptime is_const: bool) type {

    return struct {
    
        const Self = @This();

        pub const DataType = T;

        pub const SliceType = if (is_const) []const T else []T;

        const Backend = if (is_const) ImmutableBackend else MutableBackend;

        // we can detect if we have a const slice or non-const
        // and dispatch to different versions of this thing
        // depending on the circumstance.
        items: SliceType,
    
        pub usingnamespace Backend(Self);
    };
}

//////////////////////////////////
// Backends and Implementation ///

//////////////////////////////////
// ImmutableBackend:

// Used by mutable backend - only suports non-mutating 
// operations over items. Primarily used for reducing,
// scanning, and indexing. Provides non-mutating iterator
// support for both Immutable and Mutable backends.

fn ImmutableBackend(comptime Self: type) type {
    return struct {

        pub fn findFrom(
            self: Self, 
            comptime mode: std.mem.DelimiterType,
            start_index: usize,
            needle: DelimiterParam(Self.DataType, mode),
        ) ?usize {
            return switch (mode) {
                .any => std.mem.indexOfAnyPos(Self.DataType, self.items, start_index, needle),
                .scalar => std.mem.indexOfScalarPos(Self.DataType, self.items, start_index, needle),
                .sequence => std.mem.indexOfPos(Self.DataType, self.items, start_index, needle),
            };
        }
        
        pub fn find(
            self: Self, 
            comptime mode: std.mem.DelimiterType,
            needle: DelimiterParam(Self.DataType, mode),
        ) ?usize {
            return findFrom(self, mode, 0, needle);
        }

        // TODO: make conversion function for integers that handles all this?
        pub fn at(self: Self, int: anytype) Self.DataType {
            switch (@typeInfo(@TypeOf(int))) {
                .Int => |i| {
                    if (comptime i.signedness == .unsigned) {
                        return self.items[int];
                    } else {
                        const u: usize = @abs(int);
                        return self.items[if (int < 0) self.items.len - u else u];
                    }
                },
                .ComptimeInt => {
                    if (comptime int > 0) {
                        return self.items[int];
                    } else {
                        const u: usize = comptime @abs(int);
                        return self.items[if (int < 0) self.items.len - u else u];
                    }
                    return self.items[if (comptime int < 0) @as(isize, @intCast(self.items.len)) + int else int];
                },
                else => @compileError("At requires integer type parameter."),
            }
        }

        // NOTE: 
        //  using slices here because this makes it directly
        //  obvious that we're support any kind of slice and
        //  both Mutable and Immutable backends. 

        pub fn order(self: Self, items: []const Self.DataType) Order {
            return std.mem.order(Self.DataType, self.items, items);            
        }
        pub fn equal(self: Self, items: []const Self.DataType) bool {
            return order(self, items) == .eq;
        }

        ///////////////////////////////////////////////////
        // Iterator support ///////////////////////////////

        pub fn split(
            self: Self, 
            comptime mode: std.mem.DelimiterType, 
            delimiter: DelimiterParam(Self.DataType, mode),
        ) std.mem.SplitIterator(Self.DataType, mode) {
            return .{ .index = 0, .buffer = self.items, .delimiter = delimiter };
        }

        pub fn tokenize(
            self: Self, 
            comptime mode: std.mem.DelimiterType, 
            delimiter: DelimiterParam(Self.DataType, mode),
        ) std.mem.TokenIterator(Self.DataType, mode) {
            return .{ .index = 0, .buffer = self.items, .delimiter = delimiter };
        }
    };
}

//////////////////////////////////
// MutableBackend:

// Only suports mutating operations on items.
// Operations include sorting, replacing,
// permutations, and partitioning.

fn MutableBackend(comptime Self: type) type {
    return struct {


        // includes operations like reduce, find, and iterators
        pub usingnamespace ImmutableBackend(Self);

        // calls std.sort.block
        pub fn sort(self: Self, comptime mode: enum{ asc, desc }) Self {
            const SF = SortFunction(Self.DataType);
            const func = if (mode == .asc) SF.lessThan else SF.greaterThan;
            std.sort.block(Self.DataType, self.items, void{}, func);
            return self;
        }

        // TODO: future idea...

        // For mapping functions like "abs", only certain types make
        // sense there. We could prohbit those or make them no-ops
        // for certain types of scalar values... u8, for instance,
        // would be a no-op. Other types could be vectorized with
        // SIMD and use the builtin @abs function. For things that
        // can be vectorized, we should probably provide member
        // functions for those and make them no-ops if they don't
        // apply.

        // Another option is to compose backends for math-ish operations
        // that don't make sense across types and only expose them if
        // they make sense for the Self.DataType.

        // Meanwhile, we can always have a `map` fallback.
        pub fn map(self: Self, f: fn (Self.DataType) Self.DataType) Self {
            for (self.items) |*x| x.* = f(x.*);
            return self;
        }
    };
}

fn SortFunction(comptime T: type) type {
    return struct {
        fn lessThan(_: void, x: T, y: T) bool {
            return x < y;
        }
        fn greaterThan(_: void, x: T, y: T) bool {
            return x > y;
        }
    };
}

fn isConst(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .Pointer => |ptr| return ptr.is_const, 
        else => @compileError("Type must coercible to a slice."),
    }
}

fn DelimiterParam(comptime T: type, comptime mode: std.mem.DelimiterType) type {
    return switch (mode) {
        .sequence, .any => []const T,
        .scalar => T,
    };
}

// checks if we are pointing to an array
fn DeepChild(comptime T: type) type {
    // TODO: consider comptime support, should be Immutable only..

    const C = Child(T);

    return switch (@typeInfo(C)) {
        .Int, .Float => C,
        .Array => |a| a.child,
        else => @compileError("Unsupported Type"),
    };
}

//////////////////////////////////
// Immutable Testing Block ///////

const Fluent = @This();

test "Immutable Find Functions" {
    const x = Fluent.init("Hello, World!");
    const i = x.find(.scalar, ' ') orelse unreachable;
    const j = x.findFrom(.scalar, i, '!') orelse unreachable;
    try std.testing.expectEqual(6, i);
    try std.testing.expectEqual(12, j);
}

test "Immutable at Function" {

    const x = Fluent.init("Hello, World!");
    { // Normal indexing...
        const a = x.at(1);
        const b = x.at(2);
        const c = x.at(3);
        try std.testing.expectEqual('e', a);
        try std.testing.expectEqual('l', b);
        try std.testing.expectEqual('l', c);
    }
    { // Pythonic indexing...
        const a = x.at(-1);
        const b = x.at(-2);
        const c = x.at(-3);
        try std.testing.expectEqual('!', a);
        try std.testing.expectEqual('d', b);
        try std.testing.expectEqual('l', c);
    }
}

test "Immutable Iterators" {

    const x = Fluent.init("this is a test");
    { // split iterators
        var itr = x.split(.scalar, ' ');
        const s0 = Fluent.init(itr.next() orelse unreachable);
        const s1 = Fluent.init(itr.next() orelse unreachable);
        const s2 = Fluent.init(itr.next() orelse unreachable);
        const s3 = Fluent.init(itr.next() orelse unreachable);
        std.debug.assert(s0.equal("this"));
        std.debug.assert(s1.equal("is"));
        std.debug.assert(s2.equal("a"));
        std.debug.assert(s3.equal("test"));
        std.debug.assert(itr.next() == null);
    }
}

//////////////////////////////////
// Immutable Testing Block ///////

test "Mutable Map Chaining" {

    const x = Fluent.init(try std.testing.allocator.dupe(u8, "A B C D E F G"));
        defer std.testing.allocator.free(x.items);

    const toUnderscore = 
        struct { fn call(c: u8) u8 { return if (c == ' ') '_' else c; } }.call;

    _ = x.map(std.ascii.toLower).map(toUnderscore);

    try std.testing.expect(x.equal("a_b_c_d_e_f_g"));
}
