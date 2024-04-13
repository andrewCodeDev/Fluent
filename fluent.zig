const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

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

        items: SliceType,

        // we can detect if we have a const slice or non-const
        // and dispatch to different versions of this thing
        // depending on the circumstance.

        pub usingnamespace if (is_const)
            ImmutableBackend(Self)
        else
            MutableBackend(Self);

        pub usingnamespace if (DataType == u8) blk: {
            break :blk if (is_const) ImmutableStringBackend(Self) else MutableStringBackend(Self);
        } else struct {};
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

        pub fn contains(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            needle: DelimiterParam(Self.DataType, mode),
        ) bool {
            return find(self, mode, needle) != null;
        }

        pub fn containsFrom(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            start_index: usize,
            needle: DelimiterParam(Self.DataType, mode),
        ) bool {
            return findFrom(self, mode, start_index, needle) != null;
        }

        pub fn get(self: Self, idx: anytype) Self.DataType {
            return self.items[wrapIndex(self.items.len, idx)];
        }

        pub fn startsWith(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            needle: DelimiterParam(Self.DataType, mode),
        ) bool {
            if (self.items.len == 0)
                return false;

            return switch (mode) {
                .any => blk: {
                    for (needle) |n| {
                        if (self.get(0) == n) break :blk true;
                    } else break :blk false;
                },
                .sequence => std.mem.startsWith(Self.DataType, self.items, needle),
                .scalar => self.get(0) == needle,
            };
        }

        pub fn endsWith(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            needle: DelimiterParam(Self.DataType, mode),
        ) bool {
            if (self.items.len == 0)
                return false;

            return switch (mode) {
                .any => blk: {
                    for (needle) |n| {
                        if (self.get(-1) == n) break :blk true;
                    } else break :blk false;
                },
                .sequence => std.mem.endsWith(Self.DataType, self.items, needle),
                .scalar => self.get(-1) == needle,
            };
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

        pub fn sum(self: Self) Self.DataType {
            if (self.items.len == 0) return 0;
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Add, addGeneric, self.items, reduceInit(ReduceOp.Add, Self.DataType) });
        }
        pub fn product(self: Self) Self.DataType {
            if (self.items.len == 0) return 0;
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Mul, mulGeneric, self.items, reduceInit(ReduceOp.Mul, Self.DataType) });
        }
        // currently returns inf if items is empty
        pub fn min(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Min, minGeneric, self.items, reduceInit(ReduceOp.Min, Self.DataType) });
        }
        // currently returns -inf if items is empty
        pub fn max(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Max, maxGeneric, self.items, reduceInit(ReduceOp.Max, Self.DataType) });
        }

        /// count the occurence of needles in self.items returns 0 if no match is found
        pub fn count(self: Self, comptime mode: std.mem.DelimiterType, needle: DelimiterParam(Self.DataType, mode)) usize {
            if (self.items.len == 0) return 0;
            var result: usize = 0;

            switch (mode) {
                .scalar => {
                    for (self.items) |it| {
                        if (it == needle) result += 1;
                    }
                },
                .sequence => result = std.mem.count(Self.DataType, self.items, needle),
                .any => {
                    // temporary doing O(N^2) before implementing something smarter
                    std.debug.assert(self.items.len <= 1000);
                    std.debug.assert(self.needle.len <= 1000);
                    for (self.items) |it| {
                        for (needle) |n| {
                            if (it == n) result += 1;
                        }
                    }
                },
            }
            return (result);
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
        pub fn sort(self: Self, comptime mode: enum { asc, desc }) Self {
            const SF = SortFunction(Self.DataType);
            const func = if (mode == .asc) SF.lessThan else SF.greaterThan;
            std.sort.block(Self.DataType, self.items, void{}, func);
            return self;
        }

        pub fn fill(self: Self, scalar: Self.DataType) Self {
            @memset(self.items, scalar);
            return self;
        }

        pub fn copy(self: Self, items: []const Self.DataType) Self {
            @memcpy(self.items, items);
            return self;
        }

        pub fn rotate(self: Self, amount: anytype) Self {
            const len = self.items.len;

            const rot_amt: usize = blk: {
                if (amount > 0) {
                    const u: usize = @intCast(amount);
                    break :blk len - (u % len);
                }
                const u: usize = @abs(amount);
                break :blk u % len;
            };

            std.mem.rotate(Self.DataType, self.items, rot_amt);
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

//////////////////////////////////
// ImmutableStringBackend:

inline fn all(self: anytype, predicate: anytype) bool {
    for (self.items) |x| {
        if (!predicate(x)) return false;
    }
    return true;
}

// Only activated if the child data type is u8
fn ImmutableStringBackend(comptime Self: type) type {
    return struct {
        pub fn isDigit(self: Self) bool {
            return all(self, std.ascii.isDigit);
        }

        pub fn isAlpha(self: Self) bool {
            return all(self, std.ascii.isAlphabetic);
        }

        pub fn isSpaces(self: Self) bool {
            return all(self, std.ascii.isWhitespace);
        }

        pub fn isLower(self: Self) bool {
            return all(self, std.ascii.isLower);
        }

        pub fn isUpper(self: Self) bool {
            return all(self, std.ascii.isUpper);
        }

        pub fn isHex(self: Self) bool {
            return all(self, std.ascii.isHex);
        }

        pub fn isASCII(self: Self) bool {
            return all(self, std.ascii.isASCII);
        }

        pub fn isPrintable(self: Self) bool {
            return all(self, std.ascii.isPrint);
        }

        pub fn isAlnum(self: Self) bool {
            return all(self, std.ascii.isAlphanumeric);
        }
    };
}

//////////////////////////////////
// MutableStringBackend:

fn MutableStringBackend(comptime Self: type) type {
    return struct {
        pub usingnamespace ImmutableStringBackend(Self);

        pub fn lower(self: Self) Self {
            for (self.items) |*c| c.* = std.ascii.toLower(c.*);
            return self;
        }

        pub fn upper(self: Self) Self {
            for (self.items) |*c| c.* = std.ascii.toUpper(c.*);
            return self;
        }

        // more inline with the actual python behavior
        pub fn capitalize(self: Self) Self {
            if (self.items.len > 0)
                self.items[0] = std.ascii.toUpper(self.items[0]);
            if (self.items.len > 1)
                for (self.items[1..]) |*c| {
                    c.* = std.ascii.toLower(c.*);
                };
            return self;
        }

        pub fn title(self: Self) Self {
            var i: usize = 0;
            var prev: u8 = ' ';
            while (i < self.items.len) : (i += 1) {
                switch (self.items[i]) {
                    'A'...'Z' => {
                        if (!std.ascii.isWhitespace(prev))
                            self.items[i] += 32;
                    },
                    'a'...'z' => {
                        if (std.ascii.isWhitespace(prev))
                            self.items[i] -= 32;
                    },
                    else => {},
                }
                prev = self.items[i];
            }
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

fn isUnsigned(comptime T: type) bool {
    return switch (@typeInfo(@TypeOf(T))) {
        .Int => |i| return i.signedness == .unsigned,
        else => false,
    };
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

inline fn wrapIndex(len: usize, idx: anytype) usize {
    switch (@typeInfo(@TypeOf(idx))) {
        .Int => |i| {
            if (comptime i.signedness == .unsigned) {
                return idx;
            } else {
                const u: usize = @abs(idx);
                return if (idx < 0) len - u else u;
            }
        },
        .ComptimeInt => {
            const u: usize = comptime @abs(idx);
            return if (comptime idx < 0) len - u else u;
        },
        else => @compileError("Index must be an integer type parameter."),
    }
}

inline fn reduceInit(comptime op: ReduceOp, comptime T: type) T {
    const info = @typeInfo(T);

    return switch (op) {
        .Add => 0, // implicit cast
        .Mul => 1, // implicit cast
        .Min => if (comptime info == .Int)
            math.maxInt(T)
        else
            math.floatMax(T),
        .Max => if (comptime info == .Int)
            math.minInt(T)
        else
            -math.floatMax(T),
        else => @compileError("reduceInit: unsupported op"),
    };
}

fn simdReduce(
    comptime T: type,
    comptime ReduceType: anytype,
    comptime BinaryFunc: anytype,
    items: []const T,
    initial: T,
) T {
    // TODO: Check generated assembly on <= loop code gen.

    var rdx = initial;

    // reduce in size N chunks...
    var i: usize = 0;
    if (comptime std.simd.suggestVectorLength(T)) |N| {
        while ((i + N) <= items.len) : (i += N) {
            const vec: @Vector(N, T) = items[i .. i + N][0..N].*; // needs compile time length
            rdx = @call(.always_inline, BinaryFunc, .{ rdx, @reduce(ReduceType, vec) });
        }
    }

    // reduce remainder...
    while (i < items.len) : (i += 1) {
        rdx = @call(.always_inline, BinaryFunc, .{ rdx, items[i] });
    }
    return rdx;
}

// these work for @Vector as well as scalar types
inline fn maxGeneric(x: anytype, y: anytype) @TypeOf(x) {
    return @max(x, y);
}
inline fn minGeneric(x: anytype, y: anytype) @TypeOf(x) {
    return @min(x, y);
}
inline fn addGeneric(x: anytype, y: anytype) @TypeOf(x) {
    return x + y;
}
inline fn mulGeneric(x: anytype, y: anytype) @TypeOf(x) {
    return x * y;
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
        const a = x.get(1);
        const b = x.get(2);
        const c = x.get(3);
        try std.testing.expectEqual('e', a);
        try std.testing.expectEqual('l', b);
        try std.testing.expectEqual('l', c);
    }
    { // Pythonic indexing...
        const a = x.get(-1);
        const b = x.get(-2);
        const c = x.get(-3);
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

test "Immutable Reductions" {
    const x = Fluent.init(try std.testing.allocator.alloc(i32, 10000));
    defer std.testing.allocator.free(x.items);
    {
        const result = x.fill(2).sum();
        try std.testing.expectEqual(result, 20000);
    }
    {
        const result = x.fill(1).product();
        try std.testing.expectEqual(result, 1);
    }
    {
        x.items[4918] = 999;
        const result = x.max();
        try std.testing.expectEqual(result, 999);
    }
    {
        x.items[9176] = -999;
        const result = x.min();
        try std.testing.expectEqual(result, -999);
    }
}

test "Immutable count" {
    const number = &[_]i32{ 1, 2, 3, 1, 2, 3, 1, 2, 3 };
    const num_scalar = 1;
    const num_sequence = &[_]i32{ 1, 2, 3 };
    const num_any = &[_]i32{ 3, 1 };
    const string = "This is a string";
    const str_scalar = 's';
    const str_sequence = "is";
    const str_any = "sti";

    {
        const result = Fluent.init(number[0..])
            .count(.scalar, num_scalar);
        try std.testing.expect(result == 3);
    }
    {
        const result = Fluent.init(number[0..])
            .count(.sequence, num_sequence);
        try std.testing.expect(result == 3);
    }
    {
        const result = Fluent.init(number[0..])
            .count(.any, num_any);
        try std.testing.expect(result == 6);
    }
    {
        const result = Fluent.init(string[0..])
            .count(.scalar, str_scalar);
        try std.testing.expect(result == 3);
    }
    {
        const result = Fluent.init(string[0..])
            .count(.sequence, str_sequence);
        try std.testing.expect(result == 2);
    }
    {
        const result = Fluent.init(string[0..])
            .count(.any, str_any);
        try std.testing.expect(result == 7);
    }
}

//////////////////////////////////
// Mutable Testing Block ///////

test "Mutable Map Chaining" {
    const string: []const u8 = "A B C D E F G";

    var buffer: [32]u8 = undefined;

    const idx = Fluent.init(buffer[0..string.len])
        .copy(string)
        .lower()
        .sort(.asc)
        .find(.scalar, 'a') orelse unreachable;

    try std.testing.expect(std.mem.eql(u8, buffer[idx..string.len], "abcdefg"));
}

test "Mutable Rotate" {
    const string: []const u8 = "abc";

    var buffer: [32]u8 = undefined;

    const x = Fluent.init(buffer[0..string.len])
        .copy(string)
        .rotate(-3);

    std.debug.print("\n{s}\n\n", .{x.items});

    try std.testing.expect(x.equal("abc"));
}

test "Immutable Starts And Ends With" {
    const x = Fluent.init("abcdefg");

    try std.testing.expect(x.startsWith(.sequence, "abc"));
    try std.testing.expect(x.startsWith(.any, "Z#a"));
    try std.testing.expect(x.startsWith(.scalar, 'a'));

    try std.testing.expect(x.endsWith(.sequence, "efg"));
    try std.testing.expect(x.endsWith(.any, "h8g"));
    try std.testing.expect(x.endsWith(.scalar, 'g'));
}

test "String Backend" {
    const string: []const u8 = "ABCDEFG";

    var buffer: [32]u8 = undefined;

    {
        const result = Fluent.init(buffer[0..string.len])
            .copy(string)
            .isUpper();

        try std.testing.expect(result);
    }
    {
        const result = Fluent.init(buffer[0..string.len])
            .copy(string)
            .isAlpha();

        try std.testing.expect(result);
    }
    {
        const x = Fluent.init(buffer[0..string.len])
            .copy(string)
            .lower()
            .capitalize();

        try std.testing.expect(x.equal("Abcdefg"));
    }
}
