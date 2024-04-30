const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;
// name disambiguation
const Fluent = @This();

////////////////////////////////////////////////////////////////////////////////
// Public Fluent Interface Access Point                                      ///
////////////////////////////////////////////////////////////////////////////////

pub fn init(slice: anytype) FluentInterface(@TypeOf(slice)) {
    return .{ .items = slice };
}

fn FluentInterface(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const DataType = DeepChild(T);

        pub const SliceType = if (isConst(T)) []const DataType else []DataType;

        items: SliceType,

        // we can detect if we have a const slice or non-const
        // and dispatch to different versions of this thing
        // depending on the circumstance.

        pub usingnamespace if (isConst(T))
            ImmutableBackend(Self)
        else
            MutableBackend(Self);

        pub usingnamespace if (DataType == u8) blk: {
            break :blk if (isConst(T))
                ImmutableStringBackend(Self)
            else
                MutableStringBackend(Self);
        } else struct {};

        pub fn iterator(
            self: Self,
            comptime mode: IteratorMode,
        ) BaseIterator(DataType, mode) {
            return Fluent.iterator(mode, self.items);
        }
    };
}

////////////////////////////////////////////////////////////////////////////////
// Public Fluent Iterator Access Point                                      ////
////////////////////////////////////////////////////////////////////////////////

pub const IteratorMode = enum { forward, reverse };

pub fn BaseIterator(comptime T: type, mode: IteratorMode) type {
    return IteratorInterface(T, mode, void{}, identity);
}

pub fn iterator(
    comptime mode: IteratorMode,
    items: anytype,
) BaseIterator(DeepChild(@TypeOf(items)), mode) {
    const T = DeepChild(@TypeOf(items));

    if (comptime !isSlice(@TypeOf(items))) {
        return iterator(mode, @as([]const T, items));
    }

    const P = [*c]const T;

    const ptr: [*c]const T = if (comptime mode == .forward)
        @as(P, @ptrCast(items.ptr))
    else
        (@as(P, @ptrCast(items.ptr)) + items.len) - 1;

    const end: [*c]const T = if (comptime mode == .forward)
        @as(P, @ptrCast(items.ptr)) + items.len
    else
        @as(P, @ptrCast(items.ptr)) - 1;

    return .{
        .ptr = ptr,
        .end = end,
        .stride = 1,
    };
}

pub fn MatchIterator(
    comptime expression: []const u8,
) type {
    return struct {
        const Self = @This();
        const tree = ParseRegexTree(expression);
        items: []const u8,
        index: usize,

        pub fn init(items: []const u8) Self {
            return .{ .items = items, .index = 0 };
        }

        pub fn next(self: *Self) ?[]const u8 {
            while (self.index < self.items.len) : (self.index += 1) {
                if (tree.call(self.items[self.index..], 0)) |last| {
                    defer self.index += last;
                    return self.items[self.index..][0..last];
                }
            }
            return null;
        }
    };
}

pub fn match(
    comptime expression: []const u8,
    string: []const u8,
) MatchIterator(expression) {
    return MatchIterator(expression).init(string);
}

////////////////////////////////////////////////////////////////////////////////
// UNARY FUNCTION ADAPTER :                                                   //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// chain: combine multiple unary functions into a single in-order call

pub fn chain(
    comptime unary_tuple: anytype,
) type {
    return struct {
        pub fn call(x: anytype) @TypeOf(@call(.auto, unwrap, .{ 0, unary_tuple, default(@TypeOf(x)) })) {
            return @call(.always_inline, unwrap, .{ 0, unary_tuple, x });
        }
    };
}

fn unwrap(
    comptime pos: usize,
    comptime unary_tuple: anytype,
    arg: anytype,
) if (pos < tupleSize(unary_tuple))
    @TypeOf(unary_tuple[pos](default(@TypeOf(arg))))
else
    @TypeOf(arg) {
    // this is a forward-unwrap that passes
    // outcomes of one function to the next
    if (comptime pos == tupleSize(unary_tuple)) {
        return arg;
    }
    return @call(.always_inline, unwrap, .{ (pos + 1), unary_tuple, @call(.always_inline, unary_tuple[pos], .{arg}) });
}

//////////////////////////////////////////////////////////////////////
// bind: affix comptime arguments to the front of a function

pub fn bind(
    comptime bind_tuple: anytype,
    comptime function: anytype,
) bindReturn(bind_tuple, function) {
    const bind_count = comptime tupleSize(bind_tuple);
    const total_count = comptime @typeInfo(@TypeOf(function)).Fn.params.len;

    if (comptime total_count - bind_count == 1) {
        return struct {
            pub fn call(x: anytype) @TypeOf(x) {
                return @call(.always_inline, function, bind_tuple ++ .{x});
            }
        }.call;
    } else {
        return struct {
            pub fn call(x: anytype, y: anytype) @TypeOf(x) {
                return @call(.always_inline, function, bind_tuple ++ .{ x, y });
            }
        }.call;
    }
}

fn bindReturn(
    comptime bind_tuple: anytype,
    comptime function: anytype,
) type {
    const total_count = comptime @typeInfo(@TypeOf(function)).Fn.params.len;
    const bind_count = comptime tupleSize(bind_tuple);

    if (comptime total_count < bind_count)
        @compileError("too many arguments to bind");

    if (comptime total_count - bind_count > 2)
        @compileError("fluent bind must result in unary or binary function");

    const choices = struct {
        pub fn unary(x: anytype) @TypeOf(x) {
            return x;
        }
        pub fn binary(x: anytype, y: anytype) @TypeOf(x) {
            _ = &y;
            return x;
        }
    };
    return if (comptime (total_count - bind_count) == 1)
        @TypeOf(choices.unary)
    else
        @TypeOf(choices.binary);
}

////////////////////////////////////////////////////////////////////////////////
//                        Backends and Implementation                         //
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Iterator Interface Implementation:                                         //
////////////////////////////////////////////////////////////////////////////////

fn IteratorInterface(
    comptime DataType: type,
    mode: IteratorMode,
    comptime filters: anytype, // tuple or function
    comptime transforms: anytype, // tuple or function
) type {
    return struct {
        const Self = @This();
        const Mode = mode;

        ptr: [*c]const DataType,
        end: [*c]const DataType,
        stride: usize,

        pub fn next(self: *Self) ?DataType {
            if (comptime @TypeOf(filters) != void) {
                // apply single filter or tuple of filters
                switch (comptime @typeInfo(@TypeOf(filters))) {
                    .Fn => {
                        if (comptime Mode == .forward) {
                            while (self.ptr < self.end and !filters(self.ptr.*))
                                self.ptr += self.stride;
                        } else {
                            while (self.ptr > self.end and !filters(self.ptr.*))
                                self.ptr -= self.stride;
                        }
                    },
                    else => outer: { // applies inline filters
                        if (comptime Mode == .forward) {
                            inner: while (self.ptr < self.end) : (self.ptr += self.stride) {
                                inline for (filters) |f| {
                                    if (!f(self.ptr.*)) continue :inner;
                                }
                                break :outer;
                            }
                        } else {
                            inner: while (self.ptr > self.end) : (self.ptr -= self.stride) {
                                inline for (filters) |f| {
                                    if (!f(self.ptr.*)) continue :inner;
                                }
                                break :outer;
                            }
                        }
                    },
                }
            }

            // unpack transforms into single transform call
            const transform = comptime if (@typeInfo(@TypeOf(transforms)) == .Fn)
                transforms
            else
                Fluent.chain(transforms).call;

            switch (comptime Mode) {
                .forward => {
                    if (self.ptr < self.end) {
                        defer self.ptr += self.stride;
                        return @call(.always_inline, transform, .{self.ptr.*});
                    }
                },
                .reverse => {
                    if (self.ptr > self.end) {
                        defer self.ptr -= self.stride;
                        return @call(.always_inline, transform, .{self.ptr.*});
                    }
                },
            }
            return null;
        }

        pub fn strided(
            self: Self,
            stride_size: usize,
        ) Self {
            return .{
                .ptr = self.ptr,
                .end = self.end,
                .stride = stride_size,
            };
        }

        pub fn window(
            self: *Self,
            window_size: usize,
        ) ?[]const DataType {
            switch (comptime Mode) {
                .forward => {
                    if (self.ptr + window_size <= self.end) {
                        defer _ = self.next();
                        return self.ptr[0..window_size];
                    }
                },
                .reverse => {
                    if ((self.ptr + 1) - window_size > self.end) {
                        defer _ = self.next();
                        return ((self.ptr + 1) - window_size)[0..window_size];
                    }
                },
            }
            return null;
        }

        pub fn map(
            self: Self,
            comptime new_transforms: anytype,
        ) IteratorInterface(DataType, Mode, filters, new_transforms) {
            return .{
                .ptr = self.ptr,
                .end = self.end,
                .stride = self.stride,
            };
        }

        pub fn filter(
            self: Self,
            comptime new_filters: anytype,
        ) IteratorInterface(DataType, Mode, new_filters, transforms) {
            return .{
                .ptr = self.ptr,
                .end = self.end,
                .stride = self.stride,
            };
        }

        pub fn write(
            self: anytype, // for both const and non-const pointers
            items: []DataType,
        ) usize {
            // enable chaining without temporaries
            if (comptime isConst(@TypeOf(self))) {
                var tmp = self.*;
                return tmp.write(items);
            }
            var count: usize = 0;
            while (count < items.len) : (count += 1) {
                items[count] = self.next() orelse return count;
            }
            return count;
        }

        pub fn reduce(
            self: anytype, // for both const and non-const pointers
            comptime T: type,
            comptime binary_func: anytype, // single binary function
            initial: T,
        ) T {
            // enable chaining without temporaries
            if (comptime isConst(@TypeOf(self))) {
                var tmp = self.*;
                return tmp.reduce(T, binary_func, initial);
            }
            var rdx = initial;
            while (self.next()) |x| {
                rdx = @call(.always_inline, binary_func, .{ rdx, x });
            }
            return rdx;
        }
    };
}

////////////////////////////////////////////////////////////////////////////////
// IMMUTABLE BACKEND :                                                        //
//                                                                            //
// Used by mutable backend - only suports non-mutating                        //
// operations over items. Primarily used for reducing,                        //
// scanning, and indexing. Provides non-mutating iterator                     //
// support for both Immutable and Mutable backends.                           //
////////////////////////////////////////////////////////////////////////////////

fn ImmutableBackend(comptime Self: type) type {
    return struct {

        ///////////////////////
        //  PUBLIC SECTION   //
        ///////////////////////

        pub fn all(self: Self, predicate: fn (Self.DataType) bool) bool {
            return for (self.items) |x| {
                if (!predicate(x)) break false;
            } else true;
        }

        pub fn none(self: Self, predicate: fn (Self.DataType) bool) bool {
            return for (self.items) |x| {
                if (predicate(x)) break false;
            } else true;
        }

        pub fn findFrom(
            self: Self,
            comptime mode: FluentMode,
            start_index: usize,
            needle: Parameter(Self.DataType, mode),
        ) ?usize {
            return switch (mode) {
                .any => std.mem.indexOfAnyPos(Self.DataType, self.items, start_index, needle),
                .scalar => std.mem.indexOfScalarPos(Self.DataType, self.items, start_index, needle),
                .sequence => std.mem.indexOfPos(Self.DataType, self.items, start_index, needle),
            };
        }

        pub fn containsFrom(
            self: Self,
            comptime mode: FluentMode,
            start_index: usize,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            return findFrom(self, mode, start_index, needle) != null;
        }

        pub fn find(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) ?usize {
            return findFrom(self, mode, 0, needle);
        }

        pub fn contains(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            return find(self, mode, needle) != null;
        }

        pub fn getAt(self: Self, idx: anytype) Self.DataType {
            return self.items[wrapIndex(self.items.len, idx)];
        }

        pub fn startsWith(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            if (self.items.len == 0)
                return false;

            return switch (mode) {
                .any => blk: {
                    for (needle) |n| {
                        if (self.getAt(0) == n) break :blk true;
                    } else break :blk false;
                },
                .sequence => std.mem.startsWith(Self.DataType, self.items, needle),
                .scalar => self.getAt(0) == needle,
            };
        }

        pub fn endsWith(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            if (self.items.len == 0)
                return false;

            return switch (mode) {
                .any => blk: {
                    for (needle) |n| {
                        if (self.getAt(-1) == n) break :blk true;
                    } else break :blk false;
                },
                .sequence => std.mem.endsWith(Self.DataType, self.items, needle),
                .scalar => self.getAt(-1) == needle,
            };
        }

        /// supported opt = {all, leading, trailing, until, periphery, inside, inverse}
        pub fn count(self: Self, opt: CountOption, comptime mode: FluentMode, needle: Parameter(Self.DataType, mode)) usize {
            if (self.items.len == 0) return 0;

            return switch (opt) {
                .all => countAll(self, mode, needle),
                .leading => countLeading(self, mode, needle),
                .trailing => countTrailing(self, mode, needle),
                .periphery => countLeading(self, mode, needle) + countTrailing(self, mode, needle),
                .until => countUntil(self, mode, needle),
                .inside => countAll(self, mode, needle) - (countLeading(self, mode, needle) + countTrailing(self, mode, needle)),
                .inverse => self.items.len - countAll(self, mode, needle),
            };
        }

        pub fn slice(self: Self, i: anytype, j: anytype) Self {
            const I = @TypeOf(i);
            const J = @TypeOf(j);

            if (comptime !isInteger(I) or !isInteger(J))
                @compileError("slicing requires integer types.");

            // this has extended slicing behaviour similar to python
            // it can never return an out-of-bounds value, only empty
            // ranges or values truncated to 0 or items.len

            if (comptime isUnsigned(I) and isUnsigned(J)) {
                const a: usize = @min(i, self.items.len);
                const b: usize = @max(a, @min(j, self.items.len));
                return .{ .items = if (a < b) self.items[a..b] else self.items[0..0] };
            } else if (comptime I != J) { // default to isize version
                self.slice(@as(isize, @intCast(i)), @as(isize, @intCast(j)));
            } else {
                const l: isize = @intCast(self.items.len);
                const a: usize = wrapIndex(self.items.len, @min(@max(-l, i), l));
                const b: usize = wrapIndex(self.items.len, @min(@max(-l, j), l));
                return .{ .items = if (a < b) self.items[a..b] else self.items[0..0] };
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

        pub fn sum(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Add, add, self.items, reduceInit(ReduceOp.Add, Self.DataType) });
        }
        pub fn product(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Mul, mul, self.items, reduceInit(ReduceOp.Mul, Self.DataType) });
        }

        pub fn min(self: Self) ?Self.DataType {
            return if (self.items.len == 0) null else @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Min, Fluent.min, self.items, reduceInit(ReduceOp.Min, Self.DataType) });
        }
        pub fn max(self: Self) ?Self.DataType {
            return if (self.items.len == 0) null else @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Max, Fluent.max, self.items, reduceInit(ReduceOp.Max, Self.DataType) });
        }

        pub fn write(self: Self, out_buffer: []Self.DataType) Self {
            if (self.items.len == 0) return self;
            std.debug.assert(self.items.len < out_buffer.len);
            @memcpy(out_buffer[0..self.items.len], self.items);
            return (self);
        }

        pub fn print(self: Self, comptime format: []const u8) Self {
            // this is intended to work similarly to std.log.info
            const stderr = std.io.getStdErr();
            defer stderr.close();
            const writer = stderr.writer();
            std.debug.getStderrMutex().lock();
            defer std.debug.getStderrMutex().unlock();
            writer.print(format, .{self.items}) catch {};
            return self;
        }

        pub fn sample(self: Self, random: std.Random, size: usize) []const Self.DataType {
            std.debug.assert(size <= self.items.len);

            if (size == self.items.len)
                return self.items;

            const start = random.intRangeAtMost(usize, 0, self.items.len - size);

            return self.items[start..][0..size];
        }

        pub fn reduce(
            self: Self,
            comptime reduce_type: type,
            comptime binary_func: anytype,
            initial: reduce_type,
        ) reduce_type {
            var rdx = initial;
            for (self.items) |x| {
                rdx = @call(.always_inline, binary_func, .{ rdx, x });
            }
            return rdx;
        }

        pub fn mapReduce(
            self: Self,
            comptime reduce_type: type,
            comptime unary_func: anytype,
            comptime binary_func: anytype,
            initial: reduce_type,
        ) reduce_type {
            const unary_call = comptime if (@typeInfo(@TypeOf(unary_func)) == .Fn)
                unary_func
            else
                chain(unary_func).call;

            var rdx = initial;
            for (self.items) |x| {
                const y = @call(.always_inline, unary_call, .{x});
                rdx = @call(.always_inline, binary_func, .{ rdx, y });
            }
            return rdx;
        }

        pub fn concat(
            self: Self,
            items: []const Self.DataType,
            concat_buffer: []Self.DataType,
        ) FluentInterface([]Self.DataType) {
            var concat_index: usize = self.items.len;
            @memcpy(concat_buffer[0..self.items.len], self.items);
            @memcpy(concat_buffer[concat_index..][0..items.len], items);
            concat_index += items.len;
            return .{ .items = concat_buffer[0..concat_index] };
        }

        pub fn join(
            self: Self,
            collection: []const []const Self.DataType,
            join_buffer: []Self.DataType,
        ) FluentInterface([]Self.DataType) {
            std.debug.assert(self.items.len < join_buffer.len);
            var curr_idx: usize = self.items.len;

            @memcpy(join_buffer[0..self.items.len], self.items);
            for (collection) |items| {
                std.debug.assert(curr_idx + items.len <= join_buffer.len);
                @memcpy(join_buffer[curr_idx..][0..items.len], items);
                curr_idx += items.len;
            }
            return .{ .items = join_buffer[0..curr_idx] };
        }

        pub fn trim(self: Self, comptime direction: DirectionOption, comptime opt: TrimOptions, actor: Parameter(Self.DataType, opt)) Self {
            if (self.items.len <= 1) return self;
            const start: usize = if (direction == .left or direction == .periphery) trimLeft(self, opt, actor) else 0;
            const end: usize = if (direction == .right or direction == .periphery) trimRight(self, opt, actor) else self.items.len;
            return .{ .items = self.items[start..end] };
        }

        ///////////////////////////////////////////////////
        // Iterator support ///////////////////////////////

        pub fn split(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            delimiter: Parameter(Self.DataType, mode),
        ) std.mem.SplitIterator(Self.DataType, mode) {
            return .{ .index = 0, .buffer = self.items, .delimiter = delimiter };
        }

        pub fn tokenize(
            self: Self,
            comptime mode: std.mem.DelimiterType,
            delimiter: Parameter(Self.DataType, mode),
        ) std.mem.TokenIterator(Self.DataType, mode) {
            return .{ .index = 0, .buffer = self.items, .delimiter = delimiter };
        }

        ///////////////////////
        //  PRIVATE SECTION  //
        ///////////////////////

        fn trimLeft(self: Self, comptime opt: TrimOptions, actor: Parameter(Self.DataType, opt)) usize {
            if (self.items.len <= 1) return 0;
            var start: usize = 0;
            const end: usize = self.items.len;
            switch (opt) {
                .scalar => {
                    while (start < end and self.items[start] == actor) start += 1;
                },
                .predicate => {
                    while (start < end and actor(self.items[start])) start += 1;
                },
                .any => {
                    while (start < end and std.mem.indexOfScalar(Self.DataType, actor, self.items[start]) != null) start += 1;
                },
            }
            return start;
        }

        fn trimRight(self: Self, comptime opt: TrimOptions, actor: Parameter(Self.DataType, opt)) usize {
            if (self.items.len <= 1) return 0;
            const start: usize = 0;
            var end: usize = self.items.len;
            switch (opt) {
                .scalar => {
                    while (end > start and self.items[end - 1] == actor) end -= 1;
                },
                .predicate => {
                    while (end > start and actor(self.items[end - 1])) end -= 1;
                },
                .any => {
                    while (start < end and std.mem.indexOfScalar(Self.DataType, actor, self.items[end - 1]) != null) end -= 1;
                },
            }
            return end;
        }

        /// count the occurence of needles in self.items returns 0 if no match is found
        fn countAll(self: Self, comptime mode: FluentMode, needle: Parameter(Self.DataType, mode)) usize {
            var result: usize = 0;

            switch (mode) {
                .scalar => {
                    for (self.items) |it| {
                        if (it == needle) result += 1;
                    }
                },
                .sequence => result = std.mem.count(Self.DataType, self.items, needle),
                .any => {
                    for (self.items) |it| {
                        for (needle) |n| {
                            if (it == n) result += 1;
                        }
                    }
                },
            }
            return (result);
        }

        fn countLeading(self: Self, comptime mode: FluentMode, needle: Parameter(Self.DataType, mode)) usize {
            var result: usize = 0;
            switch (mode) {
                .scalar => {
                    for (self.items, 0..) |it, i| {
                        if (it != needle) return (i);
                    }
                },
                .sequence => {
                    var win_iter = std.mem.window(Self.DataType, self.items, needle.len, needle.len);
                    while (win_iter.next()) |win| : (result += 1) {
                        if (std.mem.eql(Self.DataType, win, needle) == false) break;
                    }
                },
                .any => {
                    for (self.items) |it| {
                        if (std.mem.containsAtLeast(Self.DataType, needle, 1, &[_]Self.DataType{it}) == false) break;
                        result += 1;
                    }
                },
            }
            return (result);
        }

        fn countUntil(self: Self, comptime mode: FluentMode, needle: Parameter(Self.DataType, mode)) usize {
            var result: usize = 0;
            switch (mode) {
                .scalar => {
                    for (self.items, 0..) |it, i| {
                        if (it == needle) return (i);
                    }
                },
                .sequence => {
                    if (self.items.len < needle.len) return (0);
                    var win_iter = std.mem.window(Self.DataType, self.items, needle.len, needle.len);
                    while (win_iter.next()) |win| : (result += 1) {
                        if (std.mem.eql(Self.DataType, win, needle) == true) break;
                    }
                },
                .any => {
                    for (self.items) |it| {
                        if (std.mem.containsAtLeast(Self.DataType, needle, 1, &[_]Self.DataType{it}) == true) break;
                        result += 1;
                    }
                },
            }
            return (result);
        }

        fn countTrailing(self: Self, comptime mode: FluentMode, needle: Parameter(Self.DataType, mode)) usize {
            var result: usize = 0;
            var rev_iter = std.mem.reverseIterator(self.items);
            switch (mode) {
                .scalar => {
                    while (rev_iter.next()) |item| : (result += 1) {
                        if (item != needle) break;
                    }
                },
                .sequence => {
                    if (self.items.len < needle.len) return 0;
                    var start = self.items.len - needle.len;
                    while (start != 0) : (start -|= needle.len) {
                        const win = self.items[start .. start + needle.len];
                        if (std.mem.eql(Self.DataType, win, needle) == false) break;
                        result += 1;
                    }
                },
                .any => {
                    while (rev_iter.next()) |item| : (result += 1) {
                        if (std.mem.containsAtLeast(Self.DataType, needle, 1, &[_]Self.DataType{item}) == false) break;
                    }
                },
            }
            return (result);
        }
    };
}

////////////////////////////////////////////////////////////////////////////////
// MUTABLE BACKEND                                                            //
//                                                                            //
// Only suports mutating operations on items.                                 //
// Operations include sorting, replacing,                                     //
// permutations, and partitioning.                                            //
////////////////////////////////////////////////////////////////////////////////

fn MutableBackend(comptime Self: type) type {
    return struct {

        ///////////////////////
        //  PUBLIC SECTION   //
        ///////////////////////

        // includes operations like reduce, find, and iterators
        pub usingnamespace ImmutableBackend(Self);

        pub fn sort(self: Self, comptime direction: SortDirection) Self {
            const func = if (direction == .ascending)
                std.sort.asc(Self.DataType)
            else
                std.sort.desc(Self.DataType);

            std.sort.pdq(Self.DataType, self.items, void{}, func);
            return self;
        }

        pub fn fill(self: Self, scalar: Self.DataType) Self {
            @memset(self.items, scalar);
            return self;
        }

        pub fn copy(self: Self, items: []const Self.DataType) Self {
            std.debug.assert(self.items.len >= items.len);
            @memcpy(self.items[0..items.len], items);
            return .{ .items = self.items[0..items.len] };
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

        pub fn reverse(self: Self) Self {
            std.mem.reverse(Self.DataType, self.items);
            return (self);
        }

        pub fn setAt(self: Self, idx: anytype, item: Self.DataType) Self {
            self.items[wrapIndex(self.items.len, idx)] = item;
            return self;
        }

        pub fn replace(
            self: Self,
            opt: ReplaceOption,
            comptime mode: FluentMode,
            this: Parameter(Self.DataType, mode),
            with: Parameter(Self.DataType, mode),
        ) Self {
            if (self.items.len == 0) return self;

            if (opt == .all) return replaceAll(self, mode, this, with);

            if (opt == .first or opt == .periphery)
                _ = replaceFirst(self, mode, this, with);

            if (opt == .last or opt == .periphery)
                _ = replaceLast(self, mode, this, with);

            return .{ .items = self.items[0..] };
        }

        pub fn map(self: Self, unary_func: anytype) Self {
            const unary_call = comptime if (@typeInfo(@TypeOf(unary_func)) == .Fn)
                unary_func
            else
                chain(unary_func).call;

            for (self.items) |*x| x.* = @call(.always_inline, unary_call, .{x.*});
            return self;
        }

        fn shuffle(self: Self, random: std.Random) Self {
            random.shuffle(Self.DataType, self.items);
            return self;
        }

        ///////////////////////
        //  PRIVATE SECTION  //
        ///////////////////////

        fn replaceRange(
            self: Self,
            start: usize,
            end: usize,
            comptime mode: FluentMode,
            with: Parameter(Self.DataType, mode),
        ) Self {
            return switch (mode) {
                .scalar => self.setAt(start, with),
                .sequence, .any => blk: {
                    @memcpy(self.items[start..end], with);
                    break :blk .{ .items = self.items[0..] };
                },
            };
        }

        fn replaceFirst(self: Self, comptime mode: FluentMode, this: Parameter(Self.DataType, mode), with: Parameter(Self.DataType, mode)) Self {
            switch (mode) {
                .scalar => for (self.items) |*item| {
                    if (item.* != this) continue;
                    item.* = with;
                    return (self);
                },
                .sequence => {
                    std.debug.assert(this.len == with.len);
                    var win_iter = self.iterator(.forward);
                    var offset: usize = 0;
                    while (win_iter.window(this.len)) |win| : (offset += 1) {
                        if (!std.mem.eql(Self.DataType, win, this))
                            continue;
                        return replaceRange(self, offset, offset + with.len, mode, with);
                    }
                },
                .any => {
                    std.debug.assert(this.len == with.len);
                    var index: usize = 0;
                    while (index < self.items.len) : (index += 1) {
                        if (std.mem.containsAtLeast(Self.DataType, this, 1, &[_]Self.DataType{self.items[index]})) {
                            for (this, 0..) |ch, at| {
                                if (ch == self.items[index])
                                    return (self.setAt(index, with[at]));
                            }
                        }
                    }
                },
            }
            return self;
        }

        fn replaceLast(
            self: Self,
            comptime mode: FluentMode,
            this: Parameter(Self.DataType, mode),
            with: Parameter(Self.DataType, mode),
        ) Self {
            switch (mode) {
                .scalar => {
                    var rev_iter = std.mem.reverseIterator(self.items);
                    var index: usize = self.items.len - 1;
                    while (rev_iter.next()) |item| : (index -= 1) {
                        if (item != this)
                            continue;
                        return self.setAt(index, with);
                    }
                },
                .sequence => {
                    std.debug.assert(this.len == with.len);
                    var start = self.items.len - this.len;
                    while (start != 0) : (start -|= 1) {
                        const win = self.items[start .. start + this.len];
                        if (std.mem.eql(Self.DataType, win, this) == false)
                            continue;
                        return replaceRange(self, start, start + with.len, mode, with);
                    }
                },
                .any => {
                    var index: usize = self.items.len - 1;
                    var rev_iter = std.mem.reverseIterator(self.items);
                    while (rev_iter.next()) |item| : (index -= 1) {
                        if (std.mem.containsAtLeast(Self.DataType, this, 1, &[_]Self.DataType{item})) {
                            for (this, 0..) |ch, at| {
                                if (ch == self.items[index])
                                    return (self.setAt(index, with[at]));
                            }
                        }
                    }
                },
            }
            return self;
        }

        fn replaceAll(
            self: Self,
            comptime mode: FluentMode,
            this: Parameter(Self.DataType, mode),
            with: Parameter(Self.DataType, mode),
        ) Self {
            switch (mode) {
                .scalar => std.mem.replaceScalar(Self.DataType, self.items, this, with),
                .sequence => {
                    std.debug.assert(this.len == with.len);
                    var win_iter = self.iterator(.forward);
                    var offset: usize = 0;
                    while (win_iter.window(this.len)) |win| : (offset += 1) {
                        if (std.mem.eql(Self.DataType, win, this))
                            _ = replaceRange(self, offset, offset + with.len, mode, with);
                    }
                },
                .any => {
                    std.debug.assert(this.len == with.len);
                    var index: usize = 0;
                    while (index < self.items.len) : (index += 1) {
                        if (std.mem.containsAtLeast(Self.DataType, this, 1, &[_]Self.DataType{self.items[index]})) {
                            for (this, 0..) |ch, at| {
                                if (ch == self.items[index])
                                    _ = self.setAt(index, with[at]);
                            }
                        }
                    }
                },
            }
            return self;
        }
    };
}

////////////////////////////////////////////////////////////////////////////////
// IMMUTABLE BACKEND :                                                        //
//                                                                            //
// Only activated if the child data type is u8                                //
////////////////////////////////////////////////////////////////////////////////

fn ImmutableStringBackend(comptime Self: type) type {
    return struct {

        ///////////////////////
        //  PUBLIC SECTION   //
        ///////////////////////

        pub fn isDigit(self: Self) bool {
            return self.all(std.ascii.isDigit);
        }

        pub fn isAlpha(self: Self) bool {
            return self.all(std.ascii.isAlphabetic);
        }

        pub fn isSpaces(self: Self) bool {
            return self.all(std.ascii.isWhitespace);
        }

        pub fn isLower(self: Self) bool {
            return self.all(std.ascii.isLower);
        }

        pub fn isUpper(self: Self) bool {
            return self.all(std.ascii.isUpper);
        }

        pub fn isHex(self: Self) bool {
            return self.all(std.ascii.isHex);
        }

        pub fn isASCII(self: Self) bool {
            return self.all(std.ascii.isASCII);
        }

        pub fn isPrintable(self: Self) bool {
            return self.all(std.ascii.isPrint);
        }

        pub fn isAlnum(self: Self) bool {
            return self.all(std.ascii.isAlphanumeric);
        }

        pub fn digit(self: Self, comptime T: type) ?T {
            if (comptime !isInteger(T))
                @compileError("digit: requires integer type.");

            return std.fmt.parseInt(T, self.items, 10) catch null;
        }

        pub fn float(self: Self, comptime T: type) ?T {
            if (comptime !isFloat(T))
                @compileError("float: requires floating-point type.");

            return std.fmt.parseFloat(T, self.items) catch null;
        }

        pub fn getToken(self: Self, nth: usize, charset: []const u8) Self {
            var bitset = StringBitSet.init();
            var start: usize = 0;
            var end: usize = 0;
            for (charset) |item| {
                bitset.setValue(item, true);
            }
            for (0..nth) |_| {
                start = end;
                while (start < self.items.len and bitset.isSet(self.items[start])) {
                    start += 1;
                }
                if (start >= self.items.len)
                    return .{ .items = self.items[end..] };
                end = start;
                while (end < self.items.len and !bitset.isSet(self.items[end])) {
                    end += 1;
                }
            }
            return .{ .items = self.items[start..end] };
        }

        pub fn differenceWith(
            self: Self,
            string: []const u8,
            diff_buffer: []u8,
        ) FluentInterface([]Self.DataType) {
            var items_set = StringBitSet.init();
            var string_set = StringBitSet.init();

            for (self.items) |item| {
                items_set.setValue(item, true);
            }

            for (string) |char| {
                string_set.setValue(char, true);
            }
            return .{ .items = items_set.differenceWith(string_set).fillBuffer(diff_buffer) };
        }

        pub fn unionWith(
            self: Self,
            string: []const u8,
            union_buffer: []u8,
        ) FluentInterface([]Self.DataType) {
            var items_set = StringBitSet.init();
            var string_set = StringBitSet.init();

            for (self.items) |item| {
                items_set.setValue(item, true);
            }

            for (string) |char| {
                string_set.setValue(char, true);
            }
            return .{ .items = items_set.unionWith(string_set).fillBuffer(union_buffer) };
        }

        pub fn intersectWith(
            self: Self,
            string: []const u8,
            inter_buffer: []u8,
        ) FluentInterface([]Self.DataType) {
            var items_set = StringBitSet.init();
            var string_set = StringBitSet.init();

            for (self.items) |item| {
                items_set.setValue(item, true);
            }

            for (string) |char| {
                string_set.setValue(char, true);
            }
            return .{ .items = items_set.intersectWith(string_set).fillBuffer(inter_buffer) };
        }

        ///////////////////////
        //  PRIVATE SECTION  //
        ///////////////////////

    };
}

////////////////////////////////////////////////////////////////////////////////
// MUTABLE BACKEND :                                                          //
//                                                                            //
// Only activated if the child data type is u8                                //
////////////////////////////////////////////////////////////////////////////////

fn MutableStringBackend(comptime Self: type) type {
    return struct {

        ///////////////////////
        //  PUBLIC SECTION   //
        ///////////////////////

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

        ///////////////////////
        //  PRIVATE SECTION  //
        ///////////////////////

    };
}

//////////////////////////////////////////////////////////////////////////////////
// STRING BIT SET :                                                         //
//////////////////////////////////////////////////////////////////////////////////

const StringBitSet = struct {
    const BackingSet = std.StaticBitSet(@bitSizeOf(usize));

    bits: [4]BackingSet,

    pub fn init() StringBitSet {
        return .{ .bits = .{
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
        } };
    }

    pub fn setValue(self: *StringBitSet, pos: usize, value: bool) void {
        const mod_pos = pos & 63;
        switch (pos) {
            0...63 => self.bits[0].setValue(mod_pos, value),
            64...127 => self.bits[1].setValue(mod_pos, value),
            128...191 => self.bits[2].setValue(mod_pos, value),
            192...255 => self.bits[3].setValue(mod_pos, value),
            else => unreachable,
        }
    }
    pub fn isSet(self: *const StringBitSet, pos: usize) bool {
        const mod_pos = pos & 63;
        return switch (pos) {
            0...63 => self.bits[0].isSet(mod_pos),
            64...127 => self.bits[1].isSet(mod_pos),
            128...191 => self.bits[2].isSet(mod_pos),
            192...255 => self.bits[3].isSet(mod_pos),
            else => unreachable,
        };
    }
    pub fn unionWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].unionWith(other.bits[0]),
            self.bits[1].unionWith(other.bits[1]),
            self.bits[2].unionWith(other.bits[2]),
            self.bits[3].unionWith(other.bits[3]),
        } };
    }
    pub fn differenceWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].differenceWith(other.bits[0]),
            self.bits[1].differenceWith(other.bits[1]),
            self.bits[2].differenceWith(other.bits[2]),
            self.bits[3].differenceWith(other.bits[3]),
        } };
    }
    pub fn intersectWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].intersectWith(other.bits[0]),
            self.bits[1].intersectWith(other.bits[1]),
            self.bits[2].intersectWith(other.bits[2]),
            self.bits[3].intersectWith(other.bits[3]),
        } };
    }

    pub fn count(self: StringBitSet) usize {
        return self.bits[0].count() + self.bits[1].count() + self.bits[2].count() + self.bits[3].count();
    }

    pub fn fillBuffer(self: *const StringBitSet, buffer: []u8) []u8 {
        var val: usize = 0;
        var pos: usize = 0;
        while (val < 256) : (val += 1) {
            if (self.isSet(val)) {
                buffer[pos] = @intCast(val);
                pos += 1;
            }
        }
        return buffer[0..pos];
    }
};

//////////////////////////////////////////////////////////////////////////////////
// ENUMERATED OPTIONS :                                                         //
//////////////////////////////////////////////////////////////////////////////////

const CountOption = enum {
    all,
    leading,
    trailing,
    until,
    periphery,
    inside,
    inverse,
};

const ReplaceOption = enum {
    first,
    last,
    all,
    periphery,
};

const DirectionOption = enum {
    left,
    right,
    periphery,
};

const TrimOptions = enum {
    scalar,
    predicate,
    any,
};

const SortDirection = enum {
    ascending,
    descending,
};

const SampleOption = enum {
    scalar,
    sequence,
};

// any, sequence, scalar
const FluentMode = std.mem.DelimiterType;

////////////////////////////////////////////////////////////////////////////////
// PRIVATE HELPERS :                                                          //
////////////////////////////////////////////////////////////////////////////////

fn isConst(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .Pointer => |ptr| return ptr.is_const,
        else => @compileError("Type must coercible to a slice."),
    }
}

fn isSlice(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Pointer => |ptr| ptr.size == .Slice,
        else => false,
    };
}

fn isInteger(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int, .ComptimeInt => true,
        else => false,
    };
}

fn isUnsigned(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Int => |i| i.signedness == .unsigned,
        else => false,
    };
}

fn tupleSize(comptime tuple: anytype) usize {
    return switch (@typeInfo(@TypeOf(tuple))) {
        .Struct => |s| s.fields.len,
        else => @compileError("type must be a tuple"),
    };
}

fn default(comptime T: type) T {
    if (comptime T == bool) {
        return true;
    }
    return 0;
}

// bypasses iterator transform
inline fn identity(x: anytype) @TypeOf(x) {
    return x;
}

fn isFloat(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .Float => true,
        else => false,
    };
}

fn Parameter(comptime T: type, comptime mode: anytype) type {
    const param_types = std.StaticStringMap(type).initComptime(.{
        .{ "any", []const T },
        .{ "scalar", T },
        .{ "sequence", []const T },
        .{ "range", struct { start: usize, end: usize } },
        .{ "predicate", fn (T) bool },
    });
    return comptime param_types.get(@tagName(mode)) orelse unreachable;
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
            math.inf(T),
        .Max => if (comptime info == .Int)
            math.minInt(T)
        else
            -math.inf(T),
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
    // Special thanks to the user "nyc" over at the Ziggit forum

    var ptr: [*c]const T = @ptrCast(items.ptr);
    const end: [*c]const T = ptr + items.len;

    var rdx: T = blk: {
        if (comptime std.simd.suggestVectorLength(T)) |N| {
            if (items.len < N)
                break :blk initial;

            var vec_rdx: @Vector(N, T) = @splat(initial);

            while (ptr + N <= end) : (ptr += N) {
                vec_rdx = @call(.always_inline, BinaryFunc, .{ vec_rdx, @as(*const @Vector(N, T), @ptrCast(@alignCast(ptr))).* });
            }
            break :blk @reduce(ReduceType, vec_rdx);
        } else {
            break :blk initial;
        }
    };

    while (ptr < end) : (ptr += 1) {
        rdx = @call(.always_inline, BinaryFunc, .{ rdx, ptr.* });
    }

    return rdx;
}

// these work for @Vector as well as scalar types
pub inline fn max(x: anytype, y: anytype) @TypeOf(x, y) {
    return @max(x, y);
}
pub inline fn min(x: anytype, y: anytype) @TypeOf(x, y) {
    return @min(x, y);
}
pub inline fn add(x: anytype, y: anytype) @TypeOf(x, y) {
    return x + y;
}
pub inline fn sub(x: anytype, y: anytype) @TypeOf(x, y) {
    return x - y;
}
pub inline fn div(x: anytype, y: anytype) @TypeOf(x, y) {
    return x / y;
}
pub inline fn mul(x: anytype, y: anytype) @TypeOf(x, y) {
    return x * y;
}
pub inline fn negate(x: anytype) @TypeOf(x) {
    return -x;
}

/////////////////////////////////////////////////
// REGEX                                       //
/////////////////////////////////////////////////

const RegexQuantifier = union(enum) {
    any: void, // *
    one_or_more: void, // +
    optional: void, // ?
    exact: usize, // {n}
    between: struct { start: usize, stop: usize }, // {i,j}
};

const RegexEscaped = struct {
    escaped: bool,
    char: u8,
};

const RegexCharacter = struct {
    in_square: bool, // are we a regex char set?
    escaped: bool,
    negated: bool,
    char: u8,
};

// TODO: change to RegexSymbol?
const RegexSymbol = union(enum) {
    s: RegexCharacter,
    q: RegexQuantifier,
};

fn isRegexFilter(symbol: RegexEscaped) bool {
    return symbol.escaped and switch (symbol.char) {
        'w', 'W', 's', 'S', 'd', 'D', '.' => true,
        else => false,
    };
}

fn isRegexQuantifier(symbol: RegexEscaped) bool {
    return !symbol.escaped and switch (symbol.char) {
        '+', '?', '*', '{' => true,
        else => false,
    };
}

fn isRegexBracket(symbol: RegexCharacter) bool {
    return !symbol.escaped and switch (symbol.char) {
        '(', ')', '[', ']' => true,
        else => false,
    };
}

fn bracketSet(comptime symbol: RegexCharacter) []const u8 {
    const head: u8 = if (symbol.char == '(') '(' else '[';
    const tail: u8 = if (symbol.char == '(') ')' else ']';
    return &.{ head, tail };
}

fn parseQuantity(comptime escaped: []const RegexEscaped) usize {
    comptime var count: usize = 0;
    comptime var coefficient: usize = 1;
    comptime var i: usize = escaped.len;
    while (i > 0) {
        i -= 1;

        if (comptime !std.ascii.isDigit(escaped[i].char)) {
            @compileError("parseQuantity: invalid char");
        }

        const value = escaped[i].char - '0';
        count += value * coefficient;
        coefficient *= 10;
    }
    return count;
}

fn fuseEscapes(
    comptime str: []const u8,
) []const RegexEscaped {

    // TODO:
    //   consider making this return a direct
    //   array instead a slice - we don't need
    //   to keep it around for runtime

    if (comptime str.len == 0) {
        @compileError("fuseEscapes: cannot parse empty string");
    }

    // the symbol stack to return
    comptime var es: [str.len]RegexEscaped = undefined;

    // track if last char was escape - '\'
    comptime var escaped: bool = false;

    // current symbol index
    comptime var idx: usize = 0;

    // TODO check for invalid escape character
    //'w', 'W', 's', 'S', 'd', 'D', '.', '(', ')', '[', ']'
    // '+', '?', '*', '{', '-'
    for (str) |char| {
        if (char == '\\' and !escaped) {
            escaped = true;
            continue;
        }

        es[idx] = .{ .escaped = escaped, .char = char };

        escaped = false;

        idx += 1;
    }

    if (comptime escaped) {
        @compileError("fuseEscapes: unused escape symbol");
    }

    // TODO: consider moving below to separate function

    // freeze comptime state
    const es_ = es;

    return es_[0..idx];
}

fn analyzeRegexTokens(comptime sq: []const RegexSymbol) []const RegexSymbol {
    comptime {
        const tag = std.meta.activeTag;

        var i: usize = 0;
        var j: usize = 2;
        while (j < sq.len) : ({
            i += 1;
            j += 1;
        }) {
            // any quantifier that is followed by an identical character is either invalid or idempotent
            // a + a  -> invalid
            // a + a? -> idempotent
            if (tag(sq[i]) == .s and tag(sq[j]) == .s) {
                if (tag(sq[i + 1]) != .q)
                    continue;

                const a = sq[i].s;
                const b = sq[j].s;

                if (!std.meta.eql(a, b))
                    continue;

                const q = sq[i + 1].q;

                if (tag(q) != .optional) {
                    const fmt: []const u8 = &[_]u8{ ' ', a.char, '-' } ++ @tagName(tag(q)) ++ &[_]u8{ '-', b.char };
                    @compileError("Invalid quantifier sequence:" ++ fmt);
                }

                // only a?a? is valid - check for just a?a sequences
                if (j + 1 >= sq.len or tag(sq[j + 1]) != .q or tag(sq[j + 1].q) != .optional) {
                    const fmt: []const u8 = &[_]u8{ ' ', a.char, '-' } ++ @tagName(tag(q)) ++ &[_]u8{ '-', b.char };
                    @compileError("Invalid optional ordering:" ++ fmt);
                }
            }
        }

        // backtracking optimizations
        var _sq: [sq.len]RegexSymbol = sq[0..].*;

        i = 0;
        while (i < _sq.len) : (i += 1){

            if (tag(_sq[i]) != .s or i + 3 > _sq.len)
                continue;

            //@compileLog("In loop");

            const s = _sq[i].s;

            // convert .*a -> ~a*a (only if a is not a bracket)
            if (s.char == '.' and !s.escaped and tag(_sq[i + 1]) == .q and tag(_sq[i + 2]) == .s) {
                const u = _sq[i + 2].s;
                if (isRegexBracket(u))
                    continue;

                //@compileLog(u);
                _sq[i] = .{ .s = u };
                _sq[i].s.negated = true;
            }
        }
        const __sq = _sq;
        return __sq[0..];
    }
}

fn fuseQuantifiers(
    comptime es: []const RegexEscaped,
) []const RegexSymbol {
    comptime {
        if (isRegexQuantifier(es[0])) {
            @compileError("fuseQuantifiers: 0th symbol cannot be a quanitifier");
        }

        // the symbol stack to return
        var sq: [es.len]RegexSymbol = undefined;

        // check if we are within a [] clause
        var in_square: bool = false;
        var square_head: usize = 0;
        var square_tail: usize = 0;
        var negated: bool = false;

        // current symbol index
        var i: usize = 0;
        var j: usize = 0;
        var last_quantifier: bool = false;

        // i gets incremented at loop end
        while (j < es.len) : (j += 1) {

            // implements set syntax: [abc] -> a, b, or c
            if (es[j].char == '[' and !es[j].escaped and !in_square) {
                square_head = j;
                square_tail = closingBracketEscaped(es, "[]", j);
                in_square = true;
            }

            // remove set-level negation and keep indicated escapes
            if (es[j].char == ']' and !es[j].escaped and in_square and j == square_tail) {
                in_square = false;
                negated = false;
            }

            // implements negated set syntax: [^abc] -> not a, b, or c
            if (es[j].char == '^' and in_square and (j -| 1) == square_head) {
                negated = true;
                continue;
            }

            if (!isRegexQuantifier(es[j]) or in_square) {
                last_quantifier = false;

                // every bracket within an [] clause is escaped
                const override_escape: bool = in_square and switch (es[j].char) {
                    '(', ')', '[', ']', '{', '}', '.' => (j != square_head and j != square_tail),
                    else => false,
                };

                sq[i] = .{
                    .s = .{
                        // we don't want square brackets to be within themselves...
                        .in_square = in_square and j != square_head and j != square_tail,
                        .escaped = es[j].escaped or override_escape,
                        .negated = negated and in_square,
                        .char = es[j].char,
                    },
                };
            } else {
                if (last_quantifier) {
                    @compileError("fuseQuantifiers: invalid quantifier");
                }

                last_quantifier = true;

                switch (es[j].char) {
                    '+' => {
                        sq[i] = .{ .q = .{ .one_or_more = void{} } };
                    },
                    '*' => {
                        sq[i] = .{ .q = .{ .any = void{} } };
                    },
                    '?' => {
                        sq[i] = .{ .q = .{ .optional = void{} } };
                    },
                    '{' => {
                        // scan forward, find closing brace, parse digits

                        j += 1; // move off opening brace

                        const range_i = j;
                        var range_j = j;
                        var comma = j;

                        scan: while (range_j < es.len) : (range_j += 1) {
                            switch (es[range_j].char) {
                                '}' => break :scan,
                                ',' => {
                                    comma = range_j;
                                    continue;
                                },
                                '0'...'9' => continue,
                                else => @compileError("fuseQuantifiers: invalid char in range"),
                            }
                        } else {
                            @compileError("fuseQuantifiers: unmatched '}' char");
                        }

                        if (es[range_j].escaped) {
                            @compileError("fuseQuantifiers: invalid char in range");
                        }

                        // {i,j}
                        if (range_i < comma) {
                            const start: usize = parseQuantity(es[range_i..comma]);
                            const stop: usize = parseQuantity(es[comma + 1 .. range_j]);

                            if (start >= stop) {
                                @compileError("fuseQuantifiers: invalid range");
                            }

                            sq[i] = .{ .q = .{ .between = .{ .start = start, .stop = stop } } };
                        } else {
                            const count: usize = parseQuantity(es[range_i..range_j]);

                            if (count == 0) {
                                @compileError("fuseQuantifiers: exact quantifier cannot be 0");
                            }

                            sq[i] = .{ .q = .{ .exact = count } };
                        }

                        j = range_j;
                    },
                    else => {},
                }
            }

            // this is all the way down here because
            // certain charactes can be skipped.
            i += 1;
        }

        const _sq = sq;

        return analyzeRegexTokens(_sq[0..i]);
    }
}

fn closingBracket(
    comptime sq: []const RegexSymbol,
    comptime braces: []const u8,
    comptime idx: usize,
) usize {
    comptime var count: isize = @intFromBool(sq[idx].s.char == braces[0]);

    if (comptime count == 0) {
        @compileError("closingBracket: must start on opening brace");
    }
    comptime var i: usize = idx + 1;
    while (i < sq.len) : (i += 1) {
        switch (sq[i]) {
            .s => |s| {
                count += @intFromBool(s.char == braces[0] and !s.escaped);
                count -= @intFromBool(s.char == braces[1] and !s.escaped);
                if (count == 0) return i;
            },
            else => continue,
        }
    }
    @compileError("closingBracket: no closing brace found");
}

fn closingBracketEscaped(
    comptime es: []const RegexEscaped,
    comptime braces: []const u8,
    comptime idx: usize,
) usize {
    comptime var count: isize = @intFromBool(es[idx].char == braces[0]);

    if (comptime count == 0) {
        @compileError("closingBracket: must start on opening brace");
    }
    comptime var i: usize = idx + 1;
    while (i < es.len) : (i += 1) {
        count += @intFromBool(es[i].char == braces[0] and !es[i].escaped);
        count -= @intFromBool(es[i].char == braces[1] and !es[i].escaped);
        if (count == 0) return i;
    }
    @compileError("closingBracket: no closing brace found");
}

fn pipeSearch(
    comptime sq: []const RegexSymbol,
    comptime idx: usize,
) usize {
    comptime var i: usize = idx;
    while (i < sq.len) : (i += 1) {
        switch (sq[i]) {
            .s => |s| switch (s.char) {
                '|' => if (s.escaped) continue else return i,
                '(' => i = closingBracket(sq, "()", i),
                '[' => i = closingBracket(sq, "[]", i),
                '{' => i = closingBracket(sq, "{}", i),
                ')', ']', '}' => @compileError("pipeSearch: invalid braces"),
                else => continue,
            },
            else => continue,
        }
    }
    return i;
}

fn RegexOR(
    // used for "|" or [abc] clauses
    comptime lhs: type,
    comptime rhs: type,
) type {
    return struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            if (comptime @hasDecl(rhs, "call")) {
                return lhs.call(str, i) orelse rhs.call(str, i);
            } else {
                return lhs.call(str, i);
            }
        }
    };
}

fn RegexAND(
    // used for anything outside of [] clauses,
    comptime this: type,
    comptime next: type,
) type {
    return struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            const j = this.call(str, i) orelse return null;
            if (comptime @hasDecl(next, "call")) {
                return next.call(str, j);
            } else {
                return j;
            }
        }
    };
}

fn RegexNAND(
    // only used for [^abc] type clauses,
    // should only appear in that context
    comptime this: type,
    comptime next: type,
) type {
    return struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            const j = this.call(str, i) orelse return null;
            if (comptime @hasDecl(next, "call")) {
                return next.call(str, i);
            } else {
                return j;
            }
        }
    };
}

fn RegexUnit(
    comptime callable: anytype,
    comptime quantifier: ?RegexQuantifier,
) type {
    return if (@typeInfo(@TypeOf(callable)) == .Fn) struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            if (comptime quantifier) |q| {
                var idx: usize = i;

                switch (comptime q) {
                    .exact => |n| {
                        for (0..n) |_| {
                            if (idx < str.len and callable(str[idx])) {
                                idx += 1;
                            } else {
                                return null;
                            }
                        }
                    },
                    .between => |b| {
                        var count: usize = 0;
                        while (idx < str.len and count < b.stop) : ({
                            count += 1;
                            idx += 1;
                        }) {
                            if (!callable(str[idx])) break;
                        }
                        if (count < b.start)
                            return null;
                    },
                    .any => {
                        while (idx < str.len and callable(str[idx])) idx += 1;
                    },
                    .one_or_more => {
                        var count: usize = 0;
                        while (idx < str.len) : ({
                            count += 1;
                            idx += 1;
                        }) {
                            if (!callable(str[idx])) break;
                        }
                        if (count < 1)
                            return null;
                    },
                    .optional => { // can be empty
                        if (idx < str.len and callable(str[idx])) idx += 1;
                    },
                }
                return idx;
            } else {
                return if (i < str.len and callable(str[i])) i + 1 else null;
            }
        }
    } else struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            if (comptime quantifier) |q| {
                var idx: usize = i;

                switch (comptime q) {
                    .exact => |n| {
                        for (0..n) |_| {
                            if (idx < str.len) {
                                idx += callable.call(str[idx..], 0) orelse return null;
                            } else {
                                return null;
                            }
                        }
                    },
                    .between => |b| {
                        var count: usize = 0;
                        while (idx < str.len and count < b.stop) : (count += 1) {
                            idx += callable.call(str[idx..], 0) orelse break;
                        }
                        if (count < b.start)
                            return null;
                    },
                    .any => {
                        while (idx < str.len)
                            idx += callable.call(str[idx..], 0) orelse break;
                    },
                    .one_or_more => {
                        var count: usize = 0;
                        while (idx < str.len) : (count += 1) {
                            idx += callable.call(str[idx..], 0) orelse break;
                        }
                        if (count < 1)
                            return null;
                    },
                    .optional => { // can be empty
                        if (idx < str.len)
                            idx += callable.call(str[idx..], 0) orelse @as(usize, 0);
                    },
                }
                return idx;
            } else {
                return callable.call(str, i);
            }
        }
    };
}

fn ParseRegexTreeBreadth(
    comptime sq: []const RegexSymbol,
    comptime enclosing: u8,
) type {
    comptime {
        if (sq.len == 0)
            return struct {}; // terminal node

        const pipe: usize = pipeSearch(sq, 0);

        if (pipe < sq.len) {
            return RegexOR(
                ParseRegexTreeBreadth(sq[0..pipe], enclosing),
                ParseRegexTreeBreadth(sq[pipe + 1 ..], enclosing),
            );
        } else {
            return ParseRegexTreeDepth(sq, enclosing);
        }
    }
}

fn InvertRegex(
    typical: bool, // what is the function typically?
    inverse: bool, // what does the circumstance indicate?
    function: fn (u8) bool,
) fn (u8) bool {
    const a: u1 = @intFromBool(typical);
    const b: u1 = @intFromBool(inverse);
    if (a ^ b == 1) {
        return function;
    } else { // negate the result
        return struct {
            pub fn call(c: u8) bool {
                return !@call(.always_inline, function, .{c});
            }
        }.call;
    }
}

fn EqualRegex(
    comptime char: u8,
) fn (u8) bool {
    return struct {
        pub fn call(c: u8) bool {
            return c == char;
        }
    }.call;
}

fn SpanRegex(
    comptime a: u8,
    comptime b: u8,
) fn (u8) bool {
    if (comptime a >= b)
        @compileError("Invalid character span: " ++ &[_]u8{ a, '-', b });

    return struct {
        pub fn call(c: u8) bool {
            return a <= c and c <= b;
        }
    }.call;
}

fn anyRegex(_: u8) bool {
    return true;
}

fn ParseRegexTreeDepth(
    comptime sq: []const RegexSymbol,
    comptime enclosing: u8,
) type {
    comptime {
        const tag = std.meta.activeTag;

        if (sq.len == 0)
            return struct {}; // terminal node

        var _sq = sq; // shrinking list

        // this tracks if we're in a [] clause
        // and if we need to use !(x or y) units
        var use_nand: bool = false;

        // deduce function
        const Node: type = switch (_sq[0]) {
            .s => |s| outer: {
                if (isRegexBracket(s)) {
                    // this branch deduces an entire sub-automaton
                    var closing = closingBracket(sq, bracketSet(s), 0);
                    // parse everything between the brackets
                    const T: type = ParseRegexTreeBreadth(sq[1..closing], s.char);
                    // the entire automaton can be quantified
                    const q: ?RegexQuantifier =
                        if (closing + 1 >= _sq.len) null else switch (_sq[closing + 1]) {
                        .q => |q| inner: {
                            closing += 1;
                            break :inner q;
                        },
                        .s => null,
                    };
                    // closing is a bracket or quantifier
                    if (closing + 1 >= _sq.len) {
                        _sq = _sq[0..0]; // exhaust
                    } else {
                        _sq = _sq[closing + 1 ..];
                    }
                    // don't wrap with quantifier if unnecessary
                    break :outer if (q) |_q| RegexUnit(T, _q) else T;
                }

                use_nand = s.negated and s.in_square;

                // implements [a-z] character spans...
                if (_sq.len >= 3 and s.in_square and tag(_sq[1]) == .s and tag(_sq[2]) == .s) {
                    const t = _sq[1].s;
                    const u = _sq[2].s;
                    if (t.char == '-' and !t.escaped and u.in_square) {
                        _sq = _sq[3..];
                        break :outer RegexUnit(InvertRegex(true, s.negated, SpanRegex(s.char, u.char)), null);
                    }
                }

                _sq = _sq[1..]; // pop token

                const q: ?RegexQuantifier =
                    if (0 == _sq.len) null else switch (_sq[0]) {
                    .q => |q| inner: {
                        _sq = _sq[1..]; // pop token
                        break :inner q;
                    },
                    .s => null,
                };

                if (s.escaped) {
                    switch (s.char) {
                        'w' => break :outer RegexUnit(InvertRegex(true, s.negated, std.ascii.isAlphanumeric), q),
                        'W' => break :outer RegexUnit(InvertRegex(false, s.negated, std.ascii.isAlphanumeric), q),
                        'd' => break :outer RegexUnit(InvertRegex(true, s.negated, std.ascii.isDigit), q),
                        'D' => break :outer RegexUnit(InvertRegex(false, s.negated, std.ascii.isDigit), q),
                        's' => break :outer RegexUnit(InvertRegex(true, s.negated, std.ascii.isWhitespace), q),
                        'S' => break :outer RegexUnit(InvertRegex(false, s.negated, std.ascii.isWhitespace), q),
                        else => {},
                    }
                } else {
                    switch (s.char) {
                        '.' => break :outer RegexUnit(anyRegex, q),
                        else => {},
                    }
                }

                // default to direct equals
                break :outer RegexUnit(InvertRegex(true, s.negated, EqualRegex(s.char)), q);
            },
            .q => @compileError("ParseRegexTreeRecursive: leading quantifier"),
        };

        if (use_nand) {
            return RegexNAND(Node, ParseRegexTreeDepth(_sq, enclosing));
        } else if (enclosing == '(') {
            return RegexAND(Node, ParseRegexTreeDepth(_sq, enclosing));
        } else {
            return RegexOR(Node, ParseRegexTreeDepth(_sq, enclosing));
        }
    }
}

fn ParseRegexTree(
    comptime expression: []const u8,
) type {
    return comptime ParseRegexTreeBreadth(fuseQuantifiers(fuseEscapes(expression)), '(');
}

////////////////////////////////////////////////////////////////////////////////
// TESTING BLOCK :                                                           ///
////////////////////////////////////////////////////////////////////////////////

const testing = std.testing;
const testing_allocator = std.testing.allocator;
const expect = std.testing.expect;

////////////////////////////////////////////////////////////////////////////////
// @TEST : IMMUTABLE BACKEND                                                  //
////////////////////////////////////////////////////////////////////////////////

test "findFrom(self, mode, start_index, needle) : scalar" {
    const self = init("This is a test");

    {
        const result = self.findFrom(.scalar, 0, 'T') orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.findFrom(.scalar, 12, 't') orelse unreachable;
        try expect(result == 13);
    }

    {
        const result = self.findFrom(.scalar, 6, 's') orelse unreachable;
        try expect(result == 6);
    }
}

test "findFrom(self, mode, start_index, needle) : sequence" {
    const self = init("This is a test");

    {
        const result = self.findFrom(.sequence, 0, "This") orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.findFrom(.sequence, 9, "test") orelse unreachable;
        try expect(result == 10);
    }

    {
        const result = self.findFrom(.sequence, 5, "is") orelse unreachable;
        try expect(result == 5);
    }
}

test "findFrom(self, mode, start_index, needle) : any" {
    const self = init("This is a test");

    {
        const result = self.findFrom(.any, 0, "T") orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.findFrom(.any, 9, "test") orelse unreachable;
        try expect(result == 10);
    }

    {
        const result = self.findFrom(.any, 5, "is") orelse unreachable;
        try expect(result == 5);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "find(self, mode, needle)                  : scalar" {
    const self = init("This is a testz");

    {
        const result = self.find(.scalar, 'T') orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.find(.scalar, 'z') orelse unreachable;
        try expect(result == self.items.len - 1);
    }

    {
        const result = self.find(.scalar, 'i') orelse unreachable;
        try expect(result == 2);
    }
}

test "find(self, mode, needle)                  : sequence" {
    const self = init("This is a testz");

    {
        const result = self.find(.sequence, "This") orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.find(.sequence, "testz") orelse unreachable;
        try expect(result == 10);
    }

    {
        const result = self.find(.sequence, "is") orelse unreachable;
        try expect(result == 2);
    }
}

test "find(self, mode, needle)                  : any" {
    const self = init("This is a testz");

    {
        const result = self.find(.any, "T") orelse unreachable;
        try expect(result == 0);
    }

    {
        const result = self.find(.any, "z") orelse unreachable;
        try expect(result == self.items.len - 1);
    }

    {
        const result = self.find(.any, "is") orelse unreachable;
        try expect(result == 2);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "containsFrom(self, mode, needle)          : scalar" {
    const self = init("This is a test");

    {
        const result = self.containsFrom(.scalar, 0, 'T');
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.scalar, 12, 't');
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.scalar, 6, 's');
        try expect(result == true);
    }
}

test "containsFrom(self, mode, needle)          : sequence" {
    const self = init("This is a test");

    {
        const result = self.containsFrom(.sequence, 0, "This");
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.sequence, 9, "test");
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.sequence, 5, "is");
        try expect(result == true);
    }
}

test "containsFrom(self, mode, needle)          : any" {
    const self = init("This is a test");

    {
        const result = self.containsFrom(.sequence, 0, "This");
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.sequence, 9, "test");
        try expect(result == true);
    }

    {
        const result = self.containsFrom(.sequence, 5, "is");
        try expect(result == true);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "contains(self, mode, needle)             : scalar" {
    const self = init("This is a testz");

    {
        const result = self.contains(.scalar, 'T');
        try expect(result == true);
    }

    {
        const result = self.contains(.scalar, 'z');
        try expect(result == true);
    }

    {
        const result = self.contains(.scalar, 's');
        try expect(result == true);
    }
}

test "contains(self, mode, needle)             : sequence" {
    const self = init("This is a testz");

    {
        const result = self.contains(.sequence, "This");
        try expect(result == true);
    }

    {
        const result = self.contains(.sequence, "testz");
        try expect(result == true);
    }

    {
        const result = self.contains(.sequence, "is");
        try expect(result == true);
    }
}

test "contains(self, mode, needle)             : any" {
    const self = init("This is a testz");

    {
        const result = self.contains(.any, "This");
        try expect(result == true);
    }

    {
        const result = self.contains(.any, "testz");
        try expect(result == true);
    }

    {
        const result = self.contains(.any, "is");
        try expect(result == true);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "getAt(self, idx)                         : scalar" {
    const self = init("This is a testz");

    {
        const result = self.getAt(0);
        try expect(result == 'T');
    }

    {
        const result = self.getAt(self.items.len - 1);
        try expect(result == 'z');
    }

    {
        const result = self.getAt(-1);
        try expect(result == 'z');
    }
}

////////////////////////////////////////////////////////////////////////////////

test "startsWith(self, mode, needle)           : scalar" {
    const self = init("This is a testz");

    {
        const result = self.startsWith(.scalar, 'T');
        try expect(result == true);
    }

    {
        const result = self.startsWith(.scalar, 't');
        try expect(result == false);
    }

    {
        const result = self.startsWith(.scalar, 'z');
        try expect(result == false);
    }
}

test "startsWith(self, mode, needle)           : sequence" {
    const self = init("This is a testz");

    {
        const result = self.startsWith(.sequence, "This");
        try expect(result == true);
    }

    {
        const result = self.startsWith(.sequence, "testz");
        try expect(result == false);
    }

    {
        const result = self.startsWith(.sequence, "is");
        try expect(result == false);
    }
}

test "startsWith(self, mode, needle)           : any" {
    const self = init("This is a testz");

    {
        const result = self.startsWith(.sequence, "This");
        try expect(result == true);
    }

    {
        const result = self.startsWith(.sequence, "testz");
        try expect(result == false);
    }

    {
        const result = self.startsWith(.sequence, "is");
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "endsWith(self, mode, needle)             : scalar" {
    const self = init("This is a testz");

    {
        const result = self.endsWith(.scalar, 'z');
        try expect(result == true);
    }

    {
        const result = self.endsWith(.scalar, 't');
        try expect(result == false);
    }

    {
        const result = self.endsWith(.scalar, 'T');
        try expect(result == false);
    }
}

test "endsWith(self, mode, needle)             : sequence" {
    const self = init("This is a testz");

    {
        const result = self.endsWith(.sequence, "testz");
        try expect(result == true);
    }

    {
        const result = self.endsWith(.sequence, "This");
        try expect(result == false);
    }

    {
        const result = self.endsWith(.sequence, "is");
        try expect(result == false);
    }
}

test "endsWith(self, mode, needle)             : any" {
    const self = init("This is a testz");

    {
        const result = self.endsWith(.sequence, "testz");
        try expect(result == true);
    }

    {
        const result = self.endsWith(.sequence, "this");
        try expect(result == false);
    }

    {
        const result = self.endsWith(.sequence, "is");
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "count(self, opt, mode, needle)           : scalar" {
    const self = init("000_111_000");

    {
        const result = self.count(.all, .scalar, '0');
        try expect(result == 6);
    }

    {
        const result = self.count(.leading, .scalar, '0');
        try expect(result == 3);
    }

    {
        const result = self.count(.trailing, .scalar, '0');
        try expect(result == 3);
    }

    {
        const result = self.count(.until, .scalar, '1');
        try expect(result == 4);
    }

    {
        const result = self.count(.periphery, .scalar, '0');
        try expect(result == 6);
    }

    {
        const result = self.count(.inside, .scalar, '0');
        try expect(result == 0);
    }

    {
        const result = self.count(.inverse, .scalar, '0');
        try expect(result == self.items.len - 6);
    }
    {
        // left and right are equal, should return length of array
        const result = init("0000000").count(.periphery, .scalar, '0');
        try expect(result == 7);
    }
}

test "count(self, opt, mode, needle)           : sequence" {
    const self = init("000_111_000");

    {
        const result = self.count(.all, .sequence, "000");
        try expect(result == 2);
    }

    {
        const result = self.count(.leading, .sequence, "000");
        try expect(result == 1);
    }

    {
        const result = self.count(.trailing, .sequence, "000");
        try expect(result == 1);
    }

    {
        const result = self.count(.until, .sequence, "000");
        try expect(result == 0);
    }

    {
        const result = self.count(.periphery, .sequence, "000");
        try expect(result == 2);
    }

    {
        const result = self.count(.inside, .sequence, "111");
        try expect(result == 1);
    }

    {
        const result = self.count(.inverse, .sequence, "000");
        try expect(result == self.items.len - 2);
    }
}

test "count(self, opt, mode, needle)           : any" {
    const self = init("000_111_000");

    {
        const result = self.count(.all, .any, "01_");
        try expect(result == self.items.len);
    }

    {
        const result = self.count(.leading, .any, "0_");
        try expect(result == 4);
    }

    {
        const result = self.count(.trailing, .any, "0_");
        try expect(result == 4);
    }

    {
        const result = self.count(.until, .any, "_111_");
        try expect(result == 3);
    }

    {
        const result = self.count(.periphery, .any, "_0");
        try expect(result == 8);
    }

    {
        const result = self.count(.inside, .any, "1_");
        try expect(result == 5);
    }

    {
        const result = self.count(.inverse, .any, "0_");
        try expect(result == 3);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "slice(self, start, end)                  : [start..end]" {
    const string: []const u8 = "012_345_678";

    const self = init(string);

    { // Unsigned:
        try expect(self.slice(@as(usize, 0), self.items.len).equal(string));
        try expect(self.slice(self.items.len + 1, self.items.len).items.len == 0);
        try expect(self.slice(self.items.len, self.items.len).items.len == 0);
        try expect(self.slice(self.items.len, @as(usize, 5)).items.len == 0);
        try expect(self.slice(@as(usize, 6), @as(usize, 5)).items.len == 0);
        try expect(self.slice(@as(usize, 0), @as(usize, 3)).equal(string[0..3]));
        try expect(self.slice(@as(usize, 4), @as(usize, 7)).equal(string[4..7]));
    }
    { // signed:
        try expect(self.slice(@as(isize, -6), @as(isize, -4)).equal("45"));
        try expect(self.slice(@as(isize, -3), @as(isize, -1)).equal("67"));
        try expect(self.slice(@as(isize, 5), @as(isize, -4)).equal("45"));
        try expect(self.slice(@as(isize, 8), @as(isize, -1)).equal("67"));
        try expect(self.slice(@as(isize, -6), @as(isize, 7)).equal("45"));
        try expect(self.slice(@as(isize, -3), @as(isize, 10)).equal("67"));
        try expect(self.slice(@as(isize, 0), @as(isize, -1)).equal(string[0 .. string.len - 1]));
        try expect(self.slice(@as(isize, 0), @as(isize, 3)).equal(string[0..3]));
        try expect(self.slice(@as(isize, 4), @as(isize, 7)).equal(string[4..7]));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "order(self, items)                       : Order" {
    const self = init(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 8 });

    {
        const result = self.order(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 8 });
        try expect(result == .eq);
    }

    {
        const result = self.order(&[_]i32{ 9, 2, 3, 4, 5, 6, 7, 8, 8 });
        try expect(result == .lt);
    }

    {
        const result = self.order(&[_]i32{ 0, 2, 3, 4, 5, 6, 7, 8, 8 });
        try expect(result == .gt);
    }

    {
        const result = self.order(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 9 });
        try expect(result == .lt);
    }

    {
        const result = self.order(&[_]i32{ 0, 2, 3, 4, 5, 6, 7, 8, 7 });
        try expect(result == .gt);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "equal(self, items)                       : bool" {
    const self_num = init(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 8 });
    const self_str = init("This is a string");

    {
        const result = self_num.equal(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 8 });
        try expect(result == true);
    }

    {
        const result = self_num.equal(&[_]i32{ 1, 2, 3, 4, 5, 6, 7, 8, 0 });
        try expect(result == false);
    }

    {
        const result = self_str.equal("This is a string");
        try expect(result == true);
    }

    {
        const result = self_str.equal("This is a ZIG");
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "sum(self)                                : Self.DataType" {
    const self = init(try testing_allocator.alloc(i32, 10000));
    defer testing_allocator.free(self.items);

    {
        const result = self.fill(2).sum();
        try expect(result == 20000);
    }

    {
        const result = self.fill(0).sum();
        try expect(result == 0);
    }

    {
        const result = init(&[_]i32{}).sum();
        try expect(result == 0);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "product(self)                            : Self.DataType" {
    const self = init(try testing_allocator.alloc(i32, 16));
    defer testing_allocator.free(self.items);

    {
        const result = self.fill(2).product();
        try expect(result == 65_536);
    }

    {
        const result = self.fill(1).product();
        try expect(result == 1);
    }

    {
        const result = init(&[_]i32{}).product();
        try expect(result == 1);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "min(self)                                : Self.DataType" {
    const self = init(try testing_allocator.alloc(i32, 1024));
    defer testing_allocator.free(self.items);

    {
        const result = self.fill(2).min() orelse unreachable;
        try expect(result == 2);
    }

    {
        const temp = self.fill(100);
        self.items[512] = -2147483648;
        const result = temp.min() orelse unreachable;
        try expect(result == -2147483648);
    }

    {
        const temp = self.fill(100);
        self.items[0] = 0;
        const result = temp.min() orelse unreachable;
        try expect(result == 0);
    }

    {
        const temp = self.fill(100);
        self.items[1023] = 0;
        const result = temp.min() orelse unreachable;
        try expect(result == 0);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "max(self)                                : Self.DataType" {
    const self = init(try testing_allocator.alloc(i32, 1024));
    defer testing_allocator.free(self.items);

    {
        const result = self.fill(2).max() orelse unreachable;
        try expect(result == 2);
    }

    {
        const temp = self.fill(100);
        self.items[512] = 2147483647;
        const result = temp.max() orelse unreachable;
        try expect(result == 2147483647);
    }

    {
        const temp = self.fill(100);
        self.items[0] = 999;
        const result = temp.max() orelse unreachable;
        try expect(result == 999);
    }

    {
        const temp = self.fill(100);
        self.items[1023] = 777;
        const result = temp.max() orelse unreachable;
        try expect(result == 777);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "split(self, mode, delimiter)             : SplitIterator" {
    const self = init("This is a string");
    const expected = [_][]const u8{ "This", "is", "a", "string" };

    {
        var iter = self.split(.scalar, ' ');
        for (expected) |item| {
            const result = init(iter.next() orelse unreachable);
            try expect(result.equal(item));
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
// @TEST : IMMUTABLE STRING BACKEND                                           //
////////////////////////////////////////////////////////////////////////////////

test "isDigit(self)                            : bool" {
    const test_case = [_][]const u8{ "0", "0123456789", "oops!0123456789", "0123456789oops!" };
    const expected = [_]bool{ true, true, false, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isDigit();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isAlpha(self)                            : bool" {
    const test_case = [_][]const u8{ "a", "aaaaaaaaaa", "7aaaaaaaaaaa", "aaaaaaaaaaa7" };
    const expected = [_]bool{ true, true, false, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isAlpha();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isSpaces(self)                           : bool" {
    const test_case = [_][]const u8{ " ", "          ", "7           ", "           7" };
    const expected = [_]bool{ true, true, false, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isSpaces();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isLower(self)                            : bool" {
    const test_case = [_][]const u8{ "a", "aaaaaaaaaa", "Aaaaaaaaaaa", "aaaaaaaaaaaA" };
    const expected = [_]bool{ true, true, false, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isLower();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isUpper(self)                            : bool" {
    const test_case = [_][]const u8{ "A", "AAAAAAAAAA", "aAAAAAAAAAA", "AAAAAAAAAAAa" };
    const expected = [_]bool{ true, true, false, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isUpper();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isHex(self)                              : bool" {
    const test_case = [_][]const u8{ "0", "0123456789ABCDEF", "0123456789abcdef", "0123456789abcdefZig" };
    const expected = [_]bool{ true, true, true, false };

    {
        for (test_case, expected) |item, answer| {
            const result = init(item).isHex();
            try expect(result == answer);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isASCII(self)                            : bool" {
    const self = init(try testing_allocator.alloc(u8, 255));
    defer testing_allocator.free(self.items);
    for (self.items, 0..255) |*item, i| item.* = @truncate(i);

    {
        const result = self.slice(0, 127).isASCII();
        try expect(result == true);
    }

    {
        const result = self.slice(0, 255).isASCII();
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isPrintable(self)                        : bool" {
    const self = init(try testing_allocator.alloc(u8, 255));
    defer testing_allocator.free(self.items);
    for (self.items, 0..255) |*item, i| item.* = @truncate(i);

    {
        const result = self.slice(32, 127).isPrintable();
        try expect(result == true);
    }

    {
        const result = self.slice(0, 255).isPrintable();
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isAlnum(self)                            : bool" {
    const self = init(try testing_allocator.alloc(u8, 255));
    defer testing_allocator.free(self.items);
    for (self.items, 0..255) |*item, i| item.* = @truncate(i);

    {
        const result = self.slice('A', 'Z').isAlnum();
        try expect(result == true);
    }

    {
        const result = self.slice('0', '9').isAlnum();
        try expect(result == true);
    }

    {
        const result = self.slice('a', 'z').isAlnum();
        try expect(result == true);
    }

    {
        const result = self.slice(0, 255).isAlnum();
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "getToken(self, nth, charset)             : Self" {
    const string = "This: is: a:     string:    with: stuff";
    const charset = " :";
    var buffer: [40]u8 = undefined;

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(1, charset);
        try expect(result.equal("This"));
    }

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(2, charset);
        try expect(result.equal("is"));
    }

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(3, charset);
        try expect(result.equal("a"));
    }

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(4, charset);
        try expect(result.equal("string"));
    }
    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(5, charset);
        try expect(result.equal("with"));
    }

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(6, charset);
        try expect(result.equal("stuff"));
    }

    {
        var result = init(buffer[0..string.len])
            .copy(string)
            .getToken(99, charset);
        try expect(result.equal(""));
    }
}

////////////////////////////////////////////////////////////////////////////////
// @TEST : MUTABLE BACKEND                                                    //
////////////////////////////////////////////////////////////////////////////////

test "sort(self, opt)                          : MutSelf" {
    const test_case = [_][]const i32{
        &[_]i32{ 6, 5, 4, 3, 2, 1 },
        &[_]i32{ 1, 2, 3, 6, 5, 4 },
        &[_]i32{ 1, 2, 3, 4, 5, 6 },
        &[_]i32{ 6, 5, 4, 1, 2, 3 },
        &[_]i32{ 1, 2, 3, 4, 5, 6 },
    };
    const expected = [_][]const i32{
        &[_]i32{ 1, 2, 3, 4, 5, 6 },
        &[_]i32{ 1, 2, 3, 4, 5, 6 },
        &[_]i32{ 1, 2, 3, 4, 5, 6 },
        &[_]i32{ 6, 5, 4, 3, 2, 1 },
        &[_]i32{ 6, 5, 4, 3, 2, 1 },
    };

    const sorting_order = [_]SortDirection{
        .ascending,
        .ascending,
        .ascending,
        .descending,
        .descending,
    };

    var buffer: [6]i32 = undefined;
    {
        // I love Zig <3
        inline for (expected, sorting_order, test_case) |answer, order, case| {
            const result = init(buffer[0..])
                .copy(case)
                .sort(order);
            try expect(result.equal(answer) == true);
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "fill(self, scalar)                       : MutSelf" {
    var buffer: [32]u8 = undefined;

    {
        const result = init(buffer[0..])
            .fill('1')
            .count(.all, .scalar, '1');
        try expect(result == 32);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "copy(self, scalar)                       : MutSelf" {
    var buffer: [32]u8 = undefined;

    {
        const result = init(buffer[0..])
            .copy("00001111222244445555666677778888");
        try expect(result.equal("00001111222244445555666677778888"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "concat(self, items, concat_buffer)       : MutSelf" {
    const start = "This";
    const expected = "This is a string";
    var start_buffer: [4]u8 = undefined;
    var concat_buffer: [16]u8 = undefined;

    {
        const result = init(start_buffer[0..start.len])
            .copy(start[0..])
            .concat(" is a string", concat_buffer[0..]);
        try expect(result.equal(expected));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "join(self, collection, join_buffer)      : MutSelf" {
    const collection = &[_][]const u8{
        "11",
        "222",
        "3333",
        "44444",
    };

    var start_buffer: [1]u8 = undefined;
    var join_buffer: [15]u8 = undefined;

    {
        const result = init(start_buffer[0..])
            .copy("0")
            .join(collection, join_buffer[0..]);
        try expect(result.equal("011222333344444") == true);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "trim(self, opt, kind, actor)             : scalar" {
    const source = "     This is a string     ";
    var buffer: [source.len]u8 = undefined;

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.left, .scalar, ' ');
        try expect(result.equal(source[5..]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.right, .scalar, ' ');
        try expect(result.equal(source[0 .. source.len - 5]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.periphery, .scalar, ' ');
        try expect(result.equal(source[5 .. source.len - 5]));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "trim(self, opt, kind, actor)             : predicate" {
    const source = "     This is a string     ";
    var buffer: [source.len]u8 = undefined;

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.left, .predicate, std.ascii.isWhitespace);
        try expect(result.equal(source[5..]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.right, .predicate, std.ascii.isWhitespace);
        try expect(result.equal(source[0 .. source.len - 5]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.periphery, .predicate, std.ascii.isWhitespace);
        try expect(result.equal(source[5 .. source.len - 5]));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "trim(self, opt, kind, actor)             : set" {
    const source = "     This is a string     ";
    const set = " \n\t";
    var buffer: [source.len]u8 = undefined;

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.left, .any, set[0..]);
        try expect(result.equal(source[5..]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.right, .any, set[0..]);
        try expect(result.equal(source[0 .. source.len - 5]));
    }

    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.periphery, .any, set[0..]);
        try expect(result.equal(source[5 .. source.len - 5]));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "rotate(self, amount)                     : MutSelf" {
    const string = "00110011";
    var buffer: [8]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .rotate(0);
        try expect(result.equal(string));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .rotate(1);
        try expect(result.equal("10011001"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .rotate(-1);
        try expect(result.equal("01100110"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "reverse(self)                            : MutSelf" {
    const string = "00110011";
    var buffer: [8]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .reverse()
            .reverse();
        try expect(result.equal(string));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .reverse();
        try expect(result.equal("11001100"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "setAt(self, idx, with)                   : MutSelf" {
    const string = "aabbccdd";
    var buffer: [8]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .setAt(0, 'A');
        try expect(result.equal("Aabbccdd"));
    }
    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .setAt(7, 'D');
        try expect(result.equal("aabbccdD"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "replace(self, opt, mode, this, with)     : scalar" {
    const string = "abcabcabc";
    var buffer: [9]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.first, .scalar, 'a', 'z');
        try expect(result.equal("zbcabcabc"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.last, .scalar, 'a', 'z');
        try expect(result.equal("abcabczbc"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.periphery, .scalar, 'a', 'z');
        try expect(result.equal("zbcabczbc"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.all, .scalar, 'a', 'z');
        try expect(result.equal("zbczbczbc"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "replace(self, opt, mode, this, with)     : sequence" {
    const string = "abcabcabc";
    var buffer: [9]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.first, .sequence, "abc", "000");
        try expect(result.equal("000abcabc"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.last, .sequence, "abc", "000");
        try expect(result.equal("abcabc000"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.periphery, .sequence, "abc", "000");
        try expect(result.equal("000abc000"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.all, .sequence, "abc", "000");
        try expect(result.equal("000000000"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "replace(self, opt, mode, this, with)     : any" {
    const string = "abcabcabc";
    var buffer: [9]u8 = undefined;

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.first, .any, "abc", "Zig");
        try expect(result.equal("Zbcabcabc"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.last, .any, "abc", "Zig");
        try expect(result.equal("abcabcabg"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.periphery, .any, "abc", "Zig");
        try expect(result.equal("Zbcabcabg"));
    }

    {
        const result = init(buffer[0..string.len])
            .copy(string)
            .replace(.all, .any, "abc", "Zig");
        try expect(result.equal("ZigZigZig"));
    }
}

////////////////////////////////////////////////////////////////////////////////
// @TEST : MUTABLE STRING BACKEND                                             //
////////////////////////////////////////////////////////////////////////////////

test "lower(self)                              : MutSelf" {
    const string = "THIS IS A STRING";
    var string_buffer: [16]u8 = undefined;

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .lower();
        try expect(result.equal("this is a string"));
    }

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .slice(0, 4)
            .lower();
        try expect(result.equal("this"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "upper(self)                              : MutSelf" {
    const string = "this is a string";
    var string_buffer: [16]u8 = undefined;

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .upper();
        try expect(result.equal("THIS IS A STRING"));
    }

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .slice(0, 4)
            .upper();
        try expect(result.equal("THIS"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "capitalize(self)                         : MutSelf" {
    const string = "THIS IS A STRING";
    var string_buffer: [16]u8 = undefined;

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .capitalize();
        try expect(result.equal("This is a string"));
    }

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .slice(0, 4)
            .capitalize();
        try expect(result.equal("This"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "title(self)                              : MutSelf" {
    const string = "THIS IS A STRING";
    var string_buffer: [16]u8 = undefined;

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .title();
        try expect(result.equal("This Is A String"));
    }

    {
        const result = init(string_buffer[0..string.len])
            .copy(string)
            .slice(0, 4)
            .title();
        try expect(result.equal("This"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "differenceWith(self, string, buffer)     : MutSelf" {
    const string = "abcd";
    const diff = "abce";
    var start_buffer: [4]u8 = undefined;
    var diff_buffer: [1]u8 = undefined;

    {
        const result = init(start_buffer[0..])
            .copy(string)
            .differenceWith(diff, diff_buffer[0..]);
        try expect(result.equal("d"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "unionWith(self, string, buffer)          : MutSelf" {
    const string = "abcd";
    const diff = "abce";
    var start_buffer: [4]u8 = undefined;
    var union_buffer: [5]u8 = undefined;

    {
        const result = init(start_buffer[0..])
            .copy(string)
            .unionWith(diff, union_buffer[0..]);
        try expect(result.equal("abcde"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "intersectWith(self, string, buffer)      : MutSelf" {
    const string = "abcd";
    const diff = "abce";
    var start_buffer: [4]u8 = undefined;
    var inter_buffer: [3]u8 = undefined;

    {
        const result = init(start_buffer[0..])
            .copy(string)
            .intersectWith(diff, inter_buffer[0..]);
        try expect(result.equal("abc"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "string integer and float parsing        : ConstSelf" {
    {
        const result = init("42").digit(usize) orelse unreachable;
        try expect(result == 42);
    }

    {
        const result = init("4Zed").digit(usize);
        try expect(result == null);
    }

    {
        const result = init("42.5").float(f64) orelse unreachable;
        try expect(result < 43.0);
    }

    {
        const result = init("9.009.00").float(f64);
        try expect(result == null);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "filter                                  : ConstSelf" {
    const x = init("1ab2cd3hx45");
    var buffer: [32]u8 = undefined;

    var pos: usize = 0;
    var itr = x.iterator(.forward)
        .filter(std.ascii.isDigit);

    while (itr.next()) |d| : (pos += 1) {
        buffer[pos] = d;
    }

    try expect(std.mem.eql(u8, buffer[0..pos], "12345"));
}
////////////////////////////////////////////////////////////////////////////////

test "reduce                                  : ConstSelf" {
    const all_g = struct {
        fn call(a: bool, b: anytype) bool {
            return a and (b == 'g');
        }
    }.call;

    const has_g = struct {
        fn call(a: bool, b: anytype) bool {
            return a or (b == 'g');
        }
    }.call;

    const all_result = init("abcdefg").reduce(bool, all_g, true);
    const has_result = init("abcdefg").reduce(bool, has_g, false);

    try expect(!all_result);
    try expect(has_result);
}

////////////////////////////////////////////////////////////////////////////////

test "mapReduce                                 : ConstSelf" {
    const has_g = struct {
        fn call(a: bool, b: anytype) bool {
            return a or (b == 'g');
        }
    }.call;

    const has_result = init("ABCDEFG")
        .mapReduce(bool, std.ascii.toLower, has_g, false);

    try expect(has_result);
}

////////////////////////////////////////////////////////////////////////////////

test "iterator                                 : empty range" {
    const slice: []const u8 = "hello";
    const empty: []const u8 = slice[0..0];
    {
        var itr = Fluent.iterator(.forward, empty);
        try expect(itr.next() == null);
    }
    {
        var itr = Fluent.iterator(.reverse, empty);
        try expect(itr.next() == null);
    }
    {
        var itr = Fluent.iterator(.forward, empty);
        try expect(itr.window(5) == null);
    }
    {
        var itr = Fluent.iterator(.reverse, empty);
        try expect(itr.window(5) == null);
    }
}

test "iterator                                 : next" {
    {
        var itr = Fluent.iterator(.forward, "hello");
        try std.testing.expectEqual(itr.next().?, 'h');
        try std.testing.expectEqual(itr.next().?, 'e');
        try std.testing.expectEqual(itr.next().?, 'l');
        try std.testing.expectEqual(itr.next().?, 'l');
        try std.testing.expectEqual(itr.next().?, 'o');
        try expect(itr.next() == null);
    }
    {
        var itr = Fluent.iterator(.reverse, "hello");
        try std.testing.expectEqual(itr.next().?, 'o');
        try std.testing.expectEqual(itr.next().?, 'l');
        try std.testing.expectEqual(itr.next().?, 'l');
        try std.testing.expectEqual(itr.next().?, 'e');
        try std.testing.expectEqual(itr.next().?, 'h');
        try expect(itr.next() == null);
    }
}

test "iterator                                 : window" {
    {
        var itr = Fluent.iterator(.forward, "hello");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "hel");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "ell");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "llo");
        try expect(itr.window(3) == null);
    }
    {
        var itr = Fluent.iterator(.reverse, "hello");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "llo");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "ell");
        try std.testing.expectEqualSlices(u8, itr.window(3).?, "hel");
        try expect(itr.window(3) == null);
    }
}

test "regex:                                    : match iterator" {
    { // match special characters (typical) - one or more
        var itr = match("\\d+", "123a456");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "123");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "456");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (typical) - exact
        var itr = match("\\d{3}", "123456");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "123");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "456");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (typical) - between
        var itr = match("\\d{3,4}", "123456");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "1234");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (inverse)
        var itr = match("\\D+", "123a456");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "a");
        try std.testing.expect(itr.next() == null);
    }
    { // pipe-or clauses
        var itr = match("abc|def", "_abc_def_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "abc");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "def");
        try std.testing.expect(itr.next() == null);
    }
    {
        var itr = match("(a+bc)+", "_aaabc_abcabc_bc_abc_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "aaabc");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "abcabc");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "abc");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (typical)
        var itr = match("[a1]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "a11");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "1aa11");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "a");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "1");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (negated)
        var itr = match("[^a1]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "2_2");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "2_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_x_2");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (negated)
        var itr = match("[^\\d]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_a");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "aa");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_a_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "_x_");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (compound)
        var itr = match("[abc]\\d+", "_ab112_c987b123_d16_");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "b112");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "c987");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "b123");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (spans)
        var itr = Fluent.match("[a-zA-Z]+", "bb12avxz34CBF");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "bb");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "avxz");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "CBF");
        try std.testing.expect(itr.next() == null);
    }
    { // backtracking optimization
        var itr = Fluent.match("\".*\"", "xxx\"Hello, World!\"xxx");
        try std.testing.expectEqualSlices(u8, itr.next() orelse unreachable, "\"Hello, World!\"");
        try std.testing.expect(itr.next() == null);
    }
}
