const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

////////////////////////////////////////////////////////////////////////////////
// HELPERS IMPORT                                                            ///
////////////////////////////////////////////////////////////////////////////////

const flth = @import("fluent_helpers.zig");
const add = flth.add;
const mul = flth.mul;
const min = flth.min;
const max = flth.max;
const default = flth.default;
const isConst = flth.isConst;
const isSlice = flth.isSlice;
const isFloat = flth.isFloat;
const identity = flth.identity;
const tupleSize = flth.tupleSize;
const wrapIndex = flth.wrapIndex;
const isInteger = flth.isInteger;
const DeepChild = flth.DeepChild;
const Parameter = flth.Parameter;
const simdReduce = flth.simdReduce;
const reduceInit = flth.reduceInit;
const isUnsigned = flth.isUnsigned;

////////////////////////////////////////////////////////////////////////////////
// REGEX IMPORT                                                              ///
////////////////////////////////////////////////////////////////////////////////

const fltregx = @import("fluent_regex.zig");
const ParseRegexTree = fltregx.ParseRegexTree;

////////////////////////////////////////////////////////////////////////////////
// UNARY FN ADAPTER                                                          ///
////////////////////////////////////////////////////////////////////////////////

const fltfnadapt = @import("fluent_unary_fn_adapter.zig");
const bind = fltfnadapt.bind;
const Chain = fltfnadapt.Chain;
const unwrap = fltfnadapt.unwrap;
const BindReturn = fltfnadapt.BindReturn;

////////////////////////////////////////////////////////////////////////////////
// ITERATOR IMPORTS                                                          ///
////////////////////////////////////////////////////////////////////////////////
const fltiter = @import("fluent_iterator.zig");
const split = fltiter.split;
const match = fltiter.match;
const iterator = fltiter.iterator;
const BaseIterator = fltiter.BaseIterator;
const MatchIterator = fltiter.MatchIterator;
const SplitIterator = fltiter.SplitIterator;
const IteratorInterface = fltiter.IteratorInterface;
const IteratorMode = fltiter.IteratorMode;

////////////////////////////////////////////////////////////////////////////////
// Public Fluent Interface Access Point                                      ///
////////////////////////////////////////////////////////////////////////////////

pub const Fluent = @This();

pub fn init(slice: anytype) FluentInterface(@TypeOf(slice)) {
    return .{ .items = slice };
}

pub fn FluentInterface(comptime T: type) type {
    return struct {
        const Self = @This();

        pub const DataType = DeepChild(T);

        pub const SliceType = if (isConst(T)) []const DataType else []DataType;

        items: SliceType,

        ////////////////////////////////////////////////////////////////////////////////
        /// GeneralImmutableBackend                                                  ///
        ////////////////////////////////////////////////////////////////////////////////

        pub fn all(self: Self, predicate: fn (Self.DataType) bool) bool {
            return for (self.items) |x| {
                if (!predicate(x)) break false;
            } else true;
        }

        /// none - check if no elements of the acquired slice are true by given predicate
        pub fn none(self: Self, predicate: fn (Self.DataType) bool) bool {
            return for (self.items) |x| {
                if (predicate(x)) break false;
            } else true;
        }

        /// getAt - returns an element for given positive or negative index
        pub fn getAt(self: Self, idx: anytype) Self.DataType {
            return self.items[wrapIndex(self.items.len, idx)];
        }

        /// slice - chainable slicing operation for acquired slice
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

        ///order - returns the lexicographical order compared to a given slice
        pub fn order(self: Self, items: []const Self.DataType) Order {
            return std.mem.order(Self.DataType, self.items, items);
        }

        /// equal - returns true if lexicogrpahical order is equal to a given slice
        pub fn equal(self: Self, items: []const Self.DataType) bool {
            return order(self, items) == .eq;
        }

        /// sum - returns the sum of all elements or zero if slice is empty
        pub fn sum(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Add, add, self.items, reduceInit(ReduceOp.Add, Self.DataType) });
        }

        /// product - returns the product of all elements or zero if slice is empty
        pub fn product(self: Self) Self.DataType {
            return @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Mul, mul, self.items, reduceInit(ReduceOp.Mul, Self.DataType) });
        }

        /// min - returns an optional minimum value from the acquired slice
        pub fn min(self: Self) ?Self.DataType {
            return if (self.items.len == 0) null else @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Min, Fluent.min, self.items, reduceInit(ReduceOp.Min, Self.DataType) });
        }

        /// max - returns an optional maximum value from the acquired slice
        pub fn max(self: Self) ?Self.DataType {
            return if (self.items.len == 0) null else @call(.always_inline, simdReduce, .{ Self.DataType, ReduceOp.Max, Fluent.max, self.items, reduceInit(ReduceOp.Max, Self.DataType) });
        }

        /// write - writes the acquired slice to a given buffer
        pub fn write(self: Self, out_buffer: []Self.DataType) Self {
            if (self.items.len == 0) return self;
            std.debug.assert(self.items.len < out_buffer.len);
            @memcpy(out_buffer[0..self.items.len], self.items);
            return (self);
        }

        /// print - prints the acquired slice based on a given format string
        pub fn print(self: Self, comptime print_format: []const u8) Self {
            // this is intended to work similarly to std.log.info
            std.debug.lockStdErr();
            defer std.debug.unlockStdErr();
            const writer = std.io.getStdErr().writer();
            writer.print(print_format, .{self.items}) catch {};
            return self;
        }

        pub fn format(
            self: Self,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            const fmt: []const u8 = if (Self.DataType == u8) "{s}" else "{any}";
            _ = try writer.print(fmt ++ "\n", .{self.items});
        }

        /// sample - randomly samples a range from the acquired slice given a size
        pub fn sample(self: Self, random: std.Random, size: usize) []const Self.DataType {
            std.debug.assert(size <= self.items.len);

            if (size == self.items.len)
                return self.items;

            const start = random.intRangeAtMost(usize, 0, self.items.len - size);

            return self.items[start..][0..size];
        }

        /// reduce - returns a reduction based on an intial value and binary function
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

        /// mapReduce - applies unary function and reduces on intial value and binary function
        pub fn mapReduce(
            self: Self,
            comptime reduce_type: type,
            comptime unary_func: anytype,
            comptime binary_func: anytype,
            initial: reduce_type,
        ) reduce_type {
            const unary_call = comptime if (@typeInfo(@TypeOf(unary_func)) == .@"fn")
                unary_func
            else
                Chain(unary_func).call;

            var rdx = initial;
            for (self.items) |x| {
                const y = @call(.always_inline, unary_call, .{x});
                rdx = @call(.always_inline, binary_func, .{ rdx, y });
            }
            return rdx;
        }

        /// concat - appends the aquired slice to a given slice into a given buffer
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

        /// join - appends the aquired slice to a given range of slices into a given buffer
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

        pub fn iterator(
            self: Self,
            comptime mode: IteratorMode,
        ) BaseIterator(DataType, mode) {
            return Fluent.iterator(mode, self.items);
        }
        ////////////////////////////////////////////////////////////////////////////////
        /// GeneralImmutableBackend                                                  ///
        ////////////////////////////////////////////////////////////////////////////////
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

        /// containsFrom - check if contains a given scalar, sequence, or any after a given index
        pub fn containsFrom(
            self: Self,
            comptime mode: FluentMode,
            start_index: usize,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            return findFrom(self, mode, start_index, needle) != null;
        }

        /// find - returns first index of scalar, slice, or any
        pub fn find(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) ?usize {
            return findFrom(self, mode, 0, needle);
        }

        /// contains - check if contains a given scalar, sequence, or any
        pub fn contains(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) bool {
            return find(self, mode, needle) != null;
        }

        /// startsWith - checks if the acquired slice starts with a scalar, sequence, or any
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

        /// endsWith - checks if the acquired slice ends with a scalar, sequence, or any
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

        /// count - counts all, left, right given a scalar, sequence, or any
        pub fn count(
            self: Self,
            direction: DirectionOption,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) usize {
            if (self.items.len == 0) return 0;

            return switch (direction) {
                .all => countAll(self, mode, needle),
                .left => countLeft(self, mode, needle),
                .right => countRight(self, mode, needle),
            };
        }

        /// trim - trims left, right, or all based on any, sequence, or scalar
        pub fn trim(
            self: Self,
            comptime direction: DirectionOption,
            comptime option: TrimOptions,
            comptime needle: Parameter(Self.DataType, option),
        ) Self {
            if (self.items.len == 0) return self;
            return switch (direction) {
                .left => .{ .items = self.items[trimLeft(self, option, needle)..] },
                .right => .{ .items = self.items[0..trimRight(self, option, needle)] },
                .all => self.trim(.left, option, needle).trim(.right, option, needle),
            };
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

        ///////////////////////
        //  PRIVATE SECTION  //
        ///////////////////////

        fn trimLeft(
            self: Self,
            comptime opt: TrimOptions,
            actor: Parameter(Self.DataType, opt),
        ) usize {
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

        fn trimRight(
            self: Self,
            comptime opt: TrimOptions,
            actor: Parameter(Self.DataType, opt),
        ) usize {
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

        fn countAll(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) usize {
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

        fn countLeft(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) usize {
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

        fn countRight(
            self: Self,
            comptime mode: FluentMode,
            needle: Parameter(Self.DataType, mode),
        ) usize {
            var result: usize = 0;
            switch (mode) {
                .scalar => {
                    var itr = Fluent.iterator(.reverse, self.items);
                    while (itr.next()) |item| : (result += 1) {
                        if (item != needle) break;
                    }
                },
                .sequence => {
                    if (self.items.len < needle.len) return 0;
                    var start = self.items.len - needle.len;
                    while (start != 0) : (start -|= needle.len) {
                        const win = self.items[start .. start + needle.len];
                        if (!std.mem.eql(Self.DataType, win, needle)) break;
                        result += 1;
                    }
                },
                .any => {
                    var itr = Fluent.iterator(.reverse, self.items);
                    while (itr.next()) |item| : (result += 1) {
                        if (!std.mem.containsAtLeast(Self.DataType, needle, 1, &[_]Self.DataType{item})) break;
                    }
                },
            }
            return result;
        }
        pub fn sort(self: Self, comptime direction: SortDirection) Self {
            const func = if (direction == .asc)
                std.sort.asc(Self.DataType)
            else
                std.sort.desc(Self.DataType);

            std.sort.pdq(Self.DataType, self.items, void{}, func);
            return self;
        }

        /// fill - fills the acquired slice with a scalar value
        pub fn fill(self: Self, scalar: Self.DataType) Self {
            @memset(self.items, scalar);
            return self;
        }

        /// copy - copy a given slice into the acquired slice
        pub fn copy(self: Self, items: []const Self.DataType) Self {
            std.debug.assert(self.items.len >= items.len);
            @memcpy(self.items[0..items.len], items);
            return .{ .items = self.items[0..items.len] };
        }

        /// rotate - rotates the array by both negative and positive amounts
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

        /// reverse - reverses the acquired slice
        pub fn reverse(self: Self) Self {
            std.mem.reverse(Self.DataType, self.items);
            return (self);
        }

        /// setAt - sets a given position with a provided value using index wrapping
        pub fn setAt(self: Self, idx: anytype, item: Self.DataType) Self {
            self.items[wrapIndex(self.items.len, idx)] = item;
            return self;
        }

        /// map - transforms every elment in the acquired slice with a given unary function
        pub fn map(self: Self, unary_func: anytype) Self {
            const unary_call = comptime if (@typeInfo(@TypeOf(unary_func)) == .@"fn")
                unary_func
            else
                Chain(unary_func).call;

            for (self.items) |*x| x.* = @call(.always_inline, unary_call, .{x.*});
            return self;
        }

        /// shuffle - randomly shuffles the acquired slice
        pub fn shuffle(self: Self, random: std.Random) Self {
            random.shuffle(Self.DataType, self.items);
            return self;
        }

        };
}


const StringMode = enum { regex, scalar };

fn ImmutableStringBackend(comptime Self: type) type {
    return struct {


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

        pub usingnamespace GeneralMutableBackend(Self);

        /// lower - transform all alphabetic characters to lower case
        pub fn lower(self: Self) Self {
            for (self.items) |*c| c.* = std.ascii.toLower(c.*);
            return self;
        }

        /// upper - transform all alphabetic characters to upper case
        pub fn upper(self: Self) Self {
            for (self.items) |*c| c.* = std.ascii.toUpper(c.*);
            return self;
        }

        /// replaces values in string with a provided string.
        /// Panics if the replacement string size is larger than
        /// the minimum number of possible matches.
        pub fn replace(
            self: Self,
            comptime mode: StringMode,
            comptime needle: Parameter(u8, mode),
            replacement: Parameter(u8, mode),
        ) Self {
            if (self.items.len == 0) return self;

            switch (mode) {
                .scalar => {
                    std.mem.replaceScalar(u8, self.items, needle, replacement);
                    return self;
                },
                .regex => {
                    const tree = comptime ParseRegexTree(needle);
                    const min_matches = comptime tree.minMatches();

                    if (comptime min_matches == 0)
                        @compileError("replacment matches must be greater than zero");

                    std.debug.assert(replacement.len <= min_matches);

                    var r: usize = 0; // read
                    var w: usize = 0; // write

                    while (r < self.items.len) {
                        if (tree.call(self.items, r, false)) |match_end| {
                            if (r == match_end) {
                                r += 1;
                                continue;
                            }
                            // copy from where the write head starts
                            @memcpy(self.items[w..][0..replacement.len], replacement);
                            // advance write head by size of replacement
                            w += replacement.len;
                            // start next read at last match's end
                            r = match_end;

                            continue;
                        }

                        self.items[w] = self.items[r];
                        r += 1;
                        w += 1;
                    }

                    return .{ .items = self.items[0..w] };
                },
            }
        }

        /// capitalize - transform first character to upper case and rest to lower case
        pub fn capitalize(self: Self) Self {
            if (self.items.len > 0)
                self.items[0] = std.ascii.toUpper(self.items[0]);
            if (self.items.len > 1)
                for (self.items[1..]) |*c| {
                    c.* = std.ascii.toLower(c.*);
                };
            return self;
        }

        /// title - capitalize each sequence separated by spaces
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

    /// init - returns an initEmpty instance of StringBitSet
    pub fn init() StringBitSet {
        return .{ .bits = .{
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
            BackingSet.initEmpty(),
        } };
    }

    /// setValue - sets the value of the bit at the specified position
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

    /// isSet - checks if the bit at the specified position is set
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

    /// unionWith - computes the union of two StringBitSets
    pub fn unionWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].unionWith(other.bits[0]),
            self.bits[1].unionWith(other.bits[1]),
            self.bits[2].unionWith(other.bits[2]),
            self.bits[3].unionWith(other.bits[3]),
        } };
    }

    /// differenceWith - computes the difference of two StringBitSets
    pub fn differenceWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].differenceWith(other.bits[0]),
            self.bits[1].differenceWith(other.bits[1]),
            self.bits[2].differenceWith(other.bits[2]),
            self.bits[3].differenceWith(other.bits[3]),
        } };
    }

    /// intersectWith - computes the intersection of two StringBitSets
    pub fn intersectWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{ .bits = .{
            self.bits[0].intersectWith(other.bits[0]),
            self.bits[1].intersectWith(other.bits[1]),
            self.bits[2].intersectWith(other.bits[2]),
            self.bits[3].intersectWith(other.bits[3]),
        } };
    }

    /// count - counts the number of set bits in the StringBitSet
    pub fn count(self: StringBitSet) usize {
        return self.bits[0].count() + self.bits[1].count() + self.bits[2].count() + self.bits[3].count();
    }

    /// fillBuffer - fills a buffer with the values of set bits in the StringBitSet
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

const DirectionOption = enum {
    all,
    left,
    right,
};

const ReplaceOption = enum {
    first,
    last,
    all,
    periphery,
};

const TrimOptions = enum {
    scalar,
    predicate,
    any,
};

const SortDirection = enum {
    asc,
    desc,
};

const SampleOption = enum {
    scalar,
    sequence,
};

// any, sequence, scalar
const FluentMode = std.mem.DelimiterType;

////////////////////////////////////////////////////////////////////////////////
// TESTING BLOCK :                                                           ///
////////////////////////////////////////////////////////////////////////////////

const testing = std.testing;
const testing_allocator = std.testing.allocator;
const expect = std.testing.expect;
const expectEqSlice = std.testing.expectEqualSlices;

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

test "findFrom(self, mode, start_index, needle) : regex" {
    const self = init("This is a test");
    {
        const result = self.findFrom(.regex, 0, "This") orelse unreachable;
        try testing.expectEqual(result.pos, 0);
        try testing.expectEqual(result.end, 4);
    }
    {
        const result = self.findFrom(.regex, 9, "test") orelse unreachable;
        try testing.expectEqual(10, result.pos);
        try testing.expectEqual(14, result.end);
    }
    {
        const result = self.findFrom(.regex, 5, "is") orelse unreachable;
        try testing.expectEqual(result.pos, 5);
        try testing.expectEqual(result.end, 7);
    }
    {
        const result = self.findFrom(.regex, 0, "[T]") orelse unreachable;
        try testing.expectEqual(result.pos, 0);
        try testing.expectEqual(result.end, 1);
    }
    {
        const result = self.findFrom(.regex, 9, "[test]") orelse unreachable;
        try testing.expectEqual(result.pos, 10);
        try testing.expectEqual(result.end, 11);
    }
    {
        const result = self.findFrom(.regex, 5, "[is]") orelse unreachable;
        try testing.expectEqual(result.pos, 5);
        try testing.expectEqual(result.end, 6);
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
        const result = self.containsFrom(.regex, 0, "This");
        try expect(result == true);
    }
    {
        const result = self.containsFrom(.regex, 9, "test");
        try expect(result == true);
    }
    {
        const result = self.containsFrom(.regex, 5, "is");
        try expect(result == true);
    }
    {
        const result = self.containsFrom(.regex, 0, "[This]");
        try expect(result == true);
    }
    {
        const result = self.containsFrom(.regex, 9, "[test]");
        try expect(result == true);
    }
    {
        const result = self.containsFrom(.regex, 5, "[is]");
        try expect(result == true);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "contains(self, mode, needle)              : scalar" {
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

test "contains(self, mode, needle)              : regex" {
    const self = init("This is a testz");

    {
        const result = self.contains(.regex, "This");
        try expect(result == true);
    }
    {
        const result = self.contains(.regex, "testz");
        try expect(result == true);
    }
    {
        const result = self.contains(.regex, "is");
        try expect(result == true);
    }
    {
        const result = self.contains(.regex, "[This]");
        try expect(result == true);
    }
    {
        const result = self.contains(.regex, "[z]");
        try expect(result == true);
    }
    {
        const result = self.contains(.regex, "[is]");
        try expect(result == true);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "getAt(self, idx)                          : scalar" {
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

test "startsWith(self, mode, needle)            : scalar" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const self = init(items[0..]);

    {
        const result = self.startsWith(.scalar, 0);
        try expect(result == true);
    }

    {
        const result = self.startsWith(.scalar, 1);
        try expect(result == false);
    }

    {
        const result = self.startsWith(.scalar, 9);
        try expect(result == false);
    }
}

//////////////////////////////////////////////////////////////////////////////////

test "startsWith(self, mode, needle)           : sequence" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const needle = [_]u32{ 0, 1, 2 };
    const self = init(items[0..]);

    {
        const result = self.startsWith(.sequence, needle[0..]);
        try expect(result == true);
    }

    {
        const result = self.startsWith(.sequence, needle[1..]);
        try expect(result == false);
    }

    {
        const result = self.startsWith(.sequence, &[_]u32{ 9, 8, 7 });
        try expect(result == false);
    }
}

test "startsWith(self, mode, needle)           : any" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const needle = [_]u32{ 0, 1, 2 };
    const self = init(items[0..]);

    {
        const result = self.startsWith(.any, needle[0..]);
        try expect(result == true);
    }

    {
        const result = self.startsWith(.any, needle[1..]);
        try expect(result == false);
    }

    {
        const result = self.startsWith(.any, &[_]u32{ 9, 8, 7 });
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "endsWith(self, mode, needle)             : scalar" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const self = init(items[0..]);

    {
        const result = self.endsWith(.scalar, 9);
        try expect(result == true);
    }

    {
        const result = self.endsWith(.scalar, 8);
        try expect(result == false);
    }

    {
        const result = self.endsWith(.scalar, 0);
        try expect(result == false);
    }
}

test "endsWith(self, mode, needle)             : sequence" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const self = init(items[0..]);

    {
        const result = self.endsWith(.sequence, &[_]u32{ 7, 8, 9 });
        try expect(result == true);
    }

    {
        const result = self.endsWith(.sequence, &[_]u32{ 9, 8, 7 });
        try expect(result == false);
    }

    {
        const result = self.endsWith(.sequence, &[_]u32{ 6, 8, 9 });
        try expect(result == false);
    }
}

test "endsWith(self, mode, needle)             : any" {
    const items = [_]u32{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    const self = init(items[0..]);

    {
        const result = self.endsWith(.any, &[_]u32{ 7, 8, 9 });
        try expect(result == true);
    }

    {
        const result = self.endsWith(.any, &[_]u32{ 6, 8, 7 });
        try expect(result == false);
    }

    {
        const result = self.endsWith(.any, &[_]u32{ 7, 8, 2 });
        try expect(result == false);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "count(self, opt, mode, needle)            : scalar" {
    const self = init("000_111_000");

    {
        const result = self.count(.all, .scalar, '0');
        try std.testing.expectEqual(6, result);
    }
    {
        const result = self.count(.left, .scalar, '0');
        try std.testing.expectEqual(3, result);
    }
    {
        const result = self.count(.right, .scalar, '0');
        try std.testing.expectEqual(3, result);
    }
}

test "count(self, opt, mode, needle)            : regex" {
    const self = init("000_111_000");
    {
        const result = self.count(.all, .regex, "000");
        try expect(result == 2);
    }
    {
        const result = self.count(.left, .regex, "000");
        try expect(result == 1);
    }
    {
        const result = self.count(.right, .regex, "000");
        try expect(result == 1);
    }
}

//////////////////////////////////////////////////////////////////////////////////

test "slice(self, start, end)                   : [start..end]" {
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

test "order(self, items)                        : Order" {
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

test "equal(self, items)                        : bool" {
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

test "sum(self)                                 : Self.DataType" {
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

test "product(self)                             : Self.DataType" {
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

test "min(self)                                 : Self.DataType" {
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

test "max(self)                                 : Self.DataType" {
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

test "split(self, mode, delimiter)              : SplitIterator" {
    const self = init("This is a string");
    const expected = [_][]const u8{ "This", "is", "a", "string" };
    var itr = self.split(" ");
    for (expected) |item| {
        const result = init(itr.next().?.items);
        try expect(result.equal(item));
    }
}

////////////////////////////////////////////////////////////////////////////////
// @TEST : IMMUTABLE STRING BACKEND                                           //
////////////////////////////////////////////////////////////////////////////////

test "isDigit(self)                             : bool" {
    const test_case = [_][]const u8{ "0", "0123456789", "oops!0123456789", "0123456789oops!" };
    const expected = [_]bool{ true, true, false, false };
    for (test_case, expected) |item, answer| {
        const result = init(item).isDigit();
        try expect(result == answer);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "isAlpha(self)                             : bool" {
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

test "isSpaces(self)                            : bool" {
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

test "isLower(self)                             : bool" {
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

test "isUpper(self)                             : bool" {
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

test "isHex(self)                               : bool" {
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

test "isASCII(self)                             : bool" {
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

test "isPrintable(self)                         : bool" {
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

test "isAlnum(self)                             : bool" {
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
// @TEST : MUTABLE BACKEND                                                    //
////////////////////////////////////////////////////////////////////////////////

test "sort(self, opt)                           : MutSelf" {
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
        .asc,
        .asc,
        .asc,
        .desc,
        .desc,
    };

    var buffer: [6]i32 = undefined;
    {
        // I love Zig <3
        inline for (expected, sorting_order, test_case) |answer, order, case| {
            const result = init(buffer[0..])
                .copy(case)
                .sort(order);
            try expect(result.equal(answer));
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

test "fill(self, scalar)                        : MutSelf" {
    var buffer: [32]u8 = undefined;

    {
        const result = init(buffer[0..])
            .fill('1')
            .count(.all, .scalar, '1');
        try expect(result == 32);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "copy(self, scalar)                        : MutSelf" {
    var buffer: [32]u8 = undefined;

    {
        const result = init(buffer[0..])
            .copy("00001111222244445555666677778888");
        try expect(result.equal("00001111222244445555666677778888"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "concat(self, items, concat_buffer)        : MutSelf" {
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

test "join(self, collection, join_buffer)       : MutSelf" {
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
        try expect(result.equal("011222333344444"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "trim(self, opt, kind, actor)              : scalar" {
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
            .trim(.all, .scalar, ' ');
        try expect(result.equal(source[5 .. source.len - 5]));
    }
    {
        const result = init("a").trim(.all, .scalar, 'a');
        try expect(result.items.len == 0);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "trim(self, opt, kind, actor)              : regex" {
    const source = "     This is a string     ";
    var buffer: [source.len]u8 = undefined;
    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.left, .regex, "\\s+");
        try expect(result.equal("This is a string     "));
    }
    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.right, .regex, "\\s+");
        try expect(result.equal("     This is a string"));
    }
    {
        const result = init(buffer[0..source.len])
            .copy(source)
            .trim(.all, .regex, "\\s+");
        try expect(result.equal("This is a string"));
    }
    {
        const result = Fluent.init("bababa1abab").trim(.all, .regex, "(ba|ab)+");
        try expect(result.equal("1"));
    }
}

////////////////////////////////////////////////////////////////////////////////

test "rotate(self, amount)                      : MutSelf" {
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

test "reverse(self)                             : MutSelf" {
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

test "setAt(self, idx, with)                    : MutSelf" {
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
// @TEST : MUTABLE STRING BACKEND                                             //
////////////////////////////////////////////////////////////////////////////////

test "lower(self)                               : MutSelf" {
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

test "upper(self)                               : MutSelf" {
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

test "capitalize(self)                          : MutSelf" {
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

test "title(self)                               : MutSelf" {
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

test "differenceWith(self, string, buffer)      : MutSelf" {
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

test "unionWith(self, string, buffer)           : MutSelf" {
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

test "intersectWith(self, string, buffer)       : MutSelf" {
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

test "string integer and float parsing          : ConstSelf" {
    {
        const result = init("42").digit(usize) catch unreachable;
        try expect(result == 42);
    }
    {
        const result = init("42.5").float(f64) catch unreachable;
        try expect(result < 43.0);
    }
    {
        const result = init("42").cast(usize) catch unreachable;
        try expect(result == 42);
    }
    {
        const result = init("42.5").cast(f64) catch unreachable;
        try expect(result < 43.0);
    }
}

////////////////////////////////////////////////////////////////////////////////

test "filter                                    : ConstSelf" {
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

test "reduce                                    : ConstSelf" {
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

test "iterator                                  : empty range" {
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

test "iterator                                  : next" {
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

test "iterator                                  : window" {
    {
        var itr = Fluent.iterator(.forward, "hello");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "hel");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "ell");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "llo");
        try expect(itr.window(3) == null);
    }
    {
        var itr = Fluent.iterator(.reverse, "hello");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "llo");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "ell");
        try std.testing.expectEqualSlices(u8, itr.window(3).?.items, "hel");
        try expect(itr.window(3) == null);
    }
}

test "regex                                     : match iterator" {
    { // match special characters (typical) - one or more
        var itr = match("\\d+", "123a456");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "123");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "456");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (typical) - exact
        var itr = match("\\d{3}", "123456");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "123");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "456");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (typical) - between
        var itr = match("\\d{3,4}", "123456");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "1234");
        try std.testing.expect(itr.next() == null);
    }
    { // match special characters (inverse)
        var itr = match("\\D+", "123a456");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "a");
        try std.testing.expect(itr.next() == null);
    }
    { // pipe-or clauses
        var itr = match("abc|def", "_abc_def_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "abc");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "def");
        try std.testing.expect(itr.next() == null);
    }
    {
        var itr = match("(a+bc)+", "_aaabc_abcabc_bc_abc_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "aaabc");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "abcabc");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "abc");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (typical)
        var itr = match("[a1]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "a11");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "1aa11");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "a");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "1");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (negated)
        var itr = match("[^a1]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "2_2");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "2_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_x_2");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (negated)
        var itr = match("[^\\d]+", "_a112_21aa112_a_1_x_2");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_a");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "aa");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_a_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "_x_");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (compound)
        var itr = match("[abc]\\d+", "_ab112_c987b123_d16_");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "b112");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "c987");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "b123");
        try std.testing.expect(itr.next() == null);
    }
    { // character sets (spans)
        var itr = Fluent.match("[a-zA-Z]+", "bb12avxz34CBF");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "bb");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "avxz");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "CBF");
        try std.testing.expect(itr.next() == null);
    }
    { // backtracking optimization
        var itr = Fluent.match("\".*\"", "xxx\"Hello, World!\"xxx");
        try std.testing.expectEqualSlices(u8, itr.next().?.items, "\"Hello, World!\"");
        try std.testing.expect(itr.next() == null);
    }
}

test "regex-engine1                             : match iterator -> regex" {
    {
        const expression = "\\d+";
        const string = "0123456789";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "0123456789", result);
    }
}

test "regex-engine3                             : match iterator -> regex" {
    {
        const expression = "\\d+";
        const string = "aa0123456789aa";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "0123456789", result);
    }
}

test "regex-engine4                             : match iterator -> regex" {
    const expression = "\\d+";
    const string = "\\dDmW0123456789aa\\1:";
    var itr = match(expression, string);
    const result = itr.next().?.items;
    try expectEqSlice(u8, "0123456789", result);
}

test "regex-engine5                             : match iterator -> regex" {
    const expression = "\\d*";
    const string = "\\dDmW0123456789aa\\1:";
    var itr = match(expression, string);
    const result = itr.next().?.items;
    try expectEqSlice(u8, "0123456789", result);
}

test "regex-engine6                             : match iterator -> regex" {
    // @SOLVED
    {
        const expression = "\\d?";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "0", result);
    }
}

test "regex-engine7                             : match iterator -> regex" {
    {
        const expression = "\\d{10}";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "0123456789", result);
    }
}

test "regex-engine9                             : match iterator -> regex" {
    const expression = "\\d{11}";
    const string = "abc0123456789abc";
    var itr = match(expression, string);
    const result = itr.next();
    try expect(result == null);
}

test "regex-engine10                            : match iterator -> regex" {
    const expression = "\\d{0,10}";
    const string = "abc0123456789abc";
    var itr = match(expression, string);
    const result = itr.next().?.items;
    try expectEqSlice(u8, "0123456789", result);
}

test "regex-engine11                            : match iterator -> regex" {
    {
        const expression = "\\D\\D\\D\\d\\d\\d\\d\\d\\d\\d\\d\\d\\d\\D\\D\\D";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "abc0123456789abc", result);
    }
}

test "regex-engine12                            : match iterator -> regex" {
    {
        const expression = "\\D\\D\\D\\d{0,10}\\D\\D\\D";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        const result = itr.next().?.items;
        try expectEqSlice(u8, "abc0123456789abc", result);
    }
}
//
test "regex-engine13                            : match iterator -> regex" {
    {
        const expression = "\\D|\\d";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        for (string) |ch| {
            const result = itr.next().?.items;
            try expect(result[0] == ch);
        }
    }
}

test "regex-engine14                            : match iterator -> regex" {
    {
        const expression = "\\D?|\\d?";
        const string = "abc0123456789abc";
        var itr = match(expression, string);
        for (string) |ch| {
            const result = itr.next().?.items;
            try expect(result[0] == ch);
        }
    }
}

test "regex-engine15                            : match iterator -> regex" {
    const expression = "[abc]{3}|[0-9]{10}";
    const string = "abc0123456789abc";
    var itr = match(expression, string);
    try expectEqSlice(u8, "abc", itr.next().?.items);
    try expectEqSlice(u8, "0123456789", itr.next().?.items);
    try expectEqSlice(u8, "abc", itr.next().?.items);
}

test "regex-engine16                            : match iterator -> regex" {
    const expression = "\\D[abc]+|\\d[0-9]+";
    const string = "abc0123456789abc";
    var itr = match(expression, string);
    try expectEqSlice(u8, "abc", itr.next().?.items);
    try expectEqSlice(u8, "0123456789", itr.next().?.items);
    try expectEqSlice(u8, "abc", itr.next().?.items);
}

test "regex-engine17                            : match iterator -> regex" {
    const expression = "[abc]?|[0-9]{10}";
    const string = "abc0123456789abc";
    var itr = match(expression, string);
    try expectEqSlice(u8, "a", itr.next().?.items);
    try expectEqSlice(u8, "b", itr.next().?.items);
    try expectEqSlice(u8, "c", itr.next().?.items);
    try expectEqSlice(u8, "0123456789", itr.next().?.items);
    try expectEqSlice(u8, "a", itr.next().?.items);
    try expectEqSlice(u8, "b", itr.next().?.items);
    try expectEqSlice(u8, "c", itr.next().?.items);
}
//
test "regex-engine18                            : match iterator -> regex" {
    const expression = "\\d{3}([A-Za-z]+)\\d{3}";
    const string = "123Fluent123";
    var itr = match(expression, string);
    try expectEqSlice(u8, "123Fluent123", itr.next().?.items);
}

test "regex-engine19                            : match iterator -> regex" {
    const expression = "(\\d{3}([A-Za-z]+))?|\\d{3}";
    const string = "123Fluent123";
    var itr = match(expression, string);
    try expectEqSlice(u8, "123Fluent", itr.next().?.items);
    try expectEqSlice(u8, "123", itr.next().?.items);
}

test "regex-engine20                            : match iterator -> regex" {
    const expression = "(([a-z][0-9])|([a-z][0-9]))+";
    const string = "a1b2c3d4e5f6g7h8";
    var itr = match(expression, string);
    try expectEqSlice(u8, "a1b2c3d4e5f6g7h8", itr.next().?.items);
}

test "regex-engine21                            : match iterator -> regex" {
    const expression = "(([a-z][0-9])|([a-z][0-9])?)+";
    const string = "a1b2c3d4e5f6g7h8";
    var itr = match(expression, string);
    try expectEqSlice(u8, "a1b2c3d4e5f6g7h8", itr.next().?.items);
}

test "regex-engine22                            : match iterator -> regex" {
    const expression = "(([a-z]?[0-9]?)?|([a-z]?[0-9]?)?)+";
    const string = "a1b2c3d4e5f6g7h8";
    var itr = match(expression, string);
    try expectEqSlice(u8, "a1b2c3d4e5f6g7h8", itr.next().?.items);
}
//
test "regex-engine23                            : match iterator -> regex" {
    const expression = "(([a-z]?[0-9]?)?|([a-z]?[0-9]?)?)+";
    const string = "a1b2c3d4e5f6g7h8";
    var itr = match(expression, string);
    try expectEqSlice(u8, "a1b2c3d4e5f6g7h8", itr.next().?.items);
}

test "regex-engine25                            : match iterator -> regex" {
    {
        const expression = "[^0-9]+";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefghijklmnopqrstuvwxyz", itr.next().?.items);
        try expectEqSlice(u8, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", itr.next().?.items);
    }
}

test "regex-engine26                            : match iterator -> regex" {
    {
        const expression = "[^0-8]+";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefghijklmnopqrstuvwxyz", itr.next().?.items);
        try expectEqSlice(u8, "9ABCDEFGHIJKLMNOPQRSTUVWXYZ", itr.next().?.items);
    }
}

test "regex-engine27                            : match iterator -> regex" {
    {
        const expression = "[^1-9]+";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefghijklmnopqrstuvwxyz0", itr.next().?.items);
        try expectEqSlice(u8, "ABCDEFGHIJKLMNOPQRSTUVWXYZ", itr.next().?.items);
    }
}

test "regex-engine28                            : match iterator -> regex" {
    {
        const expression = "([^a-z]+[^A-Z]+)";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "0123456789", itr.next().?.items);
    }
}

test "regex-engine29                            : match iterator -> regex" {
    {
        const expression = "([^a-z^A-Z]+)";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "0123456789", itr.next().?.items);
    }
}

test "regex-engine30                            : match iterator -> regex" {
    {
        const expression = "([^a-z^^^^A-Z]+)";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "0123456789", itr.next().?.items);
    }
}

test "regex-engine31                            : match iterator -> regex" {
    {
        const expression = "([^a-z^A-Z]+)";
        const string = "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        var itr = match(expression, string);
        try expectEqSlice(u8, "0123456789", itr.next().?.items);
    }
}

test "regex-engine32                            : match iterator -> regex" {
    {
        const expression = "a?b?c?d?e?f?g?";
        const string = "xyzabcdefg";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefg", itr.next().?.items);
    }
}

test "regex-engine33                            : match iterator -> regex" {
    {
        const expression = "a+b+c+d+e+f+g+";
        const string = "abcdefghijklmnopqrstuvwxyz";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefg", itr.next().?.items);
    }
}

test "regex-engine34                            : match iterator -> regex" {
    {
        const expression = "a{1}b{1}c{1}d{1}e{1}f{1}g";
        const string = "abcdefgh";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefg", itr.next().?.items);
    }
}

test "regex-engine35                            : match iterator -> regex" {
    {
        const expression = "a{0,1}b{0,1}c{0,1}d{0,1}e{0,1}f{0,1}g{0,1}";
        const string = "xyzabcdefg";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefg", itr.next().?.items);
    }
}

test "regex-engine36                            : match iterator-> regex" {
    {
        const expression = "(a)+(b)+(c)+(d)+(e)+(f)+(g)+";
        const string = "abcdefg";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcdefg", itr.next().?.items);
    }
}

test "regex-engine37                            : match iterator-> regex" {
    {
        const expression = "(a){0,1}(b){0,1}(c){0,1}(d){0,1}";
        const string = "abcdefg";
        var itr = match(expression, string);
        try expectEqSlice(u8, "abcd", itr.next().?.items);
    }
}

test "regex-engine38                            : match iterator-> regex" {
    {
        const string = "Call us today at 123-456-7890 or 9876543210 to rewrite your DNA in Zig!";
        var itr = Fluent.match("\\d{3}-\\d{3}-\\d{4}|\\d{10}", string);
        try std.testing.expectEqualSlices(u8, "123-456-7890", itr.next().?.items);
        try std.testing.expectEqualSlices(u8, "9876543210", itr.next().?.items);
        try std.testing.expect(itr.next() == null);
    }
    {
        const string = "Call us today at 123-456-7890 or 9876543210 to say hi!";
        var itr = Fluent.match("\\d{3}-?\\d{3}-?\\d{4}", string);
        try std.testing.expectEqualSlices(u8, "123-456-7890", itr.next().?.items);
        try std.testing.expectEqualSlices(u8, "9876543210", itr.next().?.items);
        try std.testing.expect(itr.next() == null);
    }
    {
        const string =
            "Stock  tip: \"buy dog food\" " ++
            "stock\ttips: \"Why bother\" " ++
            "StOck Tips: \"eat bread\" " ++
            "Stock   tips: \"get a job\" " ++
            "great stock tip: \"I like turtles\"";

        var itr = Fluent.match("[sS]tock\\s{0,3}tips?: \"[^\"]+\"", string);
        try std.testing.expectEqualSlices(u8, "Stock  tip: \"buy dog food\"", itr.next().?.items);
        try std.testing.expectEqualSlices(u8, "stock\ttips: \"Why bother\"", itr.next().?.items);
        try std.testing.expectEqualSlices(u8, "Stock   tips: \"get a job\"", itr.next().?.items);
        try std.testing.expectEqualSlices(u8, "stock tip: \"I like turtles\"", itr.next().?.items);
        try std.testing.expect(itr.next() == null);
    }
}

test "regex-engine39                            : fluent interface-> replace regex" {
    var buf: [100]u8 = undefined;

    try std.testing.expect(Fluent.init(buf[0..])
        .copy("This is a test test test string.")
        .replace(.regex, "test", "cow")
        .equal("This is a cow cow cow string."));

    try std.testing.expect(Fluent.init(buf[0..])
        .copy("This is a test test test string.")
        .replace(.regex, "t\\w{2,4}(?= )", "cow")
        .equal("This is a cow cow cow string."));

    try std.testing.expect(Fluent.init(buf[0..])
        .copy("qaaq aa a aaa aba a")
        .replace(.regex, "a?a", "x")
        .equal("qxq x x xx xbx x"));

    try std.testing.expect(Fluent.init(buf[0..])
        .copy("qaaq aa a aaa aba a")
        .replace(.regex, " ?a?a ?", "")
        .equal("qqb"));
}

test "regex-engine40                            : fluent interface-> word boundary" {
    const expr = "\\bWord\\b";
    const string = "WordWordWord Word WordWord";
    var itr = match(expr, string);

    const s = itr.span() orelse unreachable;

    try testing.expectEqual(13, s.pos);
    try testing.expectEqual(17, s.end);
    try testing.expect(itr.span() == null);
    try testing.expectEqualSlices(u8, string[s.pos..s.end], "Word");
}
