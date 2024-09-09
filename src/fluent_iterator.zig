const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

////////////////////////////////////////////////////////////////////////////////
// FLUENT IMPORT                                                             ///
////////////////////////////////////////////////////////////////////////////////
const flt = @import("fluent.zig");

const Fluent = flt.Fluent;
const FluentInterface = flt.FluentInterface;

////////////////////////////////////////////////////////////////////////////////
// HELPERS IMPORT                                                            ///
////////////////////////////////////////////////////////////////////////////////

const flth = @import("fluent_helpers.zig");

const DeepChild = flth.DeepChild;
const isConst = flth.isConst;
const identity = flth.identity;
const isSlice = flth.isSlice;
const Parameter = flth.Parameter;
const default = flth.default;
const tupleSize = flth.tupleSize;
const wrapIndex = flth.wrapIndex;
const isInteger = flth.isInteger;
const isUnsigned = flth.isUnsigned;
const isFloat = flth.isFloat;
const simdReduce = flth.simdReduce;
const reduceInit = flth.reduceInit;
const add = flth.add;
const mul = flth.mul;

////////////////////////////////////////////////////////////////////////////////
// REGEX IMPORT                                                              ///
////////////////////////////////////////////////////////////////////////////////

const fltregx = @import("fluent_regex.zig");
const ParseRegexTree = fltregx.ParseRegexTree;

/// enum {forward, reverse}
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

    const ptr: P = if (comptime mode == .forward)
        @as(P, @ptrCast(items.ptr))
    else
        (@as(P, @ptrCast(items.ptr)) + items.len) - 1;

    const end: P = if (comptime mode == .forward)
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

        pub fn next(self: *Self) ?FluentInterface([]const u8) {
            while (self.index < self.items.len) : (self.index += 1) {
                if (tree.call(self.items, self.index, false)) |last| {

                    // non-advancing calls
                    if (self.index == last)
                        continue;

                    defer self.index = last;
                    return Fluent.init(self.items[self.index..last]);
                }
            }
            return null;
        }

        pub fn span(self: *Self) ?struct { pos: usize, end: usize } {
            while (self.index < self.items.len) : (self.index += 1) {
                if (tree.call(self.items, self.index, false)) |last| {

                    // non-advancing calls
                    if (self.index == last)
                        continue;

                    defer self.index = last;
                    return .{ .pos = self.index, .end = last };
                }
            }
            return null;
        }
    };
}

/// match - match substrings based on an expression
pub fn match(
    comptime expression: []const u8,
    source: []const u8,
) MatchIterator(expression) {
    return MatchIterator(expression).init(source);
}

pub fn SplitIterator(comptime expression: []const u8) type {
    return struct {
        const Self = @This();
        const tree = ParseRegexTree(expression);
        items: []const u8,
        index: ?usize,

        pub fn init(items: []const u8) Self {
            return .{ .items = items, .index = 0 };
        }

        pub fn next(self: *Self) ?FluentInterface([]const u8) {
            const start = self.index orelse return null;
            var stop: usize = start;
            const end: ?usize = blk: {
                while (stop < self.items.len) : (stop += 1) {
                    const last = tree.call(self.items, stop, false) orelse continue;

                    // non-advancing calls
                    if (start == last)
                        continue;

                    break :blk last;
                } else break :blk null;
            };
            defer self.index = end;
            return Fluent.init(self.items[start..stop]);
        }

        pub fn span(self: *Self) struct { pos: usize, end: usize } {
            const start = self.index orelse return null;
            var stop: usize = start;
            const end: ?usize = blk: {
                while (stop < self.items.len) : (stop += 1) {
                    const last = tree.call(self.items, stop, false) orelse continue;

                    // non-advancing calls
                    if (start == last)
                        continue;

                    break :blk last;
                } else break :blk null;
            };
            defer self.index = end;
            return .{ .pos = start, .end = stop };
        }
    };
}

/// split - splits a string based on a delimiting expression
pub fn split(
    comptime expression: []const u8,
    source: []const u8,
) SplitIterator(expression) {
    return SplitIterator(expression).init(source);
}

pub fn IteratorInterface(
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
                    .@"fn" => {
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
            const transform = comptime if (@typeInfo(@TypeOf(transforms)) == .@"fn")
                transforms
            else
                Fluent.Chain(transforms).call;

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

        /// strided - set iterator stride (default 1)
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

        /// window - return a slice and advance by stride
        pub fn window(
            self: *Self,
            window_size: usize,
        ) ?FluentInterface([]const DataType) {
            switch (comptime Mode) {
                .forward => {
                    if (self.ptr + window_size <= self.end) {
                        defer _ = self.next();
                        return Fluent.init(self.ptr[0..window_size]);
                    }
                },
                .reverse => {
                    if ((self.ptr + 1) - window_size > self.end) {
                        defer _ = self.next();
                        return Fluent.init(((self.ptr + 1) - window_size)[0..window_size]);
                    }
                },
            }
            return null;
        }

        /// map - transforms every elment in the acquired slice with a given unary function
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

        /// filter - acquire a unary predicate or a tuple of unary predicates
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
