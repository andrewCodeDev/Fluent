const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

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
// UNARY FUNCTION ADAPTER :                                                   //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////////////////////////
// chain: combine multiple unary functions into a single in-order call

pub fn Chain(
    comptime unary_tuple: anytype,
) type {
    return struct {
        pub fn call(x: anytype) @TypeOf(@call(.auto, unwrap, .{ 0, unary_tuple, default(@TypeOf(x)) })) {
            return @call(.always_inline, unwrap, .{ 0, unary_tuple, x });
        }
    };
}

pub fn unwrap(
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
) BindRetun(bind_tuple, function) {
    const bind_count = comptime tupleSize(bind_tuple);
    const total_count = comptime @typeInfo(@TypeOf(function)).@"fn".params.len;

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

pub fn BindRetun(
    comptime bind_tuple: anytype,
    comptime function: anytype,
) type {
    const total_count = comptime @typeInfo(@TypeOf(function)).@"fn".params.len;
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
