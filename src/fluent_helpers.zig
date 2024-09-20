const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

////////////////////////////////////////////////////////////////////////////////
// PRIVATE HELPERS :                                                          //
////////////////////////////////////////////////////////////////////////////////

pub fn isConst(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .pointer => |ptr| return ptr.is_const,
        else => @compileError("Type must coercible to a slice."),
    }
}

pub fn isSlice(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .pointer => |ptr| ptr.size == .Slice,
        else => false,
    };
}

pub fn isInteger(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int, .comptime_int => true,
        else => false,
    };
}

pub fn isUnsigned(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .int => |i| i.signedness == .unsigned,
        else => false,
    };
}

pub fn tupleSize(comptime tuple: anytype) usize {
    return switch (@typeInfo(@TypeOf(tuple))) {
        .@"struct" => |s| s.fields.len,
        else => @compileError("type must be a tuple"),
    };
}

pub fn default(comptime T: type) T {
    if (comptime T == bool) {
        return true;
    }
    return 0;
}

// bypasses iterator transform
pub inline fn identity(x: anytype) @TypeOf(x) {
    return x;
}

pub fn isFloat(comptime T: type) bool {
    return switch (@typeInfo(T)) {
        .float => true,
        else => false,
    };
}

pub fn Parameter(comptime T: type, comptime mode: anytype) type {
    const param_types = std.StaticStringMap(type).initComptime(.{
        .{ "any", []const T },
        .{ "scalar", T },
        .{ "sequence", []const T },
        .{ "range", struct { start: usize, end: usize } },
        .{ "predicate", fn (T) bool },
        .{ "regex", []const u8 },
    });
    return comptime param_types.get(@tagName(mode)) orelse unreachable;
}

// checks if we are pointing to an array
pub fn DeepChild(comptime T: type) type {
    // TODO: consider comptime support, should be Immutable only..

    const C = Child(T);

    return switch (@typeInfo(C)) {
        .int, .float => C,
        .array => |a| a.child,
        else => @compileError("Unsupported Type"),
    };
}

pub inline fn wrapIndex(len: usize, idx: anytype) usize {
    switch (@typeInfo(@TypeOf(idx))) {
        .int => |i| {
            if (comptime i.signedness == .unsigned) {
                return idx;
            } else {
                const u: usize = @abs(idx);
                return if (idx < 0) len - u else u;
            }
        },
        .comptime_int => {
            const u: usize = comptime @abs(idx);
            return if (comptime idx < 0) len - u else u;
        },
        else => @compileError("Index must be an integer type parameter."),
    }
}

pub inline fn reduceInit(comptime op: ReduceOp, comptime T: type) T {
    const info = @typeInfo(T);

    return switch (op) {
        .Add => 0, // implicit cast
        .Mul => 1, // implicit cast
        .Min => if (comptime info == .int)
            math.maxInt(T)
        else
            math.inf(T),
        .Max => if (comptime info == .int)
            math.minInt(T)
        else
            -math.inf(T),
        else => @compileError("reduceInit: unsupported op"),
    };
}

pub fn simdReduce(
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
