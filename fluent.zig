const std = @import("std");

//////////////////////////////////
// Public Access Point ///////////

pub fn init(slice: anytype) Implementation(std.meta.Child(@TypeOf(slice)), isConst(@TypeOf(slice))) {
    return .{ .items = slice };
}

//////////////////////////////////
// Backends and Implementation ///

fn SortFunction(comptime T: type) type {
    return struct {
        fn lessThen(_: void, x: T, y: T) bool {
            return x < y;
        }
        fn greaterThen(_: void, x: T, y: T) bool {
            return x > y;
        }
    };
}

fn ImmutableBackend(comptime Self: type) type {
    return struct {
        pub fn find(self: Self, val: Self.dtype) ?usize {
            return std.mem.indexOf(self.DataType, self.items, val);
        }

        pub fn at(self: Self, int: anytype) Self.DataType {
            switch (@typeInfo(@TypeOf(int))) {
                .Int,  => |i| {
                    return if (i.signedness == .unsigned) self.items[int]
                        else self.items[if (int < 0) self.items.len + int else int];
                },
                else => @compileError("TODO: comptime int?"),
            }
        }
    };
}

fn MutableBackend(comptime Self: type) type {
    return struct {

        pub usingnamespace ImmutableBackend(Self);

        pub fn sort(self: Self, comptime mode: enum{ asc, desc }) Self {

            const SF = SortFunction(Self.DataType);

            const func = if (mode == .asc) SF.lessThen else SF.greaterThen;
            std.sort.block(Self.DataType, self.items, void{}, func);
            return self;
        }
    };
}

fn Implementation(comptime T: type, comptime is_const: bool) type {

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

fn isConst(comptime T: type) bool {
    switch (@typeInfo(T)) {
        .Pointer => |ptr| {
            if (ptr.size != .Slice) {
                @compileError("Type must be a slice.");
            }
            return ptr.is_const; 
        }, 
        else => @compileError("Type must be a slice."),
    }
}

