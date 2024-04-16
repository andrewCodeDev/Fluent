const std = @import("std");

const Fluent = @import("fluent.zig");

const StringBitSet = struct {

    const BackingSet = std.StaticBitSet(@bitSizeOf(usize));

    bits: [4]BackingSet,

    pub fn init() StringBitSet {
        return .{
            .bits = .{
                BackingSet.initEmpty(),
                BackingSet.initEmpty(),
                BackingSet.initEmpty(),
                BackingSet.initEmpty(),
            }
        };
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
        return .{
            .bits = .{
                self.bits[0].unionWith(other.bits[0]),
                self.bits[1].unionWith(other.bits[1]),
                self.bits[2].unionWith(other.bits[2]),
                self.bits[3].unionWith(other.bits[3]),
            }
        };
    }
    pub fn differenceWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{
            .bits = .{
                self.bits[0].differenceWith(other.bits[0]),
                self.bits[1].differenceWith(other.bits[1]),
                self.bits[2].differenceWith(other.bits[2]),
                self.bits[3].differenceWith(other.bits[3]),
            }
        };
    }
    pub fn intersectWith(self: StringBitSet, other: StringBitSet) StringBitSet {
        return .{
            .bits = .{
                self.bits[0].intersectWith(other.bits[0]),
                self.bits[1].intersectWith(other.bits[1]),
                self.bits[2].intersectWith(other.bits[2]),
                self.bits[3].intersectWith(other.bits[3]),
            }
        };
    }

    pub fn count(self: StringBitSet) usize {
        return self.bits[0].count() + self.bits[1].count() + self.bits[2].count() + self.bits[3].count();
    }

    pub fn fillBuffer(self: *const StringBitSet, buffer: []u8) void {
        var val: usize = 0;
        var pos: usize = 0;
        while (val < 256) : (val += 1) {
            if (self.isSet(val)) {
                buffer[pos] = @intCast(val);
                pos += 1;
            }
        }   
    }
};

pub fn main() !void {

    var set1 = StringBitSet.init();
    var set2 = StringBitSet.init();

    const string1: []const u8 = &.{ 0, 255, 73, 136 };
    const string2: []const u8 = &.{ 0, 255, 62, 78  };

    var buffer: [32]u8 = undefined;

    for (string1) |c| {
        set1.setValue(c, true);
    }
    for (string2) |c| {
        set2.setValue(c, true);
    }

    const u = set1.differenceWith(set2);

    u.fillBuffer(buffer[0..]);

    std.debug.print("{any}\n", .{ buffer[0..u.count()] });
}























