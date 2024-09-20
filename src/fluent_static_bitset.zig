const std = @import("std");

pub const StringBitSet = struct {
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
