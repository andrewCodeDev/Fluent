const std = @import("std");
const fluent = @import("fluent.zig");

pub const SmartIteratorMode = enum {
    scalar,
    predicate,
    any,
};

const filter = *const fn (u8) bool;

pub fn SmartIterator() type {
    return struct {
        const Self = @This();

        slice: []const u8,
        index: usize,
        filter: fluent.StringBitSet,

        const arguments = std.ComptimeStringMap(filter, .{
            .{ "isAlpha", &std.ascii.isAlphabetic },
            .{ "isAlnum", &std.ascii.isAlphanumeric },
            .{ "isDigit", &std.ascii.isDigit },
            .{ "isSpaces", &std.ascii.isWhitespace },
            .{ "isAscii", &std.ascii.isASCII },
            .{ "isLower", &std.ascii.isLower },
            .{ "isUpper", &std.ascii.isUpper },
            .{ "isPrint", &std.ascii.isPrint },
            .{ "isControl", &std.ascii.isControl },
            .{ "isHex", &std.ascii.isHex },
        });

        pub fn init(comptime slice: []const u8, filters: []const []const u8) Self {
            return Self{
                .slice = slice,
                .index = 0,
                .filter = filter: {
                    var f = fluent.StringBitSet.init();
                    for (0..255) |i| {
                        for (filters) |key| {
                            const ch: u8 = @truncate(i);
                            if (f.isSet(ch)) continue;
                            const apply = arguments.get(key) orelse unreachable;
                            f.setValue(i, apply(ch));
                        }
                    }
                    break :filter f;
                },
            };
        }

        pub fn next(self: *Self) ?[]const u8 {
            const result = self.peek() orelse return null;
            self.index += result.len;
            return result;
        }

        pub fn skip(self: *Self) bool {
            if (self.next()) |_| {
                return (true);
            } else {
                return (false);
            }
        }

        pub fn peek(self: *Self) ?[]const u8 {
            while (self.index < self.slice.len and self.isDelimiter(self.index)) : (self.index += 1) {}
            const start = if (self.index == self.slice.len) return null else self.index;
            var end = start;
            while (end < self.slice.len and !self.isDelimiter(end)) : (end += 1) {}
            return self.slice[start..end];
        }

        pub fn flush(self: Self) []const u8 {
            var index: usize = self.index;
            while (index < self.slice.len and self.isDelimiter(index)) : (index += 1) {}
            return self.slice[index..];
        }

        pub fn reset(self: *Self) void {
            self.index = 0;
        }

        fn isDelimiter(self: Self, index: usize) bool {
            return (self.filter.isSet(self.slice[index]) == true);
        }
    };
}

test "SmartIterator - basic" {
    const string = "thisSKIPisSKIPaSKIP string  which is weirdly 420formated 69but42  don't worryATALLthe smart iterator got your back";
    const expected = [_][]const u8{
        "this",  "is",       "a",       "string",
        "which", "is",       "weirdly", "formated",
        "but",   "don't",    "worry",   "the",
        "smart", "iterator", "got",     "your",
        "back",
    };
    const params = [_][]const u8{ "isUpper", "isSpaces", "isDigit" };
    var smart_iter = SmartIterator().init(string, params[0..]);
    for (expected) |output| {
        const actual = smart_iter.next() orelse unreachable;
        try std.testing.expect(std.mem.eql(u8, actual, output) == true);
    }
}
