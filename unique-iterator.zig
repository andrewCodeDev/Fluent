const std = @import("std");
const fluent = @import("fluent.zig");
const mem = std.mem;

pub fn UniqueIterator(comptime T: type, comptime mode: fluent.FluentMode) type {
    return struct {
        const Self = @This();

        slice: []const T,
        index: usize,
        delimiter: fluent.Parameter(T, mode),

        pub fn init(slice: []const T, delimiter: fluent.Parameter(T, mode)) Self {
            return Self{
                .slice = slice,
                .index = 0,
                .delimiter = delimiter,
            };
        }

        pub fn next(self: *Self) ?[]const T {
            const haystack = self.slice[0..self.index];
            const result = self.peek() orelse return null;
            self.index += result.len;
            if (self.uniqueOrNull(haystack, result) == null)
                return (self.next());
            return result;
        }

        fn uniqueOrNull(self: *Self, haystack: []const T, needle: []const T) ?[]const T {
            var tokenizer = switch (mode) {
                .scalar => std.mem.tokenizeScalar(T, haystack, self.delimiter),
                .any => std.mem.tokenizeAny(T, haystack, self.delimiter),
                .sequence => std.mem.tokenizeSequence(T, haystack, self.delimiter),
            };
            while (tokenizer.next()) |token| {
                if (std.mem.eql(T, token, needle) == true) return (null);
            }
            return (needle);
        }

        pub fn skip(self: *Self) bool {
            if (self.next()) |_| {
                return (true);
            } else {
                return (false);
            }
        }

        pub fn peek(self: *Self) ?[]const T {
            while (self.index < self.slice.len and self.isDelimiter(self.index)) {
                self.index += switch (mode) {
                    .scalar, .any => 1,
                    .sequence => self.delimiter.len,
                };
            }
            const start = if (self.index == self.slice.len) return null else self.index;
            var end = start;
            while (end < self.slice.len and !self.isDelimiter(end)) {
                end += switch (mode) {
                    .scalar, .any => 1,
                    .sequence => self.delimiter.len,
                };
            }
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
            switch (mode) {
                .sequence => return std.mem.startsWith(T, self.slice[index..], self.delimiter),
                .any => {
                    const item = self.slice[index];
                    for (self.delimiter) |delimiter_item| {
                        if (item == delimiter_item) {
                            return true;
                        }
                    }
                    return false;
                },
                .scalar => return self.slice[index] == self.delimiter,
            }
        }
    };
}

test "unique-iterator - basic" {
    const string = "This This is is a a test test";
    const expected = [_][]const u8{
        "This", "is",
        "a",    "test",
    };
    var unique_iter = UniqueIterator(u8, .scalar).init(string, ' ');
    for (expected) |output| {
        const result = unique_iter.next() orelse unreachable;
        try std.testing.expect(std.mem.eql(u8, result, output));
    }
}
