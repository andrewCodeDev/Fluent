const std = @import("std");
const fl = @import("fluent.zig");

pub const MyProgramFlag = enum {
    PrintHello,
    PrintGoodbye,
    PringHelp,

    fn toProgramFlag(maybe_flag: []const u8) void {
        if (std.mem.startsWith(u8, maybe_flag, "print-hello"))
            std.debug.print("Found print-hello   flag!\n", .{});
        if (std.mem.startsWith(u8, maybe_flag, "print-goodbye"))
            std.debug.print("Found print-goodbye flag!\n", .{});
        if (std.mem.startsWith(u8, maybe_flag, "print-help"))
            std.debug.print("Found print-help    flag!\n", .{});
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    _ = gpa.deinit();
    const allocator = gpa.allocator();

    var args = try std.process.argsWithAllocator(allocator);

    while (args.next()) |arg| {
        const maybe_flag = fl.init(arg[0..])
            .trim(.left, .predicate, std.ascii.isWhitespace)
            .trim(.left, .scalar, '-');
        MyProgramFlag.toProgramFlag(maybe_flag.items);
    }
    // Fluent git:main
    // ❯ ./example -print-help -print-hello -print-goodbye
    // Found print-help    flag!
    // Found print-hello   flag!
    // Found print-goodbye flag!
}
