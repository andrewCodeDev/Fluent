const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lib = b.addStaticLibrary(.{
        .name = "Fluent",
        .root_source_file = b.path("fluent.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(lib);

    _ = b.addModule("Fluent", .{
        .root_source_file = b.path("fluent.zig"),
        .target = target,
        .optimize = optimize,
    });
}
