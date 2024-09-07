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

    const check = b.addStaticLibrary(.{
        .name = "Fluent",
        .root_source_file = b.path("fluent.zig"),
        .target = target,
        .optimize = optimize,
        .use_llvm = false,
        .use_lld = false,
    });
    const check_step = b.step("check", "check if the code compiles");
    check_step.dependOn(&check.step);

    _ = b.addModule("Fluent", .{
        .root_source_file = b.path("fluent.zig"),
        .target = target,
        .optimize = optimize,
    });
}
