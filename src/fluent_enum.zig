const std = @import("std");

pub const StringMode = enum {
    regex,
    scalar,
};

pub const DirectionOption = enum {
    all,
    left,
    right,
};

pub const ReplaceOption = enum {
    first,
    last,
    all,
    periphery,
};

pub const TrimOptions = enum {
    scalar,
    predicate,
    any,
    regex,
};

pub const SortDirection = enum {
    asc,
    desc,
};

pub const SampleOption = enum {
    scalar,
    sequence,
};

// any, sequence, scalar
pub const FluentMode = std.mem.DelimiterType;
