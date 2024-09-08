const std = @import("std");
const Child = std.meta.Child;
const Order = std.math.Order;
const ReduceOp = std.builtin.ReduceOp;
const math = std.math;

////////////////////////////////////////////////////////////////////////////////
// HELPERS IMPORT                                                            ///
////////////////////////////////////////////////////////////////////////////////

const flth = @import("fluent_helpers.zig");

const DeepChild = flth.DeepChild;
const isConst = flth.isConst;
const identity = flth.identity;
const isSlice = flth.isSlice;
const Parameter = flth.Parameter;
const default = flth.default;
const tupleSize = flth.tupleSize;
const wrapIndex = flth.wrapIndex;
const isInteger = flth.isInteger;
const isUnsigned = flth.isUnsigned;
const isFloat = flth.isFloat;
const simdReduce = flth.simdReduce;
const reduceInit = flth.reduceInit;
const add = flth.add;
const mul = flth.mul;

/////////////////////////////////////////////////
// REGEX                                       //
/////////////////////////////////////////////////

pub const RegexQuantifier = union(enum) {
    any: void, // *
    one_or_more: void, // +
    optional: void, // ?
    exact: usize, // {n}
    between: struct { start: usize, stop: usize }, // {i,j}
};

pub const RegexEscaped = struct {
    escaped: bool,
    char: u8,
};

pub const RegexCharacter = struct {
    in_square: bool, // are we a regex char set?
    escaped: bool,
    negated: bool,
    char: u8,
};

pub const RegexSymbol = union(enum) {
    s: RegexCharacter,
    q: RegexQuantifier,
};

pub fn isRegexFilter(symbol: RegexEscaped) bool {
    return symbol.escaped and switch (symbol.char) {
        'w', 'W', 's', 'S', 'd', 'D', '.' => true,
        else => false,
    };
}

pub fn isRegexQuantifier(symbol: RegexEscaped) bool {
    return !symbol.escaped and switch (symbol.char) {
        '+', '?', '*', '{' => true,
        else => false,
    };
}

pub fn isRegexBracket(symbol: RegexCharacter) bool {
    return !symbol.escaped and switch (symbol.char) {
        '(', ')', '[', ']' => true,
        else => false,
    };
}

pub fn bracketSet(comptime symbol: RegexCharacter) []const u8 {
    const head: u8 = if (symbol.char == '(') '(' else '[';
    const tail: u8 = if (symbol.char == '(') ')' else ']';
    return &.{ head, tail };
}

pub fn parseQuantity(comptime escaped: []const RegexEscaped) usize {
    comptime var count: usize = 0;
    comptime var coefficient: usize = 1;
    comptime var i: usize = escaped.len;
    inline while (i > 0) {
        i -= 1;

        if (comptime !std.ascii.isDigit(escaped[i].char)) {
            @compileError("parseQuantity: invalid char");
        }
        if (comptime i == 0 and escaped[i].char == '0' and escaped.len > 1) {
            @compileError("parseQuantity: head zero in integer");
        }

        const value = escaped[i].char - '0';
        count += value * coefficient;
        coefficient *= 10;
    }
    return count;
}

pub fn fuseEscapes(
    comptime str: []const u8,
) []const RegexEscaped {

    // TODO:
    //   consider making this return a direct
    //   array instead a slice - we don't need
    //   to keep it around for runtime

    if (comptime str.len == 0) {
        @compileError("fuseEscapes: cannot parse empty string");
    }

    // the symbol stack to return
    comptime var es: [str.len]RegexEscaped = undefined;

    // track if last char was escape - '\'
    comptime var escaped: bool = false;

    // current symbol index
    comptime var idx: usize = 0;

    // TODO check for invalid escape character
    //'w', 'W', 's', 'S', 'd', 'D', '.', '(', ')', '[', ']'
    // '+', '?', '*', '{', '-'
    for (str) |char| {
        if (char == '\\' and !escaped) {
            escaped = true;
            continue;
        }

        es[idx] = .{ .escaped = escaped, .char = char };

        escaped = false;

        idx += 1;
    }

    if (comptime escaped) {
        @compileError("fuseEscapes: unused escape symbol");
    }

    return es[0..idx]; // don't reference at runtime
}

pub fn fuseQuantifiers(
    comptime es: []const RegexEscaped,
) []const RegexSymbol {
    comptime {
        @setEvalBranchQuota(200_000);
        if (isRegexQuantifier(es[0])) {
            @compileError("fuseQuantifiers: 0th symbol cannot be a quanitifier");
        }

        // the symbol stack to return
        var sq: [es.len]RegexSymbol = undefined;

        // check if we are within a [] clause
        var in_square: bool = false;
        var square_head: usize = 0;
        var square_tail: usize = 0;
        var negated: bool = false;

        // current symbol index
        var i: usize = 0;
        var j: usize = 0;
        var last_quantifier: bool = false;

        // i gets incremented at loop end
        while (j < es.len) : (j += 1) {

            // implements set syntax: [abc] -> a, b, or c
            if (es[j].char == '[' and !es[j].escaped and !in_square) {
                square_head = j;
                square_tail = closingBracketEscaped(es, "[]", j);
                in_square = true;
            }

            // remove set-level negation and keep indicated escapes
            if (es[j].char == ']' and !es[j].escaped and in_square and j == square_tail) {
                in_square = false;
                negated = false;
            }

            // implements negated set syntax: [^abc] -> not a, b, or c
            if (es[j].char == '^' and in_square and (j -| 1) == square_head and !es[j].escaped) {
                negated = true;
                continue;
            }

            if (!isRegexQuantifier(es[j]) or in_square) {
                last_quantifier = false;

                // every bracket within an [] clause is escaped
                const override_escape: bool = in_square and switch (es[j].char) {
                    '(', ')', '[', ']', '{', '}', '.', '^', '$' => (j != square_head and j != square_tail),
                    else => false,
                };

                sq[i] = .{
                    .s = .{
                        // we don't want square brackets to be within themselves...
                        .in_square = in_square and j != square_head and j != square_tail,
                        .escaped = es[j].escaped or override_escape,
                        .negated = negated and in_square,
                        .char = es[j].char,
                    },
                };
            } else {
                if (last_quantifier) {
                    @compileError("fuseQuantifiers: invalid quantifier");
                }

                last_quantifier = true;

                switch (es[j].char) {
                    '+' => {
                        sq[i] = .{ .q = .{ .one_or_more = void{} } };
                    },
                    '*' => {
                        sq[i] = .{ .q = .{ .any = void{} } };
                    },
                    '?' => {
                        sq[i] = .{ .q = .{ .optional = void{} } };
                    },
                    '{' => {
                        // scan forward, find closing brace, parse digits

                        j += 1; // move off opening brace

                        const range_i = j;
                        var range_j = j;
                        var comma = j;

                        scan: while (range_j < es.len) : (range_j += 1) {
                            switch (es[range_j].char) {
                                '}' => break :scan,
                                ',' => {
                                    comma = range_j;
                                    continue;
                                },
                                '0'...'9' => continue,
                                else => @compileError("fuseQuantifiers: invalid char in range"),
                            }
                        } else {
                            @compileError("fuseQuantifiers: unmatched '}' char");
                        }

                        if (es[range_j].escaped) {
                            @compileError("fuseQuantifiers: invalid char in range");
                        }

                        // {i,j}
                        if (range_i < comma) {
                            const start: usize = parseQuantity(es[range_i..comma]);
                            const stop: usize = parseQuantity(es[comma + 1 .. range_j]);

                            if (start >= stop) {
                                @compileError("fuseQuantifiers: invalid range");
                            }

                            sq[i] = .{ .q = .{ .between = .{ .start = start, .stop = stop } } };
                        } else {
                            const count: usize = parseQuantity(es[range_i..range_j]);

                            if (count == 0) {
                                @compileError("fuseQuantifiers: exact quantifier cannot be 0");
                            }

                            sq[i] = .{ .q = .{ .exact = count } };
                        }

                        j = range_j;
                    },
                    else => {},
                }
            }

            // this is all the way down here because
            // certain charactes can be skipped.
            i += 1;
        }

        const freeze = sq;

        return freeze[0..i]; // don't reference at runtime
    }
}

pub fn closingBracket(
    comptime sq: []const RegexSymbol,
    comptime braces: []const u8,
    comptime idx: usize,
) usize {
    comptime var count: isize = @intFromBool(sq[idx].s.char == braces[0] and !sq[idx].s.escaped);

    if (comptime count == 0) {
        @compileError("closingBracket: must start on opening brace");
    }
    comptime var i: usize = idx + 1;
    while (i < sq.len) : (i += 1) {
        switch (sq[i]) {
            .s => |s| {
                count += @intFromBool(s.char == braces[0] and !s.escaped);
                count -= @intFromBool(s.char == braces[1] and !s.escaped);
                if (count == 0) return i;
            },
            else => continue,
        }
    }
    @compileError("closingBracket: no closing brace found");
}

pub fn closingBracketEscaped(
    comptime es: []const RegexEscaped,
    comptime braces: []const u8,
    comptime idx: usize,
) usize {
    comptime var count: isize = @intFromBool(es[idx].char == braces[0] and !es[idx].escaped);

    if (comptime count == 0) {
        @compileError("closingBracket: must start on opening brace");
    }
    comptime var i: usize = idx + 1;
    while (i < es.len) : (i += 1) {
        count += @intFromBool(es[i].char == braces[0] and !es[i].escaped);
        count -= @intFromBool(es[i].char == braces[1] and !es[i].escaped);
        if (count == 0) return i;
    }
    @compileError("closingBracket: no closing brace found");
}

pub fn pipeSearch(
    comptime sq: []const RegexSymbol,
    comptime idx: usize,
) usize {
    comptime var i: usize = idx;
    while (i < sq.len) : (i += 1) {
        switch (sq[i]) {
            .s => |s| switch (s.char) {
                '|' => if (s.escaped) continue else return i,
                '(' => if (s.escaped) continue else {
                    i = closingBracket(sq, "()", i);
                },
                '[' => if (s.escaped) continue else {
                    i = closingBracket(sq, "[]", i);
                },
                '{' => if (s.escaped) continue else {
                    i = closingBracket(sq, "{}", i);
                },
                ')', ']', '}' => if (!s.escaped) @compileError("pipeSearch: invalid braces"),
                else => continue,
            },
            else => continue,
        }
    }
    return i;
}

pub fn RegexOR(
    // used for "|" or [abc] clauses
    comptime lhs: type,
    comptime rhs: type,
) type {
    return struct {
        pub fn minMatches() usize {
            if (comptime @hasDecl(rhs, "minMatches")) {
                return @min(lhs.minMatches(), rhs.minMatches());
            } else {
                return lhs.minMatches();
            }
        }

        pub fn call(str: []const u8, i: usize, prev: bool) ?usize {
            if (comptime @hasDecl(rhs, "call")) {
                return lhs.call(str, i, prev) orelse rhs.call(str, i, prev);
            } else {
                return lhs.call(str, i, prev);
            }
        }
    };
}

// implents [] syntax - optimization over OR branches...
pub fn RegexCharset(comptime symbols: []const RegexSymbol) type {
    return struct {
        const Self = @This();

        fn SetImpl(
            comptime char_len: usize,
            comptime span_len: usize,
            comptime func_len: usize,
        ) type {
            return struct {
                char_set: [char_len]u8,
                span_set: [span_len]u8,
                func_set: [func_len]u8,
            };
        }

        // to handle character-spans (ex: [a-z]), we first check
        // to see if we have any spans in our character set. If
        // we do not, we do a vectorized check across the whole
        // set. If we do have character spans, we move those
        // characters to their own list and make separate checks
        // for the spans.

        // memoize char array for easy access
        const impl = blk: {
            var char_len: usize = 0;
            var span_len: usize = 0;
            var func_len: usize = 0;
            var char_set: [symbols.len]u8 = undefined;
            var span_set: [symbols.len]u8 = undefined;
            var func_set: [symbols.len]u8 = undefined;

            var i: usize = 0;

            while (i < symbols.len) {
                if (isCharFunction(symbols[i].s.char) and symbols[i].s.escaped) {
                    func_set[func_len] = symbols[i].s.char;
                    func_len += 1;
                    i += 1;
                    continue;
                }

                const j = i + 1;
                const k = j + 1;

                if (j < symbols.len and k < symbols.len and symbols[j].s.char == '-' and !symbols[j].s.escaped) {
                    if (symbols[i].s.char >= symbols[k].s.char)
                        @panic("Left side of char span must be less than right side: " ++ &[_]u8{ symbols[i].s.char, '-', symbols[k].s.char });

                    span_set[span_len + 0] = symbols[i].s.char;
                    span_set[span_len + 1] = symbols[k].s.char;
                    span_len += 2;
                    i += 3;
                    continue;
                }

                char_set[char_len] = symbols[i].s.char;
                char_len += 1;
                i += 1;
            }

            break :blk Self.SetImpl(char_len, span_len, func_len){
                .char_set = char_set[0..char_len].*,
                .span_set = span_set[0..span_len].*,
                .func_set = func_set[0..func_len].*,
            };
        };

        // the entire character set is negated as a set
        const negated = symbols[0].s.negated;

        fn checkFunc(str: []const u8, i: usize) bool {
            if (comptime Self.impl.func_set.len == 0)
                return false;

            // negation must be handled as a group - do not pass
            // the negation flag to the char function.

            inline for (0..Self.impl.func_set.len) |f| {
                if (charFunction(Self.impl.func_set[f], false, str, i) != null) return true;
            }
            return false;
        }

        fn checkChar(str: []const u8, i: usize) bool {
            if (comptime Self.impl.char_set.len == 0)
                return false;

            return std.mem.indexOfScalar(u8, Self.impl.char_set[0..], str[i]) != null;
        }

        fn checkSpan(str: []const u8, i: usize) bool {
            if (comptime Self.impl.span_set.len == 0)
                return false;

            const c = str[i];

            var n: usize = 0;

            while (n < Self.impl.span_set.len) : (n += 2) {
                if (Self.impl.span_set[n] <= c and c <= Self.impl.span_set[n + 1])
                    return true;
            }
            return false;
        }

        pub fn call(str: []const u8, i: usize, _: bool) ?usize {
            if (i == str.len) return null;

            // Character sets in PCRE do not respect zero-length
            // matches. It looks like they always increment by 1.
            // This means that things [\b]w+ will not match like
            // \b\w+ like one would expect.

            if (comptime !Self.negated) {
                if (checkChar(str, i)) return i + 1;
                if (checkSpan(str, i)) return i + 1;
                if (checkFunc(str, i)) return i + 1;
                return null;
            } else {
                const b = checkChar(str, i) or
                    checkSpan(str, i) or
                    checkFunc(str, i);

                return if (!b) i + 1 else null;
            }
        }
    };
}

pub fn RegexAND(
    comptime lhs: type,
    comptime rhs: type,
) type {
    return struct {
        pub fn minMatches() usize {
            const matches: usize = blk: {
                const q = lhs.quantifier orelse break :blk 1;
                break :blk switch (q) {
                    .any => 0,
                    .exact => |n| n,
                    .between => |b| b.start,
                    .one_or_more => 1,
                    .optional => 0,
                };
            };
            if (comptime @hasDecl(rhs, "minMatches")) {
                return @max(matches + rhs.minMatches(), 1);
            } else {
                return @max(matches, 1);
            }
        }

        pub fn call(str: []const u8, i: usize, prev: bool) ?usize {

            // NOTE:
            //  any time an index had add assignment,
            //  use call(str[i..], 0) to only add the
            //  next N matches. Otherwise, always pass
            //  ass call(str, i) to accumulate.

            if (comptime !@hasDecl(rhs, "call")) {
                if (comptime lhs.quantifier) |q| {
                    switch (q) {
                        .any => {
                            var idx: usize = i;
                            while (idx < str.len) {
                                idx = lhs.call(str, idx, prev) orelse break;
                            }
                            return if (prev or idx != i) idx else null;
                        },
                        .exact => |n| {
                            var idx: usize = i;
                            for (0..n) |_| {
                                idx = lhs.call(str, idx, prev) orelse return null;
                            }
                            return idx;
                        },
                        .between => |b| {
                            var idx: usize = i;
                            var count: usize = 0;
                            while (count < b.start and idx < str.len) : (count += 1) {
                                idx = lhs.call(str, idx, prev) orelse return null;
                            }
                            // idx < str.len can break above loop early
                            if (count < b.start) return null;

                            // check if new match has occured
                            const new_match = (i != idx) or prev;

                            while (count < b.stop and idx < str.len) : (count += 1) {
                                idx = lhs.call(str, idx, new_match) orelse break;
                            }
                            // idx could have advanced - check again
                            return if (prev or idx != i) idx else null;
                        },
                        .one_or_more => {
                            var idx = lhs.call(str, i, prev) orelse return null;

                            while (idx < str.len) {
                                idx = lhs.call(str, idx, true) orelse break;
                            }
                            return idx;
                        },
                        .optional => {
                            return lhs.call(str, i, prev) orelse if (prev) i else null;
                        },
                    }
                } else {
                    return lhs.call(str, i, prev);
                }
            }

            if (comptime lhs.quantifier) |q| {
                switch (q) {
                    .any => {
                        var idx: usize = i;
                        var last: ?usize = null;

                        while (idx < str.len) {
                            last = rhs.call(str, idx, false) orelse last;
                            idx = lhs.call(str, idx, prev) orelse break;
                        }
                        return rhs.call(str, idx, i != idx) orelse last;
                    },
                    .exact => |n| {
                        var idx: usize = i;
                        for (0..n) |_| {
                            idx = lhs.call(str, idx, prev) orelse return null;
                        }
                        return rhs.call(str, idx, true);
                    },
                    .between => |b| {
                        var idx: usize = i;
                        var count: usize = 0;
                        while (count < b.start and idx < str.len) : (count += 1) {
                            idx = lhs.call(str, idx, prev) orelse return null;
                        }
                        // idx < str.len can break above loop early
                        if (count < b.start) return null;

                        // check if new match has occured
                        const new_match = (i != idx) or prev;

                        var last: ?usize = null;
                        while (count < b.stop and idx < str.len) : (count += 1) {
                            last = rhs.call(str, idx, new_match) orelse last;
                            idx = lhs.call(str, idx, new_match) orelse break;
                        }
                        // idx could have advanced - check again
                        return rhs.call(str, idx, (i != idx) or prev) orelse last;
                    },
                    .one_or_more => {
                        var idx: usize = lhs.call(str, i, prev) orelse return null;

                        var last: ?usize = null;
                        while (idx < str.len) {
                            // at least one match above has occured
                            last = rhs.call(str, idx, true) orelse last;
                            idx = lhs.call(str, idx, true) orelse break;
                        }
                        // at least one match above has occured
                        return rhs.call(str, idx, true) orelse last;
                    },
                    .optional => {
                        // a match hasn't occurred so we defer to previous
                        const j = lhs.call(str, i, prev) orelse return rhs.call(str, i, prev);
                        // a match must have occured so we switch to true
                        return rhs.call(str, j, true) orelse rhs.call(str, i, prev);
                    },
                }
            } else {
                // a match hasn't occurred so we defer to previous
                const j = lhs.call(str, i, prev) orelse return null;
                // a match must have occured so we switch to true
                return rhs.call(str, j, true);
            }
        }
    };
}

pub fn RegexLookAhead(
    // only used for (?=) and (?!) type clauses,
    // should only appear in those contextes
    comptime this: type,
    comptime positive: bool,
) type {
    return struct {
        pub fn minMatches() usize {
            return 0; // only precedes or follows a match
        }
        pub inline fn call(str: []const u8, i: usize, prev: bool) ?usize {
            if (comptime @hasDecl(this, "call")) {
                if (comptime positive) {
                    return if (this.call(str, i, prev)) |_| i else null;
                } else {
                    return if (this.call(str, i, prev)) |_| null else i;
                }
            } else {
                // case of empty lookahead
                return if (comptime positive) i else null;
            }
        }
    };
}

pub fn RegexUnit(
    comptime Callable: anytype,
    comptime Quantifier: ?RegexQuantifier,
) type {
    return struct {
        pub const callable = Callable;
        pub const quantifier = Quantifier;
        pub const info = @typeInfo(@TypeOf(callable));
        pub inline fn call(str: []const u8, i: usize, prev: bool) ?usize {
            if (comptime info == .@"fn") {
                // terminal function call..
                return callable(str, i);
            } else {
                // another parsing tree...
                return callable.call(str, i, prev);
            }
        }
    };
}

//////////////////////////////////////////////////
//        Character Matching Functions          //
//////////////////////////////////////////////////

// TODO: consider moving into the charFunction call while parsing
pub fn equalRegex(comptime char: u8) fn ([]const u8, i: usize) ?usize {
    return struct {
        pub fn call(str: []const u8, i: usize) ?usize {
            return if (i < str.len and str[i] == char) i + 1 else null;
        }
    }.call;
}

// TODO: consider moving into the charFunction call while parsing
pub fn startsWithRegex(str: []const u8, i: usize) ?usize {
    return if (str.len > 0 and i == 0) i else null;
}

// TODO: consider moving into the charFunction call while parsing
pub fn endsWithRegex(str: []const u8, i: usize) ?usize {
    return if (str.len > 0 and i == str.len) i else null;
}

// TODO: consider moving into the charFunction call while parsing
pub fn anyRegex(_: []const u8, i: usize) ?usize {
    return i + 1;
}

pub fn isWordCharacter(c: u8) bool {
    return (std.ascii.isAlphanumeric(c) or c == '_');
}

pub fn isVerticalWhitespace(c: u8) bool {
    return switch (c) {
        '\n', '\x85', std.ascii.control_code.cr, std.ascii.control_code.vt, std.ascii.control_code.ff => true,
        else => false,
    };
}

pub fn isHorizontalWhitespace(c: u8) bool {
    return switch (c) {
        ' ', '\t' => true,
        else => false,
    };
}

pub fn isWordBoundary(str: []const u8, i: usize) bool {
    if (i == str.len)
        return isWordCharacter(str[i - 1]);

    if (i == 0 and isWordCharacter(str[i]))
        return true;

    if ((i + 1) == str.len and isWordCharacter(str[i]))
        return true;

    // character, check boundary behind
    if (isWordCharacter(str[i]) and !isWordCharacter(str[i - 1]))
        return true;

    // character, check boundary behind
    if (!isWordCharacter(str[i]) and isWordCharacter(str[i - 1]))
        return true;

    return false;
}

pub fn isZeroLength(comptime c: u8) bool {
    return switch (c) {
        'b', 'B' => true,
        else => false,
    };
}

pub fn isCharFunction(comptime char: u8) bool {
    return switch (char) {
        'w', 'W', 'd', 'D', 's', 'S', 'h', 'H', 'v', 'V', 'b', 'B' => true,
        else => false,
    };
}

pub fn charFunction(
    comptime char: u8,
    comptime negated: bool,
    str: []const u8,
    i: usize,
) ?usize {
    const c = comptime if (negated) negateChar(char) else char;

    return blk: {
        if (comptime isZeroLength(char)) {

            // Zero-length matches can have i == str.len
            // and always return i as their match, hence
            // "zero-length" match.

            const b: bool = switch (comptime c) {
                'b' => isWordBoundary(str, i),
                'B' => !isWordBoundary(str, i),
                else => @compileError("Invalid character"),
            };

            break :blk if (b) i else null;
        } else {

            // Standard matches expect i < str.len
            // and advance i by 1.

            if (i == str.len)
                return null;

            const b: bool = switch (comptime c) {
                'w' => isWordCharacter(str[i]),
                'W' => !isWordCharacter(str[i]),
                'd' => std.ascii.isDigit(str[i]),
                'D' => !std.ascii.isDigit(str[i]),
                's' => std.ascii.isWhitespace(str[i]),
                'S' => !std.ascii.isWhitespace(str[i]),
                'h' => isHorizontalWhitespace(str[i]),
                'H' => !isHorizontalWhitespace(str[i]),
                'v' => isVerticalWhitespace(str[i]),
                'V' => !isVerticalWhitespace(str[i]),
                else => @compileError("Invalid character"),
            };

            break :blk if (b) i + 1 else null;
        }
    };
}

pub fn BindCharFunction(
    comptime char: u8,
    comptime negated: bool,
) fn ([]const u8, usize) callconv(.Inline) ?usize {
    return struct {
        pub inline fn call(str: []const u8, i: usize) ?usize {
            return charFunction(char, negated, str, i);
        }
    }.call;
}

pub fn negateChar(comptime c: u8) u8 {
    if (std.ascii.isLower(c)) {
        return std.ascii.toUpper(c);
    }
    return std.ascii.toLower(c);
}

////////////////////////////////////////////
//        Tree Parsing Functions          //
////////////////////////////////////////////

pub fn ParseRegexTreeBreadth(comptime sq: []const RegexSymbol) type {
    comptime {
        if (sq.len == 0)
            return struct {}; // terminal node

        const pipe: usize = pipeSearch(sq, 0);

        if (pipe < sq.len) {
            return RegexOR(
                ParseRegexTreeBreadth(sq[0..pipe]),
                ParseRegexTreeBreadth(sq[pipe + 1 ..]),
            );
        } else {
            return ParseRegexTreeDepth(sq);
        }
    }
}

pub fn ParseRegexTreeDepth(comptime sq: []const RegexSymbol) type {
    comptime {
        if (sq.len == 0)
            return struct {}; // terminal node

        var _sq = sq; // shrinking list

        // deduce function
        const Node: type = switch (_sq[0]) {
            .s => |s| outer: {
                if (isRegexBracket(s)) {
                    // this branch deduces an entire sub-automaton
                    var closing = closingBracket(sq, bracketSet(s), 0);

                    // For parsing any thing between brackets, make sure
                    // to put the code within this 'sub' block so the parser
                    // can continue beyond the end of the brackets. Otherwise,
                    // this will result in a segfault.

                    const T: type = sub: {
                        if (s.char == '[') {
                            break :sub RegexCharset(sq[1..closing]);
                        }

                        if (closing > 2 and s.char == '(') {
                            if (_sq[1] != .q or _sq[2] != .s) {
                                break :sub ParseRegexTreeBreadth(sq[1..closing]);
                            }
                            const t = _sq[1].q;
                            const u = _sq[2].s;

                            if (t == .optional) {
                                if (u.char == '=' and !u.escaped) { // (?=
                                    break :sub RegexLookAhead(ParseRegexTreeBreadth(sq[3..closing]), true);
                                }
                                if (u.char == '!' and !u.escaped) { // (?!
                                    break :sub RegexLookAhead(ParseRegexTreeBreadth(sq[3..closing]), false);
                                }
                            }
                        }

                        // parse everything between the brackets
                        break :sub ParseRegexTreeBreadth(sq[1..closing]);
                    };

                    // the entire automaton can be quantified
                    const q: ?RegexQuantifier =
                        if (closing + 1 >= _sq.len) null else switch (_sq[closing + 1]) {
                        .q => |q| inner: {
                            closing += 1;
                            break :inner q;
                        },
                        .s => null,
                    };

                    _sq = _sq[closing + 1 ..];

                    break :outer RegexUnit(T, q);
                }

                _sq = _sq[1..]; // pop token

                const q: ?RegexQuantifier =
                    if (0 == _sq.len) null else switch (_sq[0]) {
                    .q => |q| inner: {
                        _sq = _sq[1..]; // pop token
                        break :inner q;
                    },
                    .s => null,
                };

                if (s.escaped and isCharFunction(s.char)) {
                    break :outer RegexUnit(BindCharFunction(s.char, s.negated), q);
                } else {
                    if (!s.escaped) {
                        switch (s.char) {
                            '.' => break :outer RegexUnit(anyRegex, q),
                            '^' => {
                                if (q != null) @compileError("Symbol '^' cannot have a quantifier.");
                                break :outer RegexUnit(startsWithRegex, null);
                            },
                            '$' => {
                                if (q != null) @compileError("Symbol '$' cannot have a quantifier.");
                                break :outer RegexUnit(endsWithRegex, null);
                            },
                            else => {},
                        }
                    }
                }

                // default to direct equals
                break :outer RegexUnit(equalRegex(s.char), q);
            },
            .q => @compileError("ParseRegexTreeRecursive: head quantifier"),
        };

        return RegexAND(Node, ParseRegexTreeDepth(_sq));
    }
}

pub fn ParseRegexTree(
    comptime expression: []const u8,
) type {
    return comptime ParseRegexTreeBreadth(fuseQuantifiers(fuseEscapes(expression)));
}
