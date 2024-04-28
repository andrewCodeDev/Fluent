![fluent](https://github.com/andrewCodeDev/Fluent/assets/54549221/6064d88a-ff2a-4970-a494-a432a8e74515)

A fluent-interface for chaining algorithms, iterating, and performing REGEX over slices. 

# Examples

Concatenate, trim, and title string:
```Zig
const result = Fluent.init(str_a)       // initialize our interface on str_a
        .concat(str_b, buf[0..])        // concatenate str_b into buffer
        .trim(.periphery, .scalar, ' ') // trim spaces on both sides
        .title();                       // python title function
```

Fuse map-functions to calculate sigmoid to buffer:
```Zig
const x = Fluent.init(buf[0..])
    .copy(&[_]f32{ -2, -1, 0, 1, 2 })
    .map(.{
        Fluent.negate,
        std.math.exp,
        Fluent.bind(.{ 1.0 }, Fluent.add),
        Fluent.bind(.{ 1.0 }, Fluent.div),
    });
```

Copy the reverse of a list using a reverse iterator:
```Zig
const count = Fluent.iterator(.reverse, items_a[0..]).write(items_b[0..]);
```

Sum up the square of all elements that are even:
```Zig
const rdx = Fluent
    .iterator(.forward, items[0..])
    .filter(isEven)
    .map(square)
    .reduce(i32, Fluent.add, 0);
```

Set stride and iteratively produce slice-windows:
```Zig
var itr = Fluent
    .iterator(.forward, items[0..])
    .strided(2);

while (itr.window(4)) |window| { // ...
```

Fuse multiple unary functions and filters:
```Zig
var itr = Fluent
    .iterator(.forward, items[0..])
    .map(.{
        Fluent.negate,
        std.math.exp,
    }).filter(.{
        skipNans,
        skipInfs,
    });

while (itr.next()) |value| { // ...
```
Use REGEX to find all substrings starting with a, b, or c followed by digits in a string:

```Zig

var itr = Fluent.match("[abc]\\d+", "_ab112_c987b123_d16_");

while (itr.next) |substr| { // ...

```

# Installation

Fluent is a single file implementation. Add the `fluent.zig` file to your project and import like any standard utility.

# Algorithms

### General Backend:

```
Immutable:

all          - check if all elements of the acquired slice are true by given predicate
concat       - appends the aquired slice to a given slice into a given buffer
contains     - check if contains a given scalar, sequence, or any
containsFrom - check if contains a given scalar, sequence, or any after a given index
count        - counts all, leading, trailing, until, inside, inverse of scalar, sequence, any
endsWith     - checks if the acquired slice ends with a scalar, sequence, or any
equal        - returns true if lexicogrpahical order is equal to a given slice
find         - returns first index of scalar, slice, or any
findFrom     - returns first index after a given position of scalar, slice, or any
getAt        - returns an element for given positive or negative index
join         - appends the aquired slice to a given range of slices into a given buffer
mapReduce    - applies unary function and reduces on intial value and binary function
max          - returns an optional maximum value from the acquired slice
min          - returns an optional minimum value from the acquired slice
none         - check if no elements of the acquired slice are true by given predicate
product      - returns the product of all elements or zero if slice is empty
print        - prints the acquired slice based on a given format string
order        - returns the lexicographical order compared to a given slice
reduce       - returns a reduction based on an intial value and binary function
slice        - chainable slicing operation for acquired slice
startsWith   - checks if the acquired slice starts with a scalar, sequence, or any
sample       - randomly samples a range from the acquired slice given a size
sum          - returns the sum of all elements or zero if slice is empty
trim         - trims left, right, periphery of scalar, sequence, any
write        - writes the acquired slice to a given buffer

Mutable:

copy         - copy a given slice into the acquired slice
fill         - fills the acquired slice with a scalar value
map          - transforms every elment in the acquired slice with a given unary function
replace      - replaces slice, sequence, or any at left, right, periphery or all
reverse      - reverses the acquired slice
rotate       - rotates the array by both negative and positive amounts
setAt        - sets a given position with a provided value using index wrapping
shuffle      - randomly shuffles the acquired slice
sort         - sorts the range in ascending or descending order
```

### String Backend:

```
Immutable:

digit          - returns optional integer parsed from string
differenceWith - returns set diference between acquired slice and given slice
float          - returns optional floating-point number parsed from string
getToken       - extract a token given a set of delimiters
intersectWith  - returns set intersection between acquired slice and given slice
isDigit        - check if string only contains digits
isAlpha        - check if string only contains alphabetic characters
isSpaces       - check if string only contains whitespace
isLower        - check if string only contains alphabetic lower case
isUpper        - check if string only contains alphabetic upper case
isHex          - check if string only contains hexidecimal characters
isASCII        - check if string only contains ASCII characters
isPrintable    - check if string only contains printable characters
isAlnum        - check if string only contains alpha numeric characters
unionWith      - returns set union between acquired slice and given slice

Mutable:

lower       - transform all alphabetic characters to lower case
upper       - transform all alphabetic characters to upper case
capitalize  - transform first character to upper case and rest to lower case
title       - capitalize each sequence separated by spaces
```
### Iterator Support:

Fluent iterators:
```
next        - return an optional value and advance by stride
window      - return a slice and advance by stride
filter      - acquire a unary predicate or a tuple of unary predicates
map         - acquire a unary transform or a tuple of unary transforms
write       - loop and write remaining iterator values to buffer
reduce      - accumulate the range using a binary function and initial value
strided     - set iterator stride (default 1)
    
```
Standard Iterators:

```
split       - splits a sequence on a given delimiter
tokenize    - tokenizes a sequence on a given delimiter  
```
# REGEX - (PCRE Standard)

### Special Characters:
```
\d - digits
\D - no-digits
\w - alphanumeric
\W - no-alphanumeric
\s - whitespace
\S - no-whitespace
 . - any character
```
### Quantifiers:
```
+ - one or more
* - any quantity
? - none or one
{n} - exactly n
{m,n} - between m and n (inclusive)
```
### Operators:
```
| - or clause
() - capture group
[] - character set
[^] - negated character set
```


### Simple Argument Processing example :

```zig
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
}

```
### Output :
```bash
Fluent git:main
❯ ./example -print-help -print-hello -print-goodbye
Found print-help    flag!
Found print-hello   flag!
Found print-goodbye flag!
```
