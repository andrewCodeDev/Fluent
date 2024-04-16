![fluent](https://github.com/andrewCodeDev/Fluent/assets/54549221/6064d88a-ff2a-4970-a494-a432a8e74515)

A Fluent interface for chaining algorithms over generic slices.

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
partition    - partiions the acquired slice based on predicate in stable or unstable manner
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
unionWith      - returns set intersection between acquired slice and given slice

Mutable:

lower       - transform all alphabetic characters to lower case
upper       - transform all alphabetic characters to upper case
capitalize  - transform first character to upper case and rest to lower case
title       - capitalize each sequence separated by spaces
```
### Iterator Support:

```
filter      - only return elements where predicate is true
split       - splits a sequence on a given delimiter
tokenize    - tokenizes a sequence on a given delimiter  
window      - chunks a sequence in a given window size
```
