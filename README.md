![fluent](https://github.com/andrewCodeDev/Fluent/assets/54549221/6064d88a-ff2a-4970-a494-a432a8e74515)

A Fluent interface for chaining algorithms over generic slices.

# Installation

Fluent is a single file implementation. Add the `fluent.zig` file to your project and import like any standard utility.

# Algorithms

### General Backend:

```
Immutable:

  equal        - returns true if lexicogrpahical order is equal to a given slice
  contains     - check if contains a given scalar, sequence, or any
  containsFrom - check if contains a given scalar, sequence, or any after a given index
  find         - returns first index of scalar, slice, or any
  findFrom     - returns first index after a given position of scalar, slice, or any
  getAt        - returns an element for given positive or negative index
  max          - returns an optional maximum value from the acquired slice
  min          - returns an optional minimum value from the acquired slice
  product      - returns the product of all elements or zero if slice is empty
  order        - returns the lexicographical order compared to a given slice
  write        - writes the aquired slice to a given buffer
  sum          - returns the sum of all elements or zero if slice is empty

Mutable:

  concat       - appends the aquired slice to a given slice into a given buffer
  join         - appends the aquired slice to a given range of slices into a given buffer
  partition    - partiions the acquired slice based on predicate in stable or unstable manner
  reverse      - reverses the acquired slice
  sort         - sorts the range in ascending or descending order

```

### String Backend:

```
Immutable:

  isDigit      - check if string only contains digits
  isAlpha      - check if string only contains alphabetic characters
  isSpaces     - check if string only contains whitespace
  isLower      - check if string only contains alphabetic lower case
  isUpper      - check if string only contains alphabetic upper case
  isHex        - check if string only contains hexidecimal characters
  isASCII      - check if string only contains ASCII characters
  isPrintable  - check if string only contains printable characters
  isAlnum      - check if string only contains alpha numeric characters

Mutable:

  lower       - transform all alphabetic characters to lower case
  upper       - transform all alphabetic characters to upper case
  capitalize  - transform first character to upper case and rest to lower case
  title       - transform capitalize each word separated by spaces

```
