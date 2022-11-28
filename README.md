# Proc
An interpreted programming language, implementing concepts of rules and containers.

## Types
`any` - all types

`undefined` - the type of `null`

`int` - short for *integer*, a whole number (+/-)

`float` - short for *floating point number*, a decimal number (+/-)

`bool` - short for *boolean*, one bit (true/false)

`str` - short for *string*, a string of characters

`vec` - short for *vector*, a sequence of values

`obj` - short for *object*, a table of keys and values

`rule` - a [rule](#rules)

`container` - a [container](#container)

`union` - a set of types which are included

`scission` - a set of types which are excluded

## Rules
```
rule count <- n
    n is int
    n >= 0
```
A rule making sure a that the given value is of type `int` and not negative

## Container
```
container math
    global number = int|float
    var pi = 3.141
    proc abs <- x: number
        return x if x >= 0 else -x
```
A container called `math` containing the global type `number`, the variable `pi` and the procedure `abs`. These can be accessed by using the field operator like this: `math.pi` for the variable `pi`