# Proc
An interpreted programming language, implementing the concepts of rules and containers. The language is not finished yet, [To-Do](#to-do)

# Content
- [Proc](#proc)
- [Content](#content)
- [Guide](#guide)
  - [Types](#types)
  - [Rules](#rules)
  - [Container](#container)
- [To-Do](#to-do)
    - [Language relevant](#language-relevant)
    - [Planned Features](#planned-features)
    - [Other](#other)
- [Contact](#contact)

---

# Guide

## Types
`any` - all types

`undefined` - the type of `null`

`int` - short for *integer*, a whole number (+/-)

`float` - short for *floating point number*, a decimal number (+/-)

`bool` - short for *boolean*, one bit (true/false)

`str` - short for *string*, a string of characters

`vec` - short for *vector*, a sequence of values

`obj` - short for *object*, a table of keys and values

`proc` - a procedure

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

# To-Do
Even though the language is already functional, it is far from being where I want it. So here are some To-Dos. 
*If anyone wants to help with the language, [contact](#contact) me :)*

### Language relevant
- Vector element reassignment
- Vector type creation: `vec[...]`
- Ranges `start..end`
- Type procedures (`str.join`, `vec.sum`, ...)
### Planned Features
- String creation alternatives
- Lambda Functions `x -> x * 2`
- Arguement collector `proc test <- ...`
- Error handeling
    - error value type
    - `throw` statement
    - `try ... catch ...` statement
- Enums?
- OOP features
### Other
- Rust macros for *value* and *type* generation
- Built-in procedures and containers
    - types
        - `byte`, *int* from `0` to `255`
        - `count`, *int* bigger than `0`
    - io
        - `stdin`, `stdout`, `stderr` ?
        - `read <- file: file_obj`
        - `write <- file: file_obj, text: str`
    - http
        - `get <- url: str`
        - `post <- url: str, data: obj|vec[byte]|file_obj`
        - `put <- url: str, data: obj|vec[byte]|file_obj`
    - lang *(the language itself: **parser**, **lexer**, ...)*
- VSCode language extension
    - Syntax highlighting
    - Error display

# Contact

Gmail - vintendo.games@gmail.com

Discord - sty#8189