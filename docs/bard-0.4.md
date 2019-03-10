# Bard 0.4

## Lexical syntax

This section describes the **lexical syntax** used to write Bard
expressions.

### Comments

The semicolon character (';') marks a comment. Bard ignores a text
that appears on a line after a semicolon.

    pi ; r squared
    
In this example, the name `pi` is evaluated by Bard; the text `r
squared` is not, because it's a comment.

### Named constants

Bard defines four **named constants**:

`true` - The Boolean true value  
`false` - The Boolean false value  
`nothing` - An empty collection  
`undefined` - The absence of a meaningful value

### Collections

Bard offers two fundamental types of compound objects: **lists** and
**tables**.

A **list** is an ordered sequence of values of finite length.

A **table** is a finite map from **keys** to **values**. A table must
have a finite number of keys, and each key may appear in the table
only once.

### Empty collections

Bard considers all empty collections to be equal to the constant
`nothing`.

     bard> (equal? [] "" '() {})
     true

On the other hand, it does not require all empty collections to be the
same object as `nothing`. Implementations are free to represent
different type of empty collections using different objects. 

The built-in function `identical?` returns `true` if two values are
the same object, and `false` otherwise.

     bard> (identical? [] "" '() {})
     false

### Lists

Following are a few examples of the lexical syntax for lists:

    [] ; an empty list
    [true] ; a list of length one
    [1 2 3 4 5] ; a list of length five
    
A list may be any finite length. It may contain any Bard value except
`undefined`, and the values may appear in any order. A list may
contain values of different types.


##### Special types of lists

Bard defines two special types of lists: **text strings**, and
**application expressions**.

##### Text strings

A **text string** is a list of **text characters**. For example:

    "" ; an empty string
    "A" ; a single-character text string
    "one two three four" ; a longer text string

A text string may be empty, or it may be of any finite length. It can
contain only one type of value: **text characters**. Bard represents
text characters as strings of length one. For example, the firsst
character of the string "Bob" is "B".

##### Application expressions

An **application** is an expression that **applies** an **operator**
to a set of **arguments**. Following are a few examples:

    (current-time) ; calls a system function to retrieve the current time
    (not true) ; returns false
    (+ 2 3) ; returns 5
    (min 100 3 10 -4) ; returns -4

The first element of an application is an **operator**. The remaining
elements are **arguments** to the operator. When Bard evaluates an
application it identifies the operator and applies it to the arguments
according to the operator's rules of evaluation.

It's possible for an application to be empty:

    () ; returns nothing (the empty collection)

An empty application always returns `nothing`, the Bard value that
represents an empty collection.

### Tables

Following are a few examples of the lexical syntax for tables:

    {} ; an empty table
    {:name "Fred"} ; a table with a single entry
    {:name "Fred" :age 35} ; a table with a two entries

Any Bard object except `undefined` may be used as either a key or a
value in a table. 

## Types

In Bard, data are represented by **values**. Values are also called
**objects**.

The **type** of a Bard value consists of two things: a **structure**
that defines how the value is constructed, and one or more
**protocols** that define what operations can be performed on it.

Let's take a look at a simple example:

    bard> (type nothing)
    {:structure <nothing> :protocols [@Object @Boolean @Collection ...]}

The meaning of the value returned by the `type` function is that
`nothing` is represented by an instance of the structure `<nothing>`,
which participates in the protocols `@Object`, `@Boolean`, and
`@Collection` (among others).

### Structures

A **structure** is an object that describes how to build a family of
values. Bard defines a small set of **primitive structures** used to
represent atomic values like numbers, booleans, and so on. It also
defines a set of **compound structures** made up of more than one
value bound together. These compound structures include lists, tables,
and other values that are made up of collections of parts.

Each structure defines a **native protocol**: a set of functions,
parameters, and constants that can create instances of the structure
and read and write its component values. For example, every structure
implements a method of the function `make`:

    bard> (make <ascii-string>)
    ""
    bard> (make <small-integer>)
    0
    bard> (define $text (make <ascii-string> :contents ["B" "a" "n" "a" "n" "a"]))
    "Banana"

Many native protocols also define constants or parameters that capture
information about the structure or enable a programmer to adjust its
behavior.

    bard> <ascii-string>.+lowercase-letters+
    ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]

    bard> <standard-output-stream>.*standard-output*
    #<standard-output-stream "Standard Output">

You can define new structures using the `structure` special form. See
the chapter on structures later in this manual.

### Protocols

A **protocol** is an object that defines a set of operations that may
be performed on instances of one or more structures. As an example,
the `@Equal` protocol defines functions that test whether values are
in certain defined senses equal or equivalent. Any structure for which
the `@Equal` functions are defined can be described as belonging to
the `@Equal` type.

    bard> (type true)
    {:structure <boolean> :protocols [@Object @Boolean @Equal ...]}

A protocol may define any number of constants, parameters, and
functions. Here's what a very simple protocol might look like:

    (protocol @Pair
      left (Pair -> Anything)
      right (Pair -> Anything))

This definition declares that the `@Pair` protocol exists, and that it
exposes two functions: `left`, and `right`. 

The protocol by itself doesn't provide any useful functionality. We
can't call any of the functions it defines, because they're purely
abstract functions without any specified implementation. 

The protocol defines two **classes**: `Pair` and `Anything`. To be
more precise, the protocol declares that two roles are defined for
values in implementations of the `@Pair` functions: one role is named
`Pair`, and the other is named `Anything`.

Bard classes are just names for roles that values may take in
functions, as specified by some protocol. In the `@Pair` protocol, the
class `Pair` is the name for values that can by inputs to the
functions `left` and `right`.

Defining these roles doesn't automatically enable any values to fill
them, though. To do that, we must **specialize** the functions to work
with some structure.

For example,

    (define-method left (p) 
      :where {p <cons>}
      (<cons>.car p))

    (define-method right (p) 
      :where {p <cons>}
      (<cons>.cdr p))

## Procedures

## Control structures

