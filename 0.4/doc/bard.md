# bard
A programming languge

version 0.4

by mikel evins

## Introduction

Bard is a small, simple, expressive, general-purpose programming
language. It belongs to the Lisp family, and it inherits many of the
distinctive features of Lisp.

It's a new language, not a version of any existing Lisp. Besides Lisp,
it owes depbts to several other older programming languages, including
Dylan, Smalltalk, and Haskell. Like Scheme, it's an attempt to achieve
a powerful and expressive language with a minimum of features.

Bard is a labor of love. Its primary goal has been to make me as happy
as possible in my work, but by being fun and engaging to work on, and
by being fun and liberating to use in my work. It has mostly succeeded
in its goals.

I hope others can get as much pleasure from it as I have.

## Values

Bard programs are made up of **expressions** that denote **values**.

An expression that represents a constant value is known as a
**literal**. Here are a few examples of literals in Bard:

    nothing                        ; the empty collection
    0                              ; an integer
    2/3                            ; a ratio
    1.141592                       ; a floating-point number
    "Hello"                        ; a text string
    {name: "Fred" shape: 'square}  ; a map
    (^ x x)                        ; a procedure

`nothing` is a **named literal*. The named literals are unique
constants that are represented by their names. There are only a few of
them:

    nothing    ; the empty collection
    undefined  ; the absence of a useful value
    true       ; Boolean truth
    false      ; Boolean falsehood
    end        ; the end of a stream

Bard provides a rich library of built-in values organized into a small
number of abstract types. The built-in abstract types include
**numbers**, **names**, **lists**, **maps**, and **procedures**.

### Numbers

Numbers are integers, fractions, and other numeric values. Bard
provides representations of integers, ratios, floating-point, and other numbers.

### Names

Names are values used to identify variables and other parts of the
Bard language, as keys in records and maps, and as the elements of
enumerations. Symbols, keywords, and URIs are all examples of names.

### Lists

Lists are bounded, ordered collections of other values. A list is a
collection with a finite number of elements that can be visited in a
stable order. Pairs, vectors, and text strings are all examples of
lists.

By the way, don't make the mistake of assuming that means Bard's
strings are represented inefficiently as linked-lists of
characters. They aren't. Remember: List is an *abstract* type.

### Maps

Maps are lists of key/value pairs. They're equivalent to what other
languages call hashes, tables, or dictionaries.

Again, don't assume that they are literally represented by
linked-lists of two-element structures; a Bard Map may be represented
by any of a variety of different concrete data structures, each with
its own performance advantages.

### Procedures

Procedures are values that run executable code. To execute a procedure
you **apply** it to a sequence of arguments. Here's an example:

    bard> (+ 2 3)
    5

In this example we *apply* the procedure named `+` to the arguments
`2` and `3`. Bard evaluates the expression by running the code
represented by the procedure, which adds th two numbers and returns
the result.

Bard has four kinds of procedures:

- **methods** are simple procedures that execute some code when
    applied to some values.

- **functions** are **polymorphic** procedures that examine their
    arguments, choose a method that matches the arguments, and then
    executes the method.

- **macros** are procedures that transform an expression into a
    different one before executing it. You can extend the syntax of
    Bard by defining your own macros.

- **special forms** are procedures that are built into the Bard
    language at the ground floor. They make up the basic vocabulary of
    the language and sometimes have their own special evaluation
    rules. Bard has a relatively small number of special forms.

When we say something is **polymorphic**, we mean that it can do more
than one thing. Bard's functions are polymorphic, meaning that the
same function can execute two or more different methods. A Bard
function chooses the method to execute by examining the types of its
arguments.

### Abstract types

I emphasize that these are *abstract* types because there can be many
different representations of each of them. An easy way to see the
importance of that feature of Bard is to compare Bard's lists with
lists in older dialects of Lisp.

Traditionally, a list in Lisp is a specific data structure that
consists of two pointers, traditionally called the **CAR** (the head
of the list), and the **CDR** (the tail of the list). Bard supports
this type of list, but it's not the only kind of list in Bard, and you
can't assume that a Bard list is represented this way unless you
specifically tell Bard to do so.

In Bard, unlike older Lisps, a list is any value that can be treated
as a finite, ordered sequence of other values. It's an *abstract*
type, unlike traditional Lisp lists, which represent a *concrete*
representation.

This means that other values besides singly-linked lists are also
lists in Bard. Vectors, text strings, and maps are also lists, which
means you can use the same list functions on them that you use on
traditional Lisp lists.

This approach to types is pervasive in Bard. Numbers, maps,
procedures, and other built-in types are all abstractions that conform
to common protocols. Each of them has a variety of
representations. This separation of **representation** from
**behavior** is a hallmark of Bard's design.

## Types

Bard has two different kinds of types: **structures** and **classes**.

A **structure** is a descrption of how bits and bytes are arranged to
represent some data.

A **class** is a name for a role that a value can play in a procedure.

As an example, `List` is a **class**. It names any of a variety of
values that can be treated as ordered sequences. There are many
representations of `List`, each with its own strengths and
weaknesses. What makes them all instances of `List` is that all of
them can be used as arguments to the `List` functions. All of them can
be counted, iterated, mapped over, filtered, reversed, sliced, and so
on.

By contrast, `cons` is a **structure**. It's a pair of cells, each
containing a reference to a value.

'cons' is not the same thing as 'List'. It's not even the same kind of
thing. You can use `cons` to represent a `List`, and Bard includes a
full set of `List` methods defined for `cons`, but you can also use
`cons` objects for other purposes besides representing `Lists`, and
there are other structures that function as `Lists` just as well as
`cons` can.

This is the key to understanding Bard types: **structures** are
*representations* of values. **Classes** are abstract roles that the
values can play in procedures.

## Actors

A running Bard program can function as an **actor**. **Actors** are
processes that communicate by sending messages to one another, and
that respond to messages by performing actions and, optionally,
sending back replies.

A Bard process can spawn another process and keep a reference to
it. It can then send messages to that spawned process. It can receive
messages from other Bard processes, and it can respond to them by
doing anything Bard can do--including spawning other actors,
transforming itself into a different kind of actor, and passing
references to actors in messages.

A message can contain any valid Bard value, with a small number of
special-case expections (for example, it doesn't make sense for an
actor to send its open file streams to an actor on a remote machine).

## Protocols

A **protocol** is a special kind of map. Like other maps, it
associates names with values. In the case of a protocol, most of the
values are functions. The `List` protocol, for example, composes the
library of functions that make up the behavior of the `List` class.

All of Bard is organized into protocols. Even the basic special forms,
the ground-floor vocabulary of the language, are defined in the
`bard.core` protocol.

Protocols also serve a valuable purpose as namespaces. A name that
appears in two different protocols is considered to be two different
names. For example, `bard.list/append` is a different name from
`bard.map/append`, and the two names identify different functions.

Protocols are Maps; they implement the `Map` functions. They have some
additional behavior as well, though: protocols can **export** their
names for other protocols to **import**. This feature makes it
convenient to use the features of one protocol when building another.

## Reference

This section enumerates and documents Bard's built-in protocols,
including all types and special forms.
