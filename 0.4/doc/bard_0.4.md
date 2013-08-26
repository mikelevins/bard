# Bard 0.4

## Overview

Bard is a small, simple, general-purpose Lisp dialect with a few novel features. It's designed to be easy to implement, easy to port, and efficient enough to use for a variety of purposes.

Like other Lisps, Bard is an expression-oriented language: a Bard program consists of a sequence of expressions, each of which denotes a value. Running the program means feeding the expressions to an **evaluator** that reduces each expression to a value called its **normal form**. Also like other Lisps, Bard allows **side effects**, which means that evaluating an expression may cause things to happen besides production of the result value--things like printing messages, drawing pictures, or opening network connections.

Bard is a small language that mostly resembles older Lisps, but it has a few novel features. Its type system loosely resembles the Common Lisp Object System and Dylan's system of classes, but it simplifies them some, and also adds a few new features. Its syntax is mostly conventional Lisp syntax, but it uses more than one kind of parentheses in order to provide convenient shorthand for writing more kinds of data structures.

Bard's implementation bucks some recent trends and offers a language-specific virtual machine with an image file. It used to be common and conventional to implement Lisp as a kernel or virtual machine plus an image file, with the lowest-level runtime features provided by the kernel, and most of the language implemented in the image. That's still the conventional way to implement Smalltalk.

In recent years, newer Lisps have moved away from that way of implementing a lnaguage, but I think there are compelling advantages in it, and I've consciously chosen to implement Bard that way. One benefit that I'm particularly interested in is the ability to compile code on a VM running on one hardware and OS platform and run it unchanged on another. Another benefit is the ability to quickly and easily save the dynamic state of a running system and restart it elsewhere at a later time.

Bard was conceived as a programming language whose primary goal is to make me happy when I'm working. It's getting closer to that vision all the time. I hope it makes someone else happy, too.

## Expressions and Values

A Bard program is made of expressions that denote values. The simplest and most trivial Bard program is nothing at all: zero expressions that denote nothing.

A slightly more complex program is one that consists of a single constant. For example:

    5
    
That's a valid Bard program that consists of a single expression denoting the integer 5.

It's almost true that every Bard value can be written this way--as a literal constant. There are a very few exceptions--values that are more or less impossible to write this way--but as much as possible, Bard provides a way to write every value as a literal constant.

Here are examples of the built-in values in Bard:

### Numbers

    0
    1
    -1
    2.3
    2/3
    6.0221413e+23
    1+2i
    #xff
    #b1011
    
### Enumerations

An `Enumeration` is a collection of discrete, disjoint, atomic values. You define an `Enumeration` by listing the values that belong to it. Here are a few built-in values that belong to various `Enumeration` types:

    nothing
    true
    false
    \A
    \b
    \space
    name:

A few subclasses of `Enumeration` are of special interest:

`Boolean` is the class of truth values. It has two member values: `true` and `false`.

`Name` is the class of values used to name things. It has two subclasses: `Symbol` and `Keyword`. A symbol is just a name, like `fred` or `begin`. Symbols are used as the names of variables, and as the names of Bard's built-in operators. 

A keyword is similar to a symbol, but when Bard evaluates a keyword, it always returns the keyword itself. The written form of a keyword always ends in a colon (':'). Keywords can't be used as the names of variables.

For example, the symbol `apply` is the name of a function that is buit into Bard. If you evaluate it, Bard prints that function:

    bard> apply
    #<primitive apply>

...but the keyword `apply:` is a keyword, so evaluating it just returns the keyword itself:

    bard> apply:
    apply:
    
### Pairs

A pair is a value that associates two other values, called the **left** value and the **right** value. Here's an example:

    (1 . 2)
    
In this pair, the left value is 1 and the right value is 2. The class of pairs is named `Pair`.

### Lists

A list is an ordered sequence of values. Bard provides the class `List` to represent them. The most common way to write a list is like this:

    (1 2 3)
    
That should look comfortingly familiar to most Lisp programmers.

One thing that won't be familiar to most Lisp programmers is that Bard provides several kinds of lists with different ways to write them. We'll cover that in more detail later, but it's worth calling out one particular example:

    "Hello!"
    
In most Lisps, that value is a text string. It's a text string in Bard, too, but it's also a list. That doesn't mean that Bard represents text strings using some inefficient representation; it just means that Bard's type system allows any ordered sequence--including a text string--to be treated as a list, so that all the familiar list functions work with strings (and with all the other kinds of sequences, too).

### Maps

A map is an object that associates each member of a set of discrete **keys** with a distinct value. The class of maps is `Map`.

The set of keys has to be a proper set--that is, it's not allowed to contain any duplicates. The values `undefined` and `nothing` are not allowed as keys. Other than those restrictions, any Bard value may be a key or a value in a map.

Here's a simple map:

    {a: 1 b: 2}
    
The left values are the keywords `a:` and `b:`; the values associated with them are `1` ans `2`, respectively.

By the way, instances of `Map` are also instances of `List`; you can use the `List` functions on `Maps`. If you do then a map will apear to be a list of pairs, though it may not necessarily be represented that way internally.

### Streams

A stream is a value that produces or consumes a sequences of values. The class that represents them is called `Stream`. `Stream` has two subclasses: `Producer` is the class of streams that produce values; `Consumer` is the class of streams that consume them.

An example of a `Producer` is `*standard-input*`-- the stream the produces all the input that Bard gets. A similar example of a `Consumer` is `*standard-output`--the stream to which Bard normally writes all its output.

Bard doesn't provide a literal notation for writing streams, because there's not really any good way to do that. Instead, it provides predefined variables like `*standard-input*` and `*standard-output*` that refer to the most commonly-used streams, and a `Stream` protocol for constructing additional streams as-needed.

Streams are not limited to producing and consuming text or bytes, by the way, and they aren't restricted to use with I/O ports like standard input and standard output. You can open streams on lists or other data structures, or on procedures, and you can define your own types of streams. You can construct streams that accept values and feed them to procedures which in turn produce values, or pass them to other streams. You can create networks of streams that pass produced values to several consumers at the same time, or that produce values on one machine that are consumed on another.

### Procedures

Procedures are values that can be **applied** to lists of **arguments** to compute **results**. 

We'll look at Bard procedures in detail in a later section. Here's a simple example of a **method**--the most basic type of proceudre:

    (method (x y) (+ x y))
    
This is a simple method that adds two arguments together and returns the result.


### Resources

A `Resource` is a value that represents a file, a network server, a process, or generally any service provided by a machine or network that you can get data from or give data to. Files are resources; so are web servers, mice and keyboards. The literal form of a resource is a URL that identifies it:

    #<url>"http://bard.net/"
    #<url>"file:///home/mikel/.bardinit"

### Actors

Actors are related to resources. If a resource identifies an accessible Bard process, then it can be used to construct an instance of `Actor` that represents the process and can communicate with it. An instance of `Actor` is a value that provides one Bard process with a way to communicate with another.

    #<actor>{url: #<url>"bard://bard.net/test.actor"}
    
The `Actor` protocol provides functions for sending and receiving messages between actors. A message is just a Bard value--it can be any value that can be written as a literal constant.

