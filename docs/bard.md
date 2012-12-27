# Bard
## A programming language
### by mikel evins

## Introduction

Bard is a small, simple dialect of Lisp with a small but useful set of
built-in datatypes, a novel object system that supports polymorphic
generic functions, and a few simple amenities for concurrent and
distributed programming.

Bard is strongly influenced by its ancestors, Common Lisp, Scheme, and
Dylan. It also takes influence from several other programming
languages, including Smalltalk, Haskell, Lua, Erlang, Clojure, and
others.

This document is a brief overview and basic refernce for the language.

## Syntax and built-in classes

Bard is a Lisp. That means that, first, every valid piece of Bard code
is an expression that returns a value, and, second, that there are two
kinds of expressions: atoms and lists.

A list is a sequence of zero or more expressions enclosed in
parentheses, like this:

    (foo bar baz)

An atom is any valid expression that is not a list:

    foo
    1
    5.7
  
...and so on.

Like other dialects of Lisp, Bard treats a list as a function call,
unless you tell it not to. You can tell it not to by quoting the
list. Thus, this:

    (foo bar baz)

...is a function call, but this:

    '(foo bar baz)

...is not.

Besides parentheses to identify lists, Bard supplies a lexical syntax
for each of its basic built-in datatypes. These types are called
"classes". 

Here are the basic built-in classes, with example values of each:

**Undefined**

    undefined

The class of unknown, or undefined values.

**Null**       

    nothing

The value that represents an empty collection or the absence of a value.

**Boolean**    

    true, false

Values that represent the Boolean values true and false.

**Number**     

    0, 1.2, 2/3

Magnitudes and other numeric values.

**Text**       

    'foo, 'Bar, baz:, "Fred and Barney"

Symbols, keywords, and strings.

**List**

    (), (0 1 2 3 4)
    [], [0 1 2 3 4]

An ordered sequence of values. The two syntaxes shown are
interchangeable, except that the second tells Bard not to interpret
the list as a function call. The expression

    [0 1 2 3 4]

is equivalent to 

    (list 0 1 2 3 4)

**Table**

    {a: 0 b: 1}

A mapping from keys to values. 

**Method**

    (^ (x) x)          
    (^ (x y) (+ x y))

A callable object that executes some code when applied to arguments.

### Specialized classes

Besides the basic built-in classes, Bard has a few more types with more
specialized uses, and most of these don't have standard literal syntax
defined for them. These more specialized classes include:

**Function**

A function is a callable object, similar to a method. Unlike a method,
a function has no code of its own; instead, it has a set of methods
that are associated with argument types. When applied to arguments,
the function finds the method that is defined for those types and
applies it to compute a result. Functions are therefore
polymorphic--that is, the same function can execute different code,
depending on the types of its arguments.

**Actor**

Objects that represent a running Bard process. A Bard program can create
actors that run in the same process as the creating Bard process, or in a
new process. It can communicate with actors by sending messages to them;
messages can contain arbitrary Bard data--even functions, methods, or
other actors.

**Stream**

Objects that serve as sources or sinks for data. Objects that belong
to the Stream class include input ports, output ports, and generators.

**Class**

Classes are Bard's abstract data types. They define how datatypes are
related to each other, but they don't specify anything about how they
are represented.

**Schema**

Schemas are Bard's concrete datatypes. They define how bits and butes
are arranged to represent values, but they don't specify anything
about how different types are related to one another.

**Singleton**

Objects that represent data values as types. For example, the value

    (singleton 3)

is equivalent in every way to the integer 3, except that Bard
considers it a type, rather than a value. Bard functions choose the
code to run based on the types of their arguments. Using singletons,
you can make them also choose different methods for different specific
values. For example, you could write a function that runs one method
for the number 3 and a different method for the number 5.

**Type**

The class Type describes all objects that serve the purpose of
datatypes. Instances of Type include classes, singletons, and schemas,
among other possibilities.

## Bard's basic vocabulary

Besides the data structures Bard provides, a programmer needs to know
its vocabulary of basic operations. As in other Lisps, Bard's
operations fall into three general categories: **functions**,
**special forms**, and **macros**.

The differences between functions, special forms, and macros have to
do with the details of how they handle their arguments. Those details
aren't important here, so this section doesn't distinguish the
different kinds of operations. The important point here is that all
three kinds of expressions look similar: that is, they all look like
function calls.

Here is Bard's basic vocabulary of operations:

`^`

The `^` special form can also be spelled `method`. It creates a method
object.

`begin`

The basic sequencing form. Begin evaluates a series of expressions
from left to right, returning the last value.

`cond`

Along with `if`, the basic form of flow control in Bard. Bard's `cond`
works like the form of the same name in Scheme and Common Lisp.

`define`

Defines a global name, binding it to one of several different
kinds of objects.

    (define variable x y)
      defines a global variable named x whose value is the value of y

    (define macro (mname args...) body...)
      defines a new macro. The code in `body` describes how to construct 
      the code that Bard actually executes. 

    (define method (fname args...) body...)  
      Adds a new method to the function `fname`, arguments are given
      by `args`, and whose behavior is given by `body`. If `fname` is
      not defined, creates that variable and binds a new function to
      it.

    (define schema sname slots...)

      defines a global variable named `sname` whose value is a new
      schema. The schema describes how to create new values; the
      values are instances of the schema. The schema's slots describe
      how to create the fields of the instances. 
      

`ensure`

Provides a way to execute a piece of code that might signal an error,
and ensure that a cleanup form is executed aftward, even if the risky
code exits abnormally.

`if`

Along with `cond`, the basic flow-control form in Bard. Evaluates one
alternative if a test returns true, and the other if the test returns
false.

`let`

Bard's basic binding form. `let` creates a scope in which some local
variables are bound to initial values.

`loop`

Bard's basic iteration form. `loop` works like Scheme's named let. 

`match`

Bard's pattern-matcher. `match` is a binding form, like `let`, but
supports pattern-matching expressions that can take values apart and
bind the parts to variables given in the patterns.

`receive`

Accepts an Actor's next message. An Actor is a Bard process. Each Bard
process can send messages to other Bard processes, and can receive
messages from other Bard processes. `receive` is the receiving side of
such exchanges. A message can be nearly any Bard value.

`repeat`

Evaluates the expressions in its body repeatedly. Repeat doesn't test
for an exit condition, and will loop without terminating unless you
arrange for control to exit somehow. The simplest way to do that is to
place a repeat inside a `with-exit` form and call the bound exit
function. This idiom makes it extremely simple to write loops, and
extremely easy to see them in code (but it's not particularly
compatible with functional programming; for a more functional approach
to iteration, see the `loop` form).

`send`

Sends a message to an Actor. An Actor is a Bard process. Each Bard
process can send messages to other Bard processes, and can receive
messages from other Bard processes. `send` is the seinding side of
such exchanges. A message can be nearly any Bard value.

`spawn`

Creates and returns a new Actor. The Actor represents a new Bard
process. The creating process can send messages to it and receive
messages from it. Depending on details of the Bard implementation, the
new Actor may represent a Bard process running in the same
operating-system process as its creator, or in a different OS process,
or even on a different machine.

`unless`

A variant of `if`; the expression

    (unless x a b c)

means the same thing as 

    (if (not x)
      (begin a b c)
      nothing)

`values`

Returns zero or more values. Bard functions may return any number of
values, and its binding forms can bind variables to any number of
values returned by functions.

`when`

A variant of `if`; the expression

    (when x a b c)

means the same thing as 

    (if x
      (begin a b c)
      nothing)

`with-exit`

Binds a variable to an exit function, then executes the body. Code in
the body of the `with-exit` form can jump out of it, return any value,
by calling the exit function.


## Schemas, Protocols, and Classes

A Bard datatype is made up of three parts:

- a **schema** describes the layout of bits and bytes
- a **protocol** describes functions that operate on the type
- a **class** identifies values that can be used in the protocol

Here's a very simple example:

We start by defining a schema named `<ratio>`:

    (define schema <ratio> num denom)

Defining the schema adds a method to the `make` function for creating
instances of the schema. We can now create instances of `<ratio>`:

    bard> (make <ratio> num: 1 denom: 2)
    #<ratio num: 1 denom: 2>

We can use the `get` function to retrieve the values in the fields of
<ratio> instances:

    bard> (define variable r (make <ratio> num: 3 denom: 5))
    #<ratio num: 3 denom: 5>

    bard> (get r 'num)
    3

    bard> (get r 'denom)
    5

The next step in making `<ratio>` part of a full-fledged type is
making it participate in a protocol. In this case, we'll do it by
creating a new protocol, but we could also have written methods to
connect it to existing protocols.

    (define protocol Rational
      (numerator Ratio) -> Integer
      (denominator Ratio) -> Integer)

To make `<ratio>` participate in the new Rational protocol, we write
methods that specialize the protocol's methods for `<ratio>`. Here's
what that looks like:

    (define method (numerator (r <ratio>))
      (get r 'num))

    (define method (denominator (r <ratio>))
      (get r 'denom))

The protocol says that numerator accepts an argument of type
Ratio. The method definition says that it's specialized for an
argument of type `<ratio>`. Taken together, those two definitions make
the schema `<ratio>` a member of the class Ratio. 

We've created a new type consisting of a class, a protocol it
participates in, and a schema that represents it.
