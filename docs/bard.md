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

This document is a brief overview of the language.

## Syntax and built-in classes

Bard is a Lisp. That means that, first, every valid piece of Bard code
is an expression that returns a value, and, second, that there are two
kinds of expressions: atoms and lists.

A list is a sequence of zero or more expressions enclosed in
parentheses, like this:

    (foo bar baz)

An atom is any expression that is not a list:

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
for each of its basic built-in types. In Bard, these types are called
"classes"; there's more to types than just classes, and Bard classes
are a little different from classes in other languages, but for now,
all you need to know is that Bard provides around a dozen built-in
types called "classes", and it provides a literal lexical syntax for
each one.

Here they are:

**Undefined**

    undefined

The class of unknown, or undefined values.

**Null**       

    nothing

The absent or empty value.

**Boolean**    

    true, false

Logical true and false values.

**Number**     

    0, 1.2, 2/3

Magnitudes and other numeric values.

**Text**       

    'foo, 'Bar, baz:, "Fred and Barney"

Symbols, keywords, and strings.

**Pair**

    [1 2], ['a 'b]

A class that represents a pair or an association of arbitrary values.

**List**

    '(), '(0 1 2 3 4)

An orderd sequence of values

**Table**

    {a: 0 b: 1}

A mapping from keys to values. Both keys and values may be arbitrary
Bard objects, with two restrictions: neither keys nor values may be
undefined, and keys may not be nothing. A table may be treated as a
list of pairs, though it isn't necessarily represented that way.

**Method**

    (^ (x) x)          
    (^ (x y) (+ x y))

A callable object that executes some code when applied to zero or more
arguments.

**Function**

    (-> Number Number)

An abstract, polymorphic, callable object.  Functions are like
methods, except that they don't have any code of their own to
execute. Instead, they have methods that they know about. When a
function is applied to some arguments, it inspects the arguments
looking for a match to a method it knows about. If if finds a match,
it applies the method to the arguments.  If it doesn't, it raises an
exception. Functions choose methods based on the types of their
arguments.

### Specialized classes

Besides Bard's basic built-in classes, it has a few more types with
more specialized uses, and most of these don't have standard literal
syntax defined for them. These more specialized classes include:

**Actors**

Objects that represent a running Bard process. A Bard program can create
actors that run in the same process as the creating Bard process, or in a
new process. It can communicate with actors by sending messages to them;
messages can contain arbitrary Bard data--even functions, methods, or
other actors.

**Stream**

Objects that serve as sources or sinks for data. Objects that belong
to the Stream class include input ports, output ports, and generators.

**Class**

An object that represents a class

**Schema**

An object that described a concrete arrangement of data.  A schema
describes how a value is actually represented; Bard provides tools
with which users can define their own schemas and associate them with
classes.

**Singleton**

An object that presents a data value as if it were a type.  Singletons
make it possible for functions to dispatch on individual values, as
well as types of values.

**Type**

Objects that serve the purpose of types. Types include classes,
singletons, and schemas, among other possibilities.

## Bard's basic vocabulary

In order to write programs in a language, you need to know, in
addition to the data structures it provides, its basic vocabulary of
operations. As in other Lisps, Bard's operations fall into three
general categories: **functions**, **special forms**, and **macros**.

The differences between functions, special forms, and macros have to
do with the way that the handle evaluation of their arguments. The
details are not important here, so this section won't call out whether
a particular operation is a function, a special form, or a macro. The
important point is that all three kinds of expressions look similar:
like function calls.

Here is Bard's basic vocabulary of operations:

`^`

The `^` special form can also be spelled `method` or `lambda`. It
creates a method object.

`begin`

The basic sequencing form. Begin evaluates a series of expressions
from left to right, returning the last value returned.

`cond`

Along with `if`, the basic form of flow control in Bard. Bard's `cond`
works like the form of the same name in Scheme and Common Lisp.

`define`

Defines a global variable. 

`ensure`

Provides a way to execute a piece of code that might signal an error,
and ensure that a cleanup form is executed aftward, even if the risky
code exits abnormally.

`if`

With `cond`, the basic flow control in Bard. Evaluates one alternative
if a test returns true, and the other if the test returns false.

`let`

Bard's basic binding form. `let` creates a local lexical environment
in which some variables are bound to initial values. It combines
features of several similar binding forms from earlier Lisps,
including Scheme's letrec and Common Lisp's LET* and
MULTIPLE-VALUE-BIND.

`loop`

Bard's basic iteration form. `loop` works like Scheme's named let.

`match`

Bard's pattern-matcher. `match` is a binding form, like `let`, but
supports pattern-matching expressions that can deconstruct data and
bind its parts to variables in a pattern.

`receive`

Accepts the next message from the mailbox of the Bard
process. `receive` is the message-reception side of Bard's actor
abstraction.

`send`

Adds a message to the mailbox of an actor. `send` is the
message-origination side of Bard's actor abstraction.

`spawn`

Creates a new actor. The new actor executes in a new thread, either in
the same process as the creating Bard process, or a new one. The
returned object can be used as the target of `send` calls.

`unless`

A synonym for `(if (not...`.

`values`

Returns zero or more values. Bard functions may return any number of
values, and its binding forms can bind variables to any number of
values returned by functions.

`when`

A synonym for `(if ...` in the case that there is no form to execute
if the test returns false.

`with-exit`

Binds a variable to an exit function, then executes its
body. `with-exit` implements a restrictired form of continuations,
enabling Bard that executes within the `with-exit` body to at any time
exit the form by calling the exit function.

