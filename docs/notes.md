# bard 0.4 notes

## VM

bard 0.4 will use a portable VM written in Gambit Scheme and compiled
to native code for several target platforms. The intended targets are
Linux, Windows, macOS, and Android; other targets may be added.

Each VM supports the same language, library, and runtime.

The bard compiler will produce an architecture-independent bytecode
called **bardo** (for "bard objects"). Every bard vm will be able to
load and run bardo files unchanged.

bardo will contain:

- toplevel constant, variable and type definitions
- executable code
- cryptographic data that identifies the source of a bardo file and enables a VM to decide whether to trust it

A bard VM manages a set of **actors**. An actor is a self-contained
runtime environment with its own thread of control, variable
environment, and cryptographic identity. Each actor has a **mailbox**
on which it receives **messages**. A message is a bard object
contained in a wrapper that provides cryptographic data identifying
its source. If the receiving actor trusts the source then it unwraps
the object and evaluates it. If not, then it unwraps the object,
re-wraps it with an "untrusted message" condition, and sends it in a
message to itself. The default handler for untrusted messages logs
information about them and then discards them.

A vm can pause execution of all of its actors, serialize their state
to a stream, and resume execution. The serialized state data can be
stored in a bard **image** file. When bard starts up, it accepts an
image file as an argument or, if none is provided, looks for its
default image file and reads that. It deserializes the actors
contained in the image file and starts them running.

A bard image file, or a bardo object file, can be read and executed by
any bard vm, provided that the vm is configured to accept the
cryptographic identity recorded in the file. bardo and image files are
architecture independent. They can even be sent in messages from one
bard vm to another, either locally or over the network. They can be
stored on servers and downloaded for execution.

Although bardo and image files are architecture independent, the bard
VM is designed to allow native-code compilation. Bytecode and
native-code objects differ only in the data they contain. Both are
instances of a common type that is interchangeable as long as the
platform running the object supports the native code format.

I have no plans to actually implement native-code compilation for bard
0.4; the code format is in preparation for later extensions.

## Types

bard 0.4 defines two kinds of types, called **classes** and
**structures**. A **structure** is a concrete representation of
data. A **class** is an abstract family of structures. It defines a
set of functions that return instances of the class, or that accept
them as inputs. Any structure may be made a member of a class by
defining suitable methods for the functions of the class. A structure
may simultaneously belong to any number of classes.

bard is a dynamically-typed language that is designed to support
static typechecking. bard 0.4 implements dynamic types and runtime
typechecking, but it also supports type declarations, and function
definitions require the specification of input and output types. The
intention is that future versions of bard will gradually move to
provide more and more static typechecking and type inference for both
performance and safety reasons, but without giving up dynamic typing
and its associated advantages.

## Operators: functions, methods, accessors, macros, types, and special forms

bard is an **expression-oriented** language; every valid piece of a
bard program is an expression that returns zero or more values. Except
for variable references and literal constants, bard expressions are
always applications of operators to arguments.

There are six types of operators in bard:

- **special forms** are operators built into the bard language. They
    cannot be changed, and they can define their own rules of evaluation.

- **functions** are polymorphic operators whose arguments are
    evaluated before application occurs. A function examines its
    arguments to determine an applicable **method** before applying
    the method to the arguments. Different methods may be applied to
    different arguments.

- **methods** are monomorphic operators whose arguments are evaluated
    before application occurs. The arguments to a method must be of
    the correct type; otherwise an error occurs when the method is
    applied. The normal way to apply a method is for it to be selected
    by bard when a function is applied at runtime (or compile time, if
    the compiler can prove that only one method can match the given
    arguments). Methods can be defined and applied independently of
    functions, though.

- **accessors** are operators defined by the definitions of
    structures. Accessors are used to retrieve the values of elements
    of structures, or, in the case of mutable fields, to update their
    values.

- **types**, in addition to representing bard datatypes, are also
    operators that can construct instances of them. Both classes and
    structures are **constructors** that can create instances of
    themselves when applied to appropriate arguments.

- **macros** are operators that transform expressions before
    evaluating them. A macro is code that writes code, then executes
    it. Bard macros work like Common Lisp macros, with the same
    advantages and hazards. They should be used with caution.

## Modules

Bard operators, types, and variables are named by **symbols**. Symbols
inhabit **modules**. A module is a finite map from text names to
symbol objects. Every bard expression exists in exactly one module. If
no module declaration establishes the current module in a piece of
code then the current module is `bard.user`.

Two symbols with the same name are the same symbol only
if they occur in the same module, either by being defined in that
module, or by being imported into that module from another one.

Every symbol is defined in some module. By default, symbols are
private, meaning they can be referred to only from within the module
that defines them, and they cannot be imported into other modules.

A symbol is **exported** if a module definition exports it, or if it's
exported from within its module by the `export` special form. Exported
symbols can be referred to or imported from other modules.

    (define-module example1
      (export foo bar))

    (in example)

    (define foo 101)
    (define bar 1001)
    (define quux "oops")

    (define module example2
      (import-from example1 foo))

    (in example2)

    foo ; returns 101
    bar ; signals an error because we didn't import bar
    example1:bar ; returns 1001
    example1:quux ; signals an error because quux is not exported

bard is a Lisp. It does not provide absolute encapsulation of anything
because that would prevent useful interactive programming. There is
therefore a way to refer to a private symbol from another module,
using a double colon:

    example1::quux ; returns "oops"

As a matter of style, code that contains double colons should be
treated as a bug. You can use a double colon when you must, but
production code should never contain them.

### Built-in modules

- `bard.core`: the kernel of bard
- `bard.user`: the default module, used for casual exploration and experimentation
- `bard.system`: operators and variables pertaining to the runtime platform
- `bard.io`: operators and variables pertaining to files, input, and output
- `bard.net`: operators and variables pertaining to network operations

## The reader

bard is a lisp with a traditional customizable reader. The reader, in
traditional fashion, uses a **readtable** to control how program text
is converted to bard data structures, and provides a standard set of
operators and data structures for controlling and extending the
reader.

That means you can extend and customize bard's lexical syntax, but you
should do that rarely and judiciously. An example of a judicious use
of the bard reader is to define a simple syntax extension to support
literal constants for a structured datatype so that your code can read
literal values from files.

## Built-in types

bard provides a small but rich set of built-in classes and structures, including:

- `anything`: the class of all bard values

- `nothing`: the class of values that represent empty collections or absent values.
  The sole instance of `nothing` is itself.

- `boolean`: the values `true` and `false`

- `number`: the class of numeric structures, including integers,
  ratios, and floating-point numbers

- `text`: the class of text strings, including Unicode strings

- `character`: the class of glyphs, including Unicode code points

- `name`: the class of objects used to name bard types, values, and
  operators, including symbols and URIs

- `operator`: the class of objects that represent applicable
  operators, including functions, methods, accessors, and
  types. Macros and special forms are not instances of `operator`;
  they are special syntactic constructs.

- `actor`: the class of bard execution environments. An instance of
  `actor` is a bard runtime with a **mailbox**. A program can send
  messages to it in order to interact with it. A running bard runtime
  always has at least one actor, accessible by evaluating
  `(this-actor)`, and may have more.

- `list`: the class of bounded, ordered collections of values.

- `set`: the class of bounded, unique, unordered collections of values

- `stream`: the class of unbounded, ordered collections of values

- `map`: the class of finite maps

## Stores

bard 0.4 introduces the concept of **stores**. A **store** is a
serialized finite map from names to values. The `store` class defines
operations that enable you to construct stores, add mappings to them,
write them to and read them from files, and query them using a
prolog-style query language.

Instances of `store` are also instances of `map`.

