# Bard
### version 0.4

Bard 0.4 is a new version of the Bard programming language. Compared to previous versions, it offers a more complete implementation of the language with better performance and more conveniences.

## Introduction

Bard is a Lisp. It's a descendant of Scheme, Common Lisp, and Dylan with influences from Smalltalk, Haskell, ML, and several other languages. It's intended to be a reasonably small, extensible language that provides enough amenities to support general-purpose programming. It's also intended to consolidate important features of its ancestors into a coherent and congenial whole.

### Distinctive features

### Expressions

Bard programs are made up of **expressions**. Every valid element of a Bard program is an expression. Every expression is **evaluated** to yield zero or more **values**. 

Expressions are either **atoms** or **lists**. A **list** is a sequence of expressions delimited by parentheses (`(...)`), braces (`{...}`), or brackets (`[...]`). A list may be empty, or may contain any number of **subexpressions**. Each subexpression may be either an atom or a list.

An atom is any valid expression that is not a list.

### Values

Bard expressions represent **values**. In the execution of a Bard program, each expression is **evaluated** to yield zero or more values.

#### Self-evaluating expressions

In the most common case, an expression evaluates to itself. For example:

    bard> true
    true
    
    bard> 5
    5
    
Such expressions are called **self-evaluating expressions**. Most expressions in Bard are self-evaluating.

#### Variable references

A Bard **symbol** normally denotes the name of a **variable**. The variable may be defined in the local lexical scope, or it may be defined in some enclosing scope. The most inclusive scope in a Bard program is **module scope**, which is approximately equivalent to global scope. **module scope** is further explained in the chapter on **scope and extent**, and in the chapter on **modules**.

Here are two module variables and the values they yield when evaluated:

    bard> PI
    3.141592653589793
    
    bard> +
    {type: <function> signature: (Integer * -> Integer)}
    
#### List expressions

There are three kinds of list expressions: **applications**, **list constructors**, and **map constructors**.

An **application** is a list delimited by parentheses (`(...)`). It denotes a procedure call:

    bard> (odd? 2)
    false
    
    bard> (* 2 3)
    6
    
An application is evaluated by determining the procedure represented by the first element of the list and then executing it according to its **evaluation rules**. There are three kinds of procedures, each with its own rules for evaluation.

**Special forms** are operators that are built into the language, each with its own evaluation rules. Special forms make up the basic syntax of Bard, and include operators like `define` and `if`. There are relatively few special forms in Bard, and they may not be redefined by programs.

**Macros** are operators that rewrite Bard source code before executing it. When you call a macro, the macro procedure receives the source code of the call as input. It rewrites the source code according to its defined rules, then evaluates the resulting form. Macros are primarily used for extending Bard with new syntax.

A **Function** is an operator that examines the values passed to it as arguments and selects a **method** to execute. If the first element of an application is not a special for or a macro, then Bard evaluates it to obtain a function, then evaluates the argument expressions from left to right, then calls the function with the argument values as parameters. The function chooses a method applicable to the argument values and executes it to produce zero or more result values.

You can suppress the evaluation of an application by **quoting** it:

    bard> '(odd? 2)
    [odd? 2]


A **list constructor** is a list delimited by brackets (`[...]`). Evaluating a list constructor returns a **list** value whose elements are obtained by evaluating the expressions delimited by the brackets. For example:

    bard> [0 1 2 3]
    [0 1 2 3]
    
    bard> [0 (+ 1 2) 3]
    [0 3 3]

The effect of evaluating a list constructor is the same as that of evaluating an application of the built-in `list` function:
    
    bard> [0 (+ 1 2) 3]
    [0 3 3]
    
    bard> (list 0 (+ 1 2) 3)
    [0 3 3]

A **map constructor** is a list delimited by braces (`{...}`). Evaluating a map constructor returns a **map** value whose keys and values are obtained by evaluating the expressions delimited by the braces. For example:

    bard> {1 2 3 4}
    {1 2 3 4}
    
    bard> {name: "Fred" age: (* 2 16) weight: (+ 200 10)}
    {name: "Fred" age: 32 weight: 210}

A **map** is a value that aggregates components into a set of key/value pairs. It is an error to evaluate a map constructor with an odd number of elements.

### Types

A **type** in Bard is a defined set of related values that support a common set of operations. There are three components that make up each type: a **class**, a **protocol**, and a set of **schemas**.

A **class** is a value that identifies the type and its relation to other types. Classes are defined by **protocols**. Examples of Bard classes are `List` and `Number`.

A **protocol** is a collection of functions that are defined on a set of types. Here's an example: a very simple `Rational` protocol:

    (define protocol Rational
      numerator (Ratio -> Integer)
      denominator (Ratio -> Integer))

In the `Rational` protocol given here, `Rational:numerator` and `Rational:denominator` are functions that belong to the protocol. `Rational:Ratio` and `Rational:Integer` are classes defined by the protocol.

The syntax `Protocol:name` denotes a variable named `name` that belongs to the protocol `Protocol`.

A class is an abstract type with no direct instances. No Bard value can be a direct instance of any class, because classes don't describe representations of data; they only name roles in protocols.

The concrete type of a value--that is, the type that describes the value's representation, is a **schema**. Here's an example of defining a schema that could be used to represent the class `Ratio` that is defined in the `Rational` protocol:

    (define schema <ratio>
      (num type: <fixnum> reader: ratio-num)
      (denom type: <fixnum> reader: ratio-denom))

Evaluating this expression defines a new schema named `<ratio>` with two slots, named `num` and `denom`. We can create an instance of the new schema using `make`:

    bard> (make <ratio> num: 1 denom: 2)
    {type: <ratio> num: 1 denom: 2}

The schema definition also defines two **reader** functions whose names are given in the definition: `ratio-num` and `ratio-denom`. We could also provide names for **writer** functions, in which case Bard will mark the slots of the schema **mutable** and create writer functions for them with the names we supply. Most Bard values are immutable, though, and so by default it doesn't create writers.

If you don't supply the names of readers, Bard creates reader functions with default names. For example, if we instead wrote the definition of `<ratio>` this way:
    
    (define schema <ratio> num denom)

then Bard would define readers named `<ratio>-num` and `<ratio>-denom`. Since this example omits types for the slots, it would also implicitly define the types of the slots as `Anything`.

To establish the schema `<ratio>` as a **member** of the class `Ratio`, we have to define how the functions of the `Rational` protocol work when instances of `<ratio>` are passed to them. Here's how we do that:

    (define function (Rational:numerator x)
      with {x <ratio>}
      (ratio-num x))

    (define function (Rational:denominator x)
      with {x <ratio>}
      (ratio-denom x))

Each of these functions defines a **method** for a protocol function that applies when its argument is an instance of `<ratio>`. That's all it takes to make `<ratio>` a member of the class `Ratio`: methods for the protocols functions that makes them defined over instances of `<ratio>`. To put it a slightly different way: a schema is a member of a class if the class' protocol is defined on the schema. 

What if we defined `numerator` for `<ratio>`, but didn't define `denominator`? Bard would then consider `<ratio>` a member of `Ratio`, but would warn us that the `Rational` protocol was only partially defined on `<ratio>`.

It's also possible to define a schema as a member of a class without defining any methods at all, by using a `define class` expression. If you do that, then Bard will consider the protocol partially defined, just as if you defined methods for some functions but not others.

#### Categories

A **category** is a class whose members are defined by a function called a **criterion**. A **criterion** is a **predicate**--a function that accepts one argument and returns `true` or `false`. If the criterion function returns `true` then its argument is a member of the class; otherwise, it is not.



## Notation and literals



## Data Types
### The concept of type
### Protocols
### Functions
### Classes
### Schemas
### Criteria and categories

## Built-in types
### Numbers
### Characters
### Names
### Collections
### URLs
### IOStreams
### Actors
### Modules
### Readtables
### Random states
### Conditions
### Types

## Built-in protocols
### Addressing
### Comparing
### Constructing
### Converting
### Listing
### Logic
### Mapping
### Ordering
### Pairing
### Streaming
### Text
### Types
### System

## The class graph

## Scope and extent

## Program structure
### Expressions
### Self-evaluating expressions
### Variables
### Function application
### Macros and special forms
### Definitions

## Control structure
### Constants and variables
### Generalized variables
### `begin`
### Binding forms
### Conditionals
### Blocks, exits, and conditions
### Iteration: Mapping, reducing, looping, and generators
### Multiple values

## Macros

## Modules

## The reader

## The evaluator

## The printer

## Events and messages

## Filesystems

## Networks

## Presentation

## System tools (compiler, gc, image saving and loading, host environment, etc.)

