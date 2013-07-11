# Bard

version 0.4
<br/>copyright 2013 by mikel evins

## About Bard

Bard is a dynamic actor language whose design owes a great deal to Common Lisp, Scheme, and Dylan, and also draws elements from Smalltalk, Haskell, ML, Clojure, and other languages.

## Expressions and Values

Bard is a language of **expressions**. A Bard program is a sequence of expressions, and is executed by evaluating the expressions from start to finish.

Each expression denotes some number of **values**. An expression is either a **literal value**, a reference to a **variables** that is bound to a value, or a **list** that denotes an **application**, and which returns some number of values when it's evaluated.

A literal value that is not a list is called an **atom**. Atoms are expressions that denote simple values without component parts, like `true` and `false` and numbers.

A list is a sequence of other values delimited by parentheses ("`(...)`"), braces ("`[...]`"), or brackets ("`{...}`").

Parentheses delimit an **application**. An appllication is an expression that represents a procedure call, where the procedure being called is the first element of the list and the parameters passed to it are the remaining elements of the list. For example,

    (+ 2 3)
    
passes the parameters `2` and `3` to the built-in function named `+`. There are three kinds of procedures, called **functions**, **macros**, and **special forms**, each with its own rules for evaluation. These evaluation rules are discussed in the next section.

Braces delimit a **list constructor**. A list constructor returns a **list value**. A list value is a value that belongs to the `List` class. `List` comprises objects that are ordered sequences of values. Here's an example:

    bard> [1 2 3]
    [1 2 3]
    
Bard returns and prints the new list, which contains the integers 1, 2, and 3.

Bard evaluates the elements of a list constructor, so we can construct a list that includes computed elements. For example:

    bard> [1 (+ 2 3) (* 2 3)]
    [1 5 6]

Brackets delimit a **map constructor**. A map constructor returns a **map value**. A map value is a value that belongs to the `Map` class. `Map` comprises objects that are sets of key/value pairs. As an example:

    bard> {name: "Fred" age: (+ 20 12)}
    {name: "Fred" age: 32}
    
Like a list constructor, a map constructor evaluates its elements, so you can compute the keys and values as you construct the map. It is an error to write a map constructor with an odd number of elements; since a map is a set of key/value pairs, a map constructor must give a value for each key, meaning that the number of elements must be even.

### Applications and Evaluation Rules

There are three kinds of applications, one for each of the three kinds of procedures.

A **special form** is a procedure that is built into Bard and that has its own special rules for evaluation. Examples include the `define` and `if` special forms. Special forms cannot be redefined by programs. They form the foundation of the Bard language.

A **macro** is a procedure that rewrites an expression before evaluating it. For example, suppose `and` is implemented as a macro. In that case, this expression

    (and (odd? 2) 'yes)
    
might be rewritten to this expression

    (if (odd? 2) 'yes nothing)
    
before being evaluated. You can define your own macros using the `define macro` special form. The main use for macros is in extending Bard by adding new syntax.

A **function** is a procedure that selects and executes a **method** for a set of inputs and computes a result. Bard's function are **polymorphic**, meaning that they can execute different code for different sets of inputs. When you apply a function, it examines its inputs to determine the code to run. The code it selects is called a **method**. You can define methods using the `define function` special form.

### The Bard Reader

Bard uses a **programmable reader**, like many other dialects of Lisp. The reader is a program that transforms text into Bard source code, converting strings of text characters into the lists and atoms that compose Bard expressions.

You can modify the operation of the reader using built-in procedures provided by the language to define **reader macros**. A reader macro defines a transformation from text to Bard expressions.

Bard also provides a useful set of built-in reader macros. For example, list constructors and map constructors are implemented as reader macros that transform the text into `list` and `map` expressions.

The reader and reader macros are described more thoroughly in the chapter about the reader.

## Data Types

### Type in Bard

#### Schemas

Every Bard value has a single concrete datatype called its **schema**. A **schema** is a description of how to represent a family of values. For example, `<fixnum>` is a schema that describes a representation for integers that can fit into a single machine word; `<bignum>` is a different schema, describing a representation for integers that uses as much memory as needed. Bard provides many built-in schemas.

A value's schema is not the same thing as its **type**, for two reasons. First, different representations may be used for the same type. For example, both `<fixnum>` and `<bignum>` may be used to represent an integer type. Second, the same representation may be used for different types. For example, `<fixnum>` is a useful representation of a certain range of integers, but it can be used to represent other types as well. You could use `<fixnum>` to represent a packed array of Boolean values, for example, with each bit of the `<fixnum>` representing one of the packed bits.

For these reasons, a schema is only part of a Bard type; the other parts are its **class** and **protocol**.

### Defining Schemas

You can define new schemas using the `define schema` special form.

    bard> (define schema <point> x y)
    <point>

#### Protocols

A **protocol** is a set of related operations. The `Lists` protocol, for example, describes operations that can be performed on values that represent lists. Here's a small portion of the definition of the `Lists` protocol:

    (define protocol Lists
      add-first (Anything List -> List)
      add-last (List Anything -> List)
      first (List -> Anything)
      image (Function List -> List))
      
#### Defining Protocols

You can define new protocols using the `define protocol` special form:

    (define protocol Greeting
      hello (Text -> Text))
      
#### Classes

A **class** is an identifier that does two jobs: it names a role in a protocol, and it identifies a set of relationships to other types. For example, the `Lists` protocol defines a function named `image` that has this signature:

    (Function List -> List)
    
The function signature says that `image` takes two input parameters and returns one output value. The inputs are a `Function` and a `List`; the output is a `List`. `Function` and `List` are classes.

A Bard class does not define how values are represented, and does not define how functions operate. It's just a _name for an abstract type_ that identifies the role the type plays in the protocol.

A class can have **members**, and it can be a member of another class. For example, `List` is a member of the class `Anything`--in fact, every Bard type is a member of `Anything`.

#### Asserting Class Membership

A value is an instance of a class if its schema is a member of the class. Thus, instances of `<pair>` are also instances of `List`, because `<pair>` is a member of `List`.

How does a schema become a member of a class?

There are two ways: you can explicitly assert that a schema belongs to a class, or you can define a method for a protocol function that implicitly asserts the schema's membership.

The first way is simpler: just mention a schema (or a class) in a `define class` expression:

    (define class <pair> List)
    
This expression asserts that `<pair>` is a member of `List`.

You can do the same thing with a class instead of a schema:

    (define class MyListClass List)

This expression asserts that `MyListClass` is a member of `List`.

You can't reverse the schema and the class, though; you can't say that `List` is a member of `<pair>`, because `<pair>` is a schema, not a class, and schemas do not have members.

If you say 

    (define class MyListClass List)

and `MyListClass` doesn't exist already, then Bard creates it. That doesn't work with schemas, though. It works with classes because a class is just a name, but a schema is more than that. A schema is a description of how to lay data out to represent a value; without the definition of its fields, a schema is meaningless. That means you have to define a schema before you can make it a member of a class.

#### Defining Functions

The second way to make a schema a member of a class is to define a method on a protocol function. A protocol function is a function that is a member of a protocol (as all Bard functions are). As we've seen, protocols define function signatures that give classes for their inputs and outputs. When you define a method that applies to values of a particular schema, you are asserting that the schema is a member of the corresponding class.

Let's look at a very simple protocol to see what that means. Here's an extremely simple `Rational` protocol:

    (define class Ratio Number)

    (define protocol Rational
      numerator (Ratio -> Integer)
      denominator (Ratio -> Integer))

This example defines the class `Ratio` and the protocol `Rational`. The `Rational` protocol defines two functions named `numerator` and `denominator`. Each of them accepts a single input, an instance of `Ratio`, and returns a single output, an instance of `Integer`.

At this point the class `Ratio` is defined, but it has no members, because we haven't asserted any members and we haven't defined any methods that identify members of `Ratio`. Let's define a schema that we can use as a member of `Ratio`:

    (define schema <ratio> 
      (num reader: ratio-num) 
      (denom reader: ratio-denom))
    
The `define schema` expression creates a schema object and binds it to a new variable named `<ratio>`. The schema object defines two **slots** named `num` and `denom`. We also gave each slot a **reader** function, named `ratio-num` and `ratio-denom`; the reader functions enable us to get the values in the slots of a `<ratio>` instance.

We can now create an instance of `<ratio>` using the built-in `make` function:

    bard> (make <ratio> num: 1 denom: 2)
    {schema: <ratio> num: 1 denom: 2}    

Now we have a `<ratio>` schema suitable for use as a member of `Ratio`, but we haven't yet told Bard that it's a member of the class. We could do so the same way we saw in the previous section, but then the functions `numerator` and `denominator` would still be undefined for `<ratio>` values. So instead, let's tell Bard how to execute `numerator` and `denominator` when their inputs are instances of `<ratio>`:

    (define function (numerator x)
      with {x <ratio>}
      (ratio-num x))

    (define function (denominator x)
      with {x <ratio>}
      (ratio-denom x))
      
Now `numerator` and `denominator` are defined on instances of `<ratio>`. What's more, because we gave `<ratio>` as the type of the `Ratio` arguments to `numerator` and `denominator`, Bard automatically asserts that `<ratio>` is a member of `Ratio`.

What if we defined the `numerator` method, but not the `denominator` method? Bard would still automatically assert that `<ratio>` is a member of `Ratio`, because we gave `<ratio>` as the type of the `Ratio` argument to `numerator`, but it would warn us that the `Rational` protocol is only partially defined for the schema `<ratio>`. In fact, it does the same thing if we explicitly assert a schema as a member of a class using the `define class` special form, and for the same reason: because the protocol has functions that are not defined on that schema.

### Categories and Criteria

A **category** is a type whose members are defined by a **criterion**. A **criterion** is a function that accepts one argument and returns either `true` or `false`. If you apply a criterion to a value and it returns `true` then the value is an instance of the type; otherwise it's not. It's simple to define a category; here's an example:

    (def odd-integer? (compose odd? integer?))
    
    (define category OddInteger odd-integer?)
    
`OddInteger` is now a category whose instances are odd integer values. 

In most respects a category behaves like a class, but you cannot use `define class` to assert new members of a category. You can use `define class` and `define function` to add a category as a member to another class, though.

### Singletons

A **singleton** is a type that comprises exactly one value. For example, a singleton type for the value `true` has exactly one instance: the value `true`.

Singletons make it possible for Bard functions to discriminate specific values, rather than just types of values. For example, consider this definition of the Fibonacci function:

    (define function (fib n)
      with {n (exactly 0)}
      1)
      
    (define function (fib n)
      with {n (exactly 1)}
      1)
            
    (define function (fib n)
      with {n <fixnum>}
      (+ (fib (- n 1))
         (fib (- n 2))))
         
These three expressions define three methods for the function `fib`. The first two use singleton types for the input argument `n`; the first one applies to `n` when it's equal to zero; the second applies when `n` is equal to one.

The third definition applies to any other case where `n` is a `<fixnum>`.

The expression `(exactly 0)` returns a singleton type for the value `0`. That type has exactly one instance: the value `0`.

You can evaluate `(exactly 0)` anywhere you can evaluate any other Bard expression; it will always return the same singleton type. Normally, though, the only reason to use an `exactly` expression is for defining methods.


## Scope and Extent

## Program Structure

## Control Structure

## Macros

## Names and Modules

## Numbers

## Sequences, Streams, and Maps

## Arrays

## Generators and Collectors

## Input and Output

## Resources, Paths, Files, and Networks

## Actors and Messages

## Conditions

## Iteration, Mapping, and Reducing

