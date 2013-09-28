# Bard
version 0.4
by mikel evins

## Basic ideas

Bard is a language and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases within phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is a collection of expressions that, when evaluated, produces some values.

A **procedure** is a value that can be **applied** to other values to compute a result. There are four kinds of procedures: **functions**, **methods**, **macros**, and **special forms**.

A **type** is a specific way to represent values. Each type can represent some values, but not others. Every value has exactly one type.

A **class** is a collection of types with a common **protocol**. A **protocol** is a set of related **functions**.

## An Overview of Bard

### Types in Bard

A **type** describes how certain values are represented. It's a concrete description--in other words, each type describes specifically how to represent certain values. There are no abstract types.

A **class**, on the other hand, is a collection of types that share a common **protocol**. All classes are abstract. No value is ever a direct instance of a class; it's always an instance of a type. Every type belongs to at least one class, though, and its values are said to be **indirect instances** of the classes it belongs to.

Here's an example of how types and classes work: 

Let's start with the values `[1 2]` and `"Hello!"`. Let's say `[1 2]` is an instance of `<pair>` and `"Hello!"` is an instance of `<string>`. 

The type `<pair>` is a member of the class `List`, which means that the functions of the `List` protocol can be used on `[1 2]`:

    bard> (first [1 2])
    1
    
    bard> (last [1 2])
    2


The type `<string>` is also a member of `List`:
    
    bard> (first "Hello!")
    \H
    
    bard> (last "Hello!")
    \!

Usually we would just say that `[1 2]` and `"Hello!"` are lists. That's how types and classes work in Bard: when you're talking about what a value "is", and what you can do with it, you generally talk about some class that it belongs to. If for some reason you need to pay attention to exactly how a value is represented, then you talk about its type.

Bard provides a useful library of built-in types and classes, and also provides tools you can use to make your own.

There are two special data descriptions that are not regular types or classes.

**`Anything`** is called the **superclass**. It's called *the* superclass because Bard has only one. Every class is a subclass of **`Anything`**, but there are no other superclasses, and no Bard class has any subclasses.

Because every class is a subclass of `Anything`, every value is an indirect instance of `Anything`.

The second special data description is the **subtype**, **`<null>`**. Once again, there's only one. **`<null>`** has exactly one value, which is written **`nothing`**. As you might have guessed, `nothing` represents the empty set, or the null value. `nothing` is an instance of every type, and indirectly an instance of every class. `<null>` is a member of every class.

The entire language is organized around classes and protocols. Every value, type, and procedure has a place in a class and its protocol, which makes helps make it easy to learn the language and find the tools you need.

Bard's built-in classes include:

* **Class**: the class of all classes
* **Enumeration**: the class of **enumerated types**. An enumerated type is one whose values are individually and explicitly specified. Examples include `<boolean>`, `<symbol>`, and `<character>`.
* **List** includes types that represent ordered sequences of values.
* **Map** includes types that represent collections of **keys** that are associated with values. A key is a value that is used to identify another value contained in a map. For example, in the map `{a: 1 b: 2}`, the key `a:` identifies the value `1`.
* **Number** includes all the types that represent numeric values.
* **Procedure** includes types that can be applied to arguments to compute result values.
* **Resource** includes types that represent files, devices, network connections, and other abstractions of hardware capabilities.
* **Stream** includes types that produce one value after another when asked for it, or that consume one value after another when it's offered, or both.


### Literal values

Bard can print almost every value in a way that allows it to read the value back in. To take a simple example, consider the following interaction:

    bard> (+ 2 3)
    5
    
In this interaction we add the numbers 2 and 3, resulting in 5. Bard prints that result as the text `5`. It can read that text--`5`--and constuct a value that is equal to the value it printed.

Text like this--that is, text that Bard can read to construct a specific value--is called a **literal**. Bard provides literal notation for almost all values. There are a few exceptions that can't be easily treated this way, and they are printed specially to distinguish them as **unreadable values**, but as far as it's practical, Bard provides literal notation for all its values.

The following sections list examples of literals for each built-in class. Remember that a class is a collection of types; each example lists one or more types that can represent the value given in the literal.

#### Numbers

| values | types |
| --- | ------------------------- |
| `0` | `<bit>` `<u8>` `<fixed>` |
| `1.2` | `<float>` |
| `2/3` | `<ratio>` |
| `999999999999999999999999` | `<bignum>` |

#### Enumerations

| values | types |
| --- | ------------------------- |
| `true` | `<boolean>` |
| `\A` | `<character>` |
| `\space` | `<character>` |

#### Lists

| values | types |
| --- | ------------------------- |
| `nothing` | `<null>` |
| `(1 . 2)` | `<pair>` |
| `(1 0 1 0)` | `<pair>` `<vector>` `<bitvector>` |
| `(1 2 3 4)` | `<pair>` `<vector>` `<u8vector>` |
| `(1 -2 3 -4)` | `<pair>` `<vector>` `<s8vector>` |
| `(1 -2 3 -4000)` | `<pair>` `<vector>` `<s16vector>` |
| `"Hello"` | `<string>` |

#### Maps

| values | types |
| --- | ------------------------- |
| `{}` | `<null>` |
| `{a: 1 b: 2}` | `<pair>` `<ordered-map>` `<weight-balanced-treemap>` |

#### Streams

| values | types |
| --- | ------------------------- |
| `#<producer>{id: 0 element-type: <u8>}` | `<producer>` |
| `#<consumer>{id: 1 element-type: <u8>}` | `<consumer>` |
| `#<iostream>{id: 1208 element-type: <u8> direction: io}` | `<iostream>` |

#### Procedures

| values | types |
| --- | ------------------------- |
| `(-> ->)` | `<function>` |
| `(-> Number Number -> Number)` | `<function>` |
| `(^ (x) x)` | `<method>` |
| `(^ (x y) (* x y))` | `<method>` |

#### Resources

| values | types |
| --- | ------------------------- |
| `#<file>{id: 1501 url: "file:///tmp/foo.txt"}` | `<file>` |
| `#<website>{id: 11831 url: "http://bardcode.net"}` | `<website>` |
| `#<actor>{id: 32042 url: "http://bardcode.net/bardrepl"}` | `<actor>` |

### Evaluating expressions

If you type an expression at the Bard prompt, Bard reads it and evaluates it to return a result. Most values that you can type at the prompt are **self-evaluating**. When Bard evaluates a **self-evaluating** value, the result is just the same as the value you typed.

Here are a couple of examples:

    bard> 5
    5
    
    bard> "Hello!"
    "Hello!"
    
    bard> { a: 1 b: 2 }
    
#### Symbols

A symbol is a value represents a name. Here are a few examples:

    x
    *
    Fred
    ALongerSymbol
    
Symbols are not self-evaluating. When you type a symbol at the Bard prompt, Bard treats it as a reference to a **variable**. A **variable** is a name that is bound to a value.

    bard> *bard-version*
    (0 4 0)
    
When we type `*bard-version*` at the prompt, Bard recognizes it as a symbol and looks up the value of the variable with that name. In this case the value is the version on the Bard system, which is a List of numbers.

You can define your own variables:

    bard> (def n 5)
    n
    
    bard> n
    5
    
Once a variable is defined, you can assign a new value to it:
    
    bard> (set! n 105)
    105
    
    bard> n
    105
    
#### Lists

Lists are not self-evaluating, either. Bard is a Lisp, and like other Lisps, it treats lists as procedure calls. Here's what I mean:

    bard> (+ 2 3)
    5
    
When it encounters a list, Bard assumes you mean to call a procedure. The first element in the list indicates the procedure you want to call, and the remaining elements are the arguments to the procedure. In this example, the procedure is indicated by the symbol `+`. Bard looks it up and finds the addition function bound to that variable. It applies the addition function to 2 and 3, yielding 5, which it prints.

#### Quoting

What if you want to use a symbol or a list as a piece of data? In other words, what if I want to make a list of the symbols `x`, `y`, and `z`? If you write a list containing them you run into a problem:

    bard> (x y z)

    ERROR: Undefined variable: x 

As before, Bard sees a list and assumes it's a procedure call. It looks up `x` to find the procedure, and notices that there's no variable named `x` defined, so it can't proceed.

So how do we write a list of x, y, and z?

That's what **quoting** is for. When you quote a value, you're telling Bard not to evaluate it.

    bard> 'x
    x
    
Typing `'x` at the prompt tells Bard to return the symbol `x` itself, instead of looking up a variable named `x`.

We can do the same thing with a list:

    bard> '(x y z)
    (x y z)
    
Quoting most values has no effect; the result is the same as if you didn't quote them.

    bard> '5
    5
    
    bard> 'true
    true
    
    bard> '"Hello!"
    "Hello!"
    
That's because most values are self-evaluating. Evaluating them returns the same result as not evaluating them. Quoting makes a difference only with symbols and lists.

### Types

**Types** are objects that describe how to represent values. As an example, `<fixnum>` is a type that describes how to represent integers in Bard.

Every value has a type, and you can ask Bard for the type of any value.

    bard> (type 5)
    <fixnum>
    
    bard (type 999999999999999999999999999999999999999999999999)
    <bignum>
    
    bard> (type \space)
    <character>
    
    bard> (type true)
    <boolean>
    
Bard provides many built-in types, and it also provides tools for constructing new ones.

Bard types are **disjoint**. That is, a value is an instance of exactly one type; if it's an instance of one type, then it's not an instance of another.

### Classes

**Classes** are collections of related types. A class consists of a list of types and a **protocol** that they share. A **protocol** is a list of functions.

For example, the `List` class has numerous member types, including `<pair>`, `<string>`, `<vector>`, and others. It also has an associated List protocol that includes such functions as `list`, `list?`, `first`, `rest`, `last`, and so on.

Just as you can define your own types, you can also define your own classes. When you define a class, its associated protocol is automatically created along with it.

You can add a type to a class at any time like this:

    bard> (add-type! List <my-new-list-type>)
    WARNING: these List functions are not defined on the type <my-new-list-type>:
    add-first add-last any append by drop element empty? filter first insert last length 
    ...
    
When we add the new type, Bard warns us that the List protocol has functions that won't work on the new List type. We'll have to define **methods** for those functions if we want to use `<my-new-list-type>` as a List.

In fact, you can skip the call to `add-type!` and just define the methods. That's all it takes for `<my-new-list-type>` to be considered a List. If you define methods for only some of the List functions, Bard will warn you about the ones you missed.

### Procedures

Procedures are values that can be applied to arguments to compute results. There are four kinds of procedures:

* **functions:** A function examines the arguments passed to it and chooses a **method** to execute.
* **methods:** A method accepts the values passed to it as parameters and uses them to compute and return a result.
* **special forms:** A special form is a procedure that is built into Bard itself, and which has its own rules for how it evaluates and handles its arguments.
* **macros:** A macro is a procedure that transforms its expression into a different one before evaluating it.

Special forms are the basic, built-in vocabulary of Bard. Bard has a relatively small number of special forms, and it doesn't provide a way to add new ones. Think of them as the foundation of the language.

Macros are primarily used for building new syntax. Many special forms could be implemented as macros, and if you find yourself in a situation where you really wish you could use a special form that Bard doesn't have, the solution is usually to write a macro.

Functions and methods are the bread and butter of Bard programming. Most work in Bard is writing functions and methods.

#### Functions and methods

Bard's functions are **polymorphic**. That means  the same function can do different things depending on the arguments you give it. Here's a simple example:

    bard> (append '(0 1) (2 3))
    (0 1 2 3)
    
    bard> (append "AB" "CD")
    "ABCD"
    
The results of both calls to `append` look pretty much like what you might expect: the two List inputs are concatenated together.

The lists in the first example are instances of `<pair>`, and the ones in the second are instances of `<string>`. The two types represent values very differently, which means that the code `aapend` uses to concatenate pairs has to be pretty different from the code it uses to cancatenate strings.

In languages that don't support polymorphic functions, you would either have to write append as a special form, or you'd have to have two different functions. In Bard, the **append** function examines its arguments and chooses the appropriate **method** for the arguments it received. Furthermore, Bard provides tools you can use to add new methods, so that `append` can work on new types.

## Reference

### Built-in classes

Each class can be represented by one or more types. This section lists the built-in classes and the built-in types that represent them.

#### Special classes and types

**`Anything`** is the only superclass in the language. All other classes are subclasses of `Anything`. All values are instances of `Anything`.

**`<null>`** is the only subtype in the language. All other types are supertypes of `<null>`. `<null>` is an instance of every type.

A type may belong to more than one class.

#### Condition

    <error>
    <warning>

#### Enumeration

    <boolean>
    <character>
    <keyword>
    <null>
    <symbol>
    <undefined>

#### Foreign

    <foreign-pointer>
    <foreign-procedure>
    <foreign-structure>
    <foreign-value>

#### List

    <bitvector>
    <class>
    <pair>
    <protocol>
    <string>
    <s8vector>
    <s16vector>
    <s32vector>
    <s64vector>
    <s128vector>
    <u8vector>
    <u16vector>
    <u32vector>
    <u64vector>
    <u128vector>
    <vector>

#### Map

    <alist>

#### Number

    <bignum>
    <bit>
    <fixed>
    <float>
    <ratio>
    <s8>
    <s16>
    <s32>
    <s64>
    <s128>
    <u8>
    <u16>
    <u32>
    <u64>
    <u128>

#### Procedure

    <continuation>
    <function>
    <macro>
    <method>
    <special-form>


#### Resource

    <actor>
    <consumer>
    <file>
    <iostream>
    <producer>
    <url>

#### Stream

    <consumer>
    <iostream>
    <producer>


#### Type

    <class>
    <foreign-type>
    <protocol>
    <structure>
    <tuple>
    <union>

### Built-in procedures

#### Condition

    handle
    signal
    with-handlers

#### Enumeration

    character?
    keyword?
    name?
    symbol?

#### Equality

    =

#### Foreign

    foreign-procedure?
    foreign-structure?
    foreign-type?
    foreign-value?

#### Language

    and
    begin
    case
    cond
    def
    define
    ensure
    getter
    if
    let
    loop
    match
    not
    or
    quasiquote
    quote
    repeat
    set!
    setter
    unless
    unquote
    unquote-splicing
    values
    when
    with-exit

#### List

    add-first
    add-last
    any
    append
    by
    drop
    eighth
    element
    empty?
    fifth
    filter
    first
    fourth
    join-text
    last
    length
    list
    list?
    map
    member?
    ninth
    penultimate
    partition
    position
    position-if
    range
    reduce
    rest
    reverse
    second
    seventh
    sixth
    some?
    sort
    split-text
    take
    take-by
    tenth
    third

#### Map

    get
    keys
    merge
    put
    vals

#### Number

    *
    +
    -
    /
    abs
    even?
    expt
    inverse
    max
    min
    mod
    odd?
    quotient
    random
    remainder
    sqrt

#### Order

    <
    <=
    >
    >=

#### Pair

    left
    pair
    pair?
    right

#### Procedure

    '
    `
    ,
    #
    ^
    ->
    add-method
    applicable?
    apply
    complement
    compose
    constantly
    eval
    flip
    function
    function?
    identity
    method
    method?
    partial
    procedure?
    remove-method
    special-form?

#### Resource

    actor?
    consumer?
    close
    closed?
    domain
    file?
    iostream?
    load
    open
    open?
    path
    port
    producer?
    query
    receive
    scheme
    send
    url?
    with-open

#### Stream

    characters
    contents
    cycle
    direction
    generate
    iterate
    lines
    load
    next
    next-n
    objects
    octets
    produced-count
    produced-values
    range-from
    read
    readable?
    text
    words
    write
    writeable?

#### System

    current-input
    current-output
    error
    exit
    gc
    gensym
    quit
    room
    standard-error
    standard-input
    standard-output
    uuid
    version

#### Type

    as
    class
    convert
    defined?
    exactly
    initialize
    instance?
    isa
    make
    nothing?
    protocol
    satisfies
    something?
    type
    type?
    undefined?

