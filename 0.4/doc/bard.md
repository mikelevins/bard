# Bard
version 0.4
by mikel evins

## Basic ideas

The basic ideas that make up the Bard language, each briefly described in this short section, are:

* **expressions** or **forms**
* **values**
* **programs**
* **modules** and **variables**
* **structures** and **instances** of structures
* four kinds of **procedures**: **functions**, **methods**, **macros**, and **special forms**
* **types** and **protocols**
* the **reader**, **evaluator**, and **printer**
* Bard **agents**, **sessions**, **messages**, and **images**
* the Bard **library**

Bard is a language and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases within phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is an organized collection of expressions that is designed to compute some result.

A **module** is a collection of **variables**. Bard programs use modules to organize variable names so that they don't compete or collide with one another. For example, you might write a procedure named `first`, and another programmer might write a different procedure named `first`. If both procedures existed in the same namespace, that would be a problem; only one of the two could be used. By defining your procedures in two different modules, you and the other programmer can avoid that problem.

A **structure** is a blueprint for constructing values; it tells Bard how to combine simpler values to form more complex ones. It also defines how to extract parts of the data contained in a structure.

Values constructed according to a structure are called **instances** of the structure. Each Bard value is an instance of some structure.

Bard supplies a useful library of built-in structures, and provides tools you can use to define new ones.

A **procedure** is a value that can be **applied** to other values to compute a result. Procedures come in four flavors: **functions**, **methods**, **macros**, and **special forms**. 

**Special forms** are built into Bard and cannot be modified; that's what's "special" about them, They're the foundation of the language--the procedures you use to work with everything else.

**Functions** are the procedures that you use most often in programs. You **apply** a function to some arguments, and it computes a result.

Strictly speaking, a function can't actually compute its result; that's the job of a **method**. A function just looks at the arguments to determine which method applies to them, then passes them to the method. It's the method that does the actual computing.

That means that the same function can run different code for different arguments. This feature is known as **polymorphism**; we say that bard functions are **polymorphic** because they can do different things when they are given different arguments.

**Macros** are procedures that can rewrite an expression before evaluating it. The most common use for macros is to extend the syntax of the language, usually to make complicated expressions simpler to write and easer to read. Writing a macro is sort of like adding a new special form.

A **type** is a collection of values that can be treated the same way. For example, all of the values that you can use in arithmetic belong to the `Number` type, and all of the values that you can sort, reverse, and concatenate belong to the `List` type.

Bard types don't say anything about how values are represented; that's a structure's job. Types only say what you can do with values. If a value is an instance of a type, then you can use the type's functions on that value. If a value is a `Number`, then you can do arithmetic with it; if it's a `List` then you can sort it, append it, and reverse it.

Types are defined by **protocols**. A **protocol** is a collection of related functions--and sometimes some macros--that defines a set of types and all the things you can do with them. For example, the `=Number=` protocol defines functions for addition, subtraction, and multiplication, and it defines types including `Number`, `Integer`, and `Ratio`.

The `=Signaling=` protocol is worth a special mention: it defines **conditions** and procedures for working with them. A **condition** is a value that represents some exceptional situation that can arise during evaluation. An **error** is one kind of the condition; a **warning** is another. Bard provides tools for **signaling**, **catching**, and **handling** errors, warnings, and other conditions, and for defining new kinds of conditions.

Bard is designed to be used interactively; that is, it's designed to run as a program that you interact with. It prints a prompt and then waits for you to type expressions for it to evaluate. When you do, it **reads** the expressions, **evaluates** them to compute the results, and then **prints** the results.

You can buikd whole programs in this kind of dialog, and Bard provides a collection of tools to help you.

Each stage of processing is handled by a programmable service: the **reader**, the **evaluator**, and the **printer**. Bard provides tools you can use to customize each of these for your needs.

The reader and the printer are coordinated: the reader can read what the printer prints. Unless you tell it to do otherwise, the printer tries to print values in a form that allows the reader to read them and reconstruct equivalent values. For example, if you contruct a list, Bard prints it in a form that it can later read:

    bard> (add-first 0 [1 2 3])
    (0 1 2 3)

We can copy the text that bard printed, paste it into a string, and ask Bard to read it:
    
    bard> (read "(0 1 2 3)")
    (0 1 2 3)

The reader returns an equvalent value.

Bard tries to do this with all its values--it tries to print them in a way that it can read. That means that normally you can ask Bard to print a value to a file, for example, and then later ask it to read the file, and it can faithfully reproduce the stored values.

This process doesn't work for every kind of value; there are a few that resist being printed readably. Consider, for example, a network connection to a video stream. There isn't a straightforward way to write a text string that captures an object like that. For one thing, we don't have all of it! Most of it is somewhere else on the network. So Bard can't make *every* value readable. It tries to, though; and when it encounters one that refuses to cooperate, it prints it in a way that makes that clear; for example:

   #{type: <video-stream> uuid: E4CA9200-ED24-4FC5-9D03-B91144D5F9EE readable: false url: "http://www.youtube.com/watch?v=o4-YnLpLgtk"}

The program that provides the reader, evaluator, and printer is called a Bard **agent**. A Bard agent can spawn other agents, and it can send **messages** to them. It can also send messages to agents that it didn't spawn, as long as it has references to them.

The state of a running agent is called a **session**. It's created when the agent starts running, and destroyed when it stops, but you can ask an agent to save its session in a file called an **image**. If you start an agent using a saved image, the stored session will resume as if it had never stopped--even if you start it on a different agent on a different machine.

Bard agents normally start from standard saved images, but you can save customized images with your own modifications.

One feature of the standard image is a standard Bard **library** that contains all of the built-in protocols that come with Bard, as well as the reader, evaluator, and printer, and a collection of useful programming tools to help you create Bard programs. The Bard language itself is not very big; a lot of its useful features are actually part of the library.

That concludes a whirlwind tour of the Bard language. In the following sections we'll look at each of these concepts in greater detail. The manual concludes with a reference that describes the Bard library in detail.

## An Overview of Bard

### Expressions

The basic unit of meaning in Bard is the **expression**. Simple values are expressions. References to **variables** are also expressions, and so are procedure calls.

#### Values

Literals

#### Variables and modules

#### Naming conventions

#### Applying procedures 

#### Flow of control

### Types and protocols

#### Protocols

#### Defining protocols

#### Defining methods

#### Advanced: controlling dispatch

#### Advanced: conditions

### Agents

#### Sessions and images


## Using Bard
### How to run Bard
#### The `bard` command
#### The graphical environment
#### Saved images
#### Init files
#### Modules and Module variables
#### Saving images

### How to use modules
#### Module names
#### The `bard.user` and `bard.language` modules
#### Creating and using a working module
#### Imports and renaming
#### Exports

### How to use structures
#### Structures and accessors
#### Records
#### Tuples
#### Enumerations
#### Unions

### How to define protocols
#### About protocols
#### Protocols and module variables
#### Function signatures
#### Creating and defining protocols

### How to define macros

### How to use conditions
#### About conditions and the condition system
#### Signaling conditions
#### Handling conditions
#### Restarts
#### Adding condition types
#### Defining condition structures

### How to build distributed programs
#### Agents and sessions
#### Contacting an agent through a URL
#### Serialization and messages
#### Safety, security, and subordinate agents

### How to use persistent data
#### Serialization and storage
#### Using images for storage
#### Using the built-in value database
#### Using foreign stores

### How to use foreign code
#### Foreign procedures and types
#### Defining foreign procedures and types
#### Linking foreign libraries
#### Making system calls
#### Running external programs

### How to debug programs
#### Backtraces
#### Inspecting data
#### Stepping through procedures
#### Conditions and restarts

### How to deliver programs
#### Saving an executable image
#### Console images
#### Graphical images
#### Packaging resources

### How to modify Bard

## Bard reference

### Built-in protocols

#### Condition

    handle (-> (var &) & -> &)
    signal (-> Condition ->)
    with-handlers  (-> ((var &) &) -> &)

#### Enumeration

    character? (-> Anything -> <boolean>)
    keyword? (-> Anything -> <boolean>)
    name?(-> Anything -> <boolean>)
    symbol?(-> Anything -> <boolean>)

#### Equality

    = (-> Anything Anything & -> <boolean>)

#### Foreign

    foreign-procedure? (-> Anything -> <boolean>)
    foreign-structure? (-> Anything -> <boolean>)
    foreign-type? (-> Anything -> <boolean>)
    foreign-value? (-> Anything -> <boolean>)

#### Language

    and (-> )
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

### Built-in structures

    <agent>
    <alist>
    <bignum>
    <bit>
    <bitvector>
    <boolean>
    <character>
    <consumer>
    <continuation>
    <error>
    <file>
    <fixed>
    <float>
    <foreign-pointer>
    <foreign-procedure>
    <foreign-structure>
    <foreign-type>
    <foreign-value>
    <function>
    <iostream>
    <keyword>
    <macro>
    <method>
    <null>
    <ordered-map>
    <pair>
    <producer>
    <protocol>
    <ratio>
    <s128>
    <s128vector>
    <s16>
    <s16vector>
    <s32>
    <s32vector>
    <s64>
    <s64vector>
    <s8>
    <s8vector>
    <special-form>
    <string>
    <structure>
    <symbol>
    <tuple>
    <u128>
    <u128vector>
    <u16>
    <u16vector>
    <u32>
    <u32vector>
    <u64>
    <u64vector>
    <u8>
    <u8vector>
    <undefined>
    <union>
    <url>
    <vector>
    <warning>
    <wbtree>


### System tools

