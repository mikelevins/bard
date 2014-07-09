# Bard 0.4

version 0.4.0

by mikel evins

## Base singletons
Base singletons are simple atomic named constants. They are special in a couple of ways:

* like numbers or characters, they evaluate to themselves
* they are values, but they are also **structures** and **constructors**


name | description
---- | -----------
`true` , `false` | the Boolean truth values 
`nothing`        | the empty set; the absence of a useful value 
`undefined`      | the result of a failed computation 
`end`      | the value returned by taking the next value from an empty stream 

## Literals

Bard is intended to provide convenient literal syntax for all its values, to the extent that it's practical to do so. It also tries to print values in a form that it can re-read to construct equivalent values.

Bard does not promise to construct instances of any specific data structure when you present it with a literal expression, unless you include the type constraints that you want it to satisfy (see "Type descriptions").

### Characters

    #\λ
    #\U+03BB
    #\U_Greek_Small_Letter_Lambda
    #\Space
    #\NUL

### Numbers

    0
    1001
    2.3
    6.0221413e+23
    2/3
    1+2i
    #b1011
    #x00FF

### Names

    begin
    Fred
    |Hello, World!|
    name:
    #,(uri "file:///dev/null")

### Pairs

    [1 2]
    [name: "Fred"]
    ["Hello" nothing]

### Lists
Bard lists are instances of the class `List`. Classes in Bard are abstract types; a list value might be represented by any of several different concrete structures. Lists in Bard are not necessarily made up of chained `cons` structures.

One consequence of this design is that the class `List` includes many kinds of sequential types, including vectors and strings.

    ()
    (1)
    (a b c d)
    (+ (* 2 3)(/ 16 4))
    "This is a text value. It's also a list."

### Arrays

    #()
    #(1 2 3 4)
    #((0 1 2 3)(4 5 6 7)(8 9 10 11))

### Maps

    {}
    {name: "Fred" color: 'orange shape: 'square}
    {{}{}}

### Functions and methods

**Methods** are simple procedures that compute results from inputs. **Functions** are polymorphic procedures that examine their inputs and choose methods according to the properties of those inputs. Functions cannot compute anything unless methods have been defined for them.

A function is written: 

    (-> inType1 inType2 ... typeK -> outType1 outType2 ... outTypeN)

A method is written:

    (^ (arg1 arg2 ... argK) expr1 expr2 ... exprN)


Here are a few more methods:

    (^ x x)
    (^ (x y) (* x y))

...and a few more functions:

    (-> ->)
    (-> List -> Integer)

## Type constraints

Normally Bard makes no promise about the specific data structure you'll get when you type in a literal, but you can require it to choose a structure that you specify. There are two ways to do this:

1. Wrap a value expression in an `as` expression: `(as cons '(1 2 3))`
2. Tag an expression with a **type constraint**: `#:cons '(1 2 3)`

The two types of expression are completely equivalent.

Type constraints and `as` expressions accept any valid **type description** (see "Type Descriptions").

## Types

There are two kinds of types: 

* **structures** are concrete types; they describe how values are represented

* **classes** are abstract types; they identify types that fulfill named roles

Structures have **instances**: values that belong to them.

Classes have **members**: types that belong to them. Member types are also sometimes called **subtypes** or **subclasses**. When a type is a member of a class then the class is called its **superclass.**

Strictly speaking, classes cannot have instances; only structures can. If a structure belongs to a class, though, then instances of the structure may be loosely referred to as instances of the class.

Structures cannot have subclasses.

Structures are also functions; they can be applied to parameters to create new instances.

### Type names

Type names are treated specially. For most purposes, the names of structures and classes behave like constants; that is, they're like variables whose values cannot be changed. They aren't quite constants, though; `define` can update the value of a type name,if it's used with the correct defining clause. For example, `define method` can change the definition of a function; `define macro` can change the macro bound to a macro name.

Type names behave differently from variables in one other respect as well: when you define a type, the name becomes part of the type. If you define the variable `x` with the value 5, you can't ask 5 for its name in order to get `x`. When you define a record type named `point`, on the other hand, you can ask the new type for its name and get the name `point`.

## Base structures
The base structures are built-in representations of common types of values. 

### Simple structures

|structure|description|
|---------|-----------|  
|`none`|the structure of `nothing`|  
|`boolean`|`true` and `false`|  
|`ascii-character`|the elements of ASCII text |  
|`unicode-code-point`|the elements of Unicode text |  


### Numeric structures

|structure|description|
|---------|-----------|  
|`small-integer`|small integers|  
|`big-integer`|unlimited-precision integers|  
|`word`|machine words|  
|`single-float`|IEEE single-precision floating-point numbers|  
|`double-float`|IEEE double-precision floating-point numbers|  
|`ratio`|ratios of integers|  
|`complex`|numbers with a real part and an imaginary part|  

### Name structures

|structure|description|
|---------|-----------|  
|`symbol`|variable names|  
|`keyword`|literal constant names|  
|`uri`|resource names that conform to IETF and W3C URI specifications|  

### List structures

|structure|description|
|---------|-----------|  
|`box`|mutable containers that hold a single value|
|`cons`|mutable containers that hold two values|  
|`vector`|finite mutable sequences with efficient random access|  
|`expanding-vector`|vectors that grow as needed|  
|`word-vector`|finite uniform  mutable sequences of machine words|  
|`sequence`|finite immutable sequences with efficient random access|
|`ascii-string`|text strings in ASCII format|  
|`unicode-string`|text strings in Unicode format|  

###Array structures

|structure|description|
|---------|-----------|  
|`array`|multidimensional mutable collections|
|`word-array`|arrays of machine words|

###Map structures

|structure|description|
|---------|-----------|  
|`tree-map`|efficient persistent associative arrays|
|`hash-table`|mutable associative arrays|
|`protocol`|A collection of related procedures and variables|


###Stream structures

|structure|description|
|---------|-----------|  
|`output-stream`|Streams into which you can insert values one after another|
|`input-stream`|Streams from which you can take values one after another|
|`generator`|Input streams whose values are constructed by iterative processes|
|`io-stream`|Streams that can acts as both `input-stream` and `output-stream`|

###Procedure structures

|structure|description|
|---------|-----------|  
|`method`|simple monomorphic procedures|
|`function`|polymorphic generic functions|
|`macro`|procedures that rewrite expressions before evaluation|
|`special-form`|special built-in procedures|

###Process structures

|structure|description|
|---------|-----------|  
|`thread`|lightweight in-process threads|
|`process`|local or remote processes|
|`bard`|local or remote bard processes|

###Condition structures

|structure|description|
|---------|-----------|  
|`warning`|A condition that reports a notable event|
|`error`|A condition that reports an error and may transfer control|
|`restart`|A condition that offers a choice of control transfers|
|`exit`|A condition that exits a control path |

###Type constructors

**Type constructors** are structures whose instances are user-defined types.

|structure|description|
|---------|-----------|  
|`class`|named, mutable collections of types|
|`singleton`|a structure with exactly one instance|
|`record`|a structure with named slots|
|`tuple`|a structure with numbered slots|
|`synonym`|an alias for another type|

## Type descriptions

A second way to create a user-defined type is to use a **type description**. A type description is an expression that specifies a type by combining and limiting existing types.  The details are still being designed, but generally, a type description will look something like these examples:

    (a List)
    (a sequence of small-integer)
    (a Map of [Name Number])
    


## Base classes
Base classes are abstract types that organize the built-in structures into related families.

|class|description|
|---------|-----------|  
|`Anything`|all values|  
|`Atom`|simple values that are not collections|  
|`Character`|elements of text strings|  
|`Collection`|containers for groups of values|  
|`Condition`|notable events that occur during computation|  
|`Number`|numeric values|  
|`Name`|`symbol`, `keyword`, and `uri`; values used as names for variables, resources, or elements of structures|  
|`Procedure`|executable code|  
|`List`|finite sequences|  
|`Pair`|associations of a key with a value|  
|`Map`|associative arrays|  
|`Mutable`|values that can be modified in-place|  
|`Stream`|values that produce or consume other values |  
|`Text`|lists whose elements are characters|  
|`Type`|families of values|  

## Special forms

Special forms are procedures that are built into the language and form its foundation. 

|special form|syntax|description|  
|------------|------|-----------|
|`^`|(^ (*parameters*) *expressions*)| Constructs a `method`. |  
|`->`|(-> *in-type* ... -> *out-type* ...)| Constructs a `function`. |  
|`begin`|(begin *expressions*)| Evaluates the *expressions* left to right and returns the value of the last one.|  
|`define`|(define *var* *val*)| Creates a global variable named *var* with the value *val*.|  
|`define` *kind*|(define *kind* *form* ...)| Creates or modifies global definitions of several kinds, including functions, macros, structures, and protocols.|  
|`if`|(if *test* *then* *else*)| Evaluates *test*; if the result is true, evaluates *then*; otherwise, evaluates *else*.|  
|`let`|(let ( (*var* ... *val*) ...) *exp* ...)| Evaluates *exp*... in an environment where *var* ... are defined. `let` can bind multiple values returned by a single function call.|  
|`loop`|(loop *loop-name* ( (*var* ... *val*) ...) *exp* ...)| Exactly like `let` except that it also binds *loop-name* to a function that can be applied to recursively execute the body of the `loop` form with updated values for *var*...|  
|`quasiquote`|(quasiquote *x*) | Returns *x* without evaluating it, except for any subexpressions in `unquote` or `unquote-splicing` forms  |  
|`match`|(match ( (*pattern* *expr*) ...) *body-exp* ...)| Binds variables in *pattern* by matching them against the value of *expr*, then evaluates *body-expr...* in the resulting environment. |  
|`receive`|(receive [*pattern*]) | Returns the next pending message for this process, or,  with *pattern*, the next message matching *pattern*|
|`repeat`|(repeat *procedure* [ *arg* ... ]) | Returns an endless stream of values produced by repeatedly applying *procedure* to *arg*...|  
|`send`|(send *agent* *msg*) | Sends *msg* (a Bard value) to *agent* (a running local or remote process)|  
|`set!`|(set! *var* *val*)| Changes the value of `var` to `val`.|  
|`with-exit`|(with-exit (*var*) *expr* ...)| Binds an **exit procedure** to *var*, then evaluates *expr* ... Within the `with-exit` form, calling *var* immediately returns from the `with-exit` form, returning any arguments supplied to *var*.|  

## Protocols

A **protocol** is a collection of related variables and named procedures. Each protocol behaves like a map or dictionary: you can list its names and retrieve the values associated with them. A protocol is a **namespace**; you can use protocols to help prevent name collisions in your code, and to hide implementation details, while presenting a coherent API.

A protocol is also a documentation tool. It serves as a dictionary of the variables and procedures that it contains, and provides fields for documentation of its contents.

Bard is made of protocols. All of its built-in procedures, variables, and data structures are contained in protocols.

## Built-in protocols

The built-in protocols provide Bard's standard library. Following is an incomplete list of built-in protocols.

### Structure protocols

Each built-in structure has an associated protocol that includes a **constructor**, **type-description clauses**,  and **accessors**.

The **constructor** is just the structure itself, but each structure can define any number of initialization arguments that control how instances of the structure are built.

**Type-description** clauses define how type descriptions may be constructed to select subsets of the values represented by a structure.

**Accessors** are functions used to fetch parts of a value or, if the structure is mutable, to update them with new values.

###General protocols

**General protocols** are not specifically associated with particular structures; they're general-purpose protocols that provide sets of related variables and procedures. For example, the protocol `bard.math` provides procedures for working with numbers and calculations, and `bard.system` provides procedures and variables for examining and modifying the state of the running system.

|protocol|description|
|--------|-----------|  
|`bard.assignment`|Procedures for updating mutable values|  
|`bard.binding`|Procedures for working with lexical bindings|  
|`bard.construction`|Procedures for constructing and initializing values|  
|`bard.conversion`|Procedures for constructing new values from values of another type|  
|`bard.functions`|Procedures for working with function-like values|  
|`bard.iteration`|Procedures that visit elements of collections|  
|`bard.language`|The kernel language and flow-of-control forms.|  
|`bard.lists`|Procedures that construct and manipulate lists|  
|`bard.macros`|Procedures for defining and expanding macros|  
|`bard.maps`|Procedures for working with dictionary-like values|  
|`bard.math`|Arithmetic, transcendental, and other mathematical procedures and values|  
|`bard.ordering`|Procedures for comparing and sorting values|  
|`bard.resources`|Procedures for working with resources and resource names|  
|`bard.streams`|Procedures for constructing and working with streams, including common I/O operations|  
|`bard.system`|Procedures and variables that report and configure system status and features |  
|`bard.types`|Procedures for constructing and working with types |  

