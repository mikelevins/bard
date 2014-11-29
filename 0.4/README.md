# Bard 0.4

version 0.4.0

by mikel evins

## Base singletons
Base singletons are simple atomic named constants. They are special in a couple of ways:

* like numbers or characters, they evaluate to themselves
* they're values, but they're also **structures** and **constructors**


name | description
---- | -----------
`true` , `false` | the Boolean truth values 
`nothing`        | the empty set; the absence of a useful value 
`undefined`      | the result of a failed computation 
`end`      | the value returned by taking the next value from an empty stream 

## Literals

Bard tries to provide convenient literal syntax for as many of its values as possible. It also tries to print values so that it can read them and reconstruct them.

When Bard reads a value, it doesn't promise to construct any specific representation of the value. For example, if you write

    (1 2 3)

you can be sure that the value produced by the reader will be a list, but you can't be sure exactly what *representation* of a list will be used. Depending on the implementation, you might get a vector or a tree-based sequence or the traditional chain of `cons` cells. All of those are valid representations of lists in Bard, and Bard only promises to build a list for you; it doesn't promise to build any specific representation of a list.

You can take control of representation is you want to, though. For example, you can use a **type constraint** to restrict the representation that Bard chooses:

    #:cons (1 2 3)

The form `#:cons` is a **type constraint**; it instructs Bard to ensure that the following value is an instance of the `cons` structure.

Generally speaking, when you write a Bard literal you're asking for an instance of some abstract class—`List`, for example. If you want a specific concrete type, you have to specify which one using a type constraint.

If you write a literal with a type constraint that it can't satisfy, Bard signals an error. For example, the following will cause an error:

    #cons 1.3

The problem is that `1.3` denotes a `Float`, and `cons` is not a member of the `Float` class.

For more information about abstract and concrete types in Bard, see the "Types" section.

### Characters

**Characters** are the elements of **Text** values. Texts are lists; their elements are characters.

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

**Names** are values used to name parts of Bard programs, such as variables, files, and classes.

    begin
    Fred
    |Hello, World!|
    name:
    #,(uri "file:///dev/null")

### Pairs

A **Pair** is a value that associates two other values, called the pair's **left** value and its **right** value.

    [1 2]
    [name: "Fred"]
    ["Hello" nothing]

### Lists
**Lists** are sequences of values with finite length. Types that implement lists are members of the class `List`.

Historically, lists have been represented in Lisp implementations using a specific data structure known as a **cons cell**. A cons cell is a pair of pointers; in a normal list, one of the pointers points to the first element and the other points to another list that contains the remaining elements.

Bard provides cons cells, represented as instances of the `cons` structure. Unlike older Lisps, though, Bard doesn't make `cons` synonymous with `List`. In Bard, `List` is a class, which means it's an abstract type. There are many different representations of `List`; `cons` is just one of them.

This design means that there are many kinds of lists, but all of them support Bard's list protocol. You can use list functions on any `List` instance, regardless of its representation.

It also means that all sequential data of finite length are lists: `cons` instances are lists, but so are vectors and strings.

Strings are lists in Haskell as well, but in Haskell that means that they are represented inefficiently as linked lists of characters. In Bard the fact that a string is a list implies nothing about its representation. A string can be represented as a packed, uniform array of bytes, or as a vlist, or as a hashed array-mapped trie, or as any representation that can reasonably support the list protocol.

So, on one hand, all of the following values are lists:

    ()
    (1)
    (a b c d)
    (+ (* 2 3)(/ 16 4))
    "This is a text value. It's also a list."

On the other hand, none of them has to be represented as `cons` cells, unless you want to represent them that way.

### Arrays

**Arrays** are n-dimensional collections whose elements are indexed by sequences of integers. A 1-dimensional array is equivalent to a list. 

    #()
    #(1 2 3 4)
    #((0 1 2 3)(4 5 6 7)(8 9 10 11))

Arrays are also lists. A one-dimensional array supports the list protocol directly, in the obvious way. A two-dimensional array is a list of the array's elements in row-major order (see [http://en.wikipedia.org/wiki/Row-major_order](http://en.wikipedia.org/wiki/Row-major_order)).

### Maps

A **map** is a **finite map**; that is, a collection that represents a one-to-one mapping of **keys** to **values**.  Any Bard object except `nothing` or `undefined` can be a key. Any object except `undefined` can be a value.

    {}
    {name: "Fred" color: 'orange shape: 'square}
    {{}{}}

Like all Bard classes, `Map` is abstract. Instances of `Map` may be represented by any of a variety of structures: hash tables, tree maps, association lists, and so on.

A map is also a list. The map protocol includes the list protocol. Bard maps support list functions by treating the map data as a list of pairs, where the left element of each pair is the key and the right element is the value.

### Functions and methods

**Methods** are simple procedures that compute results from inputs. **Functions** are polymorphic procedures that examine their inputs and choose methods according to the properties of those inputs. Functions cannot compute anything unless methods have been defined for them.

A function is written: 

    (-> inType1 inType2 ... typeK -> outType1 outType2 ... outTypeN)

A method is written:

    (^ (arg1 arg2 ... argK) expr1 expr2 ... exprN)


Here are a few methods:

    (^ x x)
    (^ (x y) (* x y))

...and a few functions:

    (-> ->)
    (-> List -> Integer)
    (-> Anything Map -> Anything Boolean)

If a function has more than one type after the second arrow then it returns more than one value.

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

Type names are symbols used to identify structures and classes. A type name is similar to a  global variable, but with a few differences. For the most part, they behave like read-only variables (constants), but certain special forms can be used to change them. `define` can update the value of a type name,if it's used with the correct defining clause. For example, `define method` can change the definition of a function; `define macro` can change the macro bound to a macro name.

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

    (a small-integer)
    (a List)
    (an Atom)
    (a sequence of small-integer)
    (a sequence of small-integer with minimum-count: 0 maximum-count: nothing)
    (a Map of [Name Number])
    (a Map of [Name Number] with defaults: {anonymous: 0})
    
`a` and `an` are macros that return type-description objects. The syntax names `of` and `with` are not evaluated, but their parameters are.

## Base classes
Base classes are abstract types that organize the built-in structures into related families.

|class|description|
|---------|-----------|  
|`Adjustable`|collections whose size can be changed|  
|`Anything`|all values|  
|`Atom`|simple values that are not collections|  
|`Character`|elements of text strings|  
|`Collection`|containers for groups of values|  
|`Condition`|notable events that may cause a transfer of control|  
|`Consumer`|streams that consume values|  
|`Float`|floating-point numbers|  
|`Generator`|streams that produce values by executing iterative processes|  
|`List`|finite sequences|  
|`Map`|associative arrays|  
|`Mutable`|collections that can be modified in-place|  
|`Name`|values used as names for variables, resources, or elements of structures|  
|`Number`|numeric values|  
|`Pair`|associations of a key with a value|  
|`Procedure`|executable code|  
|`Producer`|streams that produce values|  
|`Rational`|numbers that can be expressed as ratios of integers |  
|`Real`|numbers with fractional parts |  
|`Stream`|values that produce or consume other values |  
|`Structure`|concrete type specifications |  
|`Text`|lists whose elements are characters|  
|`Type`|families of values|  
|`Uniform`|collections whose elements are all the same concrete type|  

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

