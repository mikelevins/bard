# Bard

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

Type names behave differently from variables in one other respect as well: when you define a type, the name becomes part of the type. If you define the variable `x` with the value 5, you can't ask 5 for its name in order to get `x`. When you define a record named `point`, on the other hand, you can ask the new type for its name and get the name `point`.

## Base structures
The base structures are built-in representations of common values. 

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
|`abort`|A condition that exits a control path |

###Type structures

|structure|description|
|---------|-----------|  
|`class`|named, mutable collections of types|
|`singleton`|a structure with exactly one instance|
|`record`|a structure with named slots|
|`tuple`|a structure with numbered slots|
|`synonym`|a name for another type|

## Type descriptions

A **type description** is an expression that specifies a type.  It consists of a list in the following form:

( *constructor* [*constraint*] * )

The constructor is one of the following:

* `anything`
* `a` *type*, or `an` *type*
* `the value` *var* `where` 

The constraint or constraints, if present, place restrictions on the values that match the type description.

### Limited types: `a` *`type`* 

### Predicate types: `anything`  

### Predicate types: `the value` *var* `where`



## Base classes
Base classes are abstract types that organize the built-in structures into related families.

|class|description|
|---------|-----------|  
|`<anything>`|all values|  
|`<atom>`|simple values that are not collections|  
|`<collection>`|containers for groups of values such as `vector` and `pair`|  
|`<number>`|numeric values such as `integer` and `float`|  
|`<name>`|values such as `symbol`, `keyword`, and `uri`, which are used as names for variables, resources, or elements of structures|  
|`<procedure>`|values that represent executable code, such as `function` and `method`|  
|`<list>`|collections organized as finite sequences|  
|`<map>`|collections organized as associative arrays|  
|`<stream>`|values that can produce or consume other values one after another|  
|`<type>`|values that represent families of types, such as `structure` and `class`|  

## Special forms

Special forms are procedures that are built into the language and form its foundation. 

|special form|syntax|description|  
|------------|------|-----------|
|`^`|(^ (*parameters*) *expressions*)| Constructs a `method`. |  
|`a`, `an`, `anything`, `the value`|see "Type descriptions"| Constructs a **type description**|  
|`begin`|(begin *expressions*)| Evaluates the *expressions* left to right and returns the value of the last one.|  
|`define`|(define *var* *val*)| Creates a global variable named *var* with the value *val*.|  
|`if`|(if *test* *then* *else*)| Evaluates *test*; if the result is true, evaluates *then*; otherwise, evaluates *else*.|  
|`quasiquote`|(quasiquote *x*) | Returns *x* without evaluating it, except for any subexpressions in `unquote` or `unquote-splicing` forms	|  
|`quote`|(quote *x*) | Returns *x* without evaluating it.	|  
|`receive`|(receive [*pattern*]) | Returns the next pending message for this process, or,  with *pattern*, the next message matching *pattern*	|  
|`send`|(send *agent* *msg*) | Sends *msg* (s Bard value) to *agent* (a running local or remote process)	|  
|`set!`|(set! *var* *val*)| Changes the value of `var` to `val`.|  
|`with-exit`|(with-exit (*var*) *expr* ...)| Binds an **exit procedure** to *var*, then evaluates *expr* ... Within the `with-exit` form, calling *var* immediately returns from the `with-exit` form, returning any arguments supplied to *var*.|  

## Built-in protocols

The built-in protocols provide Bard's standard library. Following is an incomplete list of built-in protocols.

### Built-in structure protocols

Each built-in structure has an associated protocol that includes a **constructor**, **type-description clauses**,  and **accessors**.

The **constructor** is just the structure itself, but each structure can define any number of initialization arguments that control how instances of the structure are built.

**Type-description** clauses define how type descriptions may be constructed to select subsets of the values represented by a structure.

**Accessors** are functions used to fetch parts of a value or, if the structure is mutable, to update them with new values.

**`abort`**

| constructor arguments |  
|  ------	|  
| 

### **array**

#### Constructor arguments

<pre>
(array [<em>dimensions</em>]<br/>
       [:initial-element <em>value</em>]<br/>
       [:initial-contents <em>values</em>]<br/>
       [:element-type <em>type-description</em>]&nbsp;)
</pre>

#### Type-descriptions clauses

#### Accessors

<pre>
(array.ref <em>array</em>  [<em>index</em>]+)
</pre>

<pre>
(array.set! <em>array</em>  [<em>index</em>]+ <em>value</em>)
</pre>


`ascii-character`
`ascii-string`
`bard`
`big-integer`
`boolean`
`box`
`class`
`complex`
`cons`
`double-float`
`error`
`expanding-vector`
`function`
`generator`
`hash-table`
`input-stream`
`io-stream`
`keyword`
`macro`
`method`
`none`
`output-stream`
`process`
`protocol`
`ratio`
`record`
`restart`
`sequence`
`single-float`
`singleton`
`small-integer`
`special-form`
`symbol`
`synonym`
`thread`
`tree-map`
`tuple`
`unicode-code-point`
`unicode-string`
`uri`
`vector`
`warning`
`word`
`word-array`
`word-vector`

###General protocols

|protocol|description|
|--------|-----------|  
|`Assignment`|Procedures for updating mutable values|  
|`Binding`|Procedures for working with lexical bindings|  
|`Construction`|Procedures for constructing and initializing values|  
|`Conversion`|Procedures for constructing new values from values of another type|  
|`Functions`|Procedures for working with function-like values|  
|`Lists`|Procedures that construct and manipulate lists|  
|`Maps`|Procedures for working with dictionary-like values|  
|`Math`|Arithmetic, transcendental, and other mathematical procedures and values|  
|`Ordering`|Procedures for comparing and sorting values|  
|`Resources`|Procedures for working with resources and resource names|  
|`Streams`|Procedures for constructing and working with streams, including common I/O operations|  
|`System`|Procedures and variables that report and configure system status and features |  
|`Types`|Procedures for constructing and working with types |  

