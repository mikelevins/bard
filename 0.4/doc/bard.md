# Bard

version 0.4.0

by mikel evins

## Base singletons
Base singletons are simple atomic named constants.


name | description
---- | -----------
`true` , `false` | the Boolean truth values 
`nothing`        | the empty set; the absence of a useful value 
`undefined`      | the result of a failed computation 


## Types

There are two kinds of types: 

* **structures** are concrete types; they describe how values are represented

* **classes** are abstract types; they identify types that fulfill named roles

Structures have **instances**: values that belong to them.

Classes have **members**: types that belong to them. Member types are also sometimes called **subtypes** or **subclasses**. When a type is a member of a class then the class is called its **superclass.**

Strictly speaking, classes cannot have instances; only structures can. If a structure belongs to a class, though, then instances of the structure may be loosely referred to as instances of the class.

Structures cannot have subclasses.

Structures are also functions; they can be applied to parameters to create new instances.

## Base structures
Base structures are built-in representations of common values. 

|structure|description|
|---------|-----------|  
|`none`|the structure of `nothing`|  
|`boolean`|`true` and `false`|  
|`integer`|integers|  
|`float`|floating-point numbers|  
|`symbol`|variable names|  
|`keyword`|literal constant names|  
|`character`|the elements of text strings|  
|`string`|text strings|  
|`cons`|mutable containers with two elements, named `cons.car` and `cons.cdr`|  
|`vector`|finite sequences of objects|  
|`uri`|resource names that conform to IETF and W3C URI specifications|  
|`method`|simple monomorphic procedures|
|`box`|mutable containers that hold a single value|
|`class`|named, mutable collections of types|
|`map`|immutable associative arrays|
|`seq`|immutable sequences|
|`stream`|sequential producers or consumers of values|
|`function`|polymorphic generic functions|
|`protocol`|mutable collections of functions and variables|


## Special forms

Special forms are procedures that are built into the language and form its foundation. 

|special form|syntax|description|  
|------------|------|-----------|
|`^`|(^ (*parameters*) *expressions*)| Constructs a `method`. |  
|`begin`|(begin *expressions*)| Evaluates the *expressions* left to right and returns the value of the last one.|  
|`define`|(define *var* *val*)| Creates a global variable named *var* with the value *val*.|  
|`if`|(if *test* *then* *else*)| Evaluates *test*; if the result is true, evaluates *then*; otherwise, evaluates *else*.|  
|`quote`|(quote *x*) | Returns *x* without evaluating it.	|  
|`set!`|(set! *var* *val*)| Changes the value of `var` to `val`.|  

## Built-in protocols

The built-in protocols provide Bard's standard library.

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

