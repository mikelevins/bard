# `define` forms


`define` identifies a family of special forms used to define variables, types, and functions. Each `define` form defines a different type of program structure. The definitions provided by Bard are:

* **define class**: defines a symbol as the name of a class, and optionally asserts subtype relationships to existing classes
* **define constant**: defines a read-only module variable
* **define function**: defines a method on a function, defining the function as well if it doesn't already exist
* **define macro**: defines new syntax using a provided macroexpander to rewrite expressions before they're evaluated
* **define protocol**: defines a protocol--that is, a named collection of related functions and classes that together make up a type
* **define record**: defines a record structure--that is, a structure that consists of named fields
* **define variable**: defines a mutable module variable
* **define variant**: defines a variant structure--that is, a structure with two or more defined representations
* **define vector**: defines a vector--that is, a structure that consists of indexed fields of a common type

## Classes

## Constants and Variables

## Functions and Methods

## Protocols

## Macros

## Structures

**Structures** are concrete definitions of the layout of data in a Bard program. A structure is the part of a Bard program that defines how types are represented--that is, how the bits and bytes are laid out. There are three kinds of structures: **records**, **vectors**, and **variants**.

### Records

A **record** is a structure that is composed of named fields. This is a simple record definition:

    (define record <point> x y)
    
This definition asserts that the structure `<point>` consists of a record with two named fields: `x` and `y`. If you evaluate this expression in Bard, it registers this definition, and binds the name of the record, `<point>` to a `<record>` object:

    bard> (define record <point> x y)
    #<record>{name: <point> {x {type: Anything mutable: false} y {type: Anything mutable: false}}}

Defining the record also creates accessor functions. You can use options to `define record` to control the names given to those functions, but by default their names will be of the form `%<structure-name>.<field-name>`. For example, the accessors that are automatically defined for the `<point>` structure are:

    %<point>.x
    %<point>.y
    
The naming convention in Bard is that procedures that are not meant to be exported as part of any public API are given names that start with '%'. The automatic naming of accessors reflects that assumption that accessors should be treated by default as private implementation details.

Bard also automatically defines a method on the `make` function that enables you to create instances of the record. For example:

    (make <point> x: 100 y: 200)

The Bard printer prints a record instances as a Map with a type annotation:

    #<point> {x 100 y 200}

### Vectors

A **vector** is a structure that is defined as a homogeneous array of values. Elements of a vector instance can be retrieved (or replaced, if the vector is mutable) using a zero-based index.

For example,

    (define vector <ipv4> type: <octet> minimum: 4 maximum: 4 default: 0 mutable: false)
    
Again, Bard defines a method on `make` for constructng instances of the new vector structure:

    bard> (make <ipv4>)
    #<ipv4> (0 0 0 0)

The `make` method for vetor structures accepts an `elements:` keyword for use in initializing the vector instance:

    bard> (make <ipv4> elements: [1 2 3 4])
    #<ipv4> (1 2 3 4)

The value passed with `elements:` can be any List, as long as the elements of the list are of a type congruent with the `type:` constraint given in the vector's definition.

### Variant structure

**Variant structures** provide the means to define structures with more than one data layout. A variant's definition can specify two or more named cases, and each case can use a different record or vector definition. Cases can also be empty except for the name; in that case, the name alone is an instance of the variant.

For example, here's a way to define a crude color datatype by simply listing color names:

    (define variant <color> ['Red 'Green 'Blue])
    
After this definition is evaluated the symbol `Red` is an instance of the structure `<color>`.

More commonly, variant definitions specify named alternative representations of more complex data:

    (define variant <document-type>
      { 'OSType (vector type: <octet> minimum: 4 maximum: 4 default: 0 mutable: false)
        'MIMEType (record {name {type: <string> default: nothing mutable: false}
                          {encoding {type: <string> default: "unknown/binary" mutable: false}}})})
        
        
This definition specifies a variant structure whose instances can be either `OSType` structures, which are represented as packed arrays of octents of length 4, or `MIMEType` structures, which are records with a `name` field and an `encoding` field.

When printing an instance of a variant structure, Bard acknowledges the specific variant of the value:

    #<color> #{variant: Red} Red
    #<document-type> #{variant: OSType} (\A \P \P \L)
    #<document-type> #{variant: MIMEType} {name "application/javascript" encoding "UTF8"}
    


