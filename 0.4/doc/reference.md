# Bard Reference

Version 0.4.0a19

Copyright 2013 by mikel evins

## 1. Changes

Version 0.4.0a19

* bugfix: list constructors no longer reverse their elements
* bugfix: single-element lists no longer print as dotted pairs
* refactored the vm for modularity
* removed dead code
* refactored the compiler into more modular form

Version 0.4.0a18

* Added `time`

Version 0.4.0a17

* Added `let`
* Added `map`

Version 0.4.0a16

* Added `load`
* Added Bard source "fib.bard"

Version 0.4.0a15

* Added `stream.write-octet`
* Added `stream.write-octets`
* Added `stream.write-character`
* Added `stream.write-characters`
* Added `stream.write-line`
* Added `stream.write-lines`
* Added `stream.write-object`
* Added `stream.write-objects`

Version 0.4.0a14

* Added `stream.standard-input`
* Added `stream.standard-output`
* Added `stream.standard-error`
* Added `stream.create`
* Added `stream.length`
* Added `stream.read-octet`
* Added `stream.read-octets`
* Added `stream.read-all-octets`
* Added `stream.read-character`
* Added `stream.read-characters`
* Added `stream.read-all-characters`
* Added `stream.read-line`
* Added `stream.read-lines`
* Added `stream.read-all-lines`
* Added `stream.read-object`
* Added `stream.read-objects`
* Added `stream.read-all-objects`

Version 0.4.0a13

* Added `url`
* Added `url.scheme`
* Added `url.host`
* Added `url.path`
* Added `url.port`
* Added `url.query`
* Added `as-url`

Version 0.4.0a12

* Added `alist-map?`
* Added `alist.get`
* Added `alist.put`
* Added `alist.keys`
* Added `alist.vals`
* Added `alist.merge`
* Added `as-alist-map`

Version 0.4.0a11

* Added `cons.append`
* Added `cons.slice`
* Added `cons.first`
* Added `cons.rest`
* Added `cons.last`
* Added `cons.drop`
* Added `cons.take`
* Added `string.drop`
* Added `string.take`

Version 0.4.0a10

* Added `as-keyword`
* Added `as-cons`
* Added `as-symbol`
* Added `as-string`
* Added `string.append`
* Added `string.slice`
* Added `string.first`
* Added `string.rest`
* Added `string.last`

Version 0.4.0a8

* Added `<`
* Added `>`
* Added `<=`
* Added `>=`
* Added `=`
* Added `identical?`

Version 0.4.0a7

* Added `not`
* Added `and`
* Added `or`

Version 0.4.0a6

* Fixed a bug in printing dotted pairs

Version 0.4.0a5

* Initial version of the Bard printer added

Version 0.4.0a4

* Added accessors `pair.left` and `pair.right`
* Added constructor `list`

Version 0.4.0a3

* Fixed the implementation of `def`
* added the `pair` function

Version 0.4.0a2

* Fixed a bug that prevented correct self-evaluation of keywords

Version 0.4.0a1

* Fixed a bug in the assembler that cause it to generate bogus jump targets for labels (fixes the bug in compiling `if`)
* Documented the built-in methods `exit` and `call/cc`

## 2. Built-in types

### Characters

A text character is written as a backslash ('\') followed by the character itself, or, in the case of space, tab, newline, and return, the name of the character:

    \a
    \A
    \space
    \tab

### Numbers

Integers, decimal numbers, and fractions are written in the natural way:

    2
    2.3
    2/3
    
Any of these types of numbers may be written with a leading hyphen to signify a negative number:

    -2
    -2.3
    -2/3    

#### Procedures that work with numbers

*Primitive* **`+`** *expr1* *expr2* => `Number`<br>
Returns the sum of *expr1* and *expr2*. Both arguments must be numbers.

*Primitive* **`*`** *expr1* *expr2* => `Number`<br>
Returns the product of *expr1* and *expr2*. Both arguments must be numbers.

*Primitive* **`-`** *expr1* *expr2* => `Number`<br>
Returns the difference of *expr1* and *expr2*. Both arguments must be numbers.

*Primitive* **`/`** *expr1* *expr2* => `Number`<br>
Returns the quotient of *expr1* and *expr2*. Both arguments must be numbers.


### Special constants

Bard 0.4.0a defines four special constants:

    undefined
    nothing
    true
    false
    
* **`undefined`** represents any absent, unknown, improper, or otherwise undefined value
* **`nothing`** represents the empty set, list, or collection
* **`true`** and **`false`** represent the two possible Boolean values

### Conses

A **cons** is a value that associates two other values, called the **left element** and the **right element**.

#### Procedures that work with conses

*Constructor* **`cons`** *expr1* *expr2*  => `<cons>`<br>
Returns a cons whose left element is the value of *expr1* and whose right element is the value of *expr2*. 

*Converter* **`as-cons`** *expr* => `<cons>`<br>
Returns a cons whose contents are equivalent to the contents of *expr*. *expr* must be `nothing`, a cons, or a string.

*Accessor* **`cons.left`** *expr*  => `<anything>`<br>
Returns the left element of *expr*.

*Accessor* **`cons.right`** *expr*  => `<anything>`<br>
Returns the right element of *expr*.

*Constructor* **`cons.append`** *expr1* *expr2*  => `<cons>`<br>
Returns a new cons that contains the elements of *expr1* followed by the elements of *expr2*.

*Constructor* **`cons.slice`** *cons* *start* *end*  => `<cons>`<br>
Returns a new cons that contains the elements of *cons* beginning with the element at position *start* and ending with the element before position *end*.

*Accessor* **`cons.first`** *cons*  => `<anything>`<br>
Returns the left (first) element of *expr*.

*Accessor* **`cons.rest`** *cons*  => `<cons>`<br>
Returns a cons containing all elements of *cons* except the first.

*Accessor* **`cons.last`** *cons*  => `<anything>`<br>
Returns the left element of the last cons in the chain of conses starting at *cons*. Another way to say it is that `cons.last` returns the last element of the List represented by *cons*.

*Constructor* **`cons.drop`** *count* *cons*  => `<cons>`<br>
Returns a cons chain that contains the same elements as *cons*, but without the first *count* elements.

*Constructor* **`cons.take`** *count* *cons*  => `<cons>`<br>
Returns a cons chain that contains the first *count* elements of *cons*.



#### The `list` constructor and accessors

Bard defines **List** as an abstract data type with no specified representation. In Bard 0.4.0a, the sole representation of List is `<cons>`. As a temporary measure, until the abstract type system is implemented, Bard 0.4.0a provides a set of constructors and accessors to make it easier to work with lists represented as conses.

In later versions of Bard, these temporary expedients will be replaced by the Bard List protocol, which supports a variety of List representations.

*Constructor* **`list`** *expr* * => `<cons>`<br>
Returns a cons whose left element is *expr* and whose right element is another cons containing the remaining arguments.

Here are a few examples of calls to `list` and the `cons` structures that they return:

    (list) => ()
    (list 0) => (0 . ())
    (list 0 1) => (0 . (1 . ()))
    (list 0 1 2) => (0 . (1 . (2 . ())))

**Note:** In Bard 0.4.0a, `list` is limited to 10 arguments.

*Function* **`map`** *proc* *list* * => `List`<br>
Returns a list constructed by applying the procedure *proc* to each element of *list* from left to right. The values returned by *proc* are collected in a new list in the same order that they were produced.


### Booleans

A **Boolean** is one of the special values `true` or `false`. A **generalized Boolean** is any Bard value when treated as true or false. The special values `false` and `nothing` are both false when treated as generalized booleans; all other Bard values are considered true.

Bard provides special forms, macros, and functions for working with Booleans and generalized Booleans.

#### Procedures that work with Booleans

*Macro* **`and`** *expr* * => `Boolean`<br>
`and` evaluates the first *expr*. If the result is false then `and` immediately returns `false` without evaluating any further expressions. If it's true, though, then `and` evaluates the next expression, repeating the same behavior. If all arguments to `and` return true values then `and` returns the last value produced. If any expression produces a false value then `and` immediately returns `false` without evaluating any further expressions.

If you call `and` with no arguments it returns `true`.

*Primitive* **`not`** *expr* => `Boolean`<br>
`not` evaluates *expr*. If the result is true then `not` returns `false`. If the result is false then `not` returns `true`.

*Macro* **`or`** *expr* * => `Boolean`<br>
`or` evaluates the first *expr*. If the result is true then `or` immediately returns that value without evaluating any further expressions. If it's false, though, then `or` evaluates the next expression, repeating the same behavior. If all arguments to `or` return false values then `or` returns the last value produced. If any expression produces a true value then `or` immediately returns that value without evaluating any further expressions.

If you call `or` with no arguments it returns `false`.

### Names

Names are Bard values that represent discrete pieces of text used to identify things. Examples of things identified by names are variables, types, and fields of data structures.

Two names that are written the same are guaranteed to be identical values. More specifically,

    (identical? 'a 'a)
    
always returns `true`.

Bard has two kinds of names: **keywords** are names that end in a colon (:). Evaluating a keyword always returns the keyword itself. **Symbols** are almost the same, except that they do not end in colons. The most common use of symbols is to name variables; consequently, evaluating a symbol normally returns the value of the variable that it names. You can prevent Bard from evaluating a symbol by quoting it.

Here are a few keywords:

    a:
    name:
    Title:
    ALongerKeyword:
    
These are symbols:

    a
    x
    Foo
    
These are examples of quoted symbols:

    'a
    'x
    'Foo
    
#### Procedures that work with names

*Converter* **`as-keyword`** *expr*   => `<keyword>`<br>
Returns a keyword whose name is given by *expr*. *expr* must be a string or a symbol.

*Converter* **`as-symbol`** *expr*   => `<symbol>`<br>
Returns a symbol whose name is given by *expr*. *expr* must be a string or a keyword.

### Resources

Resources are values that identify files, services, or other machine resources on the local system or on an accessible remote system. Resource values use URLs to idenity these system resources.

#### Procedures that work with resources

*Primitive* **`url`** *string*  => `<url>`<br>
Returns a new URL constructed by parsing *string*. *string* must conform to proper URL syntax.

*Primitive* **`url.scheme`** *url*  => `<symbol>`<br>
Returns the URL scheme.

*Primitive* **`url.host`** *url*  => `<string>`<br>
Returns the URL host, or `nothing` if it doesn't have one.

*Primitive* **`url.path`** *url*  => `<string>`<br>
Returns the URL path, or `nothing` if it doesn't have one.

*Primitive* **`url.port`** *url*  => `Number`<br>
Returns the URL port, or `nothing` if it doesn't have one.

*Primitive* **`url.query`** *url*  => `<string>`<br>
Returns the URL query, or `nothing` if it doesn't have one.

*Primitive* **`as-url`** *expr*  => `<url>`<br>
Returns a new URL. If *expr* is a string then the URL is constructed by parsing it; *expr* must conform to proper URL syntax. If *expr* is a URL, it is returned unchanged.


### Streams

#### Procedures that work with streams

*Method* **`load`** *stream*  => `<anything>`<br>
Reads Bard value from *stream*, which must be a character input stream. Compiles and evaluates each Bard value in the order read. Values that denote expressions that have side effects may change the state of the running Bard process--for example, `def` expressions may create or alter the values of global variables. `load` can be used to load new definitions into a running Bard process.

*Primitive* **`stream.standard-input`**  => `<stream>`<br>
Returns a stream that represents the computer's standard input.

*Primitive* **`stream.standard-output`** => `<stream>`<br>
Returns a stream that represents the computer's standard output.

*Primitive* **`stream.standard-error`**  => `<stream>`<br>
Returns a stream that represents the computer's standard error.

*Primitive* **`stream.create`** *URL* *element-type*  => `<stream>`<br>
Returns a stream of values of type *element-type*. Reading the stream reads *element-type* values; writing values of *element-type* adds them to stream one after another. *element-type* can be `octet` or `character`.

*Primitive* **`stream.length`** *stream*  => `Integer`<br>
Returns the length of *stream* in octets. If the length of the stream cannot be determined it returns `undefined`.

*Primitive* **`stream.read-octet`** *stream* *position*  => `<octet>`<br>
Returns the octet at position *position* in *stream*. *position* is a count of octets.

*Primitive* **`stream.read-octets`** *stream* *position* *count*  => `List`<br>
Returns a list of up to *count* octets obtained by reading *stream*, starting at position *position*. If fewer than *count* octets are available, the ones actually read are returned. *position* is a count of octets.

*Primitive* **`stream.read-all-octets`** *stream*  => `<List>`<br>
Returns a list of all octets available in *stream*.

*Primitive* **`stream.read-character`** *stream*  => `<character>`<br>
Returns the character at position *position* in *stream*. *position* is a count of characters.

*Primitive* **`stream.read-characters`** *stream* *position* *count*  => `Text`<br>
Returns a list of up to *count* characters obtained by reading *stream*, starting at position *position*. If fewer than *count* characters are available, the ones actually read are returned. *position* is a count of characters.

*Primitive* **`stream.read-all-characters`** *stream*  => `Text`<br>
Returns a list of all characters available in *stream*.

*Primitive* **`stream.read-line`** *stream*  => `Text`<br>
Returns the line at position *position* in *stream*. *position* is a count of lines.

*Primitive* **`stream.read-lines`** *stream* *position* *count*  => `List`<br>
Returns a list of up to *count* lines obtained by reading *stream*, starting at position *position*. If fewer than *count* lines are available, the ones actually read are returned. *position* is a count of lines.

*Primitive* **`stream.read-all-lines`** *stream*  => `List`<br>
Returns a list of all lines available in *stream*.

*Primitive* **`stream.read-object`** *stream*  => `<anything>`<br>
Returns the **object** at position *position* in *stream*. An **object** is a Bard value obtained by applying `read` as if reading input at the Bard prompt. *position* is a count of objects.

*Primitive* **`stream.read-objects`** *stream* *position* *count*  => `List`<br>
Returns a list of up to *count* **objects** obtained by reading *stream*, starting at position *position*. An **object** is a Bard value obtained by applying `read` as if reading input at the Bard prompt. If fewer than *count* objects are available, the ones actually read are returned. *position* is a count of objects.

*Primitive* **`stream.read-all-objects`** *stream*  => `List`<br>
Returns a list of all **objects** available in *stream*. An **object** is a Bard value obtained by applying `read` as if reading input at the Bard prompt.

*Primitive* **`stream.write-octet`** *stream* *position* *octet*  => `<anything>`<br>
Writes *octet* at *position* on *stream*. *position* is a count of octets. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-octets`** *stream* *position* *octets*  => `<anything>`<br>
Writes *octets* at *position* on *stream*. *position* is a count of octets. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-character`** *stream* *position* *character*  => `<anything>`<br>
Writes *character* at *position* on *stream*. *position* is a count of characters. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-characters`** *stream* *position* *characters*  => `<anything>`<br>
Writes *characters* at *position* on *stream*. *position* is a count of characters. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-line`** *stream* *position* *line*  => `<anything>`<br>
Writes *line* at *position* on *stream*. *position* is a count of lines. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-lines`** *stream* *position* *lines*  => `<anything>`<br>
Writes *lines* at *position* on *stream*. *position* is a count of lines. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-object`** *stream* *position* *object*  => `<anything>`<br>
Writes *object* at *position* on *stream*. *position* is a count of objects. Overwrites any existing data starting at *position*.

*Primitive* **`stream.write-objects`** *stream* *position* *objects*  => `<anything>`<br>
Writes *objects* at *position* on *stream*. *position* is a count of objects. Overwrites any existing data starting at *position*.

### Strings

A string value is a packed array of characters. Here are a couple of examples:

    "Hello"
    "A longer example of a text value."
    
Strings represent the abstract **Text** type. Bard 0.4.0a provides only strings, but later versions may provide a variety of Text representations.

#### Procedures that work with strings

*Converter* **`as-string`** *expr*   => `<string>`<br>
Returns a new string whose contents are given by *expr*. If *expr* is `nothing` an empty string is returned. If it's a sequence of characters then a new string containing the same sequence of characters is returned. Otherwise the result is the same sequence of characters that would be produced by printing the value using `write`.

*Constructor* **`string.append`** *expr1* *expr2*  => `<string>`<br>
Returns a string constructed by concatenating the two arguments, which must be strings.

*Constructor* **`string.slice`** *string* *start* *end*  => `<string>`<br>
Returns a string constructed by taking the characters in *string* starting at position *start*, and ending with the last character before *end*.

*Accessor* **`string.first`** *string* => `<character>`<br>
Returns the first character in *string*. 

*Constructor* **`string.rest`** *string* => `<string>`<br>
Returns a new string containing all except the first character in *string*. 

*Accessor* **`string.last`** *string* => `<character>`<br>
Returns the last character in *string*. 

*Constructor* **`string.drop`** *count* *string*  => `<string>`<br>
Returns a string that contains the same character as *string*, but without the first *count* elements.

*Constructor* **`string.take`** *count* *string*  => `<string>`<br>
Returns a string that contains the first *count* elements of *string*.

### Association lists

An **association list**, represented by the structure `<alist-map>`, is a collection of key/value pairs. The `<alist-map>` structure is one representation of the abstract type **Map**.

#### Procedures that work with `<alist-map>`

*Primitive* **`alist-map?`** *expr* => `<boolean>`<br>
Returns `true` if *expr* is an `<alist-map>`, and `false` otherwise.

*Primitive* **`alist.get`** *map* *key*  => `<anything>`<br>
Returns the value associated with *key* in *map*, or, if *key* doesn't appear in *map*, returns `undefined`.

*Primitive* **`alist.put`** *map* *key* *val*  => `<alist-map>`<br>
Returns a new `<alist-map>` with the same key/value pairs as *map*, but with the pair (*key* . *val*) added. If *key* appears in *map* then its value is replaced by *val* in the result.

*Primitive* **`alist.keys`** *map*  => `List`<br>
Returns a list of the keys in *map*.

*Primitive* **`alist.vals`** *map*  => `List`<br>
Returns a list of the values in *map*.

*Primitive* **`alist.merge`** *map1* *map2*  => `<alist-map>`<br>
Returns a new map that contains the key/value pairs of both *map1* and *map2*. If the same key appears in both maps then the key/value pair from *map2* is chosen to appear in the result.

*Primitive* **`as-alist-map`** *plist* => `<alist-map>`<br>
Returns a new `<alist-map>` whose keys and values are given by *plist*. *plist* must contain an even number of elements. The first element, and every other element after it, are used as keys; the value for each key is the element that appears directly after it in *plist*. If *plist* is empty then the empty map is returned.



### Comparisons

A **comparison** is a proceure that determines an equivalence or ordering relations between values. In other words, a comparison tells you something like whether two values are equal, or whther one is greater than the other.

#### Comparison procedures

*Primitive* **`<`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is less than *expr2*, and `false` otherwise.

*Primitive* **`>`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is greater than *expr2*, and `false` otherwise.

*Primitive* **`<=`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is less than or equal to *expr2*, and `false` otherwise.

*Primitive* **`>=`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is greater than or equal to *expr2*, and `false` otherwise.

*Primitive* **`=`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is equal to *expr2*, and `false` otherwise.

*Primitive* **`identical?`** *expr1* *expr2*  => `<boolean>`<br>
Returns `true` if *expr1* is **identical** to *expr2*, and `false` otherwise. Two values are **identical** if they are exactly the same value--that is, they are both the exact same bits stored in exactly the same place in the computer's memory--or if they are both immediate numeric values that are equal to each other.


## 3. Special forms

*Special form* **`begin`** *expr* * => `<anything>`<br>
`begin` evaluates each *expr* from left to right, returning the value of the last *expr*. The values of the expressions before the last one are lost; presumably they are evaluated for side effects.

*Special form* **`if`** *test* *then-form* *else-form* => `<anything>`<br>
`if` evaluates *test*. If the result is true then it evaluates *then-form*; otherwise it evaluates *else-form*. It is an error to evaluate an `if` expression with no *else-form*.

*Special form* **`let`** ((*var* *expr*) * ...) *body* ... => `<anything>`<br>
`let` creates a local environment in which the variables given by *var* ... are defined. Any number of variables *var* may appear in the `let` form. Each *var* is bound to the value of the corresponding *expr* form. Any number of expressions may appear in *body*. Expressions in *body* are evaluated from left to right as if in a `begin` form. They may refer to *var* and any other variables defined in the `let` form.

`let` binds variables in the order that they appear. Later variables may refer to the values of earlier variables. For example, the following `let` form is valid:

    (let ((x 2)
          (y (+ x 1)))
      (* x y))

*Special form* **`method`** `(`*parameter* * `)` *expr* * => `<method>`<br>
`method` creates and returns a new method. The formal parameters of the method are given by *parameter* *. The body of the method, given by *expr*, defines the evaluations that take place when the method is called.

*Special form* **`quote`** *expr* => *expr*<br>
`quote` returns its argument unevaluated.

*Special form* **`set!`** *var* *val* => *val*<br>
`set!` assigns the value of *val* to the global variable named *var*, creating the variable if necessary.

*Special form* **`time`** *expr* => `<anything>`<br>
`time` returns the value of *expr* and prints some rough information about the time it took to evaluate.

## 4. Macros

*Macro* **`def`** *var* *val* => *val*<br>
`def` creates a global variable named *var* and assigns the value of *exor* to it.

## 5. Primitive methods

**`compiler`** *expr* => `<anything>`<br>
Returns the code produced by compiling *expr*, which must be valid Bard code.

**`display`** *expr* => `<anything>`<br>
Prints a human-readable serialization of *expr* to standard output.

**`newline`** => <br>
Prints a newline to standard output.

**`read`** => `<anything>`<br>
Returns the next Bard value read from standard input.

**`write`** *expr* => `<anything>`<br>
Prints a machine-readable serialization of *expr* to standard output.

## 6. Built-in methods

**`exit`** => <br>
Terminates the Bard process.

**`call/cc`** *expr* => `<anything>`<br>
Calls the method given by *expr* with one argument: the current continuation.

