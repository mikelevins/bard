# Bard Reference

Version 0.4.0a4

Copyright 2013 by mikel evins

## 1. Changes

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

## 2. Built-in values and lexical syntax

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


### Special constants

Bard 0.4.0a defines four special constants:

    undefined
    nothing
    true
    false
    
* **`undefined`** represents any absent, unknown, improper, or otherwise undefined value
* **`nothing`** represents the empty set, list, or collection
* **`true`** and **`false`** represent the two possible Boolean values

### Pairs

A **pair** is a value that associates two other values, called the **left element** and the **right element**.

*Constructor* **`pair`** *expr1* *expr2*  => `<pair>`

Returns a pair whose left element is the value of *expr1* and whose right element is the value of *expr2*. 

*Accessor* **`pair.left`** *expr*  => `<anything>`

Returns the left element of *expr*.

*Accessor* **`pair.right`** *expr*  => `<anything>`

Returns the right element of *expr*.

*Constructor* **`list`** *expr* *  => `<pair>`

Returns a pair whose left element is the value of *expr1* and whose right element is another pair. The right element's left element is the value of the expression after *expr1*, and its right element is another pair containing the values of any expressions that follow. `list` can accept up to 10 arguments in Bard 0.4.0a4, enabling you to create chains of pairs up to 10 elements long.

## 3. Special forms

**`begin`** *expr* * => `<anything>`

`begin` evaluates each *expr* from left to right, returning the value of the last *expr*. The values of the expressions before the last one are lost; presumably they are evaluated for side effects.

**`if`** *test* *then-form* *else-form* => `<anything>`

`if` evaluates *test*. If the result is true then it evaluates *then-form*; otherwise it evaluates *else-form*. It is an error to evaluate an `if` expression with no *else-form*.

**`method`** `(`*parameter* * `)` *expr* * => `<method>`

`method` creates and returns a new method. The formal parameters of the method are given by *parameter* *. The body of the method, given by *expr*, defines the evaluations that take place when the method is called.

**`quote`** *expr* => *expr*

`quote` returns its argument unevaluated.

**`set!`** *var* *val* => *val*

`set!` assigns the value of *val* to the global variable named *var*, creating the variable if necessary.

## 4. Macros

**`def`** *var* *val* => *val*

`def` creates a global variable named *var* and assigns the value of *exor* to it.

## 4. Primitive methods

**`+`** *expr1* *expr2* => `Number`

Returns the sum of *expr1* and *expr2*. Both arguments must be numbers.

**`*`** *expr1* *expr2* => `Number`

Returns the product of *expr1* and *expr2*. Both arguments must be numbers.

**`-`** *expr1* *expr2* => `Number`

Returns the difference of *expr1* and *expr2*. Both arguments must be numbers.

**`/`** *expr1* *expr2* => `Number`

Returns the quotient of *expr1* and *expr2*. Both arguments must be numbers.

**`compiler`** *expr* => `<anything>`

Returns the code produced by compiling *expr*, which must be valid Bard code.

**`display`** *expr* => `<anything>`

Prints a human-readable serialization of *expr* to standard output.

**`newline`** => 

Prints a newline to standard output.

**`pair`** *expr1* *expr2* => `Pair`

Returns a pair whose left element is *expr1* and whose right element is *expr2*.

**`read`** => `<anything>`

Returns the next Bard value read from standard input.

**`write`** *expr* => `<anything>`

Prints a machine-readable serialization of *expr* to standard output.

## 5. Built-in methods

**`exit`** => 

Terminates the Bard process.

**`call/cc`** *expr* => `<anything>`

Calls the method given by *expr* with one argument: the current continuation.

