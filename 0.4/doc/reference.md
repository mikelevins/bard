# Bard Reference

Version 0.4.0a8

Copyright 2013 by mikel evins

## 1. Changes

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

### Booleans

A **Boolean** is one of the special values `true` or `false`. A **generalized Boolean** is any Bard value when treated as true or false. The special values `false` and `nothing` are both false when treated as generalized booleans; all other Bard values are considered true.

Bard provides special forms, macros, and functions for working with Booleans and generalized Booleans.

**`and`** *expr* * => `Boolean`

`and` evaluates the first *expr*. If the result is false then `and` immediately returns `false` without evaluating any further expressions. If it's true, though, then `and` evaluates the next expression, repeating the same behavior. If all arguments to `and` return true values then `and` returns the last value produced. If any expression produces a false value then `and` immediately returns `false` without evaluating any further expressions.

If you call `and` with no arguments it returns `true`.

**`not`** *expr* => `Boolean`

`not` evaluates *expr*. If the result is true then `not` returns `false`. If the result is false then `not` returns `true`.

**`or`** *expr* * => `Boolean`

`or` evaluates the first *expr*. If the result is true then `or` immediately returns that value without evaluating any further expressions. If it's false, though, then `or` evaluates the next expression, repeating the same behavior. If all arguments to `or` return false values then `or` returns the last value produced. If any expression produces a true value then `or` immediately returns that value without evaluating any further expressions.

If you call `or` with no arguments it returns `false`.

### Comparisons

A **comparison** is a proceure that determines an equivalence or ordering relations between values. In other words, a comparison tells you something like whether two values are equal, or whther one is greater than the other.

**`<`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is less than *expr2*, and `false` otherwise.

**`>`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is greater than *expr2*, and `false` otherwise.

**`<=`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is less than or equal to *expr2*, and `false` otherwise.

**`>=`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is greater than or equal to *expr2*, and `false` otherwise.

**`=`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is equal to *expr2*, and `false` otherwise.

**`identical?`** *expr1* *expr2*  => `<boolean>`

Returns `true` if *expr1* is **identical** to *expr2*, and `false` otherwise. Two values are **identical** if they are exactly the same value--that is, they are both the exact same bits stored in exactly the same place in the computer's memory--or if they are both immediate numeric values that are equal to each other.


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

