# Bard Reference

Version 0.4.0a11

Copyright 2013 by mikel evins

## 1. Changes

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

*Special form* **`method`** `(`*parameter* * `)` *expr* * => `<method>`<br>
`method` creates and returns a new method. The formal parameters of the method are given by *parameter* *. The body of the method, given by *expr*, defines the evaluations that take place when the method is called.

*Special form* **`quote`** *expr* => *expr*<br>
`quote` returns its argument unevaluated.

*Special form* **`set!`** *var* *val* => *val*<br>
`set!` assigns the value of *val* to the global variable named *var*, creating the variable if necessary.

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

