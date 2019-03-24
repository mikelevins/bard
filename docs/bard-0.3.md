# bard 0.3

Documents what's working in the 0.3 version of the bard interpreter.

## Lexical syntax

### Named constants

`undefined` - The absence of a meaningful value  
`nothing` - An empty collection of values  
`true` - The Boolean true value  
`false` - The Boolean false value  

### Characters

Characters are atomic elements that appear in text strings. Following
are a few examples of literal character syntax:

`#\c` - literal character  
`#\space` - named literal character  

### List forms

Lists are ordered sequences of values. List values include:

`(...)` - procedure application  
`[...]` - literal list  
`{...}` - literal table  
`"..."` - literal string

## Schemas

A **schema** is an object that formally describes the representation
of a set of values. Schemas may be defined by user code. The following
built-in schemas are defined by bard 0.3:

`<alist-table>` - Tables as represented by association lists (key/value pairs in a list made of cons cells)  
`<base-schema>` - the representation of basic, built-in schemas   
`<bignum>` - unlimited-precision integers  
`<boolean>` - true and false  
`<character>` - the elements of text strings  
`<class>` - abstract types consisting of sets of other classes and schemas  
`<fixnum>` - a compact representation of integers  
`<flonum>` - a compact representation of floating-point numbers  
`<foreign-schema>` - a representation of foreign data (for example, of C structs)  
`<function>` - polymorphic user-definable procedures  
`<generator>` - procedures that may be called repeatedly to produce a new value on each call 
`<interpreted-method>` - monomorphic user-definable procedures represented as interpreted code  
`<iostream>` - sources and sinks for bytes and other values  
`<keyword>` - names that evaluate to themselves  
`<null>` - the representation of `nothing`  
`<pair>` - the representation of primitive pairs (cons cells in 0.3)  
`<primitive-procedure>` - representation of procedures provided by the host runtime  
`<primitive-schema>` - representation of data structures provided by the host runtime  
`<primitive>` - representation of built-in primitive operations such as print, newline, etc.
`<protocol>` - representation of bard protocols  
`<ratnum>` - representation of ratios  
`<record>` - representation of record instances  
`<singleton>` - representation of singleton types  
`<string>` - representation of text strings  
`<structure-schema>` - representation of templates for structure instances  
`<symbol>` - representations of names  
`<tuple>` - representations of structures that consist of ordered sequences of values  
`<undefined>` - representation of absent values  
`<union>` - representations of types consisting of more than one data layout  
`<url>` - representations of URLs  
`<vector>` - representations of vector values  

## Classes

Type variables that name roles defined by standard bard
protocols. Membership in each class is conferred by the definitions of
protocol functions.

`Anything` - The omnibus type that includes all other types. Every value is an instance of `Anything`.  
`Applicable` - The type of values that can be called or applied like functions.  
`Boolean` - The type of the values `true` and `false`.  
`Character`- The type of the elements of a text string.  
`Class` - The type of objects that represent roles in a protocol.  
`File` - The type of objects that represent files.  
`Float`- The type of decimal numbers.  
`Fraction`- The type of ratios.  
`Function`- The type of polymorphic procedures.  
`InputStream` - The type of input streams; that is, sources of incoming data.  
`Integer`- The type of integer values.  
`IODirection` - The type of objects that represent stream directions&mdash; that is, `input`, `output`, or `io`.  
`IOMode` - The type of values that represent stream capabilities; that is, `read`, `write`, or `readwrite`.  
`IOType` - The answer to the question, "what is the type of this stream? The answer may be `input`, `output`, or `io`.   
`Keyword` - The type of interned names that evaluate to themselves.  
`List` - The type of ordered sequences of values with finite length.  
`Method` - The type of monomorphic procedures.  
`Null` - The type of `nothing`.  
`Number` - The type of all numeric values.  
`Orderable`- The type of all values that may be stably sorted.  
`OutputStream` - The type of output streams; that is, sinks for outgoing data.    
`Pair` - The type of containers with exactly two elements.  
`Protocol` - The type of named collections of specializable functions with associated variables.  
`Ratio` - The type of numeric ratios.  
`Schema`- The type of objects that represent data layouts for concrete types.  
`Stream` - The type of sources and sinks for output and input values.  
`Symbol`- The type of interned names used as variables.  
`Table` - The type of values that represent finite maps.  
`Text` - The type of text strings.  
`Type` - The type of Bard types.  
`Undefined` - The type of no useful value.  
`URL` - The type of names that represent URLs.  

## Protocols

### Addressing
  Classes: `Name`, `URL`

The `Addressing` protocol defines operations on URLs.

  `url scheme: Name domain: Name port: Name path: Name query: Name -> URL`  
  _Note: To be added:_ `fragment: Name`  
  
  `url-domain url-domain URL -> Name`  
  `url-path url-path URL -> Name`  
  `url-port url-port URL -> Name`  
  `url-query url-query URL -> Name`  
  `url-scheme url-scheme URL -> Name`    
  
### Applying  
  Classes: `Anything`, `Applicable`, `Boolean`

The `Applying` protocol defines operations on functions, methods, and
values that may be applied like functions.

`applicable? Anything -> Boolean`  
  `apply Applicable [Anything*] -> Anything`  
  `complement Applicable -> Applicable`  
  `compose & -> Applicable`  
  `constantly Anything -> Applicable`  
  `eval Anything -> Anything`  
  `flip Applicable -> Applicable`  
  `identity Anything -> Anything`  
  `partial Applicable & -> Applicable`  
  
### Calculating  
  Classes: `Boolean`, `Integer`, `Number`

The `Calculating` protocol defines arithmetic operations on numbers.

`* & -> Number`  
  `+ & -> Number`  
  `- & -> Number`  
  `/ & -> Number`  
  `even? Integer -> Boolean`  
  `max & -> Number`  
  `min & -> Number`  
  `odd? Integer -> Boolean`  
  `random Integer -> Integer`  
  `remainder Integer Integer -> Integer`  
  
### Comparing  
  Classes: `Anything`, `Boolean`

The `Comparing` protocol defines equality predicates.

  `= & -> Boolean`  
  `prim:= Anything Anything -> Boolean`  
  
### Converting  
  Classes: `Anything`, `Type`

The `Converting` protocol defines procedures that convert a value fro
one type to another.

  `as Type Anything -> Type`  
  
### Creating  
  `make Type & -> Type`  
  
### Generating  
  `cycle List -> Generator`  
  `generated-count Generator -> Integer`  
  `generated-values Generator -> List`  
  `iterate Procedure -> Geneartor`  
  `next Generator -> Anything`  
  `next-n Generator -> List`  
  `range-from Integer -> Generator`  
  
### Listing  
  `add-first Anything List -> List`  
  `add-last List Anything -> List`  
  `any List -> Anything`  
  `append & -> List`  
  `by Integer List -> List`  
  `drop Integer List -> List`  
  `element List Integer -> Anything`  
  `empty? List -> Boolean`  
  `filter Procedure List -> List`  
  `first List -> Anything`  
  `join-text List -> Text`  
  `last List -> Anything`  
  `length List -> Integer`  
  `list & -> List`  
  `map Procedure List -> List`  
  `member? Anything List -> Boolean`  
  `next-last List -> Anything`  
  `partition & -> &`  
  `position Anything List -> Integer`  
  `position-if Procedure List -> Integer`  
  `range Integer Integer -> List`  
  `reduce Procedure Anything &optional List -> Anything`  
  `rest List -> List`  
  `reverse List -> List`  
  `second List -> Anything`  
  `some? Procedure List -> Anything`  
  `split-text Text Character -> List`  
  `take Integer List -> List`  
  `take-by Integer Integer List -> List`  
  `vector & -> List`  
  
### Mapping  
  `get-key Map Anything -> Anything`  
  `keys Map -> List`  
  `merge Map Map -> Map`  
  `put-key Map Anything Anything -> Map`  
  `vals Map -> List`  
  `table & -> Map`  
  
### Ordering  
  `< & -> Boolean`  
  `<= & -> Boolean`  
  `> & -> Boolean`  
  `>= & -> Boolean`  
  
### Pairing  
  `left Pair -> Anything`  
  `pair Anything Anything -> Pair`  
  `right Pair -> Anything`  
  
### Reading  
  `current-input -> Stream`  
  `load URL -> Boolean`  
  `read Stream -> Anything`  
  `read-file URL -> List`  
  `read-line Stream -> List`  
  `read-lines Stream -> List`  
  `read-text Stream -> Text`  
  
### Streaming  
  `contents Stream -> List`  
  `lines Stream -> List`  
  `stream-direction Stream -> Name`  
  
### System  
  `error ->`  
  `exit ->`  
  `gc ->`  
  `gensym -> Symbol`  
  `quit ->`  
  `room ->`  
  `uuid ->` `Text`
  `version ->` `Text`
  
### TextProcessing  
  
### Typing  
  `boolean? Anything -> Boolean`  
  `character? Anything -> Boolean`  
  `class? Anything -> Boolean`  
  `false? Anything -> Boolean`  
  `float? Anything -> Boolean`  
  `foreign-value? Anything -> Boolean`  
  `function? Anything -> Boolean`  
  `input-stream? Anything -> Boolean`  
  `integer? Anything -> Boolean`  
  `iostream? Anything -> Boolean`  
  `keyword? Anything -> Boolean`  
  `nothing? Anything -> Boolean`  
  `output-stream? Anything -> Boolean`  
  `protocols Anything -> List`
  `list? Anything -> Boolean`  
  `list-protocols -> List`
  `method? Anything -> Boolean`  
  `pair? Anything -> Boolean`  
  `protocol? Anything -> Boolean`  
  `singleton Anything -> Singleton`
  `something? Anything -> Boolean`  
  `symbol? Anything -> Boolean`  
  `table? Anything -> Boolean`  
  `text? Anything -> Boolean`  
  `true? Anything -> Boolean`  
  `type Anything -> Type`
  `undefined? Anything -> Boolean`  
  
### Writing  
  `current-output -> OutputStream`  
  `display Anything ->`  
  `newline ->`  
  `print Anything ->`  
  `show Anything -> Text`  
  `write Anything OutputStream -> `  
  
## Special forms  
  
**`add-method!`** _`fn method-signature method`_  

Adds _`method`_ to the function _`fn`_, matching the types given in
_`method-signature`_. When `fn` is subsequently applied to values of
the specified types, the supplied `method` is called.

Example:
```
(add-method! glom (<fixnum> <fixnum>) (^ (x y) [x y]))
(add-method! glom (<string> <string>) (^ (x y) (append x y)))

(glom 1 2) => (1 2)
(glom "foo" "bar") => "foobar"
```

**`begin`** _`expr`_* `=> Anything`  

`begin` evaluates any number of expressions from left to right,
returning the value returned by the rightmost expression. `begin` is
most often useful for executing expressions that have side-effects,
such as updating a mutable variable or printing some output.

Evaluating an empty `begin` form returns `nothing`.

**`cond`** _`clause`_* `=> Anything`  

**`def`** _`variable value`_ `=> Anything`  

**`define class`  
`define macro`  
`define method`  
`define protocol`  
`define record`  
`define tuple`  
`define variable`  
`ensure`  
`function`  **

**`generate`** _`inits`_ _`body -> Generator`   

Returns a Generator. Calling `next` on the generator returns the
next-computed return value.

Example:
```
(define method (generate-names)
  (generate ((result []))
    (yield result)
    (resume (gen-name))))
```

Given a function `gen-name` that constructs an arbitrary name, the
example function creates a generator that returns a new name each time
`next` is applied to it.

The `generate` special form accepts a list of bindings, like a `let`
form. The bindings supply initial values to each named variable.

`yield` returns a value when `next` is applied to the gnerator. In the
example, it returns the current value of `result`.

`resume` rebinds the values of the named variables. It accepts one
argument for each of the namede bindings. For example, in the example
above, there is just one binding, named `result`; `resume` therefore
accepts exactly one argument, which then becomes the new value of
`result`.

A generator can be called an unlimited number of times, yielding a new
value for each call. A generator maybe written to eventually reach a
steady state, after which each call always returns the same value.

**`if`**  
**`let`**  
**`loop`**  
**`method`** (aka **`^`**)  
**`not`**  
**`protocol`**  
**`quasiquote`**  
**`quote`**  
**`remove-method!`**  
**`repeat`**  
**`set!`**  
**`time`**  
**`undefine`**  
**`unless`**  
**`values`**  
**`when`**  
**`with-open-file`**  
  
## Macros  
  
**`and`**  
**`or`**  
  
