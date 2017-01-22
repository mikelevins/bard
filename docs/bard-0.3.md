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

`Anything`   
`Applicable`  
`Boolean`  
`Character`  
`Class`  
`File`  
`Float`  
`Fraction`  
`Function`  
`InputStream`  
`Integer`  
`IODirection`  
`IOMode`  
`IOType`  
`Keyword`  
`List`  
`Method`  
`Null`  
`Number`  
`Orderable`  
`OutputStream`  
`Pair`  
`Protocol`  
`Ratio`  
`Schema`  
`Stream`  
`Symbol`  
`Table`  
`Text`  
`Type`  
`Undefined`  
`URL`  

## Protocols

### Addressing
  Classes: `Name`, `URL`

  `url scheme: Name domain: Name port: Name path: Name query: Name -> URL`  
  `url-domain url-domain URL -> Name`  
  `url-path url-path URL -> Name`  
  `url-port url-port URL -> Name`  
  `url-query url-query URL -> Name`  
  `url-scheme url-scheme URL -> Name`    
  
### Applying  
  `applicable? Anything -> Boolean`  
  `apply Applicable & -> Anything`  
  `complement Applicable -> Applicable`  
  `compose & -> Applicable`  
  `constantly Anything -> Applicable`  
  `eval Anything -> Anything`  
  `flip Applicable -> Applicable`  
  `identity Anything -> Anything`  
  `partial Applicable & -> Applicable`  
  
### Calculating  
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
  `=`  
  `prim:=`  
  
### Converting  
  `as`  
  
### Creating  
  `make`  
  
### Generating  
  `cycle`  
  `generated-count`  
  `generated-values`  
  `iterate`  
  `next`  
  `next-n`  
  `range-from`  
  
### Listing  
  `add-first`  
  `add-last`  
  `any`  
  `append`  
  `by`  
  `drop`  
  `element`  
  `empty?`  
  `filter`  
  `first`  
  `join-text`  
  `last`  
  `length`  
  `list`  
  `map`  
  `member?`  
  `next-last`  
  `partition`  
  `position`  
  `position-if`  
  `range`  
  `reduce`  
  `rest`  
  `reverse`  
  `second`  
  `some?`  
  `split-text`  
  `take`  
  `take-by`  
  `vector`  
  
### Mapping  
  `get`  
  `get-key`  
  `keys`  
  `merge`  
  `put`  
  `put-key`  
  `vals`  
  `table`  
  
### Ordering  
  `<`  
  `<=`  
  `>`  
  `>=`  
  
### Pairing  
  `left`  
  `pair`  
  `right`  
  
### Reading  
  `current-input`  
  `load`  
  `read`  
  `read-file`  
  `read-line`  
  `read-lines`  
  `read-text`  
  
### Streaming  
  `contents`  
  `lines`  
  `stream-direction`  
  
### System  
  `error`  
  `exit`  
  `gc`  
  `gensym`  
  `quit`  
  `room`  
  `uuid`  
  `version`  
  
### TextProcessing  
  
### Typing  
  `boolean?`  
  `character?`  
  `class?`  
  `false?`  
  `float?`  
  `foreign-value?`  
  `function?`  
  `input-stream?`  
  `integer?`  
  `iostream?`  
  `keyword?`  
  `nothing?`  
  `output-stream?`  
  `protocols`  
  `list?`  
  `list-protocols`  
  `method?`  
  `pair?`  
  `protocol?`  
  `singleton`  
  `something?`  
  `symbol?`  
  `table?`  
  `text?`  
  `true?`  
  `type`  
  `undefined?`  
  
### Writing  
  `current-output`  
  `display`  
  `newline`  
  `print`  
  `show`  
  `write`  
  
## Special forms  
  
`add-method!`  
`begin`  
`cond`  
`def`  
`define class`  
`define macro`  <
`define method`  
`define protocol`  
`define record`  
`define tuple`  
`define variable`  
`ensure`  
`function`  
`generate`  
`if`  
`let`  
`loop`  
`method` (aka `^`)  
`not`  
`protocol`  
`quasiquote`  
`quote`  
`remove-method!`  
`repeat`  
`set!`  
`time`  
`undefine`  
`unless`  
`values`  
`when`  
`with-open-file`  
  
## Macros  
  
`and`  
`or`  
  
