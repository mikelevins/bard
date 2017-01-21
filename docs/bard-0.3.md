# bard 0.3

Documents what's working in the 0.3 version of the bard interpreter.

## Lexical syntax

### Named constants

`undefined`  
`nothing`  
`true`  
`false`  

### Characters
`#\c` literal character
`#\space` named literal character

### List forms

`(...)` procedure application  
`[...]` literal list  
`{...}` literal table  
`"..."` literal string

## Schemas

`<alist-table>`  
`<base-schema>`  
`<bignum>`  
`<boolean>`  
`<character>`  
`<class>`  
`<fixnum>`  
`<flonum>`  
`<foreign-schema>`  
`<function>`  
`<generator>`  
`<interpreted-method>`  
`<iostream>`  
`<keyword>`  
`<null>`  
`<pair>`  
`<primitive-procedure>`  
`<primitive-schema>`  
`<primitive>`  
`<protocol>`  
`<ratnum>`  
`<record>`  
`<singleton>`  
`<string>`  
`<structure-schema>`  
`<symbol>`  
`<tuple>`  
`<undefined>`  
`<union>`  
`<url>`  
`<vector>`  

## Classes

`Anything`  
`Applicable`  
`Boolean`  
`Character`  
`Class`  
`File  
Float`  
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
  `url`  
  `url-domain`  
  `url-path`  
  `url-port`  
  `url-query`  
  `url-scheme`    
  
### Applying  
  `applicable?`  
  `apply`  
  `complement`  
  `compose`  
  `constantly`  
  `eval`  
  `flip`  
  `identity`  
  `partial`  
  
### Calculating  
  `*`  
  `+`  
  `-`  
  `/`  
  `even?`  
  `max`  
  `min`  
  `odd?`  
  `random`  
  `remainder`  
  
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
`define macro`  
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
  
