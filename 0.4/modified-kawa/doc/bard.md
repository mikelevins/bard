# bard 0.4

## lexical syntax

types |syntax
------ | ---- | -----------
various | `undefined`
 &nbsp; |`true` | 
 &nbsp;  |`false` | 
 &nbsp;  |`nothing` | 
 &nbsp;  |`end` | 
`Number`| `0`
 &nbsp;  |`1001` | 
 &nbsp;  |`2.3` | 
 &nbsp;  |`1.0e3` | 
  `Name`  |`foo`
 &nbsp;  |`Bar` | 
 &nbsp;  |`next:` | 
 &nbsp;  |`"A longer name"` | 
 `Sequence`  |`()` 
 &nbsp;  |`(1)` | 
 &nbsp;  |`(a b c)` | 
 &nbsp;  |`(+ 2 3)` | 
`Pair`  |`[name: "Fred"]`
 &nbsp;  |`[1 2]` | 
`Map`  |`{ name: "Al"`<br/> `age: 45 }` 
 &nbsp;  |`{1 2 3 4}` | 
 `method`  |`(^ (x) x)` |
`function`  |`(-> List -> Number)` 
`Type`  |`#<Sequence>` (equivalent to `Sequence`)  
 &nbsp;  |`#<Map Name method>` |
 &nbsp;  |`#<Sequence Number>` |
 &nbsp;  |`#<Sequence byte length: 4>` |
 &nbsp;  |`#<Sequence value: '(1 2 3 4)>` |
 &nbsp;  |`#<Map Name Number>` |
 &nbsp;  |`#<Name value: "Luna">` |

## built-in structures


name | description
---- | -----------
`undefined` | an invalid value
`none` | an empty collection or absent value 
`boolean`        | truth values 
`function`        | polymorphic procedures 
`number`      | numeric values 
`symbol`      | names and other textual values 
`cons`      | a pair of values 
`sequence`      | ordered sequences of values 
`object` | mappings from names to values
`method` | simple procedures

## special forms

name |syntax| description
---- | ----------- | -----------
`begin` |(`begin` *expr* ...)| sequential evaluation
`cond` |(`cond` (*test* *expr* ...) ...)|  conditional evaluation
`define` |(`define` *var* *val* )|  global definitions
`eval` |(`eval` *expr* )|  evaluation
`if` |(`if` *test* *then* *else* )|  conditional evaluation
`let` |(`let` ( ( *var* *val* ) ...) *expr* ...)|  lexical binding
`macro` |(`macro` ( *name* *params* ...) *pattern* ...)|  syntax construction
`method` |(`method` (  *param* ...) *expr* ...)|  procedure construction
`quasiquote` |<code>&#96;</code> *expr*|  evaluation control
`quote` |`'` *expr*|  evaluation control
`setter` |(( `setter` *place* ) *value*)|  assignment
`unquote` |`,` *expr*|  evaluation control

## type constructors

name | description
---- | -----------
`define synonym` | create a new name for one or more existing types
`define sequence` | create a sequence type with optional restrictions
`define object` | create an object type with optional restrictions

## protocols

name | variables | procedures
---- | -----------
`bard.language` | `*protocol*`
`bard.list` | 
`bard.map` | 
`bard.math` | 
`bard.resource` | 
`bard.stream` | 
`bard.system` | 
`bard.user` | 
