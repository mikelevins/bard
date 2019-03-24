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

  `url scheme: Name domain: Name port: Name path: Name query: Name => URL`  
  _Note: To be added:_ `fragment: Name`  
  
  `url-domain url-domain URL => Name`  
  `url-path url-path URL => Name`  
  `url-port url-port URL => Name`  
  `url-query url-query URL => Name`  
  `url-scheme url-scheme URL => Name`    
  
### Applying  
  Classes: `Anything`, `Applicable`, `Boolean`

The `Applying` protocol defines operations on functions, methods, and
values that may be applied like functions.

`applicable? Anything => Boolean`  
  `apply Applicable [Anything*] => Anything`  
  `complement Applicable => Applicable`  
  `compose & => Applicable`  
  `constantly Anything => Applicable`  
  `eval Anything => Anything`  
  `flip Applicable => Applicable`  
  `identity Anything => Anything`  
  `partial Applicable & => Applicable`  
  
### Calculating  
  Classes: `Boolean`, `Integer`, `Number`

The `Calculating` protocol defines arithmetic operations on numbers.

`* & => Number`  
  `+ & => Number`  
  `- & => Number`  
  `/ & => Number`  
  `even? Integer => Boolean`  
  `max & => Number`  
  `min & => Number`  
  `odd? Integer => Boolean`  
  `random Integer => Integer`  
  `remainder Integer Integer => Integer`  
  
### Comparing  
  Classes: `Anything`, `Boolean`

The `Comparing` protocol defines equality predicates.

  `= & => Boolean`  
  `prim:= Anything Anything => Boolean`  
  
### Converting  
  Classes: `Anything`, `Type`

The `Converting` protocol defines procedures that convert a value fro
one type to another.

  `as Type Anything => Type`  
  
### Creating  
  `make Type & => Type`  
  
### Generating  
  `cycle List => Generator`  
  `generated-count Generator => Integer`  
  `generated-values Generator => List`  
  `iterate Procedure => Geneartor`  
  `next Generator => Anything`  
  `next-n Generator => List`  
  `range-from Integer => Generator`  
  
### Listing  
  `add-first Anything List => List`  
  `add-last List Anything => List`  
  `any List => Anything`  
  `append & => List`  
  `by Integer List => List`  
  `drop Integer List => List`  
  `element List Integer => Anything`  
  `empty? List => Boolean`  
  `filter Procedure List => List`  
  `first List => Anything`  
  `join-text List => Text`  
  `last List => Anything`  
  `length List => Integer`  
  `list & => List`  
  `map Procedure List => List`  
  `member? Anything List => Boolean`  
  `next-last List => Anything`  
  `partition & => &`  
  `position Anything List => Integer`  
  `position-if Procedure List => Integer`  
  `range Integer Integer => List`  
  `reduce Procedure Anything &optional List => Anything`  
  `rest List => List`  
  `reverse List => List`  
  `second List => Anything`  
  `some? Procedure List => Anything`  
  `split-text Text Character => List`  
  `take Integer List => List`  
  `take-by Integer Integer List => List`  
  `vector & => List`  
  
### Mapping  
  `get-key Map Anything => Anything`  
  `keys Map => List`  
  `merge Map Map => Map`  
  `put-key Map Anything Anything => Map`  
  `vals Map => List`  
  `table & => Map`  
  
### Ordering  
  `< & => Boolean`  
  `<= & => Boolean`  
  `> & => Boolean`  
  `>= & => Boolean`  
  
### Pairing  
  `left Pair => Anything`  
  `pair Anything Anything => Pair`  
  `right Pair => Anything`  
  
### Reading  
  `current-input => Stream`  
  `load URL => Boolean`  
  `read Stream => Anything`  
  `read-file URL => List`  
  `read-line Stream => List`  
  `read-lines Stream => List`  
  `read-text Stream => Text`  
  
### Streaming  
  `contents Stream => List`  
  `lines Stream => List`  
  `stream-direction Stream => Name`  
  
### System  
  `error =>`  
  `exit =>`  
  `gc =>`  
  `gensym => Symbol`  
  `quit =>`  
  `room =>`  
  `uuid =>` `Text`
  `version =>` `Text`
  
### TextProcessing  
  
### Typing  
  `boolean? Anything => Boolean`  
  `character? Anything => Boolean`  
  `class? Anything => Boolean`  
  `false? Anything => Boolean`  
  `float? Anything => Boolean`  
  `foreign-value? Anything => Boolean`  
  `function? Anything => Boolean`  
  `input-stream? Anything => Boolean`  
  `integer? Anything => Boolean`  
  `iostream? Anything => Boolean`  
  `keyword? Anything => Boolean`  
  `nothing? Anything => Boolean`  
  `output-stream? Anything => Boolean`  
  `protocols Anything => List`
  `list? Anything => Boolean`  
  `list-protocols => List`
  `method? Anything => Boolean`  
  `pair? Anything => Boolean`  
  `protocol? Anything => Boolean`  
  `singleton Anything => Singleton`
  `something? Anything => Boolean`  
  `symbol? Anything => Boolean`  
  `table? Anything => Boolean`  
  `text? Anything => Boolean`  
  `true? Anything => Boolean`  
  `type Anything => Type`
  `undefined? Anything => Boolean`  
  
### Writing  
  `current-output => OutputStream`  
  `display Anything =>`  
  `newline =>`  
  `print Anything =>`  
  `show Anything => Text`  
  `write Anything OutputStream => `  
  
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
**`define variable`** _`variable value`_ `=> Anything`  

Binds _`variable`_ to _`value`_ in the global namespace, returning _`value`_.

**`define class`** _`name`_  

Asserts that `name` identifies a Bard class. `name` can subesquently
be used to identify parameters in the definition of protocols.

**`define macro`** `(`_`name parameter*`_ `)` _`body`_ `=>` _`name`_  

Defines _`name`_ as a macro. When Bard subsquently encounters an
application whose operator is _`name`_, it uses the code in _`body`_
to rewrite the expression before evaluating the result.

**`define method`** `(` _`name`_ _`parameter*`_ `)` _`body`_ => _`name`_  

Creates a method that, when applied to a number of values equal to the
number of names in _`parameter*`_, binds each parameter to the
corresponding value and evaluates the code in _`body`_ in the
resulting environment. _`name`_ identifies the function to which the
method is added, or, if no such function exists, Bard creates one.

Each _`parameter`_ is a name. A method definition may optionally
constrain any of its parameters to match the type or value of its
corresponding argument. For example,

```
    (define method (glom x y z)
      with: ((x <string>)
             (y List)
             (z (exactly nothing)))
    ...body...)
```

The above definition yields a method on the function `glom` that is
selected when `x` is an instance of the schema `<string>`, `y` is an
instance of any class or schema that is a member of the class `List`,
and `z` is equal to the value `nothing`.

**`define protocol`** _`pname`_ `(` _`fname cname1`_ `->` _`cname2`_ `)*`  

Creates a protocol and binds it globally to _`pname`_. Each clause 

`(`_`fname cname1*`_ `->` _`cname2`_ `)*` 

defines a function named _`fname`_ that accepts parameters
_`cname1*`_, where each _`cname1`_ is a Bard class. Each function
returns a value of type _`cname2`_, a Bard class.

Protocol functions are purely abstract and cannot be applied to
arguments until **`define method`** is used to associate methods with
concrete argument types.

**`define record`** _`name slotname*`_ `=>` _`name`_  

Creates a new record type and binds it globally to _`name`_. The new record
has named slots _`slotname*`_. Instances of the record can be created
using `make`.

As an example, we can define a cartesian point like so:

```
    bard> (define record <point> x y)
    bard> (make <point> x: 1 y: 2)
    #<point>{x 1 y 2}
```

**`define tuple`** _`name`_ `slot-count:` _`count`_ `slot-type:` _`type`_ `=>` _`name`_  

Creates a new tuple type and binds it globally to _`name`_. Instances
of the tuple can be created using `make`. 

```
    bard> (define tuple <ostype> slot-count: 4 slot-type: <fixnum>)
    bard> (make <ostype> values: [1 2 3 4])
    #<ostype>(1 2 3 4)
```

Bard 0.3 does not check or enforce the type contraints on tuple slots.

**`ensure`** _`preparation body cleanup`_ `=>` _`Anything`_   

Evaluates the form _`preparation`_, followed by the form _`body`_ and
the form _`cleanup`_. Ensures that _`cleanup`_ is evaluated even if
_`body`_ terminates abnormally&mdash;for example, even if it raises an
error.

**`function`** _`Class*`_ `->` _`Type`_ `=>` _`Function`_  

Creates and returns a `Function` instance that accepts parameters of
the types given in _`Class*`_ and returns a value of the type given by
_`Type`_. The new function cannot be called or applied until at least
one method is added to it using `define method`.

**`generate`** _`inits`_ _`body => Generator`   

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

**`if`**  _`test`_ _`consequence`_ _`alternate`_ `=>` `Anything`  

Evaluates the expression _`test`_. If the returned value is true (or
truthy), evaluates and returns the expression
_`consequence`_. Otherwise, evaluates and returns _`alternate`_.

**`let`** `( (` _`name val`_ `)* )` _`body`_ `=> Anything`  

Binds zero or more names in `(` _`name val`_ `)*` to the values of the
corresponding _`val`_ expressions, then evaluates _`body`_ in the
resulting environment, returning the value of the last expression in
_`body`_. Each binding expression in `(` _`name val`_ `)*` can see all
of the preceding bindings in the `let` expression.

**`loop`** _`name`_ `( (` _`var`_ _`val`_ `)* )` _`body`_ `=> Anything`

A variation on `let` that can evaluate its _`body`_ more than once.

Binds zero or more names in `(` _`name val`_ `)*` to the values of the
corresponding _`val`_ expressions, then evaluates _`body`_ in the
resulting environment, returning the value of the last expression in
_`body`_. Each binding expression in `(` _`name val`_ `)*` can see all
of the preceding bindings in the `loop` expression.

In _`body`_, _`name`_ is bound to a procedure that accepts the same
number of arguments as the number of variables _`var`_. Calling
_`name`_ re-enters the `loop` body with the variables bound to the
values passed to _`name`_.

For example:

```

bard> (loop next ((x 0))
        (if (>= x 10)
          x 
          (next (+ x 1))))
10

```

**`method`** `(` _`parameter*`_ `)` _`body`_ `=> Anything`  

Creates and returns a new method that accepts one argument for each
parameter in _`parameter*`_. When applied to values, the method binds
them to the parameters and evaluates _`body`_ in the resulting
environment, returning the value of the last expression in _`body`_.

**`not`** _`val`_ `=> Boolean`  

If _`val`_ is truthy, `not` returns `false`. Otherwise, it returns
`true`.

**`protocol`**  `(` _`fname cname1`_ `->` _`cname2`_ `)*`  

Creates and returns a protocol. Each clause 

`(`_`fname cname1*`_ `->` _`cname2`_ `)*` 

defines a function named _`fname`_ that accepts parameters
_`cname1*`_, where each _`cname1`_ is a Bard class. Each function
returns a value of type _`cname2`_, a Bard class.

Protocol functions are purely abstract and cannot be applied to
arguments until **`define method`** is used to associate methods with
concrete argument types.

**`quasiquote`** _`expr`_ `=> Anything`  

Returns _`expr`_ unevaluated. Within _`expr`_, the operators `,`
(unquote) and `,@` (unquote-splicing) can be used to selectively
evaluate subexpressions in the enclosing environment.

**`quote`** _`expr`_ `=> Anything`  

Returns _`expr`_ unevaluated.

**`remove-method!`** _`fn`_  `(` _`type*`_ `)` `=>` _`fn`_  

Given a function _`fn`_, evaluates the expressions in _`type`_ to
obtain a **type signature**. It then matches the type signature
against all the methods defined on _`fn`_ and, if a match is found,
removes that method from _`fn`_.

**`repeat`** _`expr`_  

Evaluates _`expr`_ repeatedly in an infinite loop. Normally used with
the special-form **`with-exit`**, which provides a way to return from
the loop.

**`set!`** _`name expr`_  

Evaluates _`expr`_ and binds it globally to the variable
_`name`_. Raises an error if the variable does not exist. New global
variables canbe created with **`define variable`** or **`def`**.

**`time`** _`expr`_  

Evaluates _`expr`_ and returns the resulting value, then prints a
report summarizing the time taken for the evaluation.

**`undefine`** _`name`_  

Returns _`name`_. If _`name`_ in bound in the global environment,
`undefine` removes the definition, rendering the variable undefined.

**`unless`** _`test body`_   



**`values`**  
**`when`**  
**`with-exit`**  
**`with-open-file`**  
  
## Macros  
  
**`and`**  
**`or`**  
  
