# Bard
## A programming language
### by mikel evins

## Introduction

Bard is a small, simple dialect of Lisp with a small but useful set of
built-in datatypes, a novel object system that supports polymorphic
generic functions, and a few simple amenities for concurrent and
distributed programming.

Bard is strongly influenced by its ancestors, Common Lisp, Scheme, and
Dylan. It also takes influence from several other programming
languages, including Smalltalk, Haskell, Lua, Erlang, Clojure, and
others.

This document is a brief overview and basic reference for the language.

## Syntax and built-in classes

Bard is a Lisp, and like other Lisps, is a language made of
expressions that are evaluated to return values. There are two basic
kinds of expressions: atoms and lists.

A list is a sequence of zero or more expressions enclosed in
parentheses, like this:

    (foo bar baz)

An atom is any valid expression that is not a list:

    foo
    1
    5.7
  
...and so on.

As in other dialects of Lisp, a list is treated as a function call
unless you tell the language not to. Thus, this:

    (foo bar baz)

...is a function call, but this:

    '(foo bar baz)

...is not; the quote mark tells Bard not to evaluate that list, but
instead to treat it as literal data.

Bard provides a lexical syntax for almost all of its basic built-in
datatypes. These built-in types are called **classes**.

Here's a list of the basic built-in classes, with example values for each:

**Undefined**

    undefined

The class of unknown or undefined values.

**Null**       

    nothing

The value that represents an empty collection or the absence of a value.

**Boolean**    

    true, false

Values that represent truth and falsehood.

**Number**     

    0, 1.2, 2/3

Various types of numeric values.

**Text**       

    'foo, 'Bar, baz:, "Fred and Barney"

Symbols, keywords, and strings.

**List**

    (), (0 1 2 3 4)

An ordered sequence of values. Bard, like other Lisps, treats a list
as a function call unless you tell it not to. That means that an
expression like

    (0 1 2 3 4)

will cause an error because the zero is not a function. 

One way to tell Bard not to treat a list as a function call is to
quote it:

    '(0 1 2 3 4)

The quote signals that the list is data, not code. Bard constructs the
list but doesn't try to treat it as a function call.

Another way to treat a list as data is to build it with the list
constructor:

    (list 0 1 2 3 4)

The list expression is a function call; the `list` function builds a
list from its arguments and returns it.

Bard provides one more syntax for creating list values and treating
them as data:

    [0 1 2 3 4]

is equivalent to 

    (list 0 1 2 3 4)

**Table**

    {}
    {0 1 2 3}
    {a: 0 b: 1}

A mapping from keys to values. The first item in each table is a key;
the second is a value. The third is a key; the fourth is a
value. Items continue alternating roles. A table must have an even
number of items in order to be well-formed.

Keys can be any value except `nothing` or `undefined`. Values can be
anything except `undefined`.

**Method**

    (^ (x) x)          
    (^ (x y) (+ x y))

A procedure. A callable object that executes some code when applied to
arguments. Methods are the basic tools in Bard for computing results.

The operator `^` can also be spelled `method`:

    (method (x y) (+ x y))

In some cases code is clearer when you use the name `method`; in
others, when you use `^`. The two operators are identical; use
whichever one makes your code easier to read.

The first argument after the `^` or `method` is a list of formal
parameters. Everything after the parameter list is the **body** of the
method. When you apply a method to some arguments, Bard binds the
argument values to the parameters and then evaluates the expressions
in the body one after another. It returns the value (or values) of the
last expression evaluated.

**Function**

    (function  -> List)          
    (function Ratio -> Integer)          
    (function Integer Integer -> Integer)          

A function is a callable object, similar to a method, but unlike a
method it can't compute results on its own. Instead, it relies on
methods to do its computation.

When you call a function, it examines its arguments and chooses a
method based on their types, then applies the method to the arguments
to compute its results. Because a function can choose a different
method for different arguments, a single function can work with a
variety of different types of data.

A function literal, like this one:

    (function Ratio -> Integer)          

describes a function that accepts arguments whose class is Ratio and
returns values whose class is Integer. Didn't I just say that
functions can work on arguments of different types? Yes; classes in
Bard are abstract types that can stand for any number of concrete
types. The type Ratio indicates that this function can accept a value
of any type, as long as it belongs to the Ratio class.

Classes and other types are explained more thoroughly later.

### Specialized classes

Besides the basic built-in classes, Bard has a few others with more
specialized uses. These other classes don't have standard literal
syntaxes defined for them. 

These classes are:

**Actor**
*Not yet implemented*

Objects that represent a running Bard process. A Bard program can create
actors that run in the same process as the creating Bard process, or in a
new process. It can communicate with actors by sending messages to them;
messages can contain arbitrary Bard data--even functions, methods, or
other actors.

**Stream**
*Not yet implemented*

Objects that serve as sources or sinks for data. Objects that belong
to the Stream class include input streams and output streams.

**Class**

Classes are Bard's abstract data types. They define how datatypes are
related to each other, but they don't specify anything about how they
are represented.

**Schema**

Schemas are Bard's concrete datatypes. They define how bits and bytes
are arranged to represent values, but they don't specify anything
about how different types are related to one another.

**Singleton**

Objects that represent data values as types. For example, the value

    (singleton 3)

is equivalent in every way to the integer 3, except that Bard
considers it a type, rather than a value. Bard functions choose the
code to run based on the types of their arguments. Using singletons,
you can make them also choose different methods for different specific
values. For example, you could write a function that runs one method
for the number 3 and a different method for the number 5.

**Type**

The class Type describes all objects that serve the purpose of
datatypes. Instances of Type include classes, singletons, and schemas,
among other possibilities.

## Bard's basic vocabulary

To use a programming language, you need to know its basic
vocabulary--the basic words and phrases you can use to direct
computation. 

This section briefly lists and explains Bard's basic functions and
other operations.

###Control and special forms**

**and**

and [ _form_ ] ...

    bard> (and (odd? 3)(even? 2)(number? "Not a number")(odd? 5))
    false

Evaluates each form from left to right. If any form returns a false
value `and` stops evaluation and returns the false value. Otherwise it
returns the value of the last form.

**begin**

begin [ _form_ ] ...

    (begin (newline)
       (display "Hello")
       (display ",World!")
       (newline)
       (* 2 3))

Evaluates each form from left to right, returning the value of the
last form. `begin` is most often used to run proceudres with
side-effects, such as printing something to the screen. Any values
returned are discarded, except for the last one, which is returned.

**cond**

cond ( _test_ _result1_) ... [(else: _resultN_ )]

    (cond
      ((= (today) 'Monday) "Someone has a case of the Mondays.")
      ((= (today) 'Friday) "Thank God!")
      ((= (today) 'Saturday) "ZZZZZZ....")
      ((= (today) 'Sunday) "Where does the time go?")
      (else: "Isn't it quitting time yet?"))

Evaluates the first test; if it returns a true value then Bard
evaluates the corresponding result. Otherwise, it repeats the process
with the next `(test result)` form. If no test succeeds, `cond`
returns `nothing`. Conventionally, the last test is the keyword
`else:`, which is logically true in Bard, and therefore guarantees
that the last clause is executed if no earlier one was.

**def**

def _variable-name_ _value-expression_

    (def *answer* 42)

Evaluates _value-expression_ and assigns the result to the global
variable _variable-name_, creating the variable if necessary, and
replacing its current value otherwise.

You can also write this expression as 

    (define variable *answer* 42)

Sometimes the shorter version is more convenient, especially when
terseness is helpful; sometimes the longer version is better,
especially when you want variable definitions to stand out
visually. Use whichever form works best in your code; the effects of
both forms are identical.

**define class**

define class _classname_ 

    (define class Person)

Defines a new class named _classname_. `define class` is extremely
simple because Bard classes are extremely simple: they are simply
names for collections of other types.

**define macro**

define macro ( _macroname_ _parameter_ ...) _expression_ ...

    (define macro (switch x y)
      `(let ((z ,y))
         (set! ,y ,x)
         (set! ,x z)))

    bard> (def x 1)
    1

    bard> (def y 2)
    2

    bard> (switch x y)
    2

    bard> x
    2    

    bard> y
    1

Defines a new macro named _macroname_. The macro constructs a Bard
expression and returns it; the returned expression is evaluated in
place of the original macro call. Used cautiously, macros enable a
programmer to conveniently extend the syntax of Bard.

**define method**

define method ( _function-name_ [ _parameter_ ] ...) _expression_ ...

    (define method (add (left <string>) (right <string>))
      (append left right))

Defines a new method and adds it to the function _function-name_. If
no such function exists, then Bard creates one. When the function is
subsequently applied, if its arguments match the types given in
_parameter_... then the new method is called.

If _parameter_ is a symbol then the type of the parameter is
`Anything`, and value will match that argument. If it's a list then it
can be either of the following forms:

    ( _parameter-name_ _type_ )

    ( _parameter-name_ (singleton _value_ ))

The first form causes the argument to match values of type _type_; the
second causes it to match only values equal to _value_.

**define protocol**

define protocol _protocol-name_ [( _function-name1_ [ _class1_ ] ... -> [ _classN_ ] ...)] ...

    (define protocol Rational
      (numerator Ratio -> Integer)
      (denominator Ratio -> Integer))

Defines a new protocol named _protocol-name_, and in the process
defines any new functions that are mentioned in the body of the
expression. Existing function definitions are not replaced; if there's
an existing function with a conflicting signature, Bard issues a
warning and abandons the protocol definition. You can correct
conflicts either by renaming functions, changing their signatures to
match the existing functions, or undefining the existing functions or
protocols.

**define record** _Not yet implemented_

define record _schema-name_ ( _slot-name_ [ _slot-option_ ] ...) ...

    (define record <mailing-address> 
      name street-address city province country postal-code)

    (define record <point> 
      (x default: 0)
      (y default: 0))

Defines a new schema--that is, a concrete type--that consists of a set
of named fields. You can then craeted instances of the new type using
`make`. 

An instance of a record schema implements the Mapping protocol--that
is, it behaves as if it's a table with a fixed set of keys.

**define variable**

define variable _variable-name_ _value-expression_

    (define variable *answer* 42)

Evaluates _value-expression_ and assigns the result to the global
variable _variable-name_, creating the variable if necessary, and
replacing its current value otherwise.

You can also write this expression as 

    (def *answer* 42)

Sometimes the shorter version is more convenient, especially when
terseness is helpful; sometimes the longer version is better,
especially when you want variable definitions to stand out
visually. Use whichever form works best in your code; the effects of
both forms are identical.

**define vector**

define vector _schema-name_ ( _slot-option_ ...) ...

    (define vector <cons> length: 2)

    (define vector <ostype> length: 4 element-type: <char>)

Defines a new schema--that is, a concrete type--that consists of a
ordered sequence of fields accessible by index. You can then create
instances of the new type with `make`.

An instance of a vector schema implements the Listing protocol; that is,
it behaves as if it's a fixed-length list.

**ensure**

ensure _before-expression_ _during-expression_ _after-expression_

    (ensure (set! *occupied* true)
            (perform-risky-operation)
            (set! *occupied* false))

Evaluates _before-expression_, then _during-expression_, then
_after-expression_. Importantly, _after-expression_ is evaluated even
if control leaves _during-expression_ abnormally--for example, if
_during-expression_ signals an error.

Thus, in the example, the variable will be set to false even if
`perform-risky-operation` aborts because of an error.

**function**

function [ _class1_ ] ... -> [ _classN_ ]

    (function Number Number -> List)

Creates and returns a new function. The new function is not bound to a
global variable and has no assocaited methods, so it's not useful in
and of itself. Neverhtless, it can be useful to be able to construct
functions dynamically and add methods to them after they're
constructed.

**generate**

generate ([( _var1_ _val-expression1_ ) ...]) [ _expression1_ ] ... [(yield [ _expressionK_ ] ...)] ... [(resume [ _val-expression_ ...])]

    ;;; fibonacci sequence
    (define variable $fibs
      (generate ((x 1)
                 (y 1))
        (yield x)
        (resume y (+ x y))))

    bard> (take 12 $fibs)
    (1 1 2 3 5 8 13 21 34 55 89 144)

Creates and returns a new generator. A generator is a value that
behaves like a list whose contents are computed as they are
needed. The example computes any number of values from the Fibonacci
sequence, but only as many as are requested.

The first argument after the name `generate` is a list of
bindings. The bindings create local variables with initial values
given by the binding form. In the example, `x` is initially bound to 1
and so is `y`. 

Everything after the bindings is the **body** of the `generate`
form. The expressions in the body are evaluated from left to right, as
in a `begin` form, and the last value is returned.

In the body of a `generate` form there are two special operators:
`yield` and `resume`. If Bard encounters a `yield` form, it
immediately returns from the body, producing the `yield`'s arguments
as results. 

After a `yield`, you can restart a generator by applying the `next`
method to it. When you do, the body of the generator continues from
the point just after the `yield`.

The `resume` method starts the generator over again from the top, but
first it replaces the initial values of the local variables with the
values of its arguments.

**if**

if _test-expression_ _then-expression_ [ _else-expression_ ]

**initialize** _Not yet implemented_

initialize _value_ [ _init-argument1_ ] ...

**let**

let ([( _var_ _val-expression_) ...]) [ _expression_ ] ...

**loop**

loop _loopname_ ([( _var_ _val-expression1_ ) ...]) [ _expression_ ] ... [( _loopname_ _val-expressionN_ ...)] ...

**make** _Not yet implemented_

make _type_ [ _init-argument1_ ] ...

**match** _Not yet implemented_

match ([ _pattern_ _value-expression_ ] ...) [ _expression_ ] ...

**method**

method ([ _parameter_ ] ...) [ _expression_ ] ...

**not**

not _form_

**or**

or [ _form_ ] ...

**quasiquote**

quasiquote _form_

**quote**

quote _form_

**receive**  _Not yet implemented_

receive

**remove-function!** _Not yet implemented_

remove-function! _protocol_ _function-name_

**repeat**

repeat _expression_

**send** _Not yet implemented_

send _actor_ _value_

**set!**

set! _variable-reference_ _value-expression_

    (set! answer 42)

Assigns a new value to the variable given by _variable-reference_. The
variable must be mutable, or an error is signaled.

`set!` can also alter the value in a slot of a table, record, or other
aggregate datatype, if the slot is mutable. The syntax for each case
mirrors the syntax for retrieving a value.

Getting a variable value:
    answer

Setting a variable value:
    (set! answer 42)

Getting a record field:
    (mypoint 'x)

Setting a variable value:
    (set! (mypoint 'x) 42)

Getting a list or vector field:
    (myvec 2)

Setting a variable value:
    (set! (myvec 2) 42)

**time**

time _expression_

**undefine** _Not yet implemented_

undefine _variable-name_

**unless**

unless _test-expression_ [ _then-expression_ ] ...

**when**

when _test-expression_ [ _then-expression_ ] ...

**with-exit**

with-exit ( _var_ ) [ _expression_ ] ...

**with-open-file**

with-open-file ([( _var_ _pathname_) [ _direction_ ] [ _mode_ ]]) [ _expression_ ] ...

### Applying structures _Not yet implemented_

Bard values that consist of named or indexed fields participate in the
Applicable protocol. You can apply a list or a table, for example, as
if it were a function. Applicable structures accept a single argument,
which is treated as a key. If a matching key is found in the
structure, Bard returns the value stored there.

For example, you can obtain element 12 of the list in `$my-list`
like this:

    ($my-list 12)

Similarly, if you have a table of user accounts keyed by username, you
can obtain a particular record like this:

    ($users "frank")

Sometimes it's clearer to write these kinds of operations using an
explicitly-named getter function. For those cases Bard provides the
function `get`. The examples above can be expressed using `get` as
follows:

    (get $my-list 12)
    (get $users "frank")

Since `get` is a polymorphic function, using it also enables you to
write custom accessors for specific types..

### Built-in protocols

Most of Bard's built-in vocabulary is collected in
**protocols**--groups of functions that share a common purpose. This
section lists and explains Bard's built-in protocols and the functions
they provide.

Bard uses a naming convention that protocols are named with present
participles that generally describe their purposes. For example, the
protocol concerned with arithmetic and other mathematical functions is
named `Calculating`, and the protocol that deals with input is called
`Reading`. The built-in protocols break this convention in a few
cases, where following it would be contrived or awkward, and so, for
example, the protocol that is concerned with system operations is
named `System`.

**Reading function signatures**

A **function signature** shows the types of values a function accepts
as input, and the types of values it can be expected to produce as
outputs. Here's a simple example:

    foo Integer -> Symbol

This signature describes a function named `foo` that accepts a single
argument of type `Integer` and returns a single result of type
`Symbol`.

Some functions accept no arguments; this type of function is often
called a `thunk`. Here's an example of a thunk's signature:

    bar -> Integer

The thunk `bar` accepts no arguments, and returns an integer when
called.

A function can also return no values. Functions that take no inputs
and produce no outputs are rare, but if you had one, its signature
would look like this:

    grault ->

Functions can return more than one result. Suppose we had a function
that accepted one argument and returned two results. Its signature
would look something like this:

    wibble Text -> Integer Boolean

A few functions can return any number of results. Here's an example:

    quux Anything -> &

The ampersand symbol (`&`) means that `quux` may return any number of
values. You can also say that a function returns at least on value,
and optionally more:

    quux Anything -> Anything &

The same rules apply to input arguments; signatures can express that a
function accepts any number of arguments, or at least a certain
number, plus optionally more:

    frobbozz & -> Anything
    frobbozz Anything & -> Anything
    frobbozz Anything Integer & -> Anything
    frobbozz Anything Integer Symbol & -> Anything

The sections below, which describe Bard's built-in protocols, use this
signature notation to indicate the inputs and outputs of protocol
functions.

When a function accepts or returns any number of values, but they must
all be of a specific type, we write the signature like this:

    quux [Text]& -> [Integer]&

When a function accepts optional arguments, we write them like this:

    quux [Text] [Integer] -> Anything

When a function accepts more than one optional argument, it isn't
possible to pass a value as the second or greater argument without
also passing one as the first. For example, there's no way to pass the
Integer argument to the `quux` function above unless you also pass the
Text argument.

**Applying**

Applying functions and methods to arguments.

    apply Applicable Anything & => &

    complement Applicable -> Applicable

    constantly Anything -> Anything

    eval Anything -> &

    flip Applicable -> Applicable

    identity Anything -> Anything

    partial Applicable & -> Applicable

**Calculating**

Arithmetic and other math.

    * [Number]& -> Number

    + [Number]& -> Number

    - [Number]& -> Number

    / [Number]& -> Number

    even? Integer -> Boolean

    odd? Integer -> Boolean

    random Integer -> Integer

**Comparing**

Comparing values for equality and equivalence.

    = [Number]& -> Boolean

**Constructing**

Constructing and initializing new values.

    initialize Anything & -> Anything

    make Type & -> Anything

**Listing**

Arranging values into ordered sequences.

    add-first Anything List -> List

    add-last List Anything -> List

    any List -> Anything

    append List & -> List

    by Integer List -> List

    drop Integer List -> List

    element List Integer -> Anything

    empty? List -> Boolean

    filter Applicable List -> List

    first List -> Anything

    join-text Text List -> Text

    last List -> Anything

    length List -> Integer

    list & -> List

    map Applicable [List]& -> List

    next-last List -> Anything

    reduce Applicable Anything List -> Anything

    range Integer Integer -> List

    rest List -> List

    second List -> Anything

    some? Applicable List -> Anything

    split-text Text Character -> List

    take Integer List -> List

    take-by Integer Integer -> List

**Mapping**

Arranging values into sets of key/value pairs.

    contains-key? Table -> Boolean (Not yet implemented)

    contains-value? Table -> Boolean (Not yet implemented)

    get Table Anything -> Anything (Not yet implemented)

    keys Table -> List (Not yet implemented)

    merge [Table]& -> Table (Not yet implemented)

    put Table Anything Anything -> Table (Not yet implemented)

    table & -> Table

    vals Table -> List (Not yet implemented)

**Ordering**

Sorting values into stable orders. 

    < [Orderable]& -> Boolean

    <= [Orderable]& -> Boolean

    > [Orderable]& -> Boolean

    >= [Orderable]& -> Boolean

**Pairing**

Associating one value with another.

    left Pair -> Anything

    pair Anything Anything -> Pair

    right Pair -> Anything

**Reading**

Input of various kinds. 

    current-input -> InputStream

    load Text -> &

    read InputStream -> Anything

    read-file Text -> Text

    read-line InputStream -> Text

    read-lines Text -> List

    read-text Text -> Anything


**System**

Operations on the Bard runtime and its underlying host system.

    error Anything -> Anything

    exit -> 

    gc -> 

    gensym -> Symbol

    quit -> 

    room -> 
    
    uuid -> Symbol

    version -> Text

**Typing**

Distinguishing the types of values from one another.

    applicable? Anything -> Boolean

    boolean? Anything -> Boolean

    character? Anything -> Boolean (Not yet implemented)

    class? Anything -> Boolean (Not yet implemented)

    false? Anything -> Boolean

    float? Anything -> Boolean

    fraction? Anything -> Boolean (Not yet implemented)

    foreign-value? Anything -> Boolean

    function? Anything -> Boolean

    input-stream? Anything -> Boolean

    integer? Anything -> Boolean

    iostream? Anything -> Boolean

    keyword? Anything -> Boolean (Not yet implemented)

    list? Anything -> Boolean

    method? Anything -> Boolean (Not yet implemented)

    nothing? Anything -> Boolean

    number? Anything -> Boolean (Not yet implemented)

    orderable? Anything -> Boolean (Not yet implemented)

    output-stream? Anything -> Boolean

    pair? Anything -> Boolean (Not yet implemented)

    protocol? Anything -> Boolean (Not yet implemented)

    ratio? Anything -> Boolean (Not yet implemented)

    schema? Anything -> Boolean (Not yet implemented)

    singleton Anything -> Singleton

    something? Anything -> Boolean

    stream? Anything -> Boolean (Not yet implemented)

    symbol? Anything -> Boolean

    table? Anything -> Boolean

    text? Anything -> Boolean

    true? Anything -> Boolean

    type? Anything -> Boolean (Not yet implemented)

    undefined? Anything -> Boolean (Not yet implemented)

**Writing**

Output of various kinds. 

    current-output -> OutputStream

    display Anything -> 

    newline -> 

    print Anything -> 

    show Anything -> Text

    write Anything OutputStream -> 


