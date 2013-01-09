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

**Function**

    (function  -> List)          
    (function Ratio -> Integer)          
    (function Integer Integer -> Integer)          

A function is a callable object, similar to a method, but unlike a
method it can't compute results on its own. A function relies on
methods to do its computation.

When you call a function, it examines its arguments and chooses a
method based on their types, then applies that method to the arguments
to compute its results. Because a function can choose a different
method for different arguments, a single function can work with a
variety of different types of data.

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

To use a programming languge, you need to know its basic
vocabulary--the basic words and phrases you can use to direct
computation. 

This section briefly lists and explains Bard's basic functions and
other operations.

**Control and special forms**

    and [_form_] ...

    begin [_form_] ...

    cond (_test_ _result_) ... [(else: _result_)]

    def _variable_ _value-expression_

    define class _classname_ 

    define macro (_macroname_ _parameter_ ...) _expression_ ...

    define method (_methodname_ [_parameter_] ...) _expression_ ...

    define protocol _protocol-name_ [(_function-name_ [_class_] ... -> [_class_] ...)] ...

    define record _schema-name_ (_slot-name_ [_slot-option_] ...) ...

    define variable _variable_ _value-expression_

    define vector _schema-name_ (_slot-option_ ...) ...

    ensure _before-expression_ _during-expression_ _after-expression_

    function [_class_] ... -> [_class_]

    generate ([(_var_ _val-expression_) ...]) [_expression_] ... [(yield [_expression_] ...)] ... [(resume [_val-expression_ ...])]

    if _test-expression_ _then-expression_ [_else-expression_]

    let ([(_var_ _val-expression_) ...]) [_expression_] ...

    loop _loopname_ ([(_var_ _val-expression_) ...]) [_expression_] ... [(_loopname_ _val-expression_ ...)] ...
    
    match ([_pattern_ _value-expression_] ...) [_expression_] ...  _Not yet implemented_

    method ([_parameter_] ...) [_expression_] ...

    not _form_

    or [_form_] ...

    quasiquote _form_

    quote _form_

    receive  _Not yet implemented_

    repeat _expression_

    send  _Not yet implemented_

    set! _variable_ _value-expression_

    time _expression_

    unless _test-expression_ [_then-expression_] ...

    when _test-expression_ [_then-expression_] ...

    with-exit (_var_) [_expression_] ...

    with-open-file ([(_var_ _pathname_) [_direction_] [_mode_]]) [_expression_] ...

**Built-in protocols**

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

The protocol that handles applying functions and methods to arguments.

    apply Applicable Anything & => &

    complement Applicable -> Applicable

    constantly Anything -> Anything

    eval Anything -> &

    flip Applicable -> Applicable

    identity Anything -> Anything

    partial Applicable & -> Applicable

**Calculating**

The protocol that handles arithmetic and other mathematical functions.

    * [Number]& -> Number

    + [Number]& -> Number

    - [Number]& -> Number

    / [Number]& -> Number

    even? Integer -> Boolean

    odd? Integer -> Boolean

    random Integer -> Integer

**Comparing**

The protocol that handles comparing values for equality and
equivalence.

    = [Number]& -> Boolean

**Constructing**

The protocol that handles constructing and initializing new values.

    initialize Anything & -> Anything

    make Type & -> Anything

**Listing**

The protocol that handles arranging values into ordered sequences.

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

The protocol that handles arranging values into sets of key/value
pairs.

    contains-key? Table -> Boolean [*Not yet implemented*]

    contains-value? Table -> Boolean [*Not yet implemented*]

    get Table Anything -> Anything [*Not yet implemented*]

    keys Table -> List [*Not yet implemented*]

    merge [Table]& -> Table [*Not yet implemented*]

    put Table Anything Anything -> Table [*Not yet implemented*]

    table & -> Table

    vals Table -> List [*Not yet implemented*]

**Ordering**

The protocol that handles sorting values into stable orders. 

    < [Orderable]& -> Boolean

    <= [Orderable]& -> Boolean

    > [Orderable]& -> Boolean

    >= [Orderable]& -> Boolean

**Pairing**

The protocol that handles associating one value with another.

    left Pair -> Anything

    pair Anything Anything -> Pair

    right Pair -> Anything

**Reading**

The protocol that handles input of various kinds. 

    current-input -> InputStream

    load Text -> &

    read InputStream -> Anything

    read-file Text -> Text

    read-line InputStream -> Text

    read-lines Text -> List

    read-text Text -> Anything


**System**

The protocol that handles operations on the Bard runtime and its
underlying host system.

    error Anything -> Anything

    exit -> 

    gc -> 

    gensym -> Symbol

    quit -> 

    room -> 

    uuid -> Symbol

    version -> Text

**Typing**

The protocol that handles distinguishing the types of values from one
another.

    applicable? Anything -> Boolean

    boolean? Anything -> Boolean

    character? Anything -> Boolean [*Not yet implemented*]

    class? Anything -> Boolean [*Not yet implemented*]

    false? Anything -> Boolean

    float? Anything -> Boolean

    fraction? Anything -> Boolean [*Not yet implemented*]

    foreign-value? Anything -> Boolean

    function? Anything -> Boolean

    input-stream? Anything -> Boolean

    integer? Anything -> Boolean

    iostream? Anything -> Boolean

    keyword? Anything -> Boolean [*Not yet implemented*]

    list? Anything -> Boolean

    method? Anything -> Boolean [*Not yet implemented*]

    nothing? Anything -> Boolean

    number? Anything -> Boolean [*Not yet implemented*]

    orderable? Anything -> Boolean [*Not yet implemented*]

    output-stream? Anything -> Boolean

    pair? Anything -> Boolean [*Not yet implemented*]

    protocol? Anything -> Boolean [*Not yet implemented*]

    ratio? Anything -> Boolean [*Not yet implemented*]

    schema? Anything -> Boolean [*Not yet implemented*]

    singleton Anything -> Singleton

    something? Anything -> Boolean

    stream? Anything -> Boolean [*Not yet implemented*]

    symbol? Anything -> Boolean

    table? Anything -> Boolean

    text? Anything -> Boolean

    true? Anything -> Boolean

    type? Anything -> Boolean [*Not yet implemented*]

    undefined? Anything -> Boolean [*Not yet implemented*]

**Writing**

The protocol that handles output of various kinds. 

    current-output -> OutputStream

    display Anything -> 

    newline -> 

    print Anything -> 

    show Anything -> Text

    write Anything OutputStream -> 


## Types, Procedures, and Protocols

There are two kinds of types in Bard: schemas and classes.

There are also two kinds of procedures: functions and methods.

Finally, there are protocols to connect types and procedures and give
each of them meaning.

### Schemas and Classes

Schemas are concrete types that describe the bits and bytes used to
represent values. Classes are abstract types that refer to collections
of values. A Class may refer to several different schemas. For
example, <fixnum> and <bignum> are two different schemas that
represent integers. The details of their memory layout differ, and
they have different performance characteristics. The class Integer
refers to values belonging to both schemas.

Why have two kinds of types? Because the representation of data is a
different concern from the role it plays in procedures. Schemas
describe representation; classes identify roles. 

By separating the concerns, we can express one without making
committments about the other. For example, we can describe a set of
named fields or a tuple of values without restricting its role in
procedures.

A tuple of two values, for instance, can be an element of an
associative array, or the building block of a linked list, or value
and an associated label. The same schema can be used for all purposes
without violating the spirit of the definition, because the schema
doesn't describe a role.

Separating representation from role also simplifies the roles. When we
define a class like Name or List, we identify roles we expect values
to fill, but we don't limit the representations that can fill
them. Any schema can be a member of a class, as long as the necessary
functions are defined. 

A List might be a chain of pairs--a singly-linked list like those
common in Lisp dialects. One the other hand, it might be a contiguous
sequence of memory locations, like an array in C. Either
representation has advantages, depending on the intended use of the
data. Both support the same operations; the important difference is
that one is more efficient for some operations and the other is more
efficient for others.

A Bard List might be represented by either one. To put it another way,
you can use any suitable schema as a List, so long as the needed
functions are defined for that schema. You can also invent your own
schema, define the functions specified by the List protocol, and your
schema is a List.

### Methods and Functions

A method is a procedure that accepts values as inputs and computes
outputs. 

A function is a procedure that accepts values as inputs, examines
their types, and on that basis selects a method to apply to
them. 

Functions are polymorphic; that is, they can do different things for
inputs of different types. Methods are monomorphic; that is, a method
always runs the same code regardless of the types of its inputs.

There are two kinds of procedures because there are two parts of the
job of making polymorphic procedures work: first, we must chooses the
right code for the values we get, and second we must run that
code. The function's job is to examine the inputs and determine which
method matches them; the method's job is to accept the inputs and
compute a result.

Polymorphic functions simplify working with data by making it
convenient to use the same function to compute the same kinds of
results with inputs using different representations. As a simple and
obvious example, consider adding two numbers together.

The conventional name for that operation is "+". What should the types
of the arguments to the "+" operation be? There are many different
representations for numbers. There are simple integers represented
directly by machine words; there are integers with unbounded precision
represented by variable-sized arrays of machine words; there are
decimal numbers of various kinds and precisions; there are ratios,
complex numbers, and so on. 

Each kind of number has a different representation; depending on the
platform, some may have several.

The "+" operation is supposed to work on all of them. If procedures
are not polymorphic then we have a choice between coming up with a
distinct flavor of "+" for each possible combination of different
representations, or defining a grand comprehensive "+" that implements
addition for all of the possible combinations.

Taking the first option yields an explosion of different versions of
"+". The second requires us to rewrite "+" every time someone wants to
support a new representation for numbers.

That's the problem that polymorphic functions solve. A polymorphic
function is an extensible function that can choose the right code for
the representations it receives. With polymorphism, there's just one
"+" function. When you feed numbers to it, the function chooses an
implenentation of addition appropriate to the representations it
receives. Because it's extensible, you don't have to rewrite it in
order to support a new representation; you just have to write the
methods for the new kinds of numbers.

### Protocols

On the one hand we have types--that is, classes and schemas. On the
other hand, we have procedures--that is, functions and methods. The
connection between them is protocols.

A protocol is a collection of functions defined over classes. For
example, here's a very simple protocol called Rational:

    (protocol Rational
      (numerator Ratio -> Integer)
      (denominator Ratio -> Integer))

In this definition, Rational is a protocol; Ratio and Integer are
classes; numerator and denominator are functions. The protocol
establishes a relationship between the classes and the
functions. Rational establishes that numerator and denominator accept
Ratio values as inputs and produce Integer values as outputs.

What schemas belong to Ratio and Integer? To put it another way, what
concrete values can we actually pass to numerator and denominator? 

The protocol doesn't say. It also doesn't say how numerator and
denominator work. A protocol just defines the relationships between
the classes and functions.

To define the schemas that belong to the classes, we have to define
some methods for those functions--methods that work on some specific
schemas. Here's an example:

    (define method (numerator (r <ratnum>))
      (ratnum:num r))

    (define method (denominator (r <ratnum>))
      (ratnum:denom r))

These expressions define implementations of numerator and denominator
where the input value is an instance of the schema `<ratnum>`. That
means that `<ratnum>` is a member of `Ratio`.


