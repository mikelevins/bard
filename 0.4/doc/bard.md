# Bard
version 0.4
by mikel evins

## Basic ideas

Bard is a langauge and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases wthin phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is a collection of expressions that, when evaluated, produces some values.

A **function** is a value that you can **apply** to other values to compute results.

A **method** is a particular way for a function to compute results for certain values. A function can use different methods for different values.

**Applying** a function means passing some values for it to use as parameters. When it's properly applied, a function chooses a method to compute a result, and applies that method to the parameter values. The method then computes a result.

A **type** is a specific way to represent data. Each type can represent some values, but not others. Every value has a type.

A **class** is a collection of types with a common **protocol**.

A **protocol** is a defined set of related functions.

## Built-in classes

Bard provides several built-in classes, and a way to write values that belong to each class.

* **Anything** is a special class that includes all values of all types.
* **Number** includes all the types that represent numeric values.
* **Enumeration** includes all **enumerated types**. An **enumerated type** is a type whose values are individually and explicitly specified values. An example of an enumerated type is `<boolean>`. The values that belong to `<boolean>` are `true` and `false`.
* **List** includes types that represent ordered sequences of values.
* **Map** includes types that represent collections of **keys** that are associated with values. A key is a value that is used to identify another value contained in a map. For example, in the map `{a: 1 b: 2}`, the key `a:` identifies the value `1`.
* **Producer** includes types that produce one value after another when asked.
* **Consumer** includes types that consume one value after another when offered. Values belonging to either Producer or Consumer are called **streams**.
* **Procedure** includes types that can be applied to arguments to compute result values.
* **Resource** includes types that represent files, devices, network connections, and other abstractions of hardware capabilities.

There's a way of writing literal values of each of the built-in classes. When Bard prints a value of any built-in class, it prints it in a way that it can be read back in by a Bard process to produce a value that is equivalent to the one that was printed. For example, if an evaluation produces a value equal to 5, Bard prints `5`. Bard can read that text to construct a value equal to 5.

There are a few exceptions to this rule. Some values--such as streams, for example, cannot be printed in a way that Bard can read them and construct an equivalent value. In cases like this one, Bard instead prints an expression that can be used to construct an approximate equivalent, to the extent that's possible.

Each built-in class is a collection of one or more related types.

### Numbers

Here are examples of Number values, listed with types that can represent them. 


| values | types |
| --- | ------------------------- |
| `0` | `<bit>` `<u8>` `<fixed>` |
| `1.2` | `<float>` |
| `2/3` | `<ratio>` |
| `999999999999999999999999` | `<bignum>` |


### Enumerations

Here are examples of Enumeration values, listed with types that can represent them. 

| values | types |
| --- | ------------------------- |
| `true` | `<boolean>` |
| `\A` `\space` | `<character>` |

### Lists

Here are examples of List values, listed with types that can represent them. 

| values | types |
| --- | ------------------------- |
| `nothing` | `<null>` |
| `(1 . 2)` | `<pair>` |
| `(1 0 1 0)` | `<pair>` `<vector>` `<bitvector>` |
| `(1 2 3 4)` | `<pair>` `<vector>` `<u8vector>` |
| `(1 -2 3 -4)` | `<pair>` `<vector>` `<s8vector>` |
| `(1 -2 3 -4000)` | `<pair>` `<vector>` `<s16vector>` |
| `"Hello"` | `<string>` |

Bard is a Lisp and, like other Lisps, normally treats lists as function calls. So, for example, when you write this list:

    (+ 2 3)
    
Bard treats it as a function call that applies the function `+` to the arguments `2` and `3`. 

If you want to write the same list and use it as a piece of data, rather than evaluating it for a result, there are two ways to do it. One is to quote it:

    '(+ 2 3)
    
The other is to use a List literal:

    [+ 2 3]
    
A List literal is almost, but not quite, the same as quoting the list. The difference is that

    '(+ 2 3)
    
means that neither the list nor its contents are to be evaluated. Writing that expression at the Bard prompt returns it exactly as you wrote it:

    bard> '(+ 2 3)
    (+ 2 3)
    
A List literal instead evaluates the contents of the list, but it doesn't treat the list as a function call. Since `+` is the name of a function, writing the List literal produces this result:

    bard> [+ 2 3]
    ((-> & -> Number) 2 3)

Bard constructs a list of three elements: the function, followed by 2 and 3.

### Maps

Here are examples of List values, listed with types that can represent them. 

| values | types |
| --- | ------------------------- |
| `{}` | `<null>` |
| `{a: 1 b: 2}` | `<pair>` `<ordered-map>` `<weight-balanced-treemap>` |

Maps serve as a kind of lingua franca for structured data in Bard. For example, the default way of writing a literal value of an arbitrary type is to write the '#' character, followed by the type of the value, followed by a map that describes the value in enough detail to uniquely identify it. For example, a reference to a file might be written this way:

    #<file>{url: "file:///tmp/foo.txt"}
    
Even more basic values can be written the same way, though they usually aren't. For example, the intereger 5 could be written like this:

    #<fixnum>{value: 5}

This way of writing values can come in handy when there are a variety of ways to represent a particular value and you want to specify exactly one representation. Here's an example:

    #<u8vector>{elements: [\F \A \C \E]}

### Streams

Here are examples of stream values, listed with types that can represent them. Streams are special in that the printed representation of a stream cannot necessarily faithfully represent its state.

| values | types |
| --- | ------------------------- |
| `#<producer>{id: 0 element-type: <u8> direction: input}` | `<producer>` |
| `#<consumer>{id: 1 element-type: <u8> direction: output}` | `<consumer>` |

### Procedures

Here are examples of Procedure values, listed with types that can represent them. 

| values | types |
| --- | ------------------------- |
| `(-> ->)` | `<function>` |
| `(-> Number Number -> Number)` | `<function>` |
| `(^ (x) x)` | `<method>` |
| `(^ (x y) (* x y))` | `<method>` |

### Resources

Here are examples of Resource values, listed with types that can represent them. 

| values | types |
| --- | ------------------------- |
| `#<file>{id: 1501 url: "file:///tmp/foo.txt"}` | `<file>` |
| `#<http>{id: 11831 url: "http://bardcode.net"}` | `<http>` |
| `#<actor>{id: 32042 url: "http://bardcode.net/bardrepl"}` | `<actor>` |

