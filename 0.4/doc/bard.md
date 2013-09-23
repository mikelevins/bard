# Bard
version 0.4
by mikel evins

## Basic ideas

Bard is a language and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases within phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is a collection of expressions that, when evaluated, produces some values.

A **function** is a value that you can **apply** to other values to compute results.

A **method** is a particular way for a function to compute results for certain values. A function can use different methods for different values.

**Applying** a function means passing some values for it to use as parameters. When it's applied, a function chooses a method to compute a result, and applies that method to the parameter values. The method then computes a result.

A **type** is a specific way to represent data. Each type can represent some values, but not others. Every value has a type.

A **class** is a collection of types with a common **protocol**. A **protocol** is a set of related functions. Creating a class also creates its protocol.

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

Each built-in class is a collection of one or more related types.

Bard provides tools for creating new types and classes.

## Literal values

There's a way of writing literal values of each of the built-in classes. When Bard prints a value of any built-in class, it prints it in a way that it can be read back in by a Bard process to produce a value that is equivalent to the one that was printed. For example, if an evaluation produces a value equal to 5, Bard prints `5`. Bard can read that text to construct a value equal to 5.

There are a few exceptions to this rule. Some values--such as streams, for example, cannot be printed in a way that Bard can read them and construct an equivalent value. In cases like this one, Bard instead prints an expression that can be used to construct an approximate equivalent, to the extent that's possible.

The following sections list examples of literal values of each of the built-in classes. Each example value is listed with one or more types that can represent it.

It's worth keeping in mind that a type can belong to more than one class. 

### Numbers

| values | types |
| --- | ------------------------- |
| `0` | `<bit>` `<u8>` `<fixed>` |
| `1.2` | `<float>` |
| `2/3` | `<ratio>` |
| `999999999999999999999999` | `<bignum>` |

### Enumerations

| values | types |
| --- | ------------------------- |
| `true` | `<boolean>` |
| `\A` | `<character>` |
| `\space` | `<character>` |

### Lists

| values | types |
| --- | ------------------------- |
| `nothing` | `<null>` |
| `(1 . 2)` | `<pair>` |
| `(1 0 1 0)` | `<pair>` `<vector>` `<bitvector>` |
| `(1 2 3 4)` | `<pair>` `<vector>` `<u8vector>` |
| `(1 -2 3 -4)` | `<pair>` `<vector>` `<s8vector>` |
| `(1 -2 3 -4000)` | `<pair>` `<vector>` `<s16vector>` |
| `"Hello"` | `<string>` |

### Maps

| values | types |
| --- | ------------------------- |
| `{}` | `<null>` |
| `{a: 1 b: 2}` | `<pair>` `<ordered-map>` `<weight-balanced-treemap>` |

### Streams

| values | types |
| --- | ------------------------- |
| `#<producer>{id: 0 element-type: <u8>}` | `<producer>` |
| `#<consumer>{id: 1 element-type: <u8>}` | `<consumer>` |
| `#<iostream>{id: 1208 element-type: <u8> direction: io}` | `<iostream>` |

### Procedures

| values | types |
| --- | ------------------------- |
| `(-> ->)` | `<function>` |
| `(-> Number Number -> Number)` | `<function>` |
| `(^ (x) x)` | `<method>` |
| `(^ (x y) (* x y))` | `<method>` |

### Resources

| values | types |
| --- | ------------------------- |
| `#<file>{id: 1501 url: "file:///tmp/foo.txt"}` | `<file>` |
| `#<http>{id: 11831 url: "http://bardcode.net"}` | `<http>` |
| `#<actor>{id: 32042 url: "http://bardcode.net/bardrepl"}` | `<actor>` |

