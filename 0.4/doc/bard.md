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

## Evaluating expressions

If I type an expression at the Bard prompt, Bard reads it and evaluates it to return a result. Most values that you can type at the prompt are **self-evaluating**. When Bard evaluates a **self-evaluating** value, the result is just the same value.

Here are a couple of examples:

    bard> 5
    5
    
    bard> "Hello!"
    "Hello!"
    
### Symbols

A symbol is a value that just consists of a name. Here are a few examples:

    x
    *
    Fred
    ALongerSymbolName
    
When you type a symbol at the Bard prompt, Bard treats it as a reference to a **variable**. A **variable** is a name that is bound to a value.

    bard> *
    (-> & -> Number)
    
When we type `*` at the prompt, Bard recognizes it as a symbol and looks up the value of the variable with that name. In this case the value is the multiplication function, written as `(-> & -> Number)`. (We'll talk later about why the function is printed like that and what it means.)

You can define your own variables and change the values associated with them.

    bard> (def n 5)
    n
    
    bard> n
    5
    
    bard> (set! n 105)
    105
    
    bard> n
    105
    
### Lists

Lists are special. Like other Lisps, Bard treats lists as procedure calls. Here's what I mean:

    bard> (+ 2 3)
    5
    
When evaluating a list, Bard treats it as a procedure call, meaning that it uses the first item in the list as a procedure and passes the remaining items to that procedure as arguments. That's what happened in the above example: Bard looked up `+` and found the addition function; it passed `2` and `3` as arguments to that function, and the function computed the result `5`.

### Quoting

What if you want to use a symbol or a list as a piece of data? In other words, what if I want to make a list of the symbols `x`, `y`, and `z`? If I just write a list containing them, I'll run into a problem:

    bard> (x y z)

    ERROR: Undefined variable: x 

As before, Bard sees a list and assumes it's a procedure call. It looks up `x` to find the procedure, and notices that there's no variable named `x` defined, so it can't proceed.

Even if it could, `x` would have to have a value that is a procedure, and that still wouldn't get the result we want: it still wouldn't return a list of the symbols `x`, `y`, and `z`. So how do we do that?

That's what **quoting** is for. When you quote a value, you're telling Bard not to evaluate it.

    bard> 'x
    x
    
Typing `'x` at the prompt tells Bard to returnthe symbol `x` itself, instead of looking up a variable named `x`.

We can do the same thing with a list:

    bard> '(x y z)
    (x y z)
    
Quoting most values has no effect at all.

    bard> '5
    5
    
    bard> 'true
    true
    
    bard> '"Hello1"
    "Hello!"
    
That's because most values are self-evaluating, so evaluating them returns the same result as not evaluating them. Only symbols and lists are affected by quoting.

## Types

**Types** are objects that describe how to represent data. As an example, `<fixnum>` is a type that describes how to represent integers in Bard.

Every value has a type, and you can ask Bard for the type of any value.

    bard> (type 5)
    <fixnum>
    
    bard (type 999999999999999999999999999999999999999999999999)
    <bignum>
    
    bard> (type \space)
    <character>
    
    bard> (type true)
    <boolean>
    
Bard provides many built-in types, and it also provides tools for constructing new ones.

Bard types are **disjoint**. That is, a value is an instance of exactly one type; if it's an instance of one type, then it's not an instance of another.

## Classes

**Classes** are collections of related types. A class comprises a list of types and a **protocol** that they share. A **protocol** is a list of functions.

Thus, for example, the `List` class has numerous member types, including `<pair>`, `<string>`, `<vector>`, and others. It also has an associated `List` protocol that includes such functions as `list`, `list?`, `first`, `rest`, `last`, and so on.

Just as you can define your own types, you can also define your own classes. When you define a class, its associated protocol is automatically created along with it.

New types may be added to existing classes. If you add a type to a class, Bard warns you that the functions belonging to the protocol don't have definitions for the new type. You can remedy that issue by defining new methods on the protocol's functions.

In fact, defining such methods is all it takes to add a type to a class. You can skip the step of explicitly adding the type to the class, and just write methods for it. As soon as at least one protocol function has a method for the type, it's considered a member of the class.

## Relationships between classes

Bard classes have no subclasses; they have only members, and only types are allowed to be members. Types have no subtypes.

Bard has one **superclass**, named `Anything`. All classes are members of `Anything`. 

Bard also has one **subtype**, named `<null>`. `<null>` is a member of every class. It has a single unique value, `nothing`. `nothing` is an insstance of every type.

