# bard 0.4 structures

A structure is a concrete representation of a datatype.

A structure is one of three kinds:

- an **atom** is a primitive datatype without internal structure, such as a character or an integer.
- a **record** is a set of named fields.
- a **tuple** is a set of fields accessed by index.

Atoms are built into the bard implementation. You cannot define new atom structures from within bard.

You can define new records using the `define record` special form.

You can define new tuples using the `define tuple` special form.

## Defining records

    (define record point x y)

The above example defines a new record structure named `point`, with fields named `x` and `y`. It also defines several procedures:

    point
    point?
    point.x
    point.y
    (setter point.x)
    (setter point.y)

**point**, the object that represents the new `point` datatype, is also a type of procedure called a **constructor**. It constructs an instance of the `point` datatype. An example call to `point` looks like this:
    (point :x 100 :y 200)

If either the `:x` or `:y` argument is missing then, unless `point` was defined with default values for those arguments, an error is signaled.

**point?** is a **type predicate**. It returns true if its argument is an instance of `point`.

**point.x** and **point.y** are **readers**. They return the values stored in the named fields of a `point` instance.

**(setter point.x)** and **(setter point.y)** are **setters**--that is, they are procedures that update the named fields of a `point` instance.

All these procedures are automatically created when you define the `point` structure. Indeed, that's what it means to define the `point` record-structure: to define this set of procedures.

## Defining tuples

    (define tuple ostype :count 4)

The above example defines a new tuple structure named `ostype`. Instances of `ostype` have four elements, indexed from zero through three. Like defining a record structure, defining a tuple structure automatically defines a set of associated procedures:

    ostype
    ostype?
    ostype.ref
    (setter ostype.ref)

The procedures that make up a tuple structure are analogous to those that make up a record structure. `ostype` is a constructor; `ostype?` is a type predicate; `ostype.ref` is a reader; `(setter ostype.ref)` is a setter.

Where a reader procedure for a record refers to a field of the record by name, a reader for a tuple instead uses *tuple-name*.ref to refer to a field by index. For instance, to return the first element of an `ostype` instance in the variable `foo`, you would use an expression like this:

    (ostype.ref foo 0)

## Type queries

Bard defines a function named `structure-of` that returns the structure of an arbitrary value. Using our example structures `point` and `ostype`, we can use `structure-of` like this:

    bard> (structure-of (point :x 0 :y 0))
    #<record-structure point aa16f0>

    bard> (structure-of (ostype :contents #\a #\b #\c #\d))
    #<tuple-structure ostype b022a0>

## `set!`

The macro `set!` expands into an expression that calls a setter appropriate to its arguments. When its arguments are structures, `set!` expands into the proper `setter` expressions to update their fields.

Below are examples of using `set!` expressions to update a `point` instance and an `ostype` instance:

    (set! (point.x p) 1001)
    (set! (ostype.ref o 0) #\x)


## Accessors

The procedures created by a structure definition are monomorphic procedures similar to methods--in fact, they may be implemented as methods, though that isn't necessarily the case. Conceptually they are a distinct kind of procedure called an **accessor**. A structure is defined by the behavior of its accessors.
