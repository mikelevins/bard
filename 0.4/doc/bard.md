# Bard
version 0.4
by mikel evins

## Introduction

Bard is a small, simple, general-purpose Lisp dialect with a few novel features. It's designed to be easy to implement, easy to port, and efficient enough to use for a variety of purposes.

Like other Lisps, Bard is an expression-oriented language: a Bard program consists of a sequence of expressions, each of which denotes a value. Running the program means feeding the expressions to an **evaluator** that reduces each expression to a **result value**. (Expressions may actually evaluate to any number of values--even none--but it's most common for them to evaluate to exactly one.) 

Also like other Lisps, Bard allows **side effects**, which means that evaluating an expression may cause things to happen besides production of the result value--things like printing messages, drawing pictures, or opening network connections.

Bard generally resembles other Lisp dialects, but also has a few unique features of its own. Its type system is distinctive, with some similarities to the Common Lisp Object System and some things in common with Haskell's type classes. It uses the familiar parenthesized s-expression syntax common to most Lisps, but adds a small number of extra lexical conventions to make it easier to write some values as literal constants, and to provide concise ways to control the reader and evaluator.

Bard is implemented as a small, simple compiler and a small, simple virtual machine. That implementation choice makes it easy to port to new platforms as-needed. The virtual machine implements only a very small kernel language; the rest of Bard is implemented in Bard, and provided by compiled code that is stored in an image file that the VM loads when it starts. Again, this design eases porting and customization, and also provides some other conveniences.

Compiled Bard code is hardware-independent and can run unchanged on the Bard VM running on any supported hardware platform. Bard functions can be serialized and passed across a network connection to a remote Bard process for execution, and can run unchanged on the destination even if it's running on a different CPU and operating system.

Bard's primary design goal is to make me happy and productive in my work. It has succeeded in this goal so far. I hope it makes someone else happy and productive, too.

## Syntax and Expressions

### Values

### Variables and constants

### Procedure calls

### Constructors, getters and setters

## Types

Bard types are made of three parts: **structures**, **protocols**, and **classes**. Every Bard type is an abstraction that consists of at least one protocol, zero or more classes, and zero or more structures. More commonly, a type is defined as a protocol plus several classes and several structures.

A **protocol** is a set of related functions that define the behavior of one or more related types. For example, there is a `Number` protocol that defines a set of operations on instances of `Number`, and there is a `List` protocol that defines a set of operations on instances of `List`. In most cases, a protocol's functions operate on and produce several related classes.

A **class** is the name of an abstract type. `Number` and `List` are classes. It's important to understand that a Bard class is really *just* a name. A Bard class has no internal structure--just the name. That name can appear in function definitions, and it can be used to assert that one type is a subtype of another--for example, `Integer` is a subclass of `Number`, but that's all you can do with classes.

A **structure** is a concrete description of how to represent some data. That description includes a definition of the **fields** that make up an instance of the structure, a **constructor** that can be used to make a new instance, and **accessors** that can be used to get and set the values of the fields. Once again, *that's all*. Structures have no additional behavior, and they have no defined relationship to other structures or types. There no member functions in structures, and no inheritance between them.

### Defining protocols and classes

So how does a value become an instance of a type?

The answer is that defining **methods** on functions turns values into instances of types.

Before we can define methods, we need some functions, and in order to define functions, we need some classes for them to operate on. Let's begin by defining the classes `Ratio` and `Integer`, pretending for the moment that they don't already exist:

    (define class Integer Number)
    (define class Ratio Number)
    
That's all there is to a class definition in Bard: you say that a name is the name of a class and, optionally, say that it belongs to one or more superclasses. In this case we said that `Integer` and `Ratio` are both subclasses of `Number`.

Now let's define a protocol that uses those classes:    

    (define protocol Rational
      numerator (-> Ratio -> Integer)
      denominator (-> Ratio -> Integer))
        
This definition says that there exists a protocol named `Rational` that comprises two functions, named `numerator` and `denominator`. Each of those functions accepts a single argument of type `Ratio`, and returns a single argument of type `Integer`.

The protocol definition does not say anything about how to actually compute the result of `numerator` or `denominator`, and it doesn't say anything about what values might be instances of `Ratio` or `Integer`.

### Making values into instances of types

To define how to compute `numerator`, we define a method on the function `numerator`:

    (define method (numerator r)
      with: {r <fixnum>}
      r)

`<fixnum>` is a structure that is built into Bard. It's one of several representations of an integer. The definition above says that when the argument to `numerator` is a `<fixnum>`, `numerator` simply returns that value. That defines a way to compute the value of `numerator`. 

That's not all, though; this definition also implicitly asserts that instances of `<fixnum>` are instances of `Ratio` and of `Integer`, because the argument to `numerator` has to be of type `Ratio`, and the return value of `numerator` has to be of type `Integer`.

That means that the structure `<fixnum>` is a now a member of the classes `Ratio` and `Integer`.

We can define `denominator` similarly:

    (define method (denominator r)
      with: {r <fixnum>}
      1)

There's a subtle point about the return value of `denominator`: this definition returns the literal value `1`. What representation of `1` do we mean? There are various possible concrete types that can be used to represent `1`, and we didn't say which one to use.

When we write a literal value, Bard will choose a representation for it. It can't assume that representation is the return type of the function, though; that will almost certainly be too restrictive, and very likely not what we meant at all. Bard will therefore identify the return type of the function as the most restrictive type that is still inclusive enough to avoid ruling out any type that could reasonably represent the value we gave.

But why don't we just eliminate the guesswork? We can specify exactly which representation we mean:

    (define method (denominator r)
      with: {r <fixnum>}
      #<fixnum> 1)

The expression `#<fixnum>` is a **value constraint**. It means we expect the Bard reader to construct the value of the next expression respecting restrictions that we specify. In this case, we specified a type of `<fixnum>`, which means Bard must use a `<fixnum>` to represent `1`. Bard can also use the value constraint to deduce that the return type of the function is `<fixnum>`, since we've now specified exactly what representation we mean.

There was no need for the value constraint in the definition of `numerator` because Bard already knew the type of `r`; we said what it was in the `with:` clause.



### Built-in Types

## Procedures

**Procedures** are values that represent executable code. A procedure is a value that can be **applied** to some list of **arguments** to compute a **result**. The result may consist of zero or more computed **values**, and it may also involve causing some **side effects**, such as printing output, drawing images on a screen, or opening a network connection.

Bard has four kinds of procedures:

* **special forms**

  **Special forms** are procedures that are built into the Bard compiler and virtual machine. They define the core of the Bard language. It's not possible to define new special forms from within Bard, nor to modify the ones that are built-in. Bard defines around two dozen special forms.

* **macros**

  **Macros** are procedures that rewrite expressions before they are compiled and evaluated. Bard defines several standard macros, and provides the `define macro` form to enable you to define new ones. Macros are best used to define new syntax that simplifies common patterns of code and makes them clearer.

* **functions**

  **Functions** are procedures that accept arguments and compute result values. Bard functions are **polymorphic**; that is, the exact code that a function executes may depend on the arguments passed to it. In fact, a function doesn't actually compute its results; it merely examines its arguments and selects a suitable **method** that is then used to compute the results.

* **methods** 

  **Methods** are procedures that accept arguments and compute result values. Methods do the actual work of computing results of functions. A function examines its arguments and chooses a matching method; the method then computes the function's results.

### Defining Procedures

### Built-in Procedures

#### Special Forms

##### `begin`
<code>(begin <i>[expr]*</i>)</code>

##### `cond`
<code>(cond <i>[</i> (<i>test</i> <i>[</i><i>expr</i><i>]&#42;</i>) <i>]&#42;</i>)</code>

##### `ensure`
<code>(ensure <i>body epilog</i>)</code>

##### `function` 
<code>(-> <i>[input-type]&#42;</i> -> <i>[output-type]&#42;</i>)</code><br>
<code>(function <i>[input-type]&#42;</i> -> <i>[output-type]&#42;</i>)</code>

##### `if`
<code>(if <i>test-expr then-expr else-expr</i>)</code>

##### `let` 
<code>(let ( <i>[</i>(<i>[name]&#42; vals-expr</i>)<i>]&#42;</i> ) <i>[expr]&#42;</i>)</code>

##### `loop` 
<code>(loop <i>loop-name</i> ( <i>[</i>(<i>[name]&#42; vals-expr</i>)<i>]&#42;</i> ) <i>[expr]&#42;</i>)</code>

##### `match` 
<code>(match ( <i>[</i>(<i>[pattern]&#42; vals-expr</i>)<i>]&#42;</i> ) <i>[expr]&#42;</i>)</code>

##### `method` 
<code>(^ ( <i>[param]&#42;</i> ) <i>[expr]&#42;</i>)</code><br>
<code>(method ( <i>[param]&#42;</i> ) <i>[expr]&#42;</i>)</code>

##### `protocol` 
<code>(protocol <i>[name function]*</i>)</code><br>

##### `quasiquote` 
<code>`<i>expr</i></code><br>
<code>(quasiquote <i>expr</i>)</code>

##### `quote`
<code>'<i>expr</i></code><br>
<code>(quote <i>expr</i>)</code>

##### `receive` 
<code>(receive)</code><br>
<code>(receive <i>pattern</i>)</code>

##### `repeat`
<code>(repeat <i>[expr]*</i>)</code>

##### `send`
<code>(send <i>dest expr</i>)</code>

##### `setter` 
<code>(setter <i>place</i>)</code>

##### `time`
<code>(time <i>[expr]*</i>)</code>

##### `undefine` 
<code>(undefine <i>name</i>)</code>

##### `unless`
<code>(unless <i>test-expr</i> <i>[expr]*</i>)</code>

##### `values`
<code>(values <i>[expr]*</i>)</code>

##### `when`
<code>(when <i>test-expr</i> <i>[expr]*</i>)</code>

##### `with-exit` 
<code>(with-exit (<i>name</i>) <i>[expr]*</i>)</code>

##### `with-open`
<code>(with-open (<i>name [param]&#42;</i>) <i>[expr]*</i>)</code>


#### Definitions

##### `define class`
<code>(define class <i>name [superclass]*</i>)</code>

##### `define constant`
<code>(define constant <i>name expr</i>)</code>

##### `define macro`
<code>(define macro (<i>macro-name</i> <i>[param]&#42;</i>) <i>[</i>with (<i>[pattern]&#42;</i>)<i>]</i> <i>[expr]*</i>)</code>

##### `define method`
<code>(define method (<i>fn-name</i> <i>[param]&#42;</i>) <i>[</i>with (<i>[constraint]&#42;</i>)<i>]</i> <i>[expr]*</i>)</code>

##### `define protocol`
<code>(define protocol <i>pname [fname function]*</i>)</code>

##### `define variable`
<code>(define variable <i>name expr</i>)</code>

#### Defining structures
##### `define record`
<code>(define record <i>name [field]*</i>)</code>

##### `define union`
<code>(define union <i>uname</i> <i>[vname struct]*</i>)</code>

##### `define vector`
<code>(define vector <i>name</i> <i>[</i> type:  <i>type ]</i> <i>[</i> minimum:  <i>min ]</i> <i>[</i> maximum:  <i>max ]</i>)</code>


#### Built-in Macros

#### Built-in Protocols


## The Virtual Machine

The **virtual machine** is the program that makes it possible to run Bard code. It simulates a machine whose native language is Bard's kernel. When you execute a Bard expression, it is compiled to the kernel language and given to the virtual machine for execution.

The Bard language includes a set of protocols for controlling and fine-tuning the virtual machine.

### Starting and controlling the VM

### Image files

### Remote Bard Processes

### The Host Environment

### Foreign Code

