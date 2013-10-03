# Bard
version 0.4
by mikel evins

## Basic ideas

Bard is a language and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases within phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is a collection of expressions that, when evaluated, produces some values.

A **structure** is a specific way to represent values. Each structure can represent some values, but not others. Every value has exactly one structure. You can define new structures in Bard.

A **procedure** is a value that can be **applied** to other values to compute a result. Procedures come in four flavors: **functions**, **methods**, **macros**, and **special forms**. We'll look more closely at each of these in later sections. You can define new functions, methods, and macros, but you can't add or change special forms; that's what's special about them. They form the foundation of the Bard language.

A **protocol** is a set of related procedures that define a **type**. A **type** is a collection of values that can all behave the same way. For example, all the values that can be used in arithmetic belong to the type `Number`. All the values that can be sorted, reversed, and appended together belong to the type `List`. You can define new protocols in Bard, and by defining protocols, create new types.

## An Overview of Bard

### Expressions

The basic unit of meaning in Bard is the **expression**. Simple values are expressions. Here are a few examples:

    bard> 4
    4
    
    bard> "Hello!"
    "Hello!"
    
    bard> ()
    nothing
    
I've shown what happens when I type thse values at Bard's prompt: it reads each value, evaluates it, and prints the result. For most values, "evaluating" just means returning the same value. Expressions that work this way are called **self-evaluating forms**. 

Not all forms are self-evaluating, though. For example:

    bard> (+ 2 3)
    5
    
The value that Bard returned and printed is not the same as the expression that I typed. That's because Bard interprets that kind of expression as a **procedure call**; that is, it finds a procedure identified in the expression and applies it to some arguments to compute a result. In this example, the procedure was `+`, the addition function, and the arguments were the numbers `2` and `3`.

An expression like this one, in which some values are delimited by parentheses, is called a **list**. Here are a few more lists:

    ()
    (* 4 5)
    (< 2 3)
    (+ 2 3 4)
    (+ (* 2 3) (* 4 5))
    (a list of random words)

All of these example lists will work as procedure calls if you type them in, except the last one. The problem with the last one is that there is no procedure in Bard named `a`. If you type in the expression as you see it here, Bard will complain by signaling an error:


    bard> (a list of random words)

    ERROR: Undefined variable: a 

It complains that the **variable** `a` is not defined--and it's quite right, because we haven't defined it. Unfortunately, that means that Bard doesn't know how to process that list. It thinks we want to call the procedure `a`, but it can't find such a procedure.

Why did it complain about an undefined **variable** instead of an undefined procedure? Because the normal way Bard finds a procedure is by looking up a variable that is the name of the procedure.

The name of a variable is called a **symbol**. Symbols are the second kind of value--besides procedures--that are not self-evaluating. Most of the time when you write a symbol it's because you want the value of the variable that it names. For example:

    bard> +
    #<function> (-> Number & -> &)
    
The variable named `+` has as its value a function. That's why it worked to evalute `(+ 2 3)`.

    bard> a
    
    ERROR: Undefined variable: a 

The variable `a` is undefined--but we already knew that.

You can actualy get Bard to process `a`, even though the variable is undefined; you just have to tell it not to try to look up a variable with that name. Here's how:

    bard> 'a
    a
    
Bard is happy to just return `a` if we tell it not to try evaluating it. I did that using a single quote ('). Marking an expression like this is called **quoting** the expression, and you can do it with anything. For example:

    bard> '(a list of random words)
    (a list of random words)

Again, as long as we ask Bard not to try evaluating the list, it's perfectly happy with it.

We can even quote self-evaluating forms:

    bard> '4
    4

The result is the same as if I didn't quote it. If I don't quote `4` its value is 4. If I do quote `4`, its value is 4. Quoting makes no difference with self-evaluating expressions.

### Literals

Another name for a self-evaluating expression is a **literal**. A literal is just the written form of a value. 

Here are a few examples of literal values:

    0
    5
    -2
    "hello"
    'a

An expression like `(+ 2 3)` is not considered a literal because it's not the written form of a self-evaluating expression. If you type it in at the Bard prompt, Bard will read and evaluate it and return 5. 5 is not the same as the expression you typed; you typed a list with three elements--the symbol `+`, and the numbers 2 and 3.

A **literal** is a self-evaluating expression that represents a value. All the numbers, for example, are literals.

Bard provides a way to write almost every kind of value as a literal. There are a few exceptions. Some data resist being printed in a convenient, readable form. A common example is an open file or network connection: there's not a really convenient way to print that as a value that Bard can read back in.

Apart from exceptions like that, though, Bard can print any value as a literal, and it can read any literal expression to reconstruct an equivalent value.

Here are a few more examples of Bard literals:

    true false ; Boolean values
    0 1.2 2/3 6.022e29 ; numbers
    \A \space ; text characters
    (+ 2 3) (a random list) ; lists (remember to quote them if they aren't procedure calls!)
    "Hello" ; text
    { name: "Fred" age: 101 } ; a map
    
Literals make it easy to type data into interactive sessions. That in turn makes it easy to test procedures interactively as you write them. It also means that in most cases it's easy to convert data into a form that your program can write to storage, or read from it, or transmit to another program.   

### Values and structures

A **structure** describes how to represent a family of values. Structures are values themselves, as well, and you can ask Bard for information about them. For example, you can ask Bard what structure is used to represent a value:

    bard> (structure-of 5)
    <fixnum>
    
The value `5` is an instance of the structure `<fixnum>`.

By convention, the names of structures start with the '<' character and end with '>'.

Here are the structures of a few more values:

    bard> (structure-of nothing)
    <null>

    bard> (structure-of "Hello!")
    <string>

    bard> (structure-of { name: "Fred" age: 101 })
    <simple-table>

    bard> (structure-of +)
    <function>

    bard> (structure-of '+)
    <symbol>

You can define new structures; we'll cover how to do that in a later section.

### Procedures 

We've already seen a simple example of calling a procedure. Here it is again:

    bard> (+ 2 3)
    5

If you write a list and you don't quote it, Bard assumes you intend to execute a procedure call--in other words, it looks up the procedure given as the first element of the list, and it applies the procedure to the remaining elements of the list.

Here are a few more examples of procedure calls:


    bard> (* 2 3)
    6

    bard> (> 2 3)
    false

    bard> (+ (* 2 3) (* 4 5))
    26

In the last example, the arguments to the procedure call are themselves procedure calls. This kind of nesting is common in Bard programs, and in all Lisp programs.

Naturally, arithmetic is not the only kind of operation we can do with procedures:


    bard> (first "hello")
    \h

    bard> (last "hello")
    \o

    bard> (append "hello " "world")
    "hello world"

You can create your own procedures and use them in your programs. When you define a new procedure, you build it out of calls to other procedures.

Most of Bard programming consists of defining structures and procedures, and then using those new structures and procedures to accomplish your task.

### Protocols and types

A **type** is a collection of values that can behave the same way. For example, all the values that can be used in arithmetic belong to the `Number` type.

Types and structures are not the same. A structure is a blueprint for constructing values. A type is a collection of values that can all be used in a certain way. **Protocols** define types.

Let's suppose that we want to create a protocol for greeting people. We want to say "hello" to a person by name. Here's the protocol:

    (protocol greet (-> UserName -> Salutation))
              
Evaluating this expression returns a protocol object. It's a value that contains the information that `greet` is a function that takes one argument of type `UserName` and returns a value of type `Salutation`.

By convention, the names of types start with capital letters.

Let's capture the protocol in a variable so we can talk about it later:

    (define variable =Greeting=
      (protocol greet (-> UserName -> Salutation)))

By convention, the names of protocols start and end with an equals sign (=).

So how do we define these types `UserName` and `Salutation`? We just did! A type is really nothing more than a named collection of values. We just defined them by creating the protocol.

Okay, if we defined the types, then what values are instances of `UserName` and `Salutation`? The answer is that, so far, no values are instances of either type, because we haven't told Bard what values should belong to them. We also haven't told it how to actually compute a result for the `greet` function, so if we try to use it then Bard will just print an error.

There are two ways to add values to a type:

* You can **assert** that values belong to a type
* You can define a **method** on one of the protocol functions

The first way is simpler, but less useful. Here's what it looks like to assert a type:

    (add-instance! UserName "Bob")
    
...or...

    (add-member! UserName <string>)
    
The first form asserts that "Bob" is an instance of `UserName`. The second asserts that `<string>` is a member of `UserName`, which means that all instances of `<string>` are also instances of `UserName`.

One reason it's generally less useful to add values to a type this way is that in most cases adding all the values or types you care about would be tedious and error-prone. Another reason it's less than ideal is that adding values this way doesn't tell Bard what to do with them. In particular, Bard won't know how to actually compute any results using the values you've added until you write a method that tells it.

It'll warn you about that problem:

    bard> (add-elements! UserName ["Bob" "Joe"])
    WARNING: Protocol =Greeting= is only partially defined
     unimplemented methods on: greet
    
What Bard means is that you told it that those text strings were instances of `UserName`, but you didn't tell it what to do if someone passes one of them to `greet`. But the whole purpose of `UserName` is to be passed to `greet`, so you've left out an important part of the type's definition.

Defining a **method** on `greet` solves all these problems:

    (define method (greet nm)
        where: (nm <string>)
      (append "Hello, " nm "!"))
      
Here's what it looks like to call this function with the argument "Pete":

    bard> (greet "Pete")
    "Hello, Pete!"
    
The method definition told Bard that when the function `greet` encounters a `<string>`, it should evaluate the code given in the definition: append the text "Hello " before it, and then "!" after it, and then return the result.

The protocol says the argument to `greet` is a `Username`. The method definition says it's a `<string>`. Logically, that means that `<string>` must be a member of `UserName`, so Bard automatically asserts that it is. You don't actually need to write

    (add-member! UserName <string>)

or any other assertion; just writing the method definition is enough. What's more, Bard won't issue any warnings about incomplete definitions of `greet`, because the `define method` form provides a complete definition.

There are more subtleties to protocols and method definitions that we will cover in later sections.

### Input and output
#### The reader
#### The printer
#### Images
#### Messaging

### Flow of control

### System procedures and data structures

### Built-in structures and protocols

