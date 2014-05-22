# Bard 0.6 #

## The Base Singletons ##

The **base singletons** are five primitive atomic values that can be written as names.

|||  
|-	|-	|  
|`undefined `	| the absence of any useful value	|  
|`nothing `	| the empty collection  
|`true `	| Boolean true   
|`false `	| Boolean false   
|`end `	| the end of a stream  

## Literals ##

Literals are expressions that denote values that evaluate to themselves. An example is a number:

    13

The text "13" denotes the integer 13. When Bard reads "13", it simply returns the number 13.

Another example is text: 

    "Hello!"

Again, when Bard reads that text, it returns the text string "Hello!".

Not all expressions are literals. For example, the following is a procedure call:

    (first [1 2 3 4])

Bard reads it, converts it to an application object, evaluates it by applying the function named `first` to the list of numbers, and returns 1.  Literals are expressions that don't need to go through all those steps to return a value; they denote a value directly, and Bard simply returns that value.

Besides the base singletons, the various kinds of Bard literals include:

|Category|Examples|Notes|
|-|-|-|  
| numbers |`0, -12, 2, 3/4, 1.2e6, #b11011` | available types depend on the platform on which Bard runs |
| keywords |`:a, :name, :Blue`|self-evaluating names|
|URIs|`#<uri>:ftp://example.org/resource.txt`<br/> `#<uri>:urn:issn:1535-3613`|Uniform Resource Locators|
|text|`"","Hello","A few words"`|text strings|
|pairs |`[1 . 2], [:a . [:b . :c]]`|structures with a left element and a right element|
|lists |`[],[1 2 3]`|ordered sequences of values|
|mappings|`{},{:a 1 :b 2},{:name "Fred" :shape "square"}`| sequences of key/value pairs|
|methods|`(^ (x)(* x x))`<br/>`(^ (a b)(> a b))`|simple procedures|
|functions|`(-> <integer> -> <integer>)`<br/>`(-> <text> <integer> -> <character>)` |abstract generic procedures|

## Interaction ##

Bard is interactive. It's designed to be used at a live prompt.

    bard> true
    true
    bard> (true? false)
    false
    bard> (+ 2 3)
    5
    bard> (* 2 3)
    6
    bard> (/ 2 3)
    2/3

Bard starts running with a useful default state called the **toplevel**. To build a program you hold a conversation with the toplevel by typing expressions at the Bard prompt. The conversation teaches the running Bard environment how to be your program.

## Expressions ##

Bard programs are made of expressions. We've already looked at literal expressions, and at a few examples of expressions that are not literals, like this one:

    (+ 2 3)

Expressions return values. Bard reads them, evaluates them by performing the computations they describe, and returns the resulting values.

    bard> (+ 2 3)
    5

This kind of expression:

    (+ 2 3)
    
is called an **application**. It's a list whose first element is a procedure, and whose remaining elements are values passed as arguments to the procedure. In the example above, the procedure is named `+` and the arguments are the numbers 2 and 3. `+` is the name of the generic addition operator. Bard applies it to 2 and 3, returning 5:

    (+ 2 3)
    5
Each of the elements of an application can be a literal, a named value, or another application. Here's an example where one of the arguments is a base singleton:

    bard> (add-first 1 nothing)
    (1)

Here's one in which the first argument is another application:

    bard> (add-last (add-first 1 nothing) 2)
    (1 2)

The procedure doesn't have to be a name; it can be an application, too:

    bard> ((second [+ * - /]) 2 3)
    6

## Special Forms ##

**Special forms** are procedures that are built into the Bard evaluator. They're the foundation of the language. They're called special forms because they don't obey the same rules of evaluation that other procedures do. The standard evaluation rule for applications is that the arguments are evaluated left-to-right, then the procedure is evaluated, then the procedure value is applied to the argument values.

Special forms can violate this rule. Let's look at an example that shows how and why special forms break the normal rules.

`if` is a special form that looks like this: 

(`if` *test-expression* *then-expression* *else-expression* )

It evaluates *test-expression*. If the result is true then it evaluates *then-expression*; otherwise it evaluates *expression-expression*.

Now consider this interaction:

    bard> (if (odd? 4)
            (/ 4 0)
            3)
    3
Why doesn't Bard signal a divide-by-zero error? The *then-expression* divides four by zero.

The answer is that the *then-expression* is never evaluated, because four is not odd. `if`'s purpose in life is to enable you to choose between two different applications, deciding which of them to evaluate. Its purpose would be defeated if it always evaluated all of its arguments, as normal applications do. It needs special evaluation rules: it need to defer evaluation of its second and third arguments until it sees the result from evaluating the first.

That's why `if` is a special form and not a normal procedure, and that's why special forms exist.

## Methods ##

## Functions ##

## Protocols and Classes ##

## Flow of Control ##

## Datatypes ##

## Defining Datatypes

## Defining Protocols and Functions ##

## Saving and Loading State ##

Bard knows how to convert the state of its running toplevel to serialized form and write it to a stream. It also knows how to read a serialized toplevel state, deserialize it, and start it running again.

The saved state of a toplevel is called an **image**. Bard is designed to load an image when it starts up. Bard itself is a small program called the **kernel**. It knows how to find and load a saved image, deserialize it, and turn over control to the toplevel saved with it. 

The normal way to build a Bard program is to interactively change the running toplevel until it becomes the program you're trying to build, then save that toplevel. Your programs the combination of the new saved toplevel with the Bard kernel, which knows how to load and run it.

## Agents and Messages ##

An **agent** is a value that represents a running Bard toplevel. You can get the agent that represents the Bard process you're interacting with by evaluating `(this-agent)`.

The `Messaging` protocol provides the functions `send` and `receive` for communicating with agents. `(send an-agent a-message)` adds `a-message` to the queue of messages waiting to be received by `an-agent`. `(receive)` takes the next message from `(this-agent)`'s queue and returns it.

The receiving agent can be any agent that we have a reference to; it can be `(this-agent)`, or some other agent running on its own thread in the same process, or an agent running in another process, either on the same machine or a different one somewhere on the network. 

Creating an agent returns it, enabling you to send messages to it. An agent can get a reference to itself by calling `(this-agent)` and can send the reference to another agent. An agent can advertise itself on a socket, and in that case other agents can obtain references to it by using URIs that identify the host and port that the agent is listening on.

`a-message` can be any serializable Bard value. The `message` datatype represents a message in serialized form. `(deserialize a-message)` returns the value that was serialized.

Sending a message is asynchronous. The `send` expression returns immediately.

Receiving a message is optionally synchronous. You can either wait for the next message to become available, or return right away if there isn't a message queued when `receive` is called.


## Macros ##

## The Standard Protocols ##

### Application

### Construction

### Conversion ###

### Cryptography ###

### Equality ###

### Function

### Iteration ###

### Mapping ###

### Messaging ###

### Ordering ###

### Sequence ###

### Serializing ###

### Stream ###

### System

### Tapping ###

## The Built-in Datatypes ##

### `undefined` ###

### `nothing` ###

### `boolean` ###

### `fixnum` ###

### `float` ###

### `ratio` ###

### `complex` ###

### `character` ###

### `symbol` ###

### `keyword` ###

### `uri` ###

### `string` ###

### `cons` ###

### `vector` ###

### `hashmap` ###

### `function` ###

### `method` ###

### `macro` ###

### `special-form` ###

### `agent` ###

### `message` ###

### `class` ###

### `protocol` ###

### `base-singleton` ###

### `primitive-type` ###

### `record-type` ###

### `tuple-type` ###

### `synonym-type` ###

### `enumerated-type` ###

