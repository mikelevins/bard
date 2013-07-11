# Bard 0.4 notes

These are notes on ideas I have about Bard 0.4. Items that continue to look good to me as time passes will become facts about the 0.4 implementation of the language.

1. Bard is a VM-and-image-based language in the style of Smalltalk. Consider using a Smalltalk to implement Bard 0.4? Or continue work on the Gambit-based implementation, knowing that I'll need to implement the image-saving and loading parts myself.
2. Bard is an impure functional language.
3. Bard is a frame language. Every value is a frame.
4. Bard is an actor language. Every value is an actor.
5. A function call is a message send. A list is ordinarily treated as a message send, where the recipient is the first element of the list and the remaining elements comprise the message. `(+ 2 3)` sends the message `[2 3]` to the actor `+`, which sends the result back to the sender.
6. A function is an actor. Its input arguments are a message.
7. Actors may be local or remote. Messaging works the same way in either case.
8. Message sends may be synchronous or asynchronous. I need to explore how best to express the different cases. An ordinary Lisp-style function call, in which the result value is returned to the caller, is a synchronous (blocking) call. I need to think about how asynchronous calls should best be expressed.
9. The Bard compiler compiles to an s-expression-based data structure that represents a low-level intermediate language suitable for recompilation to bytecode or to other output formats (e.g. to Javascript or C).
10. Sequences are by default infinite streams. Waters' SERIES idioms are fundamental to Bard sequences.
11. Programs are composed of modules. Modules are first-class objects. Modules can contain modules. Modules are environments. Modules are frames. Modules are actors. A running instance of Bard is a module.
12. Bard is not a concatenative language, but it uses combinators a lot. Try to get as close as possible to being a concatenative language without becoming awkward or cryptic.
13. Replace the Streaming protocol with two new protocols: `Generating` and `Collecting`. `Generating` defines the class `Generator`: objects from which values may be taken one after another. `Collecting` defines the class `Collector`: objects to which values may be inserted one after another.


Bard programs are made of **values**.

Values have **types**. A value's type is a concrete description of its representation. Each value has exactly one type; it is said to be an **instance** of its type. 

Types are disjoint and unrelated to one another.

Types are described by **frames**. A **frame** is an associative array whose keys are **names**. The values associated with keys may be anything. The concept of a frame is an abstraction, not a concrete type.

Types belong to **classes**. A **class** is a bag of types. If a class contains a type then the type is said to be a **member** of the class. 

Classes may also contain other classes.
    
Types are not containers, and cannot have members.

`Frame` is a class. All types are members of `Frame`. That means that the `Frame` protocol is defined over all types, and it means that every Bard value can be treated as a frame.

Types and classes are first-class values. Although not all Bard values are types, any value may optionally be treated as a type. When a value that is not normally considered a type is treated as a type, it is called a **singleton** type.

Values are **actors**. An actor is a value that accepts and processes **messages**. A message is a tuple of values. A function call in Bard is a message send. The receiver is the actor; the message is a tuple of other values.

A message send is written like this:

    (+ 2 3)
    
In this example, the actor is the value of the variable `+`; the message is the list `(2 3)`. 

This expression looks like an ordinary Lisp function call, and it is in fact exactly equivalent, except that every Bard value is an actor; therefore, every Bard value may be used as a function.