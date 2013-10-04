# Bard
version 0.4
by mikel evins

## Basic ideas

The basic ideas that make up the Bard language, each briefly described in this short section, are:

* **expressions** or **forms**
* **values**
* **programs**
* **structures** and **instances** of structures
* four kinds of **procedures**: **functions**, **methods**, **macros**, and **special forms**
* **types** and **protocols**
* the **reader**, **evaluator**, and **printer**
* Bard **agents**, **sessions**, **messages**, and **images**
* the Bard **library**

Bard is a language and processor that evaluates **expressions** to produce **values**.

A **value** is a piece of data that Bard knows how to handle.

An **expression** is a word or phrase that Bard understands. Expressions can be as simple as a single number or text character, or as complex as a large structure of phrases within phrases many pages long. An expression is also called a **form**, especially when it's more complicated than just a single word or number.

A **program** is an organized collection of expressions that is designed to compute some result.

A **structure** is a blueprint for constructing values; it tells Bard how to combine simpler values to form more complex ones. It also defines how to extract parts of the data contained in a structure.

Values constructed according to a structure are called **instances** of the structure. Each Bard value is an instance of some structure.

Bard supplies a useful library of built-in structures, and provides tools you can use to define new ones.

A **procedure** is a value that can be **applied** to other values to compute a result. Procedures come in four flavors: **functions**, **methods**, **macros**, and **special forms**. 

**Special forms** are built into Bard and cannot be modified; that's what's "special" about them, They're the foundation of the language--the procedures you use to work with everything else.

**Functions** are the procedures that you use most often in programs. You **apply** a function to some arguments, and it computes a result.

Strictly speaking, a function can't actually compute its result; that's the job of a **method**. A function just looks at the arguments to determine which method applies to them, then passes them to the method. It's the method that does the actual computing.

That means that the same function can run different code for different arguments. This feature is known as **polymorphism**.

**Macros** are procedures that can rewrite an expression before evaluating it. The most common use for macros is to exnted the syntax of the language, usually to make complicated expressions simpler to write and easer to read. Writing a macro is sort of like adding a new special form.

A **type** is a collection of values that can be treated the same way. For example, all of the values that you can use in arithmetic belong to the `Number` type, and all of the values that you can sort, reverse, and concatenate belong to the `List` types.

Types are completely abstract in Bard--in other words, a type doesn't say anything about how its instances are represented; it only describes how they behave. Bard uses structures to define how values are constructed, and types to describe what you can do with them.

Types are defined by **protocols**. A **protocol** is a collection of related functions--and sometimes some macros--that defines a set of types and all the things you can do with them. For example, the `=Number=` protocol defines functions for addition, subtraction, multiplication, and so on, and it defines types including `Number`, `Integer`, `Ratio`, and so on.

The `=Signaling=` protocol is worth a special mention: it defines **conditions** and procedures for working with them. A **condition** is a value that represents some exceptional situation that arose during evaluation. An **error** is one kind of the condition; a **warning** is another. Bard provides tools for **signaling**, **catching**, and **handling** errors, warnings, and other conditions, and for defining new kinds of conditions.

Bard is designed to be used interactively; that is, it's designed to run as a program that you interact with. It prints a prompt and then waits for you to type expressions for it to evaluate. When you do, it **reads** the expressions, **evaluates** them to compute the results, and then **prints** the results.

Each of these stages of processing is handled by a programmable service: the **reader**, **evaluator**, and **printer**.

The reader and the printer are coordinated: the reader can read what the printer prints. Unless you tell it to do otherwise, the printer tries to print values in a form that allows the reader to read them and reconstruct equivalent values. That means that normally you can ask Bard to print a value to a file, for example, and then later ask it to read the file, and it can faithfully reproduce the values that were written earlier.

This process doesn' work for every kind of value; there are a few that resist being printed readably. As far as it can, though, Bard tries to make it possible to simply print and read its values.

The program that provides the reader, evaluator, and printer is called a Bard **agent**. Bard agents can spawn other agents, and they can send **messages** to them. They can also send messages to agents that they didn't spawn, as long as they first receive a message that supplies a reference to the destination agent.

The state of a running agent is called a **session**. It's created when the agent starts running, and destroyed when it stops, but you can ask an agent to save its session in a file called an **image**. If you start an agent using a saved image, the stored session will resume as if it had never stopped--even if you start it on a different agent on a different machine.

Bard agents normally start from standard saved images, but you can save customized images with your own modifications.

One feature of the standard image is a standard Bard **library** that contains all of the built-in protocols that come with Bard, as well as the reader, evaluator, and printer, and a collection of useful programming tools to help you create Bard programs. The Bard language itself is not very big; a lot of its useful features are actually part of the library.

That concludes a whirlwind tour of the Bard language. In the following sections we'll look at each of these concepts in greater detail. The manual concludes with a reference that describes the Bard library.

## An Overview of Bard

### Expressions

The basic unit of meaning in Bard is the **expression**. Simple values are expressions. References to **variables** are also expressions, and so are procedure calls.

#### Values

Literals

#### Variables

#### Naming conventions

#### Applying procedures 

#### Flow of control

### Types and protocols

#### Protocols

#### Defining protocols

#### Defining methods

#### Advanced: controlling dispatch

#### Advanced: conditions

### Agents

#### Sessions and images

