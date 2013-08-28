# Bard
version 0.4
by mikel evins

## Introduction

Bard is a new Lisp. Its primary purpose is to make me happier and more
productive in my work. In that purpose it has already succeeded.

I'd like it if Bard also helps make other people happier and more
productive, though that goal is secondary. If you find that you like
Bard well enough to use it, please use it with my blessing; it has a
permissive license. If you don't like it, I recommend using something
else.

If you like it well enough to comment, and dislike it enough to
criticize it, I promise that I will consider the criticism. If I find
your criticism persuasive then I will most likely modify Bard to try
to improve it. If I don't find your criticism persuasive then I'll
probably ignore it.

Bard is a Lisp in the general tradition of Dylan, Common Lisp and
Scheme, and it bears a certain resemblance to them. It has also
absorbed influences from diverse other languages, including Smalltalk,
ML, Haskell, Erlang, and Clojure.

It's a small language with a smaller kernel. Like most Lisps, it uses
s-expressions as its native syntax, with a few Bard-specific lexical
conventions added. 

Bard's type system is unique, based on ideas I published many years
ago in an article about defining types in terms of protocols. It bears
a loose resemblance to Dylan's classes, and to CLOS, and some
observers have also compared it to Haskell's Type Classes.

Bard functions are polymorphic, with multiple dispatch, like generic
functions in Dylan and CLOS, or multimethods in Clojure or Julia.

Bard emphasizes, but does not enforce, the use of immutable data
structures. It provides simple mechanisms that enable one Bard process
to communicate with another, whether the other process runs on the
same machine or a distant one.

Bard is implemented as a simple compiler and a simple virtual
machine. The virtual machine implements a tiny kernel language. Bard
proper is implemented as a program written in that kernel language,
and stored in serialized form in an image file that the VM loads when
it starts.

This architecture makes it easy for me to port Bard to whatever
platform I need to work with, and also makes it easy to change and
extend the language to support whatever features I need for a given
project.

## Syntax and Expressions

## Special Forms

Special forms are built-in procedures whose behavior is defined in the
Bard virtual machine. They form the core of the Bard language. You
cannot modify Bard's special forms (at least, not without modifying
the virtual machine).

See the file "special-forms.md" for a full description of Bard's
special forms.

## Types

### How Types Work

### Built-in Types

### Defining New Types

## Functions and Methods

### How Functions and Methods Work

### Built-in Functions and Methods

### Defining Functions and Methods

## The Virtual Machine

### Starting and controlling the VM

### Image files

### Remote Bard Processes

### The Host Environment

### Foreign Code

