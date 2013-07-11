# Core Bard

## Overview

Core Bard is a simplified Bard that serves as a target for desugaring the Bard surface language. It contains many fewer constructs than Bard proper, and is simpler to compile.

## Core language

### Literals and constants

A literal is an expression that denotes a constant value. These include literal expressions for the built-in numeric types, characters, Booleans, keywords, the unique value of the `<null>` type, and text strings.

### Variable references

A variable reference is a symbol that names the variable. Bard is a lexically-scoped language; variables are defined with respect to a lexically-delimited scope established by a **binding form**. Binding forms are `^` and `let`. Examples of lexical variable references are

    x
    my-var

A single global environment encloses all lexical environments. In Bard, code can define and refer to global variables using **modules**. In Core Bard, module references are desugared into **variable locatives** that identify global variables using their module and variable names. Examples of variable locatives are

The Bard expression

    (+ 2 3)
    
written in the `bard.user` module, desugars into the Core Bard expression

    ((var bard.user +) 2 3)

### Special forms

**`^`**

<pre>
(^ (<em>arg1</em> <em>arg1</em> ... <em>arg1</em>) 
  <em>body</em>)
</pre>

**`begin`**

<pre>
(begin <em>expr1</em> <em>expr2</em> ... <em>exprN</em>)
</pre>

**`if`**

<pre>
(if <em>test-expr</em>
  <em>then-expr</em>
  <em>else-expr</em>)
</pre>

**`bind`**

<pre>
(bind (<em>var1</em> <em>var2</em> ... <em>varM</em>) 
      <em>vals-expr</em>
  <em>body</em>)
</pre>

**`quasiquote`**

<pre>
(quasiquote <em>expr</em>)
(quasiquote (<em>expr1</em> <em>expr2</em> ... (unquote <em>exprM</em>) ... <em>exprN</em>)
(quasiquote (<em>expr1</em> <em>expr2</em> ... (unquote-splicing <em>exprM</em>))
</pre>

**`setter`**

<pre>
(setter x)
(setter (foo x))
</pre>

**`var`**

<pre>
x
(var bard.user +)
</pre>

### Applications

<code>(<em>op</em> [<em>arg</em>]*)</code>

### Datatypes

**`<bignum>`**

**`<boolean>`**

**`<char>`**

**`<class>`**

**`<cons>`**

**`<exception>`**

**`<fixnum>`**

**`<flonum>`**

**`<frame>`**

**`<function>`**

**`<iostream>`**

**`<keyword>`**

**`<method>`**

**`<module>`**

**`<null>`**

**`<primitive>`**

**`<protocol>`**

**`<ratnum>`**

**`<s16vector>`**

**`<s32vector>`**

**`<s64vector>`**

**`<s8vector>`**

**`<schema>`**

**`<singleton>`**

**`<string>`**

**`<symbol>`**

**`<synonym>`**

**`<u16vector>`**

**`<u32vector>`**

**`<u64vector>`**

**`<u8vector>`**

**`<vector>`**



