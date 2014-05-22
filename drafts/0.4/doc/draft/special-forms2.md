# Bard Special Forms
version 0.4

by mikel evins

## Special Forms

Special forms are the procedures that define the core of the Bard
language. They're built into the kernel language and cannot be
modified without tinkering with the Bard VM and compiler. They're the
foundation on which Bard programs--and most of Bard itself--are built.

This section describes the built-in special forms defined by Bard 0.4.

### `begin`
*control*: Simple sequencing of forms

<code>(begin [<i>expression</i>]*)</code>
    
`begin` evaluates each *expression* from left to right, returning the value of the last one, and discarding the values of the others.

If there are no expressions then the `begin` form returns `nothing`. Expressions before the last one are presumably included for side effects; their values are discarded.

### `cond`
*control*: conditional evaluation

<code>(cond [<i>clause</i>]*)</code>

A <code><i>clause</i></code> is an expression of the form

<code>(<i>test</i> <i>body</i>)</code>

Each <code><i>test</i></code> is a single expression. Each <code><i>body</i></code> is zero or more expressions.

So a simple `cond` expression might look like this:

    (cond
      ((odd? x) 'odd)
      ((even? x) 'even))
      
`cond` examines each clause from left to right. For each clause, `cond` first evaluates the <code><i>test</i></code> expression. If the result of that evaluation is a true value then `cond` evaluates the expressions in the <code><i>body</i></code> as if they were in a `begin` form. If the result of the test is a false value, then `cond` skips the <code><i>body</i></code> and moves on to the next clause.

If `cond` reaches the end of the clauses without any <code><i>test</i></code> returning a true value then it returns `nothing`.

The <code><i>test</i></code> expression can be any Bard value or expression; if the result of evaluating it is true then the associated <code><i>body</i></code> will be evaluated. That means that any true value can be used to force evaluating of a clause's body. Conventionally, the keyword `else:` is used for this purpose, to mark a default case:

    (cond
      ((odd? x) 'odd)
      (else: 'even))


### `define`
*definitions*: Defining variables, types, and functions

A family of special forms used for defining variables, types, and functions. See "define.md" for a full description of `define`.


### `ensure`
*control*: Ensure that some code is executed

<code>(ensure <i>during-form</i> <i>after-form</i>)</code>
    
`ensure` evaluates <code><i>during-form</i></code> and returns its value, making sure that <code><i>after-form</i></code> is evaluated after the evaluation of <code><i>during-form</i></code>, but before its result is returned. `ensure` can be used to ensure that important code is executed even when <code><i>during-form</i></code> exits abnormally--for example, when evaluation of <code><i>during-form</i></code>` signals an error.

As an example, suppose the call to `read` in the following code fails:

    (ensure (read *the-file*)
            (close *the-file*))
            
`ensure` makes sure that the expression `(close *the-file*)` is evaluated even if the call to `read` fails.

`ensure` returns any values produced by evaluating <code><i>during-form</i></code>, or `nothing` if no values are produced.

### `if`
*control*: Conditional evaluation

<code>(if <i>test-form</i> <i>then-form</i> <i>else-form</i>)</code>
   
`if` evaluates <code><i>test-form</i></code>. It the evaluation returns a true value then `if` evaluates <code><i>then-form</i></code>; otherwise, it evaluates <code><i>else-form</i></code>.

For example,

    bard> (if (odd? 3)
            (+ 2 3)
            (* 2 3))
    5

    bard> (if (odd? 4)
            (+ 2 3)
            (* 2 3))
    6
    
 It is an error to omit <code><i>else-form</i></code>. For instances in which only one of the test results should trigger evaluation, see `when` and `unless`.

### `let`
*binding*: Definition of lexical variables

`let` defines and initializes local lexical variables. Here's a simple example of its use:

    bard> (let ((x 2)
                (y + x 1))
            (* x y))
    6
    
For a full description of `let` see "let.md".

### `loop`
*control* Simple iterative evaluation

    (loop loop-name bindings body)
    
`loop` provides a simple but powerful mechanism for creating iterative loops. Here's a simple example of `loop` in use:

    bard> (loop again ((i 0))
            (newline)
            (print i)
            (when (< i 4)
              (again (+ i 1))))
    0
    1
    2
    3
    4

For a complete description of `loop`, see "loop.md". 


### `match`
*binding*: Definition of lexical variables by pattern-matching

`match` defines and initializes local lexical variables by matching patterns against Bard values. Here's a simple example of its use:

    bard> (match ((a b c) '(2 3 4))
            (* b c))
    12
    
`match` provides a comprehensive pattern-matching language that can destructure complex data structures. For a full description of `match` see "match.md".

### `method`
*constructor*: Construct methods

<code>(method <i>parameters</i> <i>body</i>)</code>
    
`method` constructs and returns a new **method**. The method's formal parameters are given by <code><i>parameters</i></code>; its behavior is defined by the expressions given in <code><i>body</i></code>.

When the method is applied to a list of arguments it binds the argument values to its formal parameters, then evaluates the body expressions in the resulting environment. Each of the parameters becomes a lexical variable, and expressions in the body may refer to those variables.

Expressions in the body can be anything that can appear in a `begin` form.

For a full description of `method` and the <code><i>parameters</i></code> form, see "method.md".

### `receive`
*accessor*: Receiving messages

`receive` receives the next available **message**, or the next message that matches a provided pattern. A message is a Bard value that has been sent to the running Bard process using the `send` special form.

For example:

    bard> (receive)
    "Hello"
    
    bard> (receive (_ 2 3))
    (1 2 3)
    
    
    bard> (receive (_ 2 3))
    nothing
    

The first call to `receive` asked to receive whatever the next message happened to be, and got the string `"Hello"`.

The second asked for any pending message that matched the pattern `(_ 2 3)`. The symbol `_` matches any value, so `receive` got the first message that was a list whose second element was 2 and whose third was 3.

The third call to `receive` got `nothing` because there were no more pending messages that matched the supplied pattern.

For a full description of how `send` and `receive` work, see "messaging.md".

### `send`
*accessor*: Sending messages

`send` sends a **message** to a Bard process. The process can be local or remote, and the message can be any Bard value that can be serialized (that means nearly any value).

A process can even send a message to itself. Here's an example that does just that:

    bard> (send (this-process) "hello!")
    
The receiving process can get the message at any time after it has been sent by calling `receive`.

For a full description of how `send` and `receive` work, see "messaging.md".

### `set!`
*accessor*: Assigning new values to variables and other bindings

    (set! place val)

To assign a new value to a lexical or module variable, use `set!` like this:

    (set! x 4)
    
This is a simple case of assignment, but assignment in Bard is not always quite this simple. Both getting and setting the values of variables and of fields in data structures can be customized using **getters** and **setters**, and Bard provides support for immutable variables and data structures.

For a full description of how `set!` works, and of **getters** and **setters**, see "getters-and-setters.md".

