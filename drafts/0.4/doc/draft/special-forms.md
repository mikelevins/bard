# Special forms

This document describes the built-in special forms provided by Bard 0.4.

### `begin`
*control*: Simple sequencing of forms

    (begin [expression]*)
    
`[expression]*` denotes a sequence of zero or more valid expressions. Each expression in `[expression]*` is evaluated from left to right. The value returned by evaluating the last expression is returned as the value of the `begin` form. All other values produced are discarded. Expressions that appear before the last are presumably included for the sake of side-effects they cause when evaluated. If `[expression]*` is empty then `nothing` is returned as the value of the `begin` form.

### `cond`
*control*: Multiway conditional branching

    (cond [clause]*)
    
Each `clause` consists of a list whose first element is a test form and whose remaining elements are expressions to be evaluated if the value of the test form is true. `cond` examines clauses from left to right, taking the test form from each clause and evaluating it. If a test form returns a true value then `cond` evalutes the remaining expressions of the clause and ignores any additional clauses in the `cond` form. The expressions in a clause after the test form are evaluated from left to right as if in a `begin` form, with all values produced being discarded except the last; the value of the last expression is returned as the value of the `cond` form.

Example:

    bard> (cond
            ((odd? 2) 'odd)
            ((even? 2) 'even))
    even

A test form may be any valid expression. Its truth value is used to determine whether to evaluate the remaining expressions in the clause. Any true value causes a clause to be selected; it's conventional to use the keyword `else:` as the test form in the final clause, to select a default set of expressions to evaluate:

    (cond
      ((saturday?)(sleep-in))
      ((sunday?)(sleep-in))
      (else: (get-up-early)))

### `define`
*definitions*: Defining variables, types, and functions

A family of special forms used for defining variables, types, and functions. See the file "define-forms.md".

### `ensure`
*control*: Ensure that cleanup code is executed

    (ensure before-form during-form after-form)
    
`ensure` evaluates `before-form`, then `during-form`, then `after-form`. The important feature of `ensure` is that `after-form` is executed even if `before-form` or `during-form` exit abnormally--for example, even if one of them signals an error. Each of `before-form`, `during-form`, and `after-form` may be any valid expression. The value of returned by `during-form`, if any, is returned as the value of the `ensure` form.

### `if`
*control*: Conditional evaluation

    (if test-expression then-expression else-expression)
    
`[expression]*` denotes a sequence of zero or more valid expressions. Each expression in `[expression]*` is evaluated from left to right. The value returned by evaluating the last expression is returned as the value of the `begin` form. All other values produced are discarded. Expressions that appear before the last are presumably included for the sake of side-effects they cause when evaluated. If `[expression]*` is empty then `nothing` is returned as the value of the `begin` form.

It is an error to write an `if` form with no `else-expression`. For single-armed conditionals, see `when` and `unless`.


### `let`
*binding*: Definition of lexical variables

    (let bindings body)
    
`let` creates a local lexical environment in which one or more lexical variables may be defined. The `bindings` part of the `let` form defines and initializes the variables; the `body` portion consists of zero or more expressions. The expressions in the body are evaluated in an environment where the variables from the `bindings` are defined.

The `body` can be any number of expressions, and they're evaluated from left to right as if in the body of a `begin` form.

The `bindings` part of the `let` form is a list of variable definitions:

    (def1 def2 def3...)
    
A definition is also a list. It takes the form

    (var val)
    
Putting it all together, here is a simple example that defines and initialize two lexical variables and then uses them to compute a result:

    bard> (let ((x 2)
                (y (+ x 1)))
            (* x y))
    6

Variables are initialized from left to right, and so later variable initializations may refer to variables that are defined earlier, as the initialization of `y` here refers to the value of `x`.

A single definition may initialize more than one variable, by using an initialization form that returns multiple values:

    bard> (let ((x y z (values 2 3 4)))
            (* y z))
    12

If there are more variable names than values then the extra variables are bound to the value `nothing`.

### `loop`
*control* Simple iterative evaluation

    (loop loop-name bindings body)
    
`loop` creates a lexical environment in which the variables defined in `bindings` are defined and initialized, and the expressions in `body` are then evaluated in order from left to right. The `bindings` and `body` expressions are treated as if in a `let` form, with one additional feature: the name `loop-name` is locally bound to a procedure that, when invoked, causes the `body` expressions to be evaluated again with updated values in `bindings`.

Here's a simple example:

    bard> (loop again ((i 0))
            (newline)
            (print i)
            (when (< i 10)
              (again (+ i 1))))
    0
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10
    
In this example, `loop` initially binds `i` to 0, prints a newline and then i, checks to see if it's less than 10, and since it is, calls `again` with an argument of i + 1. Calling `again` starts the evaluation from the top again, but with `i` bound to the value passed to `again` instead of the initial value. The process repeats until i is no longer less than 10.

### `method`
*constructor*: Construct methods

    (method parameters body)
    
`method` creates a new method whose formal parameters are given in `parameters` and whose behavior is defined in `body`. When the method is applied to arguments, the arguments are evaluated and their values are bound to the formal parameters given in `parameters`. The expressions in `body` are then evaluated in the resulting environment as if they were in a `begin` form. The value of the last expression evaluated is returned as the value of the method call.

`parameters` is a list with two parts: the **required parameters** and the **rest parameter**. Both required parameters and rest parameters are optional. If the rest parameter is present, it's separated from the required parameters by the symbol `&`.

This is a method that accepts no arguments:

    (method () ...)
    
This is one that accepts exactly one argument:

    (method (x) ...)
    
This is a method that accepts any number of arguments--zero or more--and binds all of them to a lexical variable named `args`:

    (method (& args) ...)
    
Here's a method that accepts three or more arguments; it binds the first three (the required parameters) to the names `x`, `y`, and `z`, and binds any additional arguments to the name `more`:

    (method (x y z & more) ...)
    
The rest parameter can be a map instead of a symbol. If it's a map then the keys have to be keywords. The names of the keywords are used as the names of lexical variables in the environment of the method. For example:

    (method (x & {a: 2})
      (* x a))
      
Suppose we bound this method to the name `frob`; then an interaction with it would look like this:

    bard> (frob 2)
    4
    
It returns the value of the expression in the method's body. `x` is 2 because we passed 2 as the argument. `a` is 2 because we said so in the construction of the method; that's what the map in the rest parameter meant.
    
We can change the result by passing a different value for `a`, using the `a:` keyword:

    bard> (frob 2 a: 3)
    6
    
When we pass the keyword `a:` and a value, the value we pass replaces the initial value of `a` in the method's environment.

Parameters that are defined by giving a map as the rest parameter are called **keyword parameters**. Keyword parameters can be convenient both because they can be used to make the meanings of method parameters more clear, and because they can be used to provide default values for parameters.

### `receive`
*accessor*: Receiving messages

A Bard process can communicate with other Bard processes by sending and receiving **messages**. A **message** is simply any Bard value that can be serialized. 

Each Bard process has an associated message queue. Calling `receive` retrieves a message from that queue.

### `set!` and `setter`
*accessor*: Assigning new values to variables and other bindings

    (set! place val)
    ((setter place) val)

To assign a new value to a lexical or module variable, use `set!` like this:

    (set! x 4)
    
This example assigns the value `4` to the variable `x`.

To assign a new value to a field in a record, use `set!` like this:

    (set! (color *sky*) 'blue)
    
This example assigns the symbol `blue` to the `color` field of the record bound to `*sky*`. 

Assignments are performed by procedures called **setters**. The `setter` special form returns the setter procedure associated with a variable or field. In fact, `set!` is a macro that expands to a `setter` expression.

For example, `(set! x 4)` expands to `((setter x) 4)`. The second example, `(set! (color *sky*) 'blue)` expands to `((setter (color *sky*)) 'blue)`.

Normally there's no need for a Bard programmer to care about setter procedures, but in some cases it can be useful to capture a particular setter procedure for later use, and Bard provides the means to redefine setter procedures so that you can, for example, added memoizing or validation features to assignments. Bard can also use setters internally to implement thread-safe updates for data structures.

Not all variables and fields are mutable. Lexical variables are always mutable, but you can use `define constant` to define immutable module variables, and the fields of structures are immutable by default; if you want a field to be mutable you must specify so in its definition.

