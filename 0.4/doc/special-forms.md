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

The `bindings` portion provides several options for defining and initializing lexical variables. The `bindings` form appears as a map that associates variable names with expressions used to initialize them. For example, here's a very simple `let` form:

    (let {x 2
          y (+ x 1)}
      (* x y))
      
This example returns 6 when evaluated. It first creates a lexical variable named `x` and binds it to `2`. It next creates a second lexical variable named `y` and binds it to the value of `x` plus 1. FInally, the single expression in the body multiplies `x` and `y` to obtain `6`.

As the example illustrates, the initializationof a variable can refer to any variable that was defined earlier in the bindings part of the `let` form. It can't refer to variables defined after its own definition, though; only to ones defined before.

`let` supports binding multiple values:

    (let { [object characters-read file-position] (read-object *logfile*) 
           remaining-characters (- (file-length *logfile*) file-position) }
      ...)
      
In this contrived example, the function `read-object` returns three value: an object read from a file, the number of characters processed during the read, and the file position after the read is complete. The example `let` form binds these values to lexical variables and then uses the file-position value in a further computation to initialize another lexical variable, `remaining-characters`.

As you see here, if the left side of a variable definition is a list of variable names, Bard collects multiple values returned by the value expression and binds them in order to the named variables. If there are more variables named than values produced, Bard binds the values to the variables in order, and then binds `nothing` to any variables that are left over.

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
    
`method` creates a new method whos formal parameters are given in `parameters` and whose behavior is defined in `body`. When the method is applied to arguments, the arguments are evaluated and their values are bound to the formal parameters given in `parameters`. The expressions in `body` are then evaluated in the resulting environment as if they were in a `begin` form. The value of the last expression evaluated is returned as the value of the method call.

**TODO:** document `method` parameter lists.

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

