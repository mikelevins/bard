# Building the Kernel

Read `kernel.md` first for what the machine is. This is how to write it.

Everything here follows from one idea:

> **The machine steps a reified computation. A frame is that computation.**

If a detail below seems arbitrary, check it against that sentence. It should
follow.

---

## Part 1 — The instructions

The current frame is the only register. "Push" and "pop" mean the operand area of
the current frame's `slots`, tracked by `sp`.

### Values

**`op_CONST k`** — push constant `k`.
The only instruction that produces a value from nothing; everything else consumes
something.

**`op_LOCAL up slot`** — push a value from the lexical chain.
`up` counts levels outward: `0` is the current frame's own slots, `1` is
`fn.captured-frame`, `2` is that frame's `fn.captured-frame`, and so on. Local
variables and captured variables are read by the same instruction — a captured
variable is just a larger `up`.

**`op_GLOBAL b`** — push the value held in binding `b`.
Note what this does *not* do: it does not push a value the compiler knew. It
reads a mutable cell at run time. That indirection is the entire reason a
redefinition takes effect in code compiled before it.

If `b.dynamic?` is set, `op_GLOBAL` first searches the current thread's `dynenv` for
the innermost rebinding of `b`, falling through to the cell if there is none. The
flag is false for nearly every binding, so the ordinary path costs one
predictable branch on an object already in hand.

### Stores

**`op_SET-GLOBAL b`** — store the top into binding `b`. **Does not pop.**
This is definition and redefinition, as one instruction. It mirrors `op_GLOBAL` with
respect to `dynamic?`: a rebound dynamic binding is assigned in the `dynenv`, not
in the cell.

**`op_SET-LOCAL up slot`** — store the top into a lexical slot. **Does not pop.**

Both leave their value so an assignment can be used as an expression. For effect
only, follow with `op_DROP`.

**`op_DROP`** — discard the top. Sequencing.

### Control

**`op_GOTO n`** — set `pc` to `n`.

**`op_BRANCH-FALSE n`** — pop; if false, set `pc` to `n`.
One conditional is enough; the compiler inverts the test when it wants the other
sense.

### Functions

**`op_CLOSE k`** — push a closure over code object `k`, capturing the **current
frame**. That capture is what makes it a closure rather than a code pointer, and
it is why frames and environments need only one representation.

**`op_CALL n`** — push arguments left to right, then the callee, then `op_CALL n`.

```
callee ← pop
if callee is a function:
    check n against callee.arity
    allocate a frame: callee.n-locals slots plus operand space
    move n operands into slots 0..n-1
    frame.fn ← callee;  frame.pc ← 0;  frame.parent ← current
    current ← frame
else:
    dispatch on callee's descriptor
```

Two things worth dwelling on. **Allocating the frame is what calling means** —
there is no separate instruction to bind arguments, because binding arguments is
building the computation. And the callee is *checked*, not assumed; that one
branch is what later lets primitives, generic functions, foreign functions, and
native-compiled functions all be reached by this instruction.

**`op_TAILCALL n`** — identical, except the new frame's `parent` is the *caller's*
parent. The caller's frame is simply abandoned, so a loop written as tail
recursion runs in constant space.

**`op_RETURN n`** — move the top `n` values of the current frame's operand area into
the parent's, then push `n` itself; set `current ← current.parent`. **If `parent`
is nil the thread is finished** and those values are its result. There is no halt
instruction because thread termination already is one.

A parent is nil only at the bottom of a thread's chain -- the frame the thread
was started with. Every frame `op_CALL` builds gets the caller as its parent, so
an ordinary return always has somewhere to go. The one case worth remembering is
that `op_TAILCALL` hands the callee *the caller's* parent, so a tail call made
from a thread's base frame gives the callee a nil parent, and that callee's
return is what ends the thread.

**`op_RECV k`** — pop the count; adjust the values beneath it to exactly `k`,
padding with `nothing` and discarding extras.

**`op_RECV-ALL`** — pop the count; collect that many values into one list and push
it.

**Every call site is followed by a receiver.** The caller cannot know statically
how many values a callee produced, so the operand area's shape is unpredictable
until a receiver fixes it. `op_RECV 1` is the overwhelmingly common case — a value
used as an operand. `op_RECV 0` discards a call made for effect. `op_RECV j` receives a
fixed multiple-value binding. `op_RECV-ALL` handles value lists and apply.

A tail call is the exception: it does not return to you, so *your* caller's
receiver handles its values. A `op_TAILCALL` is never followed by anything.

The count rides on the operand stack rather than in a register, because a
register would have to be saved and restored across every capture, thread switch,
and breakloop — the argument-count mistake in a new costume.

### Concurrency

**`op_YIELD`** — hand the machine to another runnable thread.

Because a thread's entire state is its current frame, a switch is: store the
current frame into this thread, pick another, load its frame. Green threads are
one machine loop doing that; OS threads are several machine loops each with their
own current frame. The instruction is the same either way.

---

## Part 2 — Programs

Each is minimal and adds one thing. A program is the code of a function invoked
with a nil parent, so its `op_RETURN` ends the thread and yields the result.

### 2.1 A constant

```
0: op_CONST 42
1: op_RETURN 1
```

`op_CONST`, `op_RETURN`, and thread termination.

### 2.2 Calling a primitive — `(+ 2 3)`

```
0: op_CONST 2
1: op_CONST 3
2: op_GLOBAL +
3: op_CALL 2
4: op_RECV 1
5: op_RETURN 1
```

`op_GLOBAL`, `op_CALL`'s descriptor branch, and a receiver. The handler for a primitive
consumes its arguments, pushes its results and a count, and creates no frame —
the call returns immediately. `op_RECV 1` then fixes the shape to one value.

`+` arrived through a binding. Rebind it and this already-compiled code calls the
new one. That is the point of P2.

Note that `+` is a *language* name — polymorphic, redefinable, defined in the
prelude. The baked-in operation underneath is `_fixnum-add`: monomorphic,
fixed-arity, sigil-prefixed to keep it out of the language's namespace. There is
no single addition primitive; there are several, and `+` is what dispatches among
them. All polymorphism lives above the kernel.

### 2.3 A conditional — `(if (< n 3) "small" "big")`

```
0: op_GLOBAL n
1: op_CONST 3
2: op_GLOBAL <
3: op_CALL 2
4: op_RECV 1
5: op_BRANCH-FALSE 8
6: op_CONST "small"
7: op_GOTO 9
8: op_CONST "big"
9: op_RETURN 1
```

### 2.4 Sequencing — `(begin (set! x 10) (set! y 20) (+ x y))`

```
0:  op_CONST 10
1:  op_SET-GLOBAL x      ; 10 remains on the stack
2:  op_DROP
3:  op_CONST 20
4:  op_SET-GLOBAL y
5:  op_DROP
6:  op_GLOBAL x
7:  op_GLOBAL y
8:  op_GLOBAL +
9:  op_CALL 2
10: op_RECV 1
11: op_RETURN 1
```

The `op_SET-GLOBAL`/`op_DROP` pair is what a top-level definition compiles to. Defining
something is an ordinary instruction, not a special mode.

Note the two ways to discard: `op_DROP` removes a value already on the stack;
`op_RECV 0` discards whatever a call returned. They are not interchangeable.

### 2.5 A function — `((fn (x) (* x x)) 7)`

Code object `SQUARE` — arity 1, n-locals 1:

```
0: op_LOCAL 0 0
1: op_LOCAL 0 0
2: op_GLOBAL *
3: op_CALL 2
4: op_RECV 1
5: op_RETURN 1
```

Main:

```
0: op_CONST 7
1: op_CLOSE SQUARE
2: op_CALL 1
3: op_RECV 1
4: op_RETURN 1
```

`SQUARE` has no prologue. `op_CALL` already placed the argument in slot 0, because
building the frame is the call.

Trace it once. `op_CALL 1` allocates a frame, moves `7` into slot 0, links `parent`
to main's frame, and assigns `current`. `op_RETURN 1` moves `49` into main's operand
area, pushes the count `1`, and restores `current`. `op_RECV 1` pops the count and
leaves `49`.

### 2.6 A closure over a mutable variable — a counter

`(fn (n) (fn () (set! n (+ n 1))))`

`MAKE-COUNTER` — arity 1, n-locals 1:

```
0: op_CLOSE BUMP
1: op_RETURN 1
```

`BUMP` — arity 0, n-locals 0:

```
0: op_LOCAL 1 0        ; n — one level out the lexical chain
1: op_CONST 1
2: op_GLOBAL +
3: op_CALL 2
4: op_RECV 1
5: op_SET-LOCAL 1 0    ; n ← sum; sum remains
6: op_RETURN 1
```

`BUMP` reaches `n` at `up = 1` because `BUMP`'s function captured
`MAKE-COUNTER`'s frame. Two counters made from two calls have two frames and
therefore two independent `n`s, with no special machinery — the frames were
already separate computations.

### 2.7 A loop — tail recursion

`(fn (n) (if (= n 0) 'done (countdown (- n 1))))`

```
0:  op_LOCAL 0 0
1:  op_CONST 0
2:  op_GLOBAL =
3:  op_CALL 2
4:  op_RECV 1
5:  op_BRANCH-FALSE 8
6:  op_CONST done
7:  op_RETURN 1
8:  op_LOCAL 0 0
9:  op_CONST 1
10: op_GLOBAL -
11: op_CALL 2
12: op_RECV 1
13: op_GLOBAL countdown
14: op_TAILCALL 1
```

Run it with a large `n` and watch the parent chain stay short.

Note that `op_TAILCALL` at 14 is **not** followed by a receiver. It does not return
here; whatever it produces goes to our parent, and our caller's receiver handles
it. That is also how a function returns exactly the values another call produced
— tail-call it.

### 2.8 Multiple values

A function returning two values. `DIVMOD` — arity 2, n-locals 2:

```
0:  op_LOCAL 0 0
1:  op_LOCAL 0 1
2:  op_GLOBAL _fixnum-div
3:  op_CALL 2
4:  op_RECV 1
5:  op_LOCAL 0 0
6:  op_LOCAL 0 1
7:  op_GLOBAL _fixnum-mod
8:  op_CALL 2
9:  op_RECV 1
10: op_RETURN 2          ; quotient and remainder
```

Three callers, differing only in the receiver:

```
    op_CONST 7                 op_CONST 7                 op_CONST 7
    op_CONST 2                 op_CONST 2                 op_CONST 2
    op_CLOSE DIVMOD            op_CLOSE DIVMOD            op_CLOSE DIVMOD
    op_CALL 2                  op_CALL 2                  op_CALL 2
    op_RECV 2                  op_RECV 1                  op_RECV-ALL
    op_GLOBAL +                op_RETURN 1                op_RETURN 1
    op_CALL 2
    op_RECV 1
    op_RETURN 1
```

The first wants both and adds them, giving `4`. The second wants one and the
remainder is discarded, giving `3`. The third collects them into the list
`(3 1)`.

The callee is identical in all three. **How many values a call produces and how
many the caller wants are independent**, and the receiver is where they are
reconciled. That is the whole of the multiple-value protocol.

### 2.9 Two threads

`PING` — arity 0:

```
0: op_CONST "ping"
1: op_GLOBAL print
2: op_CALL 1
3: op_RECV 0
4: op_YIELD
5: op_GOTO 0
```

`PONG` is the same with `"pong"`. Create two threads over these and run the
scheduler; they alternate.

Notice how little `op_YIELD` needed: each thread is a frame, so switching is
assigning a pointer. Nothing was saved or restored, because there is nothing
outside the frame to save.

### 2.10 A dynamic binding

Assume `*out*` is a binding whose `dynamic?` flag is set. `REPORT` — arity 0:

```
0: op_GLOBAL *out*     ; dynamic? set → searches the thread's dynenv first
1: op_RETURN 1
```

A caller that rebinds it:

```
0: op_CONST <binding *out*>    ; bindings are values; the compiler has this one
1: op_CONST "log.txt"
2: op_GLOBAL _push-rebinding
3: op_CALL 2
4: op_RECV 0
5: op_CLOSE REPORT
6: op_CALL 0
7: op_RECV 1
8: op_RETURN 1
```

`REPORT` sees `"log.txt"` even though the cell still holds whatever it held
before, and another thread reading `*out*` sees the cell, because the `dynenv` is
per thread.

This is the mechanism, not the interface. A real `dynamic-let` pairs the push
with an unwind that survives non-local exit, which is `dynamic-wind` and lives in
the library. The kernel supplies the field, the flag, and the branch.

All fifteen instructions have now appeared.

### 2.11 The one that matters — calling something undefined

```
0: op_CONST 7
1: op_GLOBAL bar        ; bar has no value
2: op_CALL 1
3: op_RECV 1
4: op_RETURN 1
```

`op_GLOBAL` finds an unbound binding while `pc` is 1. Under P4 the machine does
**not** unwind: it calls the error hook with the current frame exactly as it is.
Under P5 the hook can see that the faulting instruction was at 1, not 2.

So a debugger can run there, define `bar`, set `pc` back to 1, and resume — and
the call succeeds. The `7` is still in the frame's operand area; nothing was
lost.

This is the whole design in five instructions. Build the machine the conventional
way — signal by unwinding to a host handler, advance `pc` and forget it — and
this program is unrecoverable no matter what you add afterward.

---

## Conventions and contracts

Three things an implementer needs that are not visible in the instruction set.

### Instruction names carry an `op_` sentinel

`op_CONST`, `op_SET-GLOBAL`. The sentinel puts instructions in a namespace of
their own so that none can collide with a symbol of the host language. In Common
Lisp `RETURN` and `CLOSE` did collide, and a host `return` inside the machine's
own loop was once mistaken for the instruction of the same name — identical to
the compiler, misleading to a reader.

The sentinel is lowercase so it recedes and the instruction name is what the eye
lands on. The underscore separates it visibly from names that themselves contain
hyphens. Both survive transliteration to any target language, where a port
converts the remaining hyphens to underscores as it would for any name.

Enforce this rather than remember it: refuse to load if an instruction lacks the
sentinel or shadows a host symbol.

### Dispatch resolves names at compile time, not read time

The inner loop dispatches on a fixnum opcode, and a jump table wants literal
integers as its keys. Writing those integers directly would defeat the point of
naming instructions. Reaching for read-time evaluation to compute them resolves
the names in whatever namespace happens to be current when the form is *read*,
fails outright where read-time evaluation is disabled, and makes the file
unreadable unless its dependencies are already loaded — turning a compile-order
problem into a read-order problem.

Resolve the names at macroexpansion time instead, looking them up by string so
the namespace a clause was written in does not matter, and signalling a real
error on a misspelling rather than silently emitting a clause that can never
fire.

### Failure and resumption

The machine reports a fault by signalling **without unwinding first**, with the
frame and the faulting pc in hand (properties P4 and P5). A handler therefore
runs inside the environment where the fault was discovered, and can see and
repair it. Three restarts say what happens next:

```
retry           run the faulting instruction again, having repaired
                whatever made it fail
supply-value    push a value in place of the failed operation and carry
                on at the next instruction
abort-thread    abandon this thread; the others keep running
```

The restarts are established only when a fault occurs, so ordinary stepping pays
nothing for them.

That third power is what everything else was for. A program calls something
undefined; the handler defines it and retries; the operand the call was about to
use is still on the frame, so the call now succeeds. **A machine that signalled
by unwinding to the host would have destroyed that frame before anyone could look
at it, and no amount of later work would bring it back.**

An implementation gets a usable breakloop before it has written one, provided the
host's own debugger runs handlers before unwinding and offers established
restarts. In Common Lisp it does: an unhandled fault drops you into the debugger
with the frame reachable and `retry`, `supply-value`, and `abort-thread` on the
menu.

### The handler contract

`op_CALL` asks the callee's descriptor for a handler rather than assuming a
bytecode function (property P3). A handler takes the callee, the argument count,
the frame, and the faulting pc, and returns the frame the machine should continue
stepping. That return value carries more than it appears to, and both `op_CALL`
and `op_TAILCALL` depend on it:

```
a NEW frame      the callee is a computation of its own. It will run, and
                 eventually op_RETURN into whatever frame it was given as its
                 parent. Its values arrive there later.

the SAME frame   the callee ran to completion in place. It has already pushed
                 its values and their count onto that frame's operand area.
                 Nothing arrives later.
```

Nothing else distinguishes the two cases, deliberately. `op_TAILCALL` has to tell
them apart: it abandons the caller's frame, so a callee that would have returned
there must be re-pointed at the caller's parent, while a callee that has already
delivered its values in place needs those values forwarded instead.

`op_TAILCALL` asks the returned frame rather than testing the callee's type. That
is why a new kind of applicable — a generic function, a foreign function, native
code — behaves correctly by honouring this contract rather than by being added to
a list of special cases. It is what P3 buys, and it is bought only for as long as
new handlers keep to it.

---

## Part 3 — Constructing the kernel

Eleven stages. Each ends with a program from Part 2 running, so you always know
exactly what you have. This doubles as the conformance ladder.

### Stage 1 — Representations

No interpreter yet. Define the seven things from `kernel.md` §2.

Three details that later stages assume and that are painful to retrofit:

- A **binding** needs a distinct *unbound* state, not a null value (Stage 11
  detects it), and a `dynamic?` flag (Stage 10 uses it).
- A **frame** holds locals and operands in one vector with an `sp`. One
  allocation is what makes a frame a single capturable object.
- A **thread** has a `dynenv` field. Empty for now.

### Stage 2 — The loop, `op_CONST`, `op_RETURN`

Write the dispatch loop: fetch `code[pc]`, **save the fetched `pc`**, increment,
dispatch. Saving the pre-increment `pc` is P5 and costs one variable. Do it now;
every later stage assumes it exists.

Implement `op_CONST`, and `op_RETURN n` for the nil-parent case that ends the thread.

→ **2.1 runs.**

### Stage 3 — Bindings, primitive calls, receivers

Implement `op_GLOBAL`, `op_SET-GLOBAL`, `op_DROP`, `op_RECV k`, and only the *descriptor*
branch of `op_CALL`: if the callee is a primitive, consume its arguments, push its
result and a count of 1. Install `_fixnum-add`, `_fixnum-sub`, `_fixnum-mul`,
`_fixnum-lt`, `_fixnum-eq`, and bind `+`, `-`, `*`, `<`, `=` to them for now.

Primitives push a count like everything else. Uniformity here is what lets a
receiver follow *any* call without knowing what it called.

→ **2.2 and 2.4 run.**

You have a calculator whose operators are redefinable. That is a larger step than
it looks.

### Stage 4 — Control

Implement `op_GOTO` and `op_BRANCH-FALSE`.

→ **2.3 runs.**

### Stage 5 — Frames and calls

Implement `op_CLOSE`, `op_LOCAL` with `up = 0`, and `op_CALL`'s function branch: arity
check, frame allocation, argument move, parent link, switch. Extend `op_RETURN n` to
the non-nil-parent case. This is the largest stage; everything before it was
preparation.

→ **2.5 runs.**

### Stage 6 — The lexical chain

Implement `op_SET-LOCAL`, and `op_LOCAL` with `up > 0` walking `fn.captured-frame`.

→ **2.6 runs.**

### Stage 7 — Tail calls

Implement `op_TAILCALL`. Test with a countdown large enough that the non-tail
version would exhaust memory.

→ **2.7 runs.**

### Stage 8 — Multiple values

`op_RETURN n` for `n ≠ 1`, `op_RECV k` for `k ≠ 1`, and `op_RECV-ALL`. Most of this
already works if Stages 2 and 3 were written generally rather than special-cased
to one value; if they were not, this is where you find out.

→ **2.8 runs, all three variants.**

### Stage 9 — Threads

Add `op_YIELD` and two primitives: make a thread over a function, and run the
scheduler. Round-robin over a list is enough; anything smarter belongs above the
kernel.

→ **2.9 runs.**

### Stage 10 — The dynamic environment

Make `op_GLOBAL` and `op_SET-GLOBAL` consult `dynamic?` and search the thread's
`dynenv`. Add primitives to push and pop rebindings.

→ **2.10 runs.**

All fifteen instructions are implemented. What remains is not an instruction.

### Stage 11 — The error hook

Route every failure — unbound binding, arity mismatch, a primitive's own error,
an unknown opcode — through a single hook, called with the current frame intact
and **before** anything unwinds. Give it the saved faulting `pc` from Stage 2.

Give the hook three powers: abort the thread, return a value in place of the
failed operation, or set `pc` back to the faulting instruction and resume.

→ **2.11 runs**, and you can define `bar` and continue.

That third power is the whole design. Everything deferred out of the kernel — the
condition system, restarts, the breakloop, the debugger, remote attach — is built
from a hook that can resume, over frames that are values.

---

## What you have

Fifteen instructions, one register, seven representations, one hook.

What it lacks — an object system, generic functions, conditions, a debugger,
modules, a scheduler worth the name, a compiler — is not missing so much as not
yet written, and each is reachable from what is here. `kernel.md` §5 names the
property that keeps each one reachable. You do not need the argument for it to
build this.
