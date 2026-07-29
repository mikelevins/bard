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

**`CONST k`** — push constant `k`.
The only instruction that produces a value from nothing; everything else consumes
something.

**`LOCAL up slot`** — push a value from the lexical chain.
`up` counts levels outward: `0` is the current frame's own slots, `1` is
`fn.captured-frame`, `2` is that frame's `fn.captured-frame`, and so on. Local
variables and captured variables are read by the same instruction — a captured
variable is just a larger `up`.

**`GLOBAL b`** — push the value held in binding `b`.
Note what this does *not* do: it does not push a value the compiler knew. It
reads a mutable cell at run time. That indirection is the entire reason a
redefinition takes effect in code compiled before it.

If `b.dynamic?` is set, `GLOBAL` first searches the current thread's `dynenv` for
the innermost rebinding of `b`, falling through to the cell if there is none. The
flag is false for nearly every binding, so the ordinary path costs one
predictable branch on an object already in hand.

### Stores

**`SET-GLOBAL b`** — store the top into binding `b`. **Does not pop.**
This is definition and redefinition, as one instruction. It mirrors `GLOBAL` with
respect to `dynamic?`: a rebound dynamic binding is assigned in the `dynenv`, not
in the cell.

**`SET-LOCAL up slot`** — store the top into a lexical slot. **Does not pop.**

Both leave their value so an assignment can be used as an expression. For effect
only, follow with `DROP`.

**`DROP`** — discard the top. Sequencing.

### Control

**`GOTO n`** — set `pc` to `n`.

**`BRANCH-FALSE n`** — pop; if false, set `pc` to `n`.
One conditional is enough; the compiler inverts the test when it wants the other
sense.

### Functions

**`CLOSE k`** — push a closure over code object `k`, capturing the **current
frame**. That capture is what makes it a closure rather than a code pointer, and
it is why frames and environments need only one representation.

**`CALL n`** — push arguments left to right, then the callee, then `CALL n`.

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

**`TAILCALL n`** — identical, except the new frame's `parent` is the *caller's*
parent. The caller's frame is simply abandoned, so a loop written as tail
recursion runs in constant space.

**`RETURN n`** — move the top `n` values of the current frame's operand area into
the parent's, then push `n` itself; set `current ← current.parent`. **If `parent`
is nil the thread is finished** and those values are its result. There is no halt
instruction because thread termination already is one.

**`RECV k`** — pop the count; adjust the values beneath it to exactly `k`,
padding with `nothing` and discarding extras.

**`RECV-ALL`** — pop the count; collect that many values into one list and push
it.

**Every call site is followed by a receiver.** The caller cannot know statically
how many values a callee produced, so the operand area's shape is unpredictable
until a receiver fixes it. `RECV 1` is the overwhelmingly common case — a value
used as an operand. `RECV 0` discards a call made for effect. `RECV j` receives a
fixed multiple-value binding. `RECV-ALL` handles value lists and apply.

A tail call is the exception: it does not return to you, so *your* caller's
receiver handles its values. A `TAILCALL` is never followed by anything.

The count rides on the operand stack rather than in a register, because a
register would have to be saved and restored across every capture, thread switch,
and breakloop — the argument-count mistake in a new costume.

### Concurrency

**`YIELD`** — hand the machine to another runnable thread.

Because a thread's entire state is its current frame, a switch is: store the
current frame into this thread, pick another, load its frame. Green threads are
one machine loop doing that; OS threads are several machine loops each with their
own current frame. The instruction is the same either way.

---

## Part 2 — Programs

Each is minimal and adds one thing. A program is the code of a function invoked
with a nil parent, so its `RETURN` ends the thread and yields the result.

### 2.1 A constant

```
0: CONST 42
1: RETURN 1
```

`CONST`, `RETURN`, and thread termination.

### 2.2 Calling a primitive — `(+ 2 3)`

```
0: CONST 2
1: CONST 3
2: GLOBAL +
3: CALL 2
4: RECV 1
5: RETURN 1
```

`GLOBAL`, `CALL`'s descriptor branch, and a receiver. The handler for a primitive
consumes its arguments, pushes its results and a count, and creates no frame —
the call returns immediately. `RECV 1` then fixes the shape to one value.

`+` arrived through a binding. Rebind it and this already-compiled code calls the
new one. That is the point of P2.

Note that `+` is a *language* name — polymorphic, redefinable, defined in the
prelude. The baked-in operation underneath is `_fixnum-add`: monomorphic,
fixed-arity, sigil-prefixed to keep it out of the language's namespace. There is
no single addition primitive; there are several, and `+` is what dispatches among
them. All polymorphism lives above the kernel.

### 2.3 A conditional — `(if (< n 3) "small" "big")`

```
0: GLOBAL n
1: CONST 3
2: GLOBAL <
3: CALL 2
4: RECV 1
5: BRANCH-FALSE 8
6: CONST "small"
7: GOTO 9
8: CONST "big"
9: RETURN 1
```

### 2.4 Sequencing — `(begin (set! x 10) (set! y 20) (+ x y))`

```
0:  CONST 10
1:  SET-GLOBAL x      ; 10 remains on the stack
2:  DROP
3:  CONST 20
4:  SET-GLOBAL y
5:  DROP
6:  GLOBAL x
7:  GLOBAL y
8:  GLOBAL +
9:  CALL 2
10: RECV 1
11: RETURN 1
```

The `SET-GLOBAL`/`DROP` pair is what a top-level definition compiles to. Defining
something is an ordinary instruction, not a special mode.

Note the two ways to discard: `DROP` removes a value already on the stack;
`RECV 0` discards whatever a call returned. They are not interchangeable.

### 2.5 A function — `((fn (x) (* x x)) 7)`

Code object `SQUARE` — arity 1, n-locals 1:

```
0: LOCAL 0 0
1: LOCAL 0 0
2: GLOBAL *
3: CALL 2
4: RECV 1
5: RETURN 1
```

Main:

```
0: CONST 7
1: CLOSE SQUARE
2: CALL 1
3: RECV 1
4: RETURN 1
```

`SQUARE` has no prologue. `CALL` already placed the argument in slot 0, because
building the frame is the call.

Trace it once. `CALL 1` allocates a frame, moves `7` into slot 0, links `parent`
to main's frame, and assigns `current`. `RETURN 1` moves `49` into main's operand
area, pushes the count `1`, and restores `current`. `RECV 1` pops the count and
leaves `49`.

### 2.6 A closure over a mutable variable — a counter

`(fn (n) (fn () (set! n (+ n 1))))`

`MAKE-COUNTER` — arity 1, n-locals 1:

```
0: CLOSE BUMP
1: RETURN 1
```

`BUMP` — arity 0, n-locals 0:

```
0: LOCAL 1 0        ; n — one level out the lexical chain
1: CONST 1
2: GLOBAL +
3: CALL 2
4: RECV 1
5: SET-LOCAL 1 0    ; n ← sum; sum remains
6: RETURN 1
```

`BUMP` reaches `n` at `up = 1` because `BUMP`'s function captured
`MAKE-COUNTER`'s frame. Two counters made from two calls have two frames and
therefore two independent `n`s, with no special machinery — the frames were
already separate computations.

### 2.7 A loop — tail recursion

`(fn (n) (if (= n 0) 'done (countdown (- n 1))))`

```
0:  LOCAL 0 0
1:  CONST 0
2:  GLOBAL =
3:  CALL 2
4:  RECV 1
5:  BRANCH-FALSE 8
6:  CONST done
7:  RETURN 1
8:  LOCAL 0 0
9:  CONST 1
10: GLOBAL -
11: CALL 2
12: RECV 1
13: GLOBAL countdown
14: TAILCALL 1
```

Run it with a large `n` and watch the parent chain stay short.

Note that `TAILCALL` at 14 is **not** followed by a receiver. It does not return
here; whatever it produces goes to our parent, and our caller's receiver handles
it. That is also how a function returns exactly the values another call produced
— tail-call it.

### 2.8 Multiple values

A function returning two values. `DIVMOD` — arity 2, n-locals 2:

```
0:  LOCAL 0 0
1:  LOCAL 0 1
2:  GLOBAL _fixnum-div
3:  CALL 2
4:  RECV 1
5:  LOCAL 0 0
6:  LOCAL 0 1
7:  GLOBAL _fixnum-mod
8:  CALL 2
9:  RECV 1
10: RETURN 2          ; quotient and remainder
```

Three callers, differing only in the receiver:

```
    CONST 7                 CONST 7                 CONST 7
    CONST 2                 CONST 2                 CONST 2
    CLOSE DIVMOD            CLOSE DIVMOD            CLOSE DIVMOD
    CALL 2                  CALL 2                  CALL 2
    RECV 2                  RECV 1                  RECV-ALL
    GLOBAL +                RETURN 1                RETURN 1
    CALL 2
    RECV 1
    RETURN 1
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
0: CONST "ping"
1: GLOBAL print
2: CALL 1
3: RECV 0
4: YIELD
5: GOTO 0
```

`PONG` is the same with `"pong"`. Create two threads over these and run the
scheduler; they alternate.

Notice how little `YIELD` needed: each thread is a frame, so switching is
assigning a pointer. Nothing was saved or restored, because there is nothing
outside the frame to save.

### 2.10 A dynamic binding

Assume `*out*` is a binding whose `dynamic?` flag is set. `REPORT` — arity 0:

```
0: GLOBAL *out*     ; dynamic? set → searches the thread's dynenv first
1: RETURN 1
```

A caller that rebinds it:

```
0: CONST <binding *out*>    ; bindings are values; the compiler has this one
1: CONST "log.txt"
2: GLOBAL _push-rebinding
3: CALL 2
4: RECV 0
5: CLOSE REPORT
6: CALL 0
7: RECV 1
8: RETURN 1
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
0: CONST 7
1: GLOBAL bar        ; bar has no value
2: CALL 1
3: RECV 1
4: RETURN 1
```

`GLOBAL` finds an unbound binding while `pc` is 1. Under P4 the machine does
**not** unwind: it calls the error hook with the current frame exactly as it is.
Under P5 the hook can see that the faulting instruction was at 1, not 2.

So a debugger can run there, define `bar`, set `pc` back to 1, and resume — and
the call succeeds. The `7` is still in the frame's operand area; nothing was
lost.

This is the whole design in five instructions. Build the machine the conventional
way — signal by unwinding to a host handler, advance `pc` and forget it — and
this program is unrecoverable no matter what you add afterward.

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

### Stage 2 — The loop, `CONST`, `RETURN`

Write the dispatch loop: fetch `code[pc]`, **save the fetched `pc`**, increment,
dispatch. Saving the pre-increment `pc` is P5 and costs one variable. Do it now;
every later stage assumes it exists.

Implement `CONST`, and `RETURN n` for the nil-parent case that ends the thread.

→ **2.1 runs.**

### Stage 3 — Bindings, primitive calls, receivers

Implement `GLOBAL`, `SET-GLOBAL`, `DROP`, `RECV k`, and only the *descriptor*
branch of `CALL`: if the callee is a primitive, consume its arguments, push its
result and a count of 1. Install `_fixnum-add`, `_fixnum-sub`, `_fixnum-mul`,
`_fixnum-lt`, `_fixnum-eq`, and bind `+`, `-`, `*`, `<`, `=` to them for now.

Primitives push a count like everything else. Uniformity here is what lets a
receiver follow *any* call without knowing what it called.

→ **2.2 and 2.4 run.**

You have a calculator whose operators are redefinable. That is a larger step than
it looks.

### Stage 4 — Control

Implement `GOTO` and `BRANCH-FALSE`.

→ **2.3 runs.**

### Stage 5 — Frames and calls

Implement `CLOSE`, `LOCAL` with `up = 0`, and `CALL`'s function branch: arity
check, frame allocation, argument move, parent link, switch. Extend `RETURN n` to
the non-nil-parent case. This is the largest stage; everything before it was
preparation.

→ **2.5 runs.**

### Stage 6 — The lexical chain

Implement `SET-LOCAL`, and `LOCAL` with `up > 0` walking `fn.captured-frame`.

→ **2.6 runs.**

### Stage 7 — Tail calls

Implement `TAILCALL`. Test with a countdown large enough that the non-tail
version would exhaust memory.

→ **2.7 runs.**

### Stage 8 — Multiple values

`RETURN n` for `n ≠ 1`, `RECV k` for `k ≠ 1`, and `RECV-ALL`. Most of this
already works if Stages 2 and 3 were written generally rather than special-cased
to one value; if they were not, this is where you find out.

→ **2.8 runs, all three variants.**

### Stage 9 — Threads

Add `YIELD` and two primitives: make a thread over a function, and run the
scheduler. Round-robin over a list is enough; anything smarter belongs above the
kernel.

→ **2.9 runs.**

### Stage 10 — The dynamic environment

Make `GLOBAL` and `SET-GLOBAL` consult `dynamic?` and search the thread's
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
