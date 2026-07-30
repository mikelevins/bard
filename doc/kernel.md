# The Kernel

This is the whole thing to learn. If something is not in this file, you do not
need it to write a working kernel from memory.

- `kernel-tutorial.md` — what each instruction means, worked programs, staged
  construction.
- `performance.md` — strategies for the costs this design accepts. Consult, do
  not memorize.
- `decisions.md` — adopted design decisions for the language above the machine.

**Scope.** The kernel language is s-expressions. An infix surface syntax is not a
kernel concern and may never exist.

**Guard.** Nothing in this file may depend on any other document. If a kernel
decision needs deferred or external material to justify it, either the
justification is wrong or the thing is not really deferred. This rule exists
because a deferred syntax decision once leaked in and constrained a naming
convention here.

**Size reference.** Norvig's `machine` from *Paradigms of AI Programming* — 19
instructions, 5 registers, 7 representations — is a yardstick for cognitive load
and nothing else. Nothing below is derived from it; the counts are compared
against it afterward.

---

## The organizing idea

> **The machine steps a reified computation. A frame is that computation.
> Everything else follows.**

A breakloop is a frame you kept. A continuation is a frame you kept. A thread is
a frame you kept. Resuming any of them assigns one register.

This is a test, not a slogan. **Anything in the machine that does not follow from
it is suspect and should be re-derived.**

---

## 1. Machine state

**One register: the current frame.**

There is no operand-stack register, no environment register, and no
argument-count register. A computation's entire state lives in its frame, because
a computation must be a *value* — you hold several at once (threads), you keep
one aside (a breakloop), you hand one around (a continuation).

An OS thread runs one machine loop with its own current-frame register. Green
threads are one loop reassigning that register. Both work, with no additional
machinery.

---

## 2. Representations

Seven.

**1. Frame** — the computation.

```
parent   the frame to return into — this chain is the continuation
fn       what is running
pc       where in fn's code
slots    one vector: locals in the low positions, operand stack above
sp       top of the operand area
```

Operands live *in the frame*, not in a machine-wide stack, so a captured frame is
self-contained.

**2. Function** — `code` and `captured-frame`.

A closure captures the frame it was created in, so **environments and frames are
the same representation**. There are exactly two chains and both are obvious:
`parent` is dynamic, `captured-frame` is lexical.

**3. Code** — a vector of fixed-width instructions, plus `arity`, `rest?`,
`n-locals`, and `frame-size`. Fixed width so decoding is a load and a shift, with no branch
before the dispatch branch. `frame-size` is `n-locals` plus the maximum operand
depth, which the compiler knows — so allocating a frame is adding a constant.

**4. Binding** — a mutable cell for a global name, which may be *unbound*, plus
a `dynamic?` flag. The unbound state is not optional; it is what P4 and P5 make
useful. The flag is false for almost every binding and is what keeps dynamic
binding off the fast path (§3.3).

**5. Descriptor** — reachable from every value. The kernel stores, copies, and
compares descriptors; it never interprets one.

**6. Primitive** — `descriptor`, host procedure, `arity`.

**7. Thread** — a current frame, a status, and a `dynenv`: a list of
`(binding . value)` rebindings in effect for this thread. Almost always empty.

---

## 3. Instructions

Fifteen.

### Values

| | |
|---|---|
| `op_CONST k` | push constant `k` |
| `op_LOCAL up slot` | push `slot`, `up` levels out along the lexical chain |
| `op_GLOBAL b` | push the value in binding `b` |

### Stores

| | |
|---|---|
| `op_SET-GLOBAL b` | store the top into binding `b`; does not pop |
| `op_SET-LOCAL up slot` | store the top into a lexical slot; does not pop |
| `op_DROP` | discard the top |

### Control

| | |
|---|---|
| `op_GOTO n` | `pc ← n` |
| `op_BRANCH-FALSE n` | pop; if false, `pc ← n` |

### Functions

| | |
|---|---|
| `op_CLOSE k` | push a closure over code `k`, capturing the current frame |
| `op_CALL n` | call with `n` arguments |
| `op_TAILCALL n` | as `op_CALL`, without linking a new parent |
| `op_RETURN n` | return `n` values into `parent`; if `parent` is nil, the thread ends |
| `op_RECV k` | receive a return: adjust the delivered values to exactly `k` |
| `op_RECV-ALL` | receive a return: collect all delivered values into one list |

### Concurrency

| | |
|---|---|
| `op_YIELD` | switch the current frame to another runnable thread |

### 3.1 What `op_CALL` does

It is the only instruction that does much:

```
op_CALL n:
    callee ← pop
    if callee is a function:
        check n against callee.arity and callee.rest?
        allocate a frame of callee.code.frame-size
        move n operands into its low slots
        frame.fn ← callee;  frame.pc ← 0;  frame.parent ← current
        current ← frame
    else:
        dispatch on callee's descriptor
```

Allocating the frame *is* what calling means, once a frame is the computation —
so there is no separate argument instruction and no argument-count register. A
callee declaring a rest parameter gets the extra arguments consed into the slot
just past its required ones, which is the only place that convention lives.
`op_TAILCALL` is identical except that the new frame's `parent` is the *caller's*
parent, so the caller's frame is abandoned.

### 3.2 Multiple values

`op_RETURN n` moves the top `n` values of the current frame's operand area into the
parent's, then pushes `n` itself. So after any return the parent sees `n` values
with the count on top.

The count travels **on the operand stack**, not in a register. A register would
have to be saved and restored across every capture, every thread switch, and
every breakloop — which is the argument-count mistake in a new costume. The
frame already holds everything else; the count belongs there too.

**Every call site is followed by a receiver**, because the caller cannot know
statically how many values a callee produced. `op_RECV k` pops the count and adjusts
what is beneath it to exactly `k` — padding with `nothing`, discarding extras.
`op_RECV-ALL` pops the count and collects that many values into a single list.
Two receivers cover every case a compiler encounters: `k = 1` for an operand,
`k = 0` for a discarded statement, `k = j` for a fixed multiple-value binding,
and `op_RECV-ALL` for value lists and apply.

A static verifier can check that every call is followed by a receiver; without
one, the operand area's shape is not predictable.

**Pass-through is tail position.** A function that wants to return exactly the
values some other call produced tail-calls it. That is why `op_RETURN` takes a
static count and needs no dynamic form.

### 3.3 The dynamic environment

`op_GLOBAL b` reads `b`'s cell **unless** `b.dynamic?` is set, in which case it
searches the current thread's `dynenv` for the innermost rebinding of `b` and
falls through to the cell if there is none. `op_SET-GLOBAL` mirrors it.

The check is a flag test on an object already loaded, false for nearly every
binding, so the fast path costs a predictable branch. Rebinding, unwinding, and
`dynamic-wind` are library concerns built on a primitive that pushes and pops
`dynenv` entries; the kernel supplies only the field and the branch.

Rebindings are **per thread**. A spawned thread starts with an empty `dynenv`.

---

## 4. What is absent, and why

| Absent | Because |
|---|---|
| Operand-stack, environment, argument-count registers | the frame holds them |
| A separate argument instruction | allocating the frame is the call |
| A direct primitive-application instruction | `op_CALL` dispatches on the descriptor |
| A halt instruction | `op_RETURN` into a nil parent ends the thread |
| A return-count register | the count rides on the operand stack |
| Continuation capture and restore instructions | a continuation *is* `parent`; both are primitives |

None of these were removed. None were ever required.

---

## 5. Properties

Not features. Properties the machine must have so that larger things stay
buildable. Each costs approximately nothing now; none can be retrofitted.

**P1 — Every value carries a descriptor**, uninterpreted by the kernel.
*Keeps open:* types, dispatch, the object system, instance migration, inspection,
printing.

**P2 — Global names resolve through a binding object**, never directly to a
value. *Keeps open:* redefinition while running, dependency tracking, dynamic
variables, modules.

**P3 — `op_CALL` dispatches on the callee's descriptor** rather than assuming a
bytecode function. *Keeps open:* generic functions, execution tiering, foreign
functions, native-compiled functions, lazily recompiled stale functions.

**P4 — Errors call a hook *before* unwinding, with the frame intact.** An error
must drop you into a full REPL inside the dynamic environment of the failure. A
machine that signals by unwinding to a host handler has destroyed that
environment before anyone can look, and nothing added later recovers it. Costs no
instruction — it is a discipline about how failure is reported.

**P5 — The faulting instruction's `pc` is recoverable**, so an instruction can be
retried after repair. The machine advances `pc` before dispatch; the previous
value must reach the hook. This is what makes the headline case work: call an
undefined function, define it in the resulting breakloop, resume, and have the
call succeed. Without it you can inspect a failure but never continue through
one.

**P6 — No observable value is derived from an address.** Identity hashes are
stored, not computed from location. *Keeps open:* a moving or generational
collector. Violating this closes that door permanently.

**P7 — Mutation goes through a small closed set of primitives.**
*Keeps open:* write barriers, hence generational collection.

And the organizing idea itself is the eighth, and the one never to trade away for
speed: **frames are heap objects, never native stack frames.** That is what makes
breakloops, continuations, threads, remote inspection, and portability to every
target possible at the same time.

---

## 6. Deliberately deferred

| Deferred | Added later via | Safe because |
|---|---|---|
| Direct primitive application | one instruction | pure addition; the first thing to add |
| Second conditional branch | one instruction | the compiler inverts the test |
| Slot access instructions | two instructions | available as primitives; only speed |
| Superinstructions, inline caches | production kernel only | see `performance.md` |
| Object system, generic functions | library | P1, P3 |
| Conditions, restarts, breakloop | library | P4, P5, frames as values |
| Redefinition tracking | library | P2 |
| Modules | library | P2 |
| Scheduler, thread creation | library and primitives | `op_YIELD`, thread representation |
| Native compilation, sealing, remote attach | separate work | P3, P6, P7 |

---

## 7. Count

| | Norvig (yardstick) | this |
|---|---|---|
| Instructions | 19 | **15** |
| Registers | 5 | **1** |
| Representations | 7 | **7** |
| Properties | — | **7 + the organizing idea** |

The two accepted costs — dispatch misprediction, and a heap frame per call — are
structural and deliberate. `performance.md` holds the standing strategy for
reducing both without touching anything above.
