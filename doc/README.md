# Bard documentation

Five documents. What each is, and when to read it.

| | |
|---|---|
| [`vision.md`](vision.md) | What Bard is and what "good" means. Read once; consult when a design decision is genuinely undecided and you need a tiebreaker. |
| [`kernel.md`](kernel.md) | The machine: 15 instructions, one register, seven representations, seven properties. **This is the document to hold in mind.** If something is not in it, you do not need it to write a working kernel from memory. |
| [`kernel-tutorial.md`](kernel-tutorial.md) | What each instruction means, eleven worked programs, and an eleven-stage construction that doubles as a conformance ladder. Read when implementing or porting the machine. |
| [`performance.md`](performance.md) | The two costs this design knowingly accepts, the strategies for reducing them, and a standing track of candidates. Consult; do not memorize. |
| [`decisions.md`](decisions.md) | Adopted design decisions for the language above the machine, with rationale. Consult constantly while implementing. |

## Reading order

Starting from nothing: `vision.md`, then `kernel.md`, then `kernel-tutorial.md`.
That is enough to build a working machine.

Implementing the language on top of it: `decisions.md` becomes the working
document, with `kernel.md` as the reference for what the machine guarantees.

Making it fast: `performance.md`, and nothing in it may change `kernel.md`.

## One rule

`kernel.md` does not depend on any other document here. If a kernel decision
seems to need justification from elsewhere, either the justification is wrong or
the thing it depends on is not really outside the kernel. That rule exists
because a deferred decision once leaked in and quietly constrained a naming
convention in the machine.
