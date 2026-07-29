# Adopted Design Decisions

Decisions about the language above the machine, with rationale. The machine
itself is specified in `kernel.md` and argued for there; this document covers
everything built on it.

**Status: being populated.** Decisions reached during the design of the kernel
and during earlier work on Bard are being reviewed and migrated here. A decision
appears in this document once it has been re-examined against the current design
rather than inherited from it.

Each entry should record what was decided, why, and what it would cost to
reverse. A decision without a reason is a preference, and preferences do not
survive contact with the next design question.

---

## Settled at the machine boundary

These are recorded here because they constrain the language even though they are
visible in the machine.

**Primitives are not the operators.** `+` is a language-level name: polymorphic,
redefinable, defined in the prelude. The baked-in operation is a different
thing — monomorphic, fixed-arity, type-specific — and there is no single addition
primitive but several. Primitives therefore observe a naming convention that
keeps them out of the language's namespace: a leading underscore, as in
`_fixnum-add`. The consequence worth remembering is that **the primitive set is
monomorphic and all polymorphism lives above the machine.**

**Multiple return values are a day-one concern**, not a later addition. `RETURN
n` delivers `n` values and a count; every call site is followed by a receiver.
Retrofitting this onto an existing calling convention has already cost one
implementation restart and will not be attempted again.

**Dynamic binding is opt-in.** Only bindings declared dynamic consult the
thread's dynamic environment; ordinary bindings read and assign their cell
directly. Rebindings are per thread, and a spawned thread starts with none.

**The kernel language is s-expressions.** An infix surface syntax is not a
machine concern and may never exist; that question stays open until the language
satisfies everything else.

---

## To be migrated

Language-level decisions from earlier work — the place model, the numeric tower,
string types, the actor and task model, serialization, the condition system, the
module system — are under review and will be added here as each is re-examined
against the current design.
