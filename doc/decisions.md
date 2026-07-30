# Adopted Design Decisions

Decisions about the language above the machine. The machine itself is specified
in `kernel.md` and argued for there.

**How to read this.** Each decision records what was decided and why. A decision
without a reason is a preference, and preferences do not survive contact with the
next design question. Entries marked *(open)* are genuinely undecided, not merely
unwritten.

Decisions reached during earlier work were re-examined against the current
machine before being recorded here; §9 lists what did not survive that
re-examination and why.

---

## 1. Governing principles

These decide the others when the others are close.

### 1.1 Smallness

Bard must be small: few instructions, a VM hot loop that stays in cache, on the
order of a dozen foundational concepts, graspable by one person in two or three
sittings, portable in about a thousand lines.

**Why.** The project has a single steward whose capacity is finite and will not
grow. Bard must fit one mind, over time, with interruptions. Every other goal is
subordinate to *one person can hold it*.

**Re-graspability is co-equal with smallness.** Intermittent stewardship means
the written record must let the mental model be rebuilt from cold, so
rationale-bearing documents are part of the design, not commentary on it. It also
means preferring proven mechanisms from Common Lisp, Dylan, and Clojure over
novel ones: less to hold, and understanding transfers.

**Operational form.** Every addition to the machine or the primitive set faces a
standing test — *earn your place, or be prelude*. Pushing richness into the
prelude costs nothing against the budget; adding to the machine costs
permanently.

**The budgets may be adjusted; they may not be abandoned.** The figures were set
from intuition and may move as the realized design shows what is actually
irreducible. A revision is a deliberate, recorded act — never a quiet relaxation
under pressure.

**Current standing against the budget:** 15 instructions, 1 register, 7
representations, 7 properties. Comfortably inside "a few dozen instructions."
Reporting against the budget is required of every stage, not optional.

### 1.2 Conformance is semantic; performance is incremental

**Correctness, not performance, defines conformance.** An implementation is Bard
when its semantics are correct — including with no dispatch caching whatsoever.
Performance infrastructure is never a conformance gate.

**But the design must admit performance work gracefully.** Optimization
subsystems are often genuinely hard and arrive late; the design must let them be
introduced and improved gradually without disturbing the language or the
development environment. Plan the seams up front, fill them in over time.

Expected to generalize beyond dispatch caching — treat it as a general stance
with caching as the first worked instance. `performance.md` is this principle
applied to the machine.

---

## 2. Architecture

**One language.** There is no Kernel-Bard/Full-Bard distinction. There is Bard,
and a **bootstrap core**: the minimal subset implemented on the host to start the
system and keep porting cheap. The core is not a separate language.

**Self-hosting defines the core.** The core is exactly the minimal subset
sufficient to express the rest of Bard in Bard — not a list chosen by taste.

**Two layers, not three:** the machine's instruction surface, and Bard.

**Assembly surface (goal).** The instruction surface should be a complete,
interactively programmable assembly language in its own right — a text syntax, an
assembler, and an interactive monitor path — sufficient with the machine to build
the core, the language, and the tools by hand if it came to that. Not the
preferred path, but a real one. The standing test is *"could the core be written
in it?"* Consequence: the core is host-implemented by preference but
assembly-implementable in principle, so only the machine is irreducibly
host-specific. The strongest porting story is **port the machine; the assembled
core and the Bard prelude ride along as portable artifacts.**

**Macros are in the core**, `defmacro`-style and **unhygienic**. Expander,
registry, and `gensym` are core; the macro library is prelude.

**Auto-gensym adopted** — Clojure's `name#` inside backquote yields a fresh,
name-prefixed gensym, consistent within one backquote form and fresh across
forms; read-time, with quasiquote as a core reader desugaring. **Auto-gensym
only** — no namespace auto-qualification. Backquote stays CL-style
non-qualifying.

---

## 3. Naming conventions

**Formal sigils**, each with exactly one meaning:

| | |
|---|---|
| `<angle-brackets>` | concrete types |
| CamelCase | roles and protocols |
| `_`-prefix | primitive / low-level, flat names only |
| `?`-suffix | predicates |
| ALL_CAPS | machine instructions |
| `&` | variadic: `(method (a b & rest) ...)`, `(method (& all) ...)` |
| `:name` | keywords |
| `datatype.slot` | monomorphic concrete-type accessor, public, underscore-free |

The **dot** means "concrete-type accessor" and nothing else; it is a name
separator, not call sugar. The reader treats `.` as a symbol constituent except
when whitespace-isolated, where it makes a dotted pair. Monomorphic accessors
ground the polymorphic protocol functions.

**Privacy is a module-export concern, not a sigil.**

**Primitives are not the operators.** `+` is a language-level name: polymorphic,
redefinable, prelude. The baked-in operation is monomorphic, fixed-arity, and
type-specific — `_fixnum-add` — and there are several addition primitives, not
one. All polymorphism lives above the machine.

**Informal conventions**, inherited from Lisp, carrying intent but no
language-enforced meaning. Enforcement, where it exists, is a separate
declaration; name-as-intention and declaration-as-enforcement may agree or
deliberately diverge.

- `*name*` — a global intended to be assigned or rebound; pairs with opt-in
  dynamic (§4).
- `+name+` — a global intended as constant.
- `$name` — a throwaway or file-private global.
- `%name` — a private, narrow-context function.

These coexist with the formal sigils: `%foo` (private function) is distinct from
`_foo` (primitive).

---

## 4. Variables, assignment, setters

**Terminology.** A **place** is any settable location — the target of `set!` and
of setters. A **dynamic variable** is an opt-in dynamically-rebindable cell.
"String" is not a type name, only an informal gloss. Avoid "global variable";
modules and actors make it ill-defined.

**Dynamic variables are opt-in.** Only declared cells are rebindable; ordinary
cells read and assign directly, never consulting the dynamic environment.
Rebindings are per task, over an actor-shared base cell. A spawned task starts
with a clean dynamic environment. `dynamic-wind` is the primitive; `dynamic-let`
and a `defparameter` analogue are prelude.

*Realized in the machine:* `kernel.md` §3.3 — a `dynamic?` flag on the binding, a
`dynenv` on the thread, one branch in `op_GLOBAL`.

**`set!` is a shape-dispatched special form**, Dylan/CL style. A bare symbol
stores — to a lexical slot or a place, consulting the dynamic environment for
dynamic variables, hitting the innermost binding and falling through to the base.
`(getter args…)` rewrites to `((setter getter) new-value args…)`, new value
first.

The form **never refuses on shape grounds**. It is not infallible: a well-formed
`set!` to an immutable place signals at run time. **Immutability, not shape, is
the only thing that makes a well-formed `set!` fail.**

No first-class setters for bare variables — a fixed-location setter is just a
closure. Reify only compound-place setters.

**Setter layering.** The `((setter g) v a…)` rewrite is uniform at both layers.
`setter` is a primitive flat getter→setter table in the core and a generic in the
prelude; the compiler peepholes built-ins. The prelude extends settability by
**registering, not recompiling**. `setter` is itself a settable place.

**Immutability is reserved, not pervasive** *(open)*. Two intended forms: types
immutable by construction, and variables declared immutable at definition time,
with escape hatches that preserve livecoding. "Declared immutable" is a second
per-definition property alongside "dynamic"; how the two interact is undecided.

---

## 5. The value substrate

**Immutable by construction:** fixnum, float, character (a Unicode code point),
symbol, `nothing` (which is also false, the empty list, and the empty
collection), `true`, and strings.

**Mutable:** cons (car and cdr are places), vector (elements are places), and
`string-buffer` — a mutable accumulator used *only* to construct strings.
**Strings themselves are never mutable**; the buffer is a construction helper,
not a mutable string.

**Keywords follow the CL model:** self-evaluating symbols in a single global
keyword namespace, not a separate type, naming no place, with canonical `eq?`
identity. Not namespace-qualified — Clojure's qualified keywords are rejected —
and they stay global once modules exist. `::` is unused for keywords.

**Numeric floor:** fixnum and float are primitive. Bignum, ratio, and complex are
prelude tower classes. Fixnum overflow signals a condition that the prelude's `+`
turns into promotion. Staging: a first REPL may ship fixnum-only, with float soon
after.

**Reflective floor (core):** primitive instances — a raw slotted record plus type
objects — a primitive table/map, and `apply`. This is the floor over which the
prelude's classes, generics, dispatch, and namespaces are written.

**The applicable family.** Methods, functions, macros, and special forms are all
variants of a notional *applicable*. That supertype does **not** literally exist
in the core, which implements no subtyping, but most likely does exist in the
object system. Implementation and type-system representation come apart on
purpose: special forms are implemented specially by the compiler yet
*represented* as a kind of applicable; macros are implemented by recognizing a
defined macro at expansion time and applying its expander, yet likewise
*represented* as a kind of applicable.

*Held since roughly Bard 0.3, and the machine now realizes it directly:* `op_CALL`
dispatches on the callee's descriptor rather than assuming a bytecode function,
so anything can be made applicable by supplying a handler. `kernel.md` P3.

**Digit separator:** `_` between any two digits, uniformly across integer,
fraction, and exponent. Read-side only; canonical printing has no separators.
Comma rejected — it collides with unquote, with decimal-comma conventions, and
with its delimiter role.

---

## 6. Strings

Strings are **immutable UTF-8 byte vectors**, validated well-formed at
construction. Serialization stores raw UTF-8, so in-memory and wire forms are the
same.

**Two concrete types.** `<ascii-string>` — all bytes below 0x80,
core-interpretable, character equals byte, O(1) character operations.
`<utf8-string>` — may contain non-ASCII; a core concrete type that the core
carries, byte-compares, prints, and serializes **without interpreting**, exposing
byte-level accessors. The prelude builds character, grapheme, normalization, and
case handling on top. Both unify under the abstract **`Text`** role in the
prelude, where the generic `length` lives.

**The core interprets only ASCII and carries UTF-8 uninterpreted** — carry, do
not reject. Serialization forces this: the serializer is core-level and must
round-trip prelude text. The enabler is that no ASCII byte ever appears inside a
multibyte UTF-8 sequence, so byte-scanning is correct on arbitrary UTF-8 and
slicing at ASCII delimiters never splits a character. The core is decoder-free.

**"Uninterpreted" is not "opaque."** `<utf8-string>` exposes byte-length (O(1)),
byte-ref, byte-slice, and construct/freeze — enough for the `Text` protocol to be
built *in Bard*. The core lacks the interpretation protocol, not raw access.

**Construction.** A mutable `string-buffer`; freezing yields `<ascii-string>` if
all-ASCII, otherwise `<utf8-string>`. Only two string types — no width zoo.
Literals dispatch to a concrete type by content.

**Two length senses, distinctly named, neither aliasing the generic `length`:**
`byte-length` (core, O(1)) and `grapheme-count` (prelude; UAX #29 plus
version-dependent tables, too heavy for the core). Code-point count is not
blessed as a length.

Default string equality is byte-wise. Normalization is deliberately deferred to a
prelude library.

**Non-ASCII character literals use a codepoint escape in the core**, so any
character is writable in core source without a decoder and character literals
stay decoder-free. A raw non-ASCII glyph in character-literal position is a
prelude reader extension; the base reader reads ASCII glyphs plus the escape.
This resolves the tension between "character = any code point" and a decoder-free
core.

---

## 7. Core and prelude

**Core.** The compiler — special forms `quote`, `begin`, `set!`, `if`, `method`,
and continuation capture; the macroexpander and registry; `gensym`; a base reader
including backquote, auto-gensym, the codepoint escape, and the `_` and `.`
lexical rules; a base printer. The machine and its runtime — the instruction set,
the multiple-value calling convention and receivers, tail calls, the dynamic
environment and `dynamic-wind`, thread creation and switching, a serialization
primitive, and the error hook. The primitive substrate — the value types above,
the reflective floor, and the numeric, I/O, and setter primitives.

**Prelude, written in Bard.** The macro library; classes, `defclass`, C3, and the
`datatype.slot` accessors; generics, methods, protocols, and roles; the numeric
tower above fixnum and float; modules; the handler/restart condition system and
`<condition>`; collection protocols and the standard library; the `Text` layer;
`dynamic-let`, `parameterize`, and the `defparameter` analogue; the surface task
and actor API; the scheduler; the extensible printer and reader; the breakloop;
the monitor.

**Seams — real, and not blockers.** The object system bootstraps metacircularly,
via an ordered hand-written early prelude in the manner of CLOS and PCL. A few
features live in both layers as base plus extension: the reader, the printer, and
conditions. Multiple-value surface forms need the core receiver. A reader-macro
hook is required if user-defined read syntax is ever wanted. Dispatch performance
benefits from PCL-style caching — an incremental optimization, never a
conformance gate (§1.2).

---

## 8. Deferred

Each with its natural home. These are open, not merely unwritten.

- The immutability mechanism and its escape hatches; how immutable and dynamic
  interact.
- Conveyance of dynamic bindings on spawn.
- Declaration surfaces — the `defparameter` analogue, the immutable-variable
  declaration.
- Compile-time versus run-time resolution of dynamic-ness and setter
  specialization.
- The string cursor and iteration API; the prelude `Text`, decoder, and grapheme
  layer.
- Dispatch performance and caching.
- The reader-macro hook.
- The module system. Needed — idiomatic output in foreign host languages has
  nothing to map onto without it — but deliberately small and late. The
  requirement is *a named set of definitions with an export list, and references
  that are either local or qualified*, shaped after Clojure and R7RS rather than
  CL packages, whose symbol/package distinctions are a reliable source of
  confusion.

---

## 9. Superseded by the current machine

Recorded so the changes are deliberate rather than silent.

**Two stacks.** Earlier work committed to separate value and control stacks as a
precondition for the calling convention and for continuation capture. The current
machine achieves both differently: operands live inside the frame, and the
control chain is the frame's `parent`. The requirement is met; the mechanism is
not the one specified.

**Continuation capture as a distinct mechanism.** Earlier work specified that a
continuation captures the value stack with the count riding along. A continuation
is now simply a frame's `parent` — capture is reading a field, installation is
assigning one register. There is nothing to capture.

**Scheduler, actors, channels, serialization, and image save/load as core.** The
earlier core inventory placed these in the machine. The current machine provides
thread creation and switching, a serialization primitive, and nothing else; the
scheduler and the actor and channel API are prelude. This follows the earlier
record's own stated discipline — *find the smallest port-essential primitive set
and build the rest in Bard* — carried further than it had been.

**Continuation capture as a foundational concept.** It was one of the enumerated
dozen. It is now a consequence of frames being values rather than a concept in
its own right, which returns a slot to the concept budget.

**A conflict resolved.** Two earlier documents disagreed on dynamic binding: one
specified uniform places with no per-place flag, the other recorded an amendment
to opt-in. The amendment is later, and §4 adopts it.
