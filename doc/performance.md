# Performance Strategy

A consulting document, not a memorizing document. `kernel.md` is the thing to
keep in your head; nothing here belongs there.

---

## 0. The standing commitment

This design deliberately buys features with runtime cost. Two costs are
structural — they follow from the organizing idea and will not be designed away:

1. **Dispatch branch misprediction**, inherent to bytecode interpretation.
2. **Frame allocation per call**, the price of "a frame is the computation."

We accept both. We also commit to **remaining permanently interested in reducing
them**, because the alternative — accepting a cost once and then never
revisiting it — is how a design ossifies around its first implementation.

Three rules govern that interest:

**R-a. No strategy may compromise the essentials.** Livecoding, inspectability,
resumability, and portability are not negotiable for speed. A tactic that
requires discarding runtime information is not a tactic; it is a different
language.

**R-b. No strategy may grow the memorizable kernel.** Superinstructions, inline
caches, and specialized handlers all *add instructions*. They belong to the
production kernel, generated from tables, never hand-written into the fifteen.
The bootstrap kernel stays at fifteen forever.

**R-c. Measure before and after, or it is folklore.** Both of these costs are
areas where intuition is unreliable and published results disagree across CPU
generations. A tactic adopted without a measurement is a superstition with good
PR.

---

## 1. Dispatch branch misprediction

### 1.1 Why it happens

The interpreter's inner loop ends in an indirect jump whose target is determined
by the *next bytecode*. From the CPU's point of view that target is data, and
data it has not seen yet.

Modern indirect-branch predictors (ITTAGE and relatives) are not naive — they
index their prediction tables by recent branch *history*, so they can learn
correlations. That is the lever. **The whole strategy is to give the predictor
enough context to distinguish situations it can actually predict.**

Bytecode streams are far from random. Real sequences have strong local
structure: an `op_LOCAL` is usually followed by another `op_LOCAL` or by an
`op_GLOBAL`; an `op_GLOBAL` naming a function is usually followed by an
`op_CALL`; the arithmetic pattern in our own examples is
`op_LOCAL op_LOCAL op_GLOBAL op_CALL op_RECV`, over and over. That regularity is
predictable *if the predictor can tell which handler it is jumping from*.

### 1.2 Tactic 1 — Threaded dispatch (do this from the start)

Instead of one `switch` at the top of the loop, every handler ends with its own
copy of "fetch the next instruction and jump to its handler."

```
    /* switch dispatch: ONE indirect branch site */
    for (;;) { switch (op(code[pc++])) { case op_CONST: ...; break; ... } }

    /* threaded dispatch: ONE SITE PER HANDLER */
    #define NEXT() goto *table[op(code[pc++])]
    do_CONST:  ...; NEXT();
    do_LOCAL:  ...; NEXT();
```

With one site, the predictor keeps one entry for "the interpreter's jump" and
must guess the next opcode from global history alone. With fifteen sites, it
effectively predicts *"given that I just ran `op_CONST`, what comes next?"* — which
is a question the bytecode's local structure actually answers.

Cost: about twelve bytes per handler, ~180 bytes total. Take it at the start;
retrofitting means restructuring every handler.

**The trap.** Compilers tail-merge identical code. GCC in particular will
happily collapse your thirteen dispatch tails back into one and silently undo
the optimization. Verify in the disassembly that the dispatch is replicated. If
it has been merged, the usual remedies are `-fno-gcse` on GCC, or making the
tails non-identical.

**Honest caveat.** On older CPUs threaded dispatch was worth 20–50%. On recent
cores with long-history indirect predictors the gap has narrowed considerably,
and on some workloads switch dispatch is nearly as good. It is never *worse*, it
is cheap, and it is hard to add later — so take it. But do not budget a specific
speedup for it. Measure (§1.6).

### 1.3 Tactic 2 — Dispatch less often

The cheapest misprediction is one that never happens. Fewer dispatches per unit
of work means less exposure.

Two ways, in order of preference:

**Make handlers do more.** The misprediction cost is amortized over the work per
dispatch. `op_CALL` does a great deal and its dispatch is nearly free in relative
terms; `op_DROP` does nothing and is nearly all dispatch. This is a standing
argument against splitting instructions finely for elegance — every split
doubles the dispatch count on that path.

**Superinstructions.** Fuse common sequences into single instructions:
`op_CALL`+`op_RECV 1` (see §3), `op_LOCAL op_LOCAL`, `op_GLOBAL op_CALL`, the whole
`op_LOCAL op_LOCAL op_GLOBAL op_CALL op_RECV 1` arithmetic idiom. Each fusion removes one to three dispatches.

Subject to **R-b**: these are produced by a table in the production build, from
measured frequency data, not chosen by taste and not added to the thirteen. The
generator reads a profile, emits fused handlers, and the compiler emits fused
opcodes. The human-memorizable kernel never sees them.

### 1.4 Tactic 3 — Fixed-width instruction encoding

Decoding a variable-length instruction requires branching on the opcode before
you know how long it is — a data-dependent branch *before* the dispatch branch,
on every single instruction.

**Use a fixed-width encoding.** Decode becomes a load and some shifts, with no
branches at all. This is free, it is decided at design time, and it simultaneously
serves R1: a fixed layout is easier to reconstruct from memory than a
variable-length scheme with escape cases.

### 1.5 Tactic 4 — Attack the data-dependent branches inside handlers

Dispatch is not the only unpredictable branch. Our largest one is inside `op_CALL`:

> is the callee a bytecode function or something else?

That executes on every call, and in polymorphic code it is genuinely
unpredictable. The remedy is **inline caching** — record at each call site what
the callee turned out to be last time, and check that guess first. A correct
guess turns an unpredictable indirect branch into a predictable compare-and-fall-
through.

We have an unusual advantage here worth remembering: because redefinition goes
through binding objects and (later) a dependency graph, **invalidation can be
exact rather than heuristic**. A JIT that cannot know when a definition changes
must keep its guards; we can be told, and drop them. The livecoding machinery is
an information source, not only a tax.

Subject to R-b: inline caches live in the production kernel.

### 1.6 How to measure

Do not adopt any of the above on faith.

- Primary metric: **branch mispredictions per bytecode dispatched.** Absolute
  mispredict counts are meaningless without the dispatch count as a denominator.
- Secondary: **IPC** (instructions per cycle). Interpreters typically run at low
  IPC; a dispatch improvement should show up here.
- Tooling: `perf stat -e branches,branch-misses,instructions,cycles` on Linux;
  `xctrace` / Instruments on macOS; the equivalent counters elsewhere.
- Method: keep a small fixed benchmark set that exercises different bytecode
  *shapes* — arithmetic-heavy, call-heavy, branch-heavy, allocation-heavy —
  because tactics here help different shapes by very different amounts. A single
  benchmark will mislead you.
- Establish the baseline **before** the first optimization, and keep it runnable.

### 1.7 Order of work

1. Fixed-width encoding — free, decide now.
2. Threaded dispatch — cheap, hard to retrofit, take it now.
3. Build the benchmark set and record a baseline.
4. Inline caching on `op_CALL`'s callee check — the largest single data-dependent
   branch in the machine.
5. Superinstructions, from measured profiles, production kernel only.

---

## 2. Frame allocation

### 2.1 The verdict: marginal *or* integer multiples, depending entirely on discipline

The question of whether this design is adoptable turns on one number, and the
answer is not a single number — it is a factor of ten apart depending on how
frames are allocated.

**Per-call cost, conventional stack VM.** Push a return address, set a frame
pointer; arguments are already in place because the stack is shared.
**≈ 5–8 machine instructions, no allocation.**

**Per-call cost, naive implementation.** A general heap allocation per frame,
plus copying `n` arguments from the caller's operand area into the callee's
slots, plus eventual collection of every frame ever created.
**≈ 5–20× a conventional VM.** Integer multiples. Not adoptable.

**Per-call cost, with the discipline in §2.2.** A bump-pointer add, a limit
check, four header stores, one register store, and `n` argument moves.
**≈ 8 + 2n machine instructions, roughly 1.5–2× on the call sequence** — and much
less than that on a whole program, since calls do real work between them.
Call-heavy code, perhaps 10–30%. Marginal, and an ongoing optimization target
(§3).

The conclusion that matters: **the allocation discipline is not an optimization,
it is a condition of adoption.** Documented as an optimization, someone builds
the naive version, measures 10×, and concludes the platform is unusable. It
belongs in the design.

### 2.2 The discipline: three rules

> **Frames live on a stack until someone wants to keep one; that one moves to
> the heap.**

That sentence is the whole of it. Expanded:

**Bump allocation from a per-thread region.** Allocation is a pointer increment
and a limit check. The region is private to the thread, so nothing synchronizes.

**Reset on return.** A frame that was never captured is reclaimed by resetting
the bump pointer — one store. Most frames never reach the collector at all, so
**GC pressure becomes proportional to captures rather than to calls.**

**Copy out on capture.** When something retains a frame — the error hook, an
explicit continuation capture, a thread switch — copy the live chain out of the
region into the general heap. Capture is rare; calls are constant. Paying in
proportion to *use* of a feature rather than to its *availability* is the right
shape, and it is the same principle as §1's approach to dispatch.

Detecting capture can be conservative: assume it whenever the frame chain is
read by anything other than `op_RETURN`. The readers are the debugger, the
scheduler, and explicit capture — all already distinguished operations, not a new
analysis.

**On a host with a generational collector, the host already does all three.**
SBCL allocates by bumping a thread-local pointer and reclaims short-lived nursery
objects by copying out survivors, which is rules 1–3 exactly. There the kernel
implements the discipline by doing nothing: `make-frame` on call, drop the
reference on return. Hand implementation is needed on C, and on refcounted hosts
such as Objective-C and Swift, which have neither a nursery nor cycle
collection.

Deep non-tail recursion outgrows a region; chain regions and keep the reset
discipline per region.

### 2.3 Other candidates, unevaluated

Recorded so they are not re-invented from scratch each time:

- **Free-list reuse by size class**, rather than a bump region. Simpler, worse
  locality.
- **Compile-time escape analysis** — prove that a given call's frame cannot be
  captured, and allocate it in a cheaper way. Requires a closed world for the
  callee, so it interacts with sealing.
- **Arena pools sized from empirical analysis, discarded wholesale.** Plausible,
  but note the pinning hazard: a single captured frame keeps its entire arena
  alive. Viable only combined with promotion-on-capture, at which point §2.2 is
  the simpler formulation of the same idea.
- **Frame-header shrinking.** Four header fields are stored per call. Packing
  `pc` and `sp` into one word, or deriving `fn` from the code pointer, would cut
  stores on the hottest path in the machine.
- **Frame elision for leaf calls** that neither allocate nor can be captured.

### 2.4 How to measure

- Primary: **bytes allocated per call**, and **fraction of frames that are ever
  captured.** The second number decides whether §2.2 is worth anything; if
  capture is common, the whole approach is misconceived.
- Secondary: cache miss rate on frame access, and time in collection.
- Watch for the pathological case: deep non-tail recursion that never returns
  before a capture, which defeats reset-on-return.

---

## 3. Standing track: recovering call cost

**This is an open, ongoing thread, not a checklist to complete.** The design
knowingly gives up per-call performance for reified frames. This section is where
candidates for winning it back accumulate, get measured, and get promoted or
discarded. Adding a candidate costs nothing; the only rule is that nothing leaves
this section without a measurement (R-c) and nothing enters `kernel.md` at all
(R-b).

**Nothing here is to be implemented for the current project.** It exists so the
thinking is not lost and does not have to be re-derived.

| Candidate | Est. payoff | Cost | Status |
|---|---|---|---|
| **Overlapping argument placement** — arrange the callee's frame to begin where its arguments already sit, so they are never copied | ~20–30% of call cost | Frame header must move to the high end, or the caller must reserve header-sized padding, or headers go in a parallel region. Real complexity for a modest gain. | Demoted here from the adoption criteria on 2026-07-29 — it is an optimization, not a condition |
| **Single-allocation frames** — header and slots in one vector rather than two objects | halves allocations per call | Less readable; but it is also the C layout, so it makes the port more direct | Strong candidate; measure first |
| **`op_CALL` + `op_RECV 1` fusion** | one dispatch per call | Production kernel only | Highest-value superinstruction; see §1.3 |
| **Frame-header shrinking** — pack `pc` and `sp` into one word; derive `fn` from the code pointer | 1–2 stores per call | Encoding fiddliness | Cheap, unmeasured |
| **Leaf-frame elision** — a call that makes no further calls and cannot be captured needs no frame of its own | large on leaf-heavy code | Requires knowing "cannot be captured" | Needs escape analysis |
| **Compile-time escape analysis** | enables the two above | Needs a closed world, so it interacts with sealing | Long-term |
| **Frame reuse at a call site** — the next call from the same site wants the same size; skip re-initializing fields that do not change | 1–3 stores per call | Correctness care around capture | Unexplored |
| **Native frames plus deoptimization** — run on the host stack, materialize heap frames only on capture | approaches conventional VM cost | The hardest thing in any JIT; also reintroduces a native-stack dependency, so it must be per-target | Last resort, high ceiling |

### Other future directions

- **Dynamic handler replication** — duplicating hot handlers so frequently-taken
  predecessor/successor pairs get their own dispatch sites, beyond static
  threading.
- **Register-based bytecode for the production kernel**, keeping stack-discipline
  bytecode for the bootstrap. Fewer instructions executed per expression, at the
  cost of a register allocator — which is exactly why the memorizable kernel must
  not use one.
- **Native code as an alternative applicable kind**, with bytecode as the
  semantic authority and native as a cache; deoptimization back to bytecode where
  native cannot be regenerated.
- **Emitting C or WASM** rather than machine code, so one backend serves every
  platform.
- **Exploiting exact invalidation** to remove guards a conventional JIT is
  obliged to keep — likely the most distinctive performance opportunity this
  design has, and the one least explored by existing systems.

## 4. The guard

Every tactic in this document is subject to R-a, R-b, and R-c. In particular,
**if a proposed optimization would change what `kernel.md` says, it is out of
scope by construction.** The thirteen instructions, one register, and seven
representations are not a starting point to be optimized away; they are the
artifact. Performance work happens in the implementation of that machine and in
the production kernel built from it, never in the machine's definition.
