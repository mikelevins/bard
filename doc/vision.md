# Bard: A Good Lisp

## What Bard Is

Bard is a programming language and development environment in the Lisp family. It aims to be approachable without being limited, powerful without being forbidding, and portable without being abstract. It is a live system: you interact with your program while it runs, redefine things while they're in use, inspect anything, and save your entire working state to resume later or move to another machine.

Bard is inspired by Coral Common Lisp, a Macintosh development environment from the late 1980s that combined an easy, welcoming Lisp with unrestricted access to the full power of the platform. Coral was not the fastest Lisp or the most theoretically ambitious. It was *good* — good in the way that matters most: you sat down, it worked, and you could build anything with it. When someone at an AI conference in 1989 heard you worked with Coral Common Lisp, the response was simply: "Ah. The Good Lisp."

That kind of practical goodness — drama-free, undeniable, complete — is what Bard aims for, updated for the current generation of systems.

## The Problem

In 2026 there is no easily approachable Lisp that is also a full-power development environment without boundaries.

If you want a livecoding Lisp, practically speaking it must be Common Lisp. But Common Lisp in 2026 means either an open-source implementation with Emacs and SLIME (excellent for experts, bewildering for newcomers) or a commercial environment like LispWorks or Allegro CL (professional tools with professional price tags and professional learning curves). Some Clojure stacks get partway to the live experience. Scheme48 has the right runtime semantics but is woefully neglected. The INTERLISP revival is underway but has a long road ahead.

All of these are good in various ways. None of them are The Good Lisp — the one you'd hand to a curious person and say "try this," knowing that the same environment would carry them from their first expression all the way to building and deploying real applications.

## What "Good" Means

A Good Lisp has two properties that are individually common but rarely found together:

**Approachable.** You open it. You see a prompt. You type something. Something happens and you understand why. Within your first session you've made values, defined functions, maybe drawn something on screen or fetched something from the web. There is no project setup, no build system, no configuration, no incidental complexity between you and the experience of programming. The distance from "I've never tried this" to "I made something" is measured in minutes.

**Without boundaries.** The environment you learn in is the environment you build in. There is no training-wheels mode that you graduate out of. The listener you used on day one is the same listener you use to debug production systems. Every facility of every platform Bard runs on is accessible. You can build applications that look and feel native because they use native capabilities. You can build distributed systems because actors and message passing are built in. You can build anything — and you build it in the same environment where you first typed `(+ 2 3)`.

These two properties reinforce each other. Approachability without power is a toy. Power without approachability is a tool only for experts. The combination is what Coral achieved and what has been missing since: a system where beginners and experts inhabit the same world and use the same tools, just at different depths.

## Design Principles

When we face a design decision and aren't sure which way to go, we ask: **which option gets us closer to The Good Lisp?**

More specifically:

**Immediacy over ceremony.** If something can happen in the listener, it should. If something requires a setup step, that step should be as small as possible. The default experience is interactive.

**One environment, not two.** There is no separate "learning mode" and "professional mode." No "scripting interface" and "real IDE." The system is one thing. Beginners use less of it; experts use more of it. Nobody switches tools.

**Liveness is the default.** You change a definition and existing objects update. You hit an error and you're in a breakloop where you can inspect everything, fix the problem, and continue. You never lose your train of thought to a restart cycle.

**Transparency over magic.** You can inspect any object, any method, any protocol. You can see how things work by asking the system. Discovery is a conversation with a live environment, not a search through documentation.

**Portability is a feature, not a constraint.** Bard runs on many platforms. Code you write on your laptop runs on a server, in a browser, on a Raspberry Pi. Actors move between machines carrying their state. This isn't an abstract portability that gives you a lowest common denominator — you can access platform-specific facilities when you want them, through protocols that make native capabilities available in a Bard-native way.

**Taste matters.** A thousand small decisions — how errors are reported, what the inspector shows first, how much the listener remembers, what the default font is — are the difference between "powerful" and "good." These decisions must be felt out through use, not derived from architecture. We pay attention to them.

## The Experience

### First Contact

You open Bard. You see a listener with a `bard>` prompt and a brief welcome. You type:

```
bard> (+ 2 3)
5
```

It works. You try more things. The built-in tutorial isn't a document — it's a sequence of things to try in the listener, each building on the last. "Type this. See what happens. Now change this part." Within your first session you've made values, defined methods, built a small data structure, and maybe seen it displayed graphically.

### Building Something

You define some types, some protocols, some methods. You test them in the listener as you go. You make a mistake; the system drops you into a breakloop where you can see what went wrong, fix it, and continue without restarting. You save your image — your entire working state, everything you've built, ready to resume next time.

### Going Deeper

You decide your application should run partly on a server. You write an actor, test it locally, and deploy it. The messaging and serialization handle the transition. Your local listener can still talk to the remote actor for debugging and inspection.

You need native UI on macOS, or a web frontend, or access to a hardware peripheral. The platform's facilities are available through protocols. You call them from the same listener where you started.

### Sharing

You hand someone your Bard image. They open it and they're in your environment — your types, your methods, your data, your application. They can inspect how it works, modify it, extend it. The system is the documentation.

## Architecture in Service of Experience

Bard's technical design is motivated by the experience described above, not the
other way around.

**One organizing idea.** The machine steps a reified computation, and a frame is
that computation. A breakloop is a frame you kept; a continuation is a frame you
kept; a thread is a frame you kept. Resuming any of them assigns one register.
Everything below follows from that.

**A two-layer architecture** — a small portable kernel, with the full language
built on top in Bard itself. The kernel is 15 instructions, one register, and
seven representations: small enough to hold in mind entirely and to reimplement
from memory on a new host in a sitting or two. That is how one environment runs
everywhere: port the kernel, load the language. See `kernel.md`.

**Protocol orientation** gives you discoverability. "What can I do with this
value?" is a question the system answers. Protocols are categories of capability,
not hierarchies of inheritance.

**Actors and serialization** give you distribution that feels like local
programming. Because a live computation is an ordinary graph of heap objects
with no hidden machine stack anywhere, moving a running actor between machines is
a serialization operation rather than a rewrite.

**Liveness** — redefinition, breakloops, instance updating — means you never
leave the flow of thought to fight the tools. The machine reports failure without
unwinding, so an error puts you inside the environment where it happened, and the
faulting instruction can be retried once you have fixed the problem.

**Image-based persistence** for the same reason as actor mobility: the entire
working state is reachable heap structure. Close your laptop, open it tomorrow,
pick up where you left off — or hand your image to a colleague.

**A self-hosting compiler in the kernel language** means the development tools
are part of the portable payload. Port the machine to a new platform, load the
compiler and REPL images, and you have a complete development environment with
no host-language dependency on the target.

## Open Challenges

Some hard problems remain. We acknowledge them here rather than pretending they don't exist.

**Graphics and UI.** Coral had one platform. Bard targets many. A cross-platform UI story is hard. Options range from web technologies everywhere (pragmatic, aesthetically unsatisfying) to native UI per platform behind protocols (ideal, enormous work) to a Bard-native rendering approach (ambitious, possibly foolish). This deserves sustained design attention.

**An integrated editor.** Coral had Fred — a capable, Lisp-aware editor built in Lisp, integrated into the environment. Bard needs something equivalent: an editor that beginners can use without installing anything else, that experts can extend and modify because it's written in Bard, and that's wired into the live system for evaluation, inspection, and debugging. Building a good editor is a significant undertaking.

**Performance.** Bard's bytecode interpreter will be slower than native code for compute-heavy work, and the design knowingly accepts two structural costs: dispatch branch misprediction, and a heap frame per call. Both have a standing strategy rather than a shrug; see `performance.md`. For I/O-bound and actor-heavy applications this will not matter much. For number crunching, a good FFI story — call into C, Rust, or whatever for hot paths — remains the pragmatic answer.

**Community and ecosystem.** Coral was a product — it came with documentation, examples, and support. Bard needs to build community through good documentation, a welcoming culture, and enough early adopters to create momentum. The approachability story is central: if people can make things in their first session, they'll come back and bring others.

## The Name

A bard tells stories, makes things vivid, brings ideas to life through language. The name fits a programming environment whose purpose is to make ideas tangible — to let you say what you mean and see it come alive.

---

