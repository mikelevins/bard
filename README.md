# Bard

Bard is a programming language and development environment in the Lisp family.
It aims to be approachable without being limited, powerful without being
forbidding, and portable without being abstract.

It is a **live** system. You interact with a program while it runs: redefine
things while they are in use, inspect anything, recover from an error inside the
environment where the error happened, and save your entire working state to
resume later or move to another machine.

The goal is what an earlier generation of Lisp users meant when they called
Coral Common Lisp "The Good Lisp" — a system you sit down at, that works, that
carries you from your first expression to a deployed application without ever
switching tools. Two properties, individually common and rarely found together:

- **Approachable.** Open it, see a prompt, type something, understand what
  happened. No project setup, no build system, no configuration between you and
  the experience of programming.
- **Without boundaries.** The environment you learn in is the environment you
  build in. There is no training-wheels mode to graduate out of, and no facility
  of the underlying platform is off limits.

## Status

**The machine runs. The language above it is beginning.**

Bard is being rebuilt on a new kernel design. The previous implementation is
preserved and reachable — see *History* below — but it is not the basis for
what comes next.

Working today: the virtual machine, complete; an assembler and a disassembler; a
compiler for the kernel language; macros; and a small prelude — `define`, `let`,
`when`, `cond`, `and`, `or`, list handling — written in Bard itself. Two test
suites, one in Lisp and one in Bard.

The new kernel rests on one idea:

> The machine steps a reified computation. A frame is that computation.

A breakloop is a frame you kept. A continuation is a frame you kept. A thread is
a frame you kept. Resuming any of them assigns one register.

Working out from that gives a virtual machine of **15 instructions, one
register, and seven representations** — small enough to hold in mind entirely,
and to reimplement from memory on a new host in a sitting or two. That property
is deliberate: it is what makes Bard portable to every platform it targets
without a per-platform reinterpretation of what the language means, and it is
what lets the development tools travel as part of the portable payload rather
than being rebuilt per host.

The kernel language on top of it is six special forms — `quote`, `if`, `begin`,
`set!`, `method`, `values` — plus application and macros. Everything else is
written in Bard.

Deliberately kept out of the kernel, and reachable from it: the object system and
generic functions, conditions and restarts, the debugger, redefinition tracking,
modules, the scheduler, native compilation, and remote attachment.

## Targets

macOS, Linux, Windows, Android, iOS, and the web.

## History

The implementation that preceded this design is preserved in full:

- Tag `pre-frame-kernel-2026-07-29` — the state of this repository before the
  redesign.
- Tag `frame-kernel-2026-07-29` — the boundary at which work on the new kernel
  begins.

Nothing was discarded; the earlier work is a checkout away.

## Repository

This repository holds Bard itself: the implementation, and the documents that
record its vision, architecture, design strategies, and implementation
decisions.

Design exploration and working notes are kept separately, so that this
repository stays focused on the language and the machine.
