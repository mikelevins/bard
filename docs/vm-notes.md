# bard 0.4 vm notes

The bard vm process manages one or more **actors**. An **actor** is a
single-threaded virtual machine that executes a low-level
reresentation of bard code. Each actor has its own variable
environment in addition to a vm-wide table of global variable
bindings. Each actor has its own mailbox and its own thread of
control.

In 0.4, the code executed by each actor is **kernel bard**, a
simplified bard kernel language without any unexpanded macros. A 0.4
actor interprets the code by walking the AST.

The representation of bard code and the execution strategy are
implementation details. The intention is to move to more efficient
representations and execution strategies in stages with future
versions. For example, a next step might be to replace kernel bard
with vectors of bytecode, and a farther future enhancment might enable
bard methods to be represented as native code.

The bard vm provides primitives for creating and managing actors. Each
actor has a unique id string. A vm can run any number of actors
concurrently. bard vms on different processes or different hosts can
run their own actors, and an actor created on one vm can be serialized
and transferred to another vm. Code running in one actor can pass
messages to another actor using bard primitives, whether the
destination actor is running in the same vm, a different vm on the
same host, or a different machine accessible across the network.

