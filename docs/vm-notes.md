# bard 0.4 vm notes

The bard vm process manages one or more **actors**. An **actor** is a
single-threaded virtual machine that executes a low-level
reresentation of bard code. Each actor has its own variable
environment in addition to a vm-wide table of global variable
bindings. Each actor has its own mailbox and its own thread of
control.

The bard vm provides primitives for creating and managing actors. Each
actor has a unique id string. A vm can run any number of actors
concurrently. bard vms on different processes or different hosts can
run their own actors, and an actor created on one vm can be serialized
and transferred to another vm. Code running in one actor can pass
messages to another actor using bard primitives, whether the
destination actor is running in the same vm, a different vm on the
same host, or a different machine accessible across the network.

