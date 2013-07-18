# Bard VM 0.4

This directory contains the source files for the Bard 0.4 virtual machine. This README file explains the organization and purpose of the sources.

## Load order

`load.scm` loads the VM sources in the proper order into a running session of Gambit-C Scheme for interactive development and testing. 

`Makefile` (which doesn't exist yet) compiles those same files into ANSI C code, and thence to machine code for the target architeture, converting the VM to a native machine-code executable. `Makefile` takes one additional source file, `bard.scm` (which also doesn't exist yet), which provides the main entry point of the resulting executable.

`bard.scm` is not loaded in the `load.scm` file because it's not needed for interactive developement. It provides only a toplevel entry point. In interactive development, the Gambit Scheme toplevel serves that purpose.

## The VM

The Bard VM is a program that loops forever, repeatedly fetching and executing Bard instructions from a **program** provided as input. The **program** is a vector of Bard instructions produced by a Bard compiler, and it may be stored in a Bard object (".bardo") file, or in a Bard image (".bardim"). A bardo file is a file that contains a vector of bard instructions in relocatable format. A bardim file contains a complete serialized image of a running Bard environment; when the vm loads a bardim file, it reconstructs the dynamic state of the environment that was saved into the image.

The files listed in `load.scm` in the definition of the `$bard-files` variable are the VM sources in the proper order for loading into Gambit. Definitions in files that appear later in the list may rely on definitions in the earlier files.

Following is a brief explanation of the purpose of each source file:

| file             | notes 
| ---------------- | -----
| constants.scm    | constant values, such as version numbers, relied on by the vm 
| utils.scm        | general-purpose utility functions, such as list and vector tools 
| values.scm       | implementation of basic Bard value semantics 
| stacks.scm       | implementation of the vm stack 
| environments.scm | representation of lexical environments 
| globals.scm      | representation of global variables 
| programs.scm     | representation of compiled Bard programs 
| vmstate.scm      | the VM's registers and related operations 
| primitives.scm   | representation and implementation of vm primitives 
| functions.scm    | representation of Bard's function values 
| transfers.scm    | representation and implementation of nonlocal transfers of control, including continuations, function returns, and conditions 
| instructions.scm | representation and implementation of the Bard VM's instruction set 
| vm.scm           | the main logic of the VM itself 
| system.scm       | supporting and diagnostic tools, such as printers and inspectors 

## The Stack

The stack is the data structure that the VM uses to keep track of argument and return values, and of pending function calls when functions are called recursively or by other functions.

Bard VM 0.4 uses a single stack for both values and control transfer. This is made possible by the fact that all nonlocal transfers of control are represented by first-class objects.

It's possible to separate the stacks for values and control transfers, and there may be advantages to doing so, but using a single stack is simpler, and so that's what 0.4 does. If I judge that the advantages of multiple stacks justify the small added complexity, then I'll adopt that model in a future version.

The stack in version 0.4 is just a Lisp list, again for the sake of simplicity.

## Environments and Globals

Bard is lexically scoped. Lexical scope in the source language is represented directly in the VM in the form of environment data structures. In 0.4 and environment is simply a list of pairs in which the CAR of the pair is a variable name and the CDR is its value.

Once again, this representation is chosen for simplicity of implementation. There are well-known techniques that provide more efficient variable lookup (e.g. representing an environment as a list of frames, each of which is a vector of values). A future version of the VM will most likely adopt some such representation for the sake of efficiency.

Global variables are global per VM instance, and are represented as entries in a global table.

Bard defines a mutability attribute for both local and global variables. The 0.4 version of the VM does not represent that attribute directly, and does not enforce it.

## Programs

A program is a value that represents executable Bard code--in other words, the output of a Bard compiler, suitable for execution by the VM. In order to run, the VM requires a valued program.

## VM Registers and State

The state of the Bard VM is represented by a set of registers:

| register | purpose
| -------- | -------
| program  | the Bard program currently being executed
| function | the function currently being executed
| pc       | the index of the next instruction to execute
| nvals    | the number of values currently on the stack
| stack    | the stack itself
| env      | the current lexical environment
| globals  | the currently-defined table of global variables
| haltfn   | the procedure used to terminate execution of the VM

All Bard programs consist of instructions that direct the VM code to manipulate the values in these registers.

## Primitives

A **primitive** is a built-in Bard procedure that is not implemented inline in the main logic of the VM itself. Instead, the primitive is a separate subroutine that is stored in a table of such routines, and executed when the VM reaches a `PRIM` instruction that refers to the primitive.

Primitives could instead be implemented inline in the main VM logic, but doing so would tend to make that main logic grow quite large, and would also require recompilation of the main VM logic any time we wanted to add or modify a built-in procedure. For that reason, only the most central and performance-critical operations are defined inline; others are implemented as separate subroutines and provided as primitives.

In addition, the primtitives facility provides an easy way to add or customize VM features for different purposes or platforms, and an easy way to add extensions to the VM.

## Functions

A Bard function is a value that represents executable code. The VM implementation of functions is a data structure that represents Bard functions.

## Transfers

There are three kinds of nonlocal transfers of control in Bard; transfers are data structures that represent them.

The three kinds of transfers are:

| transfer type | purpose
| ------------- | -------
| return        | the transfer that occurs at the end of a function call to return its results 
| continuation  | a general-purpose jump to any point in a program
| condition     | a transfer provided as part of Bard's condition system, its feacility for handling exceptions and other unusual circumstances

In fact, both returns and condition transfers are logically equivalent to continuations, and the three kinds of transfers may be merged into one once I'm sure I understand everything they need to represent in order to be able to properly handle all needed cases.

Bard's continuations work like Scheme continuations: they provide a first-class representation of the concept of resuming a computation, complete with its required environment.

## The Instruction Set

The core instruction set for the VM is defined and implemented, and can execute basic compiled Bard programs. Many useful instructions are not yet implemented, but most of them are fairly trivial instructions that will exist primiarly for convenience or efficiency. The core semantics of the Bard VM is defined by the following instructions, all of which are currently implemented:

| instruction  | arguments | description
| -----------  | --------- | -----------
| HALT   |     | stop the VM
| CONST  | k   | push k on the stack
| LREF   | v   | look up v in env and push its value on the stack
| LSET   | v   | pop the stack; lookup v in env and replace its value with the popped value
| GREF   | v   | look up v in globals and push its value on the stack
| GSET   | v   | pop the stack; lookup v in globals and replace its value with the popped value
| GO     | d   | set pc to d
| TGO    | d   | pop the stack; if the popped value is true then set pc to d; otherwise set it to pc+1
| FGO    | d   | pop the stack; if the popped value is false then set pc to d; otherwise set it to pc+1
| FN     |     | pop three values from the stack; call them p, e, and b; create a new function with parameters p, environment e, and body b; push the new function on the stack
| CC     |     | create a new continuation object that captures the current state of the vm; push it on the stack
| SETCC  |     | pop the stack; the popped value must be a continuation; update the state of the VM with the state stored in the continuation
| PRIM   | p   | lookup p in the VM's table of primitives; pass the vmstate to the primitive
| SAVE   | d   | create a new return object that captures the current state of the vm; push it on the stack
| ARGS   |     | pop the stack; the popped value must be a function; pop the remaining values from the stack and create a local environment in which the function's parameters are bound to the popped values; merge that environment onto env, replacing env; push the function back on the stack
| CALL   |     | pop the stack; the popped value must be a function; set pc to zero and set function to the popped value
| RETURN |     | pop nvals values from the stack and hold them; pop the stack again to obtain the pending return object; update the VM state with the state stored in the return object; push the held values back on the stack


## System Tools

System tools include conveniences for the development, debugging, and operation of the VM program. Examples include functions to print the state of the VM for inspection, to step the VM one instruction at a time, and to change the values in VM registers.

## The Main Program

During interactive development, the VM's main program is the Gambit Scheme repl. When deployed, it has a different main program: a simple function that loads the provided Bard program, initializes the vmstate, and starts execution. The main program is simple in concept, but is likely to grow more complicated with the passage of time, as we add things like runtime flags, additional ways to pass programs and images to the VM, and so on.



