# Bard VM JavaScript Implementation - File Manifest

## Directory Structure

```
/Users/mikel/repos/lisp/bard/js/bardvm-claude1/
├── Core VM Implementation
│   ├── structs.js              # Data structures (Fn, RetAddr)
│   ├── opcodes.js              # Instruction helpers
│   ├── globals.js              # Global variable management
│   ├── vm.js                   # Main VM (MACHINE function) ⭐
│   └── worker-vm.js            # Worker thread wrapper
│
├── Test Infrastructure
│   ├── test-runner-node.js     # Node.js test runner
│   └── test-runner-browser.html # Browser test runner
│
├── Test Cases
│   └── tests/
│       ├── simple-arithmetic.json
│       ├── closure.json
│       ├── conditional.json
│       └── factorial-todo.json
│
├── Documentation
│   ├── README.md               # User guide & reference
│   ├── HANDOFF.md              # Detailed handoff document ⭐
│   ├── QUICKSTART.md           # Quick start guide
│   ├── SUMMARY.md              # This session summary
│   └── MANIFEST.md             # This file
│
└── Project Files
    ├── package.json            # Node.js package definition
    └── .gitignore              # Git ignore rules
```

## File Purposes

### structs.js
**Purpose**: Define core data structures  
**Exports**: Fn, RetAddr  
**Size**: 1.1 KB  
**Dependencies**: None  
**Used By**: vm.js, worker-vm.js, test runners

### opcodes.js
**Purpose**: Instruction manipulation helpers  
**Exports**: opcode(), arg1(), arg2(), arg3(), is(), top(), rest2(), rest3()  
**Size**: 2.2 KB  
**Dependencies**: None  
**Used By**: vm.js

### globals.js
**Purpose**: Global variable storage and management  
**Exports**: getGlobalVar(), setGlobalVar(), clearGlobalVars(), etc.  
**Size**: 1.8 KB  
**Dependencies**: None  
**Used By**: vm.js, test runners

### vm.js ⭐
**Purpose**: Core virtual machine implementation  
**Exports**: machine()  
**Size**: 16.8 KB (528 lines)  
**Dependencies**: structs.js, opcodes.js, globals.js  
**Used By**: worker-vm.js, test runners  
**Note**: This is the heart of the implementation - faithful translation of Norvig's MACHINE function

### worker-vm.js
**Purpose**: Enable concurrent VM execution in worker threads  
**Exports**: None (worker entry point)  
**Size**: 3.1 KB  
**Dependencies**: All core VM files  
**Used By**: test-runner-node.js (Node.js workers), test-runner-browser.html (Web Workers)

### test-runner-node.js
**Purpose**: Command-line test runner for Node.js  
**Exports**: loadBytecode(), runDirect(), runInWorker(), runTest()  
**Size**: 6.8 KB  
**Dependencies**: All core VM files, worker-vm.js  
**Usage**: `node test-runner-node.js [--worker] tests/*.json`

### test-runner-browser.html
**Purpose**: Web-based visual test runner  
**Exports**: N/A (HTML page)  
**Size**: 8.3 KB  
**Dependencies**: All core VM files, worker-vm.js  
**Usage**: Open in browser after starting local server

### Test Files (tests/*.json)
**Purpose**: Example bytecode programs for testing  
**Format**: JSON with code, expected result, description  
**Count**: 4 files  
**Total Size**: ~1 KB

### README.md
**Purpose**: Complete user documentation and reference  
**Audience**: Users, developers  
**Size**: 10.8 KB  
**Contains**: Architecture, instruction reference, usage examples

### HANDOFF.md ⭐
**Purpose**: Comprehensive handoff document for continuity  
**Audience**: Next Claude instance, developers  
**Size**: 14.2 KB  
**Contains**: Implementation details, testing strategy, known issues, debugging guide

### QUICKSTART.md
**Purpose**: Quick start guide for immediate use  
**Audience**: New users  
**Size**: 4.3 KB  
**Contains**: Installation, first test, common patterns

### SUMMARY.md
**Purpose**: High-level summary of this session's work  
**Audience**: Project manager, future reference  
**Size**: 5.2 KB  
**Contains**: Files created, statistics, requirements met

## Import/Dependency Graph

```
vm.js
  ├─ requires: structs.js (Fn, RetAddr)
  ├─ requires: opcodes.js (opcode, arg1, arg2, arg3, is, top, rest2, rest3)
  └─ requires: globals.js (getGlobalVar, setGlobalVar)

worker-vm.js
  ├─ requires: structs.js
  ├─ requires: opcodes.js
  ├─ requires: globals.js
  └─ requires: vm.js

test-runner-node.js
  ├─ requires: structs.js
  ├─ requires: vm.js
  ├─ requires: globals.js
  └─ spawns: worker-vm.js (in worker threads)

test-runner-browser.html
  ├─ loads: structs.js
  ├─ loads: opcodes.js
  ├─ loads: globals.js
  ├─ loads: vm.js
  └─ spawns: worker-vm.js (in Web Workers)
```

## Execution Paths

### Direct Execution (Node.js)
```
test-runner-node.js
  → loads bytecode from JSON
  → creates Fn object (structs.js)
  → calls machine() (vm.js)
    → uses opcodes.js helpers
    → uses globals.js for GVAR/GSET
  → compares result to expected
  → prints summary
```

### Worker Execution (Node.js)
```
test-runner-node.js
  → loads bytecode from JSON
  → creates Worker
  → posts message to worker-vm.js
    → worker creates Fn object
    → worker calls machine()
    → worker returns result
  → main receives result
  → compares to expected
  → prints summary
```

### Browser Execution
```
test-runner-browser.html
  → user uploads JSON files
  → parses bytecode
  → either:
    ├─ Direct: calls machine() directly
    └─ Worker: spawns Web Worker
       → worker-vm.js processes
       → returns result
  → displays results visually
```

## Key Entry Points

1. **Command Line**: `test-runner-node.js`
2. **Browser**: `test-runner-browser.html`
3. **Programmatic**: `machine()` from `vm.js`
4. **Worker**: `worker-vm.js`

## How Files Work Together

### Testing Flow
1. Test file (JSON) contains bytecode + expected result
2. Test runner loads and parses JSON
3. Creates Fn object with bytecode
4. Calls machine() to execute
5. Compares result to expected
6. Reports pass/fail

### VM Execution Flow
1. machine() receives Fn object
2. Initializes registers (code, pc, env, stack, n_args)
3. Enters main loop
4. Fetches instruction at pc
5. Increments pc
6. Dispatches on opcode
7. Executes instruction
8. Loops until HALT or error

### Multi-threading Flow
1. Main thread creates Worker
2. Posts message with bytecode
3. Worker imports all VM files
4. Worker creates Fn and runs machine()
5. Worker posts result back
6. Main thread receives and processes

## Critical Files for Modification

### To add opcodes:
Edit `vm.js` switch statement

### To change data structures:
Edit `structs.js` classes

### To add global functions:
Use `globals.js` setGlobalVar()

### To add test cases:
Create new JSON in `tests/` directory

### To modify test runner:
Edit `test-runner-node.js` or `test-runner-browser.html`

## File Relationships

**Core Dependencies** (required):
- structs.js
- opcodes.js
- globals.js
- vm.js

**Optional Dependencies** (for specific features):
- worker-vm.js (for threading)
- test runners (for testing)

**Documentation** (informational):
- README.md (user guide)
- HANDOFF.md (developer guide)
- QUICKSTART.md (quick reference)
- SUMMARY.md (session summary)

## Size Summary

| Category | Files | Total Size | Lines |
|----------|-------|------------|-------|
| Core VM | 5 | ~25 KB | ~800 |
| Testing | 2 | ~15 KB | ~450 |
| Test Cases | 4 | ~1 KB | ~100 |
| Documentation | 5 | ~45 KB | ~1500 |
| **Total** | **16** | **~86 KB** | **~2850** |

## Module Compatibility

All core VM files (structs.js, opcodes.js, globals.js, vm.js) support:
- ✅ Node.js (CommonJS)
- ✅ Browser (via script tags)
- ✅ Web Workers
- ✅ Node.js worker_threads

## Version Information

**Implementation**: Draft 1  
**Date**: October 30, 2025  
**Based On**: Peter Norvig's PAIP compile3.lisp  
**Target**: Bard Programming Language  
**Status**: Complete but untested

## Quick Navigation

**Start Here**: README.md  
**Quick Test**: QUICKSTART.md  
**Detailed Info**: HANDOFF.md  
**This Session**: SUMMARY.md  
**Source Code**: vm.js  

---

*This manifest provides a complete overview of the implementation structure and relationships.*
