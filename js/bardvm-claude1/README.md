# Bard VM - JavaScript Implementation

A faithful JavaScript implementation of Peter Norvig's Scheme virtual machine from "Paradigms of Artificial Intelligence Programming" (PAIP), Chapter 23.

## Overview

This implementation provides a complete bytecode VM based on Norvig's design, with the following features:

- **All Norvig Opcodes**: Faithful implementation of all 28+ instructions
- **Lexical Closures**: Full support for closures with proper environment capture
- **Tail Call Optimization**: CALLJ instruction for proper tail calls
- **Continuations**: First-class continuations via CC/SET-CC instructions
- **Multi-threading**: Support for running multiple VMs in worker threads
- **Test Infrastructure**: Complete test runners for both Node.js and browser

## Architecture

### Core Components

1. **structs.js** - Data structures
   - `Fn` class: Function/closure objects (code, env, name, args)
   - `RetAddr` class: Return addresses for SAVE/RETURN mechanism

2. **opcodes.js** - Helper functions
   - Instruction parsing: `opcode()`, `arg1()`, `arg2()`, `arg3()`
   - Stack operations: `top()`, `rest2()`, `rest3()`

3. **globals.js** - Global variable management
   - Uses JavaScript Map to avoid prototype issues
   - `getGlobalVar()`, `setGlobalVar()`, `clearGlobalVars()`

4. **vm.js** - Main virtual machine
   - `machine(fn)` - The core VM loop
   - Complete instruction dispatch
   - All 28+ opcodes from Norvig's design

5. **worker-vm.js** - Worker thread wrapper
   - Enables concurrent VM execution
   - Works with Node.js worker_threads and Web Workers

### VM Registers

The VM maintains the following registers during execution:

- **code**: Array of instructions (bytecode)
- **pc**: Program counter (index into code array)
- **env**: Environment (array of frames, each frame is an array)
- **stack**: Evaluation stack (array)
- **n_args**: Number of arguments for function calls
- **instr**: Current instruction

### Environment Structure

```javascript
env = [
  [local0, local1, local2],  // Current frame (innermost)
  [outer0, outer1],          // Outer frame
  ...                        // More frames
]
```

- LVAR with frame=0 accesses current frame
- LVAR with frame=1 accesses outer frame, etc.

## Instruction Set

### Variable/Stack Manipulation

- **LVAR** frame offset - Push local variable
- **LSET** frame offset - Set local variable
- **GVAR** name - Push global variable
- **GSET** name - Set global variable
- **POP** - Pop stack
- **CONST** value - Push constant

### Control Flow

- **JUMP** label - Unconditional jump
- **FJUMP** label - Jump if false (nil)
- **TJUMP** label - Jump if true (not nil)

### Function Calls

- **SAVE** label - Save return address
- **RETURN** - Return from function
- **CALLJ** n-args - Tail call (jump to function)
- **ARGS** n - Set up n arguments in new frame
- **ARGS.** n - Set up n+ arguments with rest parameter
- **FN** template - Create closure

### Continuations

- **CC** - Capture current continuation
- **SET-CC** - Restore continuation

### Arithmetic

- **+**, **-**, **\***, **/** - Binary arithmetic
- **<**, **>**, **<=**, **>=** - Comparisons
- **=**, **/=** - Equality

### List Operations

- **CONS** - Construct pair
- **CAR**, **CDR**, **CADR** - List accessors
- **LIST1**, **LIST2**, **LIST3** - List constructors

### Other

- **NOT** - Logical negation
- **HALT** - Stop execution and return value
- **PRIM** fn - Call primitive function

### Constants

- **T**, **NIL**, **0**, **1**, **2**, **-1** - Constant shortcuts

## Bytecode Format

Bytecode is represented as JSON with the following structure:

```json
{
  "name": "test-name",
  "description": "Test description",
  "code": [
    ["ARGS", 0],
    ["CONST", 42],
    ["RETURN"]
  ],
  "globals": {
    "varname": value
  },
  "expected": 42
}
```

## Usage

### Node.js

```bash
# Run tests directly
node test-runner-node.js tests/simple-arithmetic.json

# Run tests in worker threads
node test-runner-node.js --worker tests/*.json

# Make executable
chmod +x test-runner-node.js
./test-runner-node.js tests/*.json
```

### Browser

1. Start a local web server:
   ```bash
   python3 -m http.server 8000
   ```

2. Open http://localhost:8000/test-runner-browser.html

3. Load test files and click "Run Direct" or "Run in Workers"

### Programmatic Use

```javascript
const { Fn } = require('./structs.js');
const { machine } = require('./vm.js');
const { setGlobalVar } = require('./globals.js');

// Create a function
const fn = new Fn({
  code: [
    ['ARGS', 0],
    ['CONST', 2],
    ['CONST', 3],
    ['+'],
    ['RETURN']
  ],
  name: 'add-example'
});

// Run it
const result = machine(fn);
console.log(result); // 5
```

## Testing

Example test files are provided in the `tests/` directory:

- **simple-arithmetic.json** - Basic arithmetic operations
- **closure.json** - Lexical closure test
- **conditional.json** - If expression test

### Creating New Tests

Create a JSON file with this structure:

```json
{
  "name": "my-test",
  "description": "What this test does",
  "code": [
    ["ARGS", 0],
    ... instructions ...
    ["RETURN"]
  ],
  "globals": {
    "function-name": {
      "code": [ ... ]
    }
  },
  "expected": expectedResult
}
```

## Differences from Norvig's Lisp

1. **Environment representation**: Arrays instead of lists
2. **Stack growth**: JavaScript arrays grow at the end (push/pop)
3. **Booleans**: JavaScript true/false instead of T/NIL
4. **Lists**: JavaScript arrays instead of cons cells
5. **Terminology**: "method" instead of "lambda" (per Bard conventions)

## Implementation Notes

### Faithful Translation

This implementation faithfully follows Norvig's design:

- Same instruction set and opcodes
- Same calling conventions
- Same environment structure
- Same continuation mechanism

### Thread Safety

Each worker thread has its own:
- Global variable namespace
- VM state (code, pc, env, stack)
- Memory space

Workers communicate via message passing only.

## Performance

The VM is designed for correctness and clarity rather than raw speed. For production use, consider:

- JIT compilation to JavaScript
- Bytecode optimization passes
- Specialized instructions for common patterns

## Future Enhancements

Potential improvements:

1. **Debugging**: Breakpoints, single-stepping, stack traces
2. **Profiling**: Instruction counts, hotspot analysis
3. **Optimization**: Peephole optimizer, constant folding
4. **I/O**: SCHEME-READ, NEWLINE implementations
5. **Serialization**: Save/restore VM state

## References

- Peter Norvig, "Paradigms of Artificial Intelligence Programming", Chapter 23
- Bard Programming Language Design Documents
- Mark Tarver's Shen language (similar portable VM approach)

## License

Based on code from PAIP, Copyright (c) 1991 Peter Norvig.
JavaScript implementation for the Bard project.
