# Quick Start Guide

## Installation

No installation needed! This is pure JavaScript with no dependencies.

## Quick Test

```bash
cd /Users/mikel/repos/lisp/bard/js/bardvm-claude1

# Test simple arithmetic
node test-runner-node.js tests/simple-arithmetic.json

# Expected output:
# ======================================================================
# Running: simple-arithmetic.json
# ======================================================================
# Name: simple-arithmetic
# Description: Test basic arithmetic: (+ 2 3) = 5
# Mode: Direct
#
# Result: 5
# Expected: 5
# Status: ✓ PASS
```

## Run All Tests

```bash
# Direct execution (in main thread)
node test-runner-node.js tests/*.json

# Worker threads (concurrent execution)
node test-runner-node.js --worker tests/*.json

# Or using npm scripts
npm test          # direct mode
npm run test:worker  # worker mode
```

## Browser Testing

```bash
# Start web server
npm run serve
# or
python3 -m http.server 8000

# Open browser to:
# http://localhost:8000/test-runner-browser.html
```

## Creating a Simple Test

Create `my-test.json`:

```json
{
  "name": "my-first-test",
  "description": "Multiply 6 by 7",
  "code": [
    ["ARGS", 0],
    ["CONST", 6],
    ["CONST", 7],
    ["*"],
    ["RETURN"]
  ],
  "expected": 42
}
```

Run it:
```bash
node test-runner-node.js my-test.json
```

## Programmatic Usage

```javascript
// Load the VM
const { Fn } = require('./structs.js');
const { machine } = require('./vm.js');

// Create bytecode
const code = [
  ['ARGS', 0],      // No arguments
  ['CONST', 10],    // Push 10
  ['CONST', 32],    // Push 32
  ['+'],            // Add them
  ['RETURN']        // Return result
];

// Create function object
const fn = new Fn({ code, name: 'add-numbers' });

// Run it!
const result = machine(fn);
console.log(result); // 42
```

## Understanding the Stack

The VM uses a stack for evaluation:

```
Code: ['CONST', 10]
Stack: [] → [10]

Code: ['CONST', 32]
Stack: [10] → [10, 32]

Code: ['+']
Stack: [10, 32] → [42]

Code: ['RETURN']
Returns: 42
```

## Common Bytecode Patterns

### Simple Calculation
```javascript
[
  ['ARGS', 0],
  ['CONST', 5],
  ['CONST', 3],
  ['+'],
  ['RETURN']
]
// Result: 8
```

### If Expression
```javascript
[
  ['ARGS', 0],
  ['CONST', 5],
  ['CONST', 3],
  ['<'],              // Is 5 < 3?
  ['FJUMP', 6],       // If false, jump to 6
  ['CONST', 100],     // True branch
  ['RETURN'],
  ['CONST', 200],     // False branch (label 6)
  ['RETURN']
]
// Result: 200
```

### Function with Arguments
```javascript
[
  ['ARGS', 2],        // Expect 2 arguments
  ['LVAR', 0, 0],     // First argument
  ['LVAR', 0, 1],     // Second argument
  ['+'],
  ['RETURN']
]
// Call with: machine(fn) where env contains [5, 3]
// Result: 8
```

## Troubleshooting

### "Unknown opcode" error
- Check opcode spelling (case-sensitive)
- Ensure opcode is in vm.js switch statement

### "Wrong number of arguments" error
- ARGS instruction doesn't match actual arguments
- Check n_args is set correctly before ARGS

### "Cannot call: X (not a function)" error
- Stack doesn't have a Fn object where expected
- Check CALLJ calling convention

### Stack underflow
- Too many POP operations
- Binary operations without enough values
- Check stack state before each operation

## Next Steps

1. Read HANDOFF.md for detailed implementation notes
2. Read README.md for complete documentation
3. Look at test files in tests/ directory
4. Try creating your own test cases
5. Report bugs or issues

## Getting Help

Check these files in order:
1. This file (QUICKSTART.md) - basic usage
2. README.md - comprehensive documentation
3. HANDOFF.md - implementation details and debugging
4. Source code comments - implementation specifics

Happy coding! 🚀
