/**
 * vm.js - Bard Virtual Machine
 * Based on Norvig's MACHINE function from PAIP compile3.lisp
 * 
 * This is a faithful JavaScript translation of Norvig's abstract machine.
 * The VM executes bytecode with proper support for:
 * - Lexical closures
 * - Tail call optimization
 * - First-class continuations (call/cc)
 * - Proper return address handling
 */

// Import dependencies
let Fn, RetAddr, opcode, arg1, arg2, arg3, is, top, rest2, rest3;
let getGlobalVar, setGlobalVar;

// Setup imports for Node.js or browser
if (typeof module !== 'undefined' && module.exports) {
  ({ Fn, RetAddr } = require('./structs.js'));
  ({ opcode, arg1, arg2, arg3, is, top, rest2, rest3 } = require('./opcodes.js'));
  ({ getGlobalVar, setGlobalVar } = require('./globals.js'));
}

/**
 * machine(f) - Run the abstract machine on the code for f
 * 
 * This is a direct translation of Norvig's MACHINE function.
 * It implements a stack-based VM with the following registers:
 * 
 * - code: Array of instructions (bytecode)
 * - pc: Program counter (index into code)
 * - env: Environment (array of frames, each frame is an array)
 * - stack: Evaluation stack (array)
 * - n_args: Number of arguments for function calls
 * - instr: Current instruction
 * 
 * Corresponds to:
 * (defun machine (f)
 *   "Run the abstract machine on the code for f."
 *   (let* ((code (fn-code f))
 *          (pc 0)
 *          (env nil)
 *          (stack nil)
 *          (n-args 0)
 *          (instr nil))
 *     (loop ...)))
 */
function machine(f) {
  if (!(f instanceof Fn)) {
    throw new Error(`machine: expected Fn, got ${typeof f}`);
  }

  // VM registers
  let code = f.code;
  let pc = 0;
  let env = [];
  let stack = [];
  let n_args = 0;
  let instr = null;

  // Main instruction dispatch loop
  while (true) {
    // Fetch instruction and advance PC
    instr = code[pc];
    pc++;

    const op = opcode(instr);

    // Dispatch on opcode
    switch (op) {
      
      // ===== Variable/Stack Manipulation Instructions =====
      
      case 'LVAR': {
        // Local variable reference
        // (LVAR frame offset) - Push value from environment frame
        const frame = arg1(instr);
        const offset = arg2(instr);
        stack.push(env[frame][offset]);
        break;
      }

      case 'LSET': {
        // Local variable assignment
        // (LSET frame offset) - Set environment variable to top of stack
        const frame = arg1(instr);
        const offset = arg2(instr);
        env[frame][offset] = top(stack);
        break;
      }

      case 'GVAR': {
        // Global variable reference
        // (GVAR name) - Push global variable value
        const varName = arg1(instr);
        stack.push(getGlobalVar(varName));
        break;
      }

      case 'GSET': {
        // Global variable assignment
        // (GSET name) - Set global variable to top of stack
        const varName = arg1(instr);
        setGlobalVar(varName, top(stack));
        break;
      }

      case 'POP': {
        // Pop top value from stack
        stack.pop();
        break;
      }

      case 'CONST': {
        // Push constant
        // (CONST value)
        stack.push(arg1(instr));
        break;
      }

      // ===== Branching Instructions =====

      case 'JUMP': {
        // Unconditional jump
        // (JUMP label)
        pc = arg1(instr);
        break;
      }

      case 'FJUMP': {
        // Jump if false (nil)
        // (FJUMP label) - Jump if top of stack is nil
        if (stack.pop() === null || stack.pop() === false) {
          pc = arg1(instr);
        }
        break;
      }

      case 'TJUMP': {
        // Jump if true (not nil)
        // (TJUMP label) - Jump if top of stack is not nil
        const val = stack.pop();
        if (val !== null && val !== false) {
          pc = arg1(instr);
        }
        break;
      }

      // ===== Function Call/Return Instructions =====

      case 'SAVE': {
        // Save continuation point
        // (SAVE label) - Push return address onto stack
        stack.push(new RetAddr({
          pc: arg1(instr),
          fn: f,
          env: env
        }));
        break;
      }

      case 'RETURN': {
        // Return from function
        // Return value is top of stack; ret-addr is second
        // Get return address (second element on stack)
        const retAddr = stack[stack.length - 2];
        
        if (!(retAddr instanceof RetAddr)) {
          // No return address - this is a top-level return
          return top(stack);
        }

        // Restore VM state from return address
        f = retAddr.fn;
        code = f.code;
        env = retAddr.env;
        pc = retAddr.pc;
        
        // Keep return value, discard ret-addr
        // Corresponds to: (setf stack (cons (first stack) (rest2 stack)))
        const returnValue = stack[stack.length - 1];
        stack = stack.slice(0, -2);
        stack.push(returnValue);
        break;
      }

      case 'CALLJ': {
        // Tail call (jump to function)
        // (CALLJ n-args) - Call function with n arguments
        env.shift(); // discard the top frame: (pop env)
        
        f = stack.pop();
        if (!(f instanceof Fn)) {
          throw new Error(`Cannot call: ${f} (not a function)`);
        }
        
        code = f.code;
        env = f.env;
        pc = 0;
        n_args = arg1(instr);
        break;
      }

      case 'ARGS': {
        // Set up arguments in new frame
        // (ARGS n) - Pop n arguments and create environment frame
        const nArgs = arg1(instr);
        
        if (n_args !== nArgs) {
          throw new Error(
            `Wrong number of arguments: ${nArgs} expected, ${n_args} supplied`
          );
        }
        
        // Create new frame
        const frame = new Array(nArgs);
        
        // Pop arguments in reverse order (last arg first)
        for (let i = nArgs - 1; i >= 0; i--) {
          frame[i] = stack.pop();
        }
        
        // Push frame onto environment
        env.unshift(frame);
        break;
      }

      case 'ARGS.': {
        // Set up arguments with rest parameter
        // (ARGS. n) - At least n arguments, rest go into list
        const nRequired = arg1(instr);
        
        if (n_args < nRequired) {
          throw new Error(
            `Wrong number of arguments: ${nRequired} or more expected, ${n_args} supplied`
          );
        }
        
        // Create frame with one extra slot for rest args
        const frame = new Array(nRequired + 1);
        
        // Collect extra arguments into rest list
        const restArgs = [];
        for (let i = 0; i < (n_args - nRequired); i++) {
          restArgs.unshift(stack.pop());
        }
        frame[nRequired] = restArgs;
        
        // Pop required arguments in reverse order
        for (let i = nRequired - 1; i >= 0; i--) {
          frame[i] = stack.pop();
        }
        
        // Push frame onto environment
        env.unshift(frame);
        break;
      }

      case 'FN': {
        // Create closure
        // (FN fn-template) - Create closure with current environment
        const fnTemplate = arg1(instr);
        
        stack.push(new Fn({
          code: fnTemplate.code,
          env: env,  // Capture current environment
          name: fnTemplate.name,
          args: fnTemplate.args
        }));
        break;
      }

      case 'PRIM': {
        // Call primitive function
        // (PRIM fn) - Call JavaScript function with n_args arguments
        const primFn = arg1(instr);
        
        // Pop arguments
        const args = [];
        for (let i = 0; i < n_args; i++) {
          args.unshift(stack.pop());
        }
        
        // Call primitive and push result
        let result;
        if (typeof primFn === 'function') {
          result = primFn(...args);
        } else if (typeof primFn === 'string') {
          // Look up global function by name
          result = getGlobalVar(primFn)(...args);
        } else {
          throw new Error(`Invalid primitive: ${primFn}`);
        }
        
        stack.push(result);
        break;
      }

      // ===== Continuation Instructions =====

      case 'CC': {
        // Capture current continuation
        // Create a function that restores the current stack
        const continuationCode = [
          ['ARGS', 1],
          ['LVAR', 0, 0],  // Get the argument
          ['SET-CC'],      // Restore stack
          ['RETURN']
        ];
        
        stack.push(new Fn({
          code: continuationCode,
          env: [[...stack]],  // Capture current stack
          name: 'continuation',
          args: ['value']
        }));
        break;
      }

      case 'SET-CC': {
        // Restore continuation
        // Replace entire stack with captured stack
        const capturedStack = top(stack);
        if (!Array.isArray(capturedStack)) {
          throw new Error(`SET-CC: expected array, got ${typeof capturedStack}`);
        }
        stack = [...capturedStack];
        break;
      }

      // ===== Nullary Operations =====

      case 'SCHEME-READ':
      case 'NEWLINE': {
        // These would require I/O support
        throw new Error(`${op} not implemented in JavaScript VM`);
      }

      // ===== Unary Operations =====

      case 'CAR': {
        const list = stack.pop();
        stack.push(list[0]);
        break;
      }

      case 'CDR': {
        const list = stack.pop();
        stack.push(list.slice(1));
        break;
      }

      case 'CADR': {
        const list = stack.pop();
        stack.push(list[1]);
        break;
      }

      case 'NOT': {
        const val = stack.pop();
        stack.push(val === null || val === false);
        break;
      }

      case 'LIST1': {
        const arg = stack.pop();
        stack.push([arg]);
        break;
      }

      case 'COMPILER': {
        throw new Error('COMPILER not implemented in JavaScript VM');
      }

      case 'DISPLAY':
      case 'WRITE': {
        const val = stack.pop();
        console.log(val);
        stack.push(val);
        break;
      }

      case 'RANDOM': {
        const n = stack.pop();
        stack.push(Math.floor(Math.random() * n));
        break;
      }

      // ===== Binary Operations =====

      case '+': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a + b);
        break;
      }

      case '-': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a - b);
        break;
      }

      case '*': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a * b);
        break;
      }

      case '/': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a / b);
        break;
      }

      case '<': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a < b);
        break;
      }

      case '>': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a > b);
        break;
      }

      case '<=': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a <= b);
        break;
      }

      case '>=': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a >= b);
        break;
      }

      case '/=':
      case '!=': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a !== b);
        break;
      }

      case '=':
      case '==': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a === b);
        break;
      }

      case 'CONS': {
        const cdr = stack.pop();
        const car = stack.pop();
        // In JavaScript, represent cons as [car, ...cdr]
        if (Array.isArray(cdr)) {
          stack.push([car, ...cdr]);
        } else {
          stack.push([car, cdr]);
        }
        break;
      }

      case 'LIST2': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push([a, b]);
        break;
      }

      case 'NAME!': {
        const name = stack.pop();
        const fn = stack.pop();
        if (fn instanceof Fn && !fn.name) {
          fn.name = name;
        }
        stack.push(name);
        break;
      }

      case 'EQ': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a === b);
        break;
      }

      case 'EQUAL': {
        const b = stack.pop();
        const a = stack.pop();
        // Deep equality check
        stack.push(JSON.stringify(a) === JSON.stringify(b));
        break;
      }

      case 'EQL': {
        const b = stack.pop();
        const a = stack.pop();
        stack.push(a === b);
        break;
      }

      // ===== Ternary Operations =====

      case 'LIST3': {
        const c = stack.pop();
        const b = stack.pop();
        const a = stack.pop();
        stack.push([a, b, c]);
        break;
      }

      // ===== Constants =====

      case 'T':
      case true: {
        stack.push(true);
        break;
      }

      case 'NIL':
      case null:
      case false: {
        stack.push(null);
        break;
      }

      case -1:
      case 0:
      case 1:
      case 2: {
        stack.push(op);
        break;
      }

      // ===== Other =====

      case 'HALT': {
        return top(stack);
      }

      default:
        throw new Error(`Unknown opcode: ${op}`);
    }
  }
}

// Export for both Node.js and browser
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { machine };
}
