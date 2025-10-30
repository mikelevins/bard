/**
 * opcodes.js - Opcode helper functions
 * Based on Norvig's PAIP compile3.lisp
 */

/**
 * Get the opcode of an instruction
 * Corresponds to: (defun opcode (instr) (if (label-p instr) :label (first instr)))
 */
function opcode(instr) {
  if (typeof instr === 'string' || typeof instr === 'number') {
    // Simple opcode like 'HALT' or constant shortcuts like 0, 1, 2, etc.
    return instr;
  }
  return Array.isArray(instr) ? instr[0] : instr;
}

/**
 * Get first argument of an instruction
 */
function arg1(instr) {
  return Array.isArray(instr) ? instr[1] : undefined;
}

/**
 * Get second argument of an instruction
 */
function arg2(instr) {
  return Array.isArray(instr) ? instr[2] : undefined;
}

/**
 * Get third argument of an instruction
 */
function arg3(instr) {
  return Array.isArray(instr) ? instr[3] : undefined;
}

/**
 * Check if instruction's opcode is one of the given ops
 * Corresponds to: (defun is (instr op) ...)
 */
function is(instr, op) {
  const instrOp = opcode(instr);
  if (Array.isArray(op)) {
    return op.includes(instrOp);
  }
  return instrOp === op;
}

/**
 * Get the top element of the stack
 */
function top(stack) {
  return stack[stack.length - 1];
}

/**
 * Get rest of list after first two elements
 * Corresponds to: (defun rest2 (x) (rest (rest x)))
 */
function rest2(list) {
  return list.slice(2);
}

/**
 * Get rest of list after first three elements
 */
function rest3(list) {
  return list.slice(3);
}

// Export for both Node.js and browser
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    opcode,
    arg1,
    arg2,
    arg3,
    is,
    top,
    rest2,
    rest3
  };
}
