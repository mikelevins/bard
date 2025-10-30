/**
 * structs.js - Data structures for Bard VM
 * Based on Norvig's PAIP compile3.lisp
 */

/**
 * Fn - Function/closure object
 * Corresponds to (defstruct (fn ...) code env name args)
 */
class Fn {
  constructor({ code = null, env = null, name = null, args = null } = {}) {
    this.code = code;    // Array of instructions (bytecode)
    this.env = env;      // Environment (array of frames)
    this.name = name;    // Optional function name
    this.args = args;    // Argument list (for display purposes)
  }

  toString() {
    return `{${this.name || '??'}}`;
  }
}

/**
 * RetAddr - Return address (continuation point)
 * Corresponds to (defstruct ret-addr fn pc env)
 */
class RetAddr {
  constructor({ fn = null, pc = 0, env = null } = {}) {
    this.fn = fn;        // Function to return to
    this.pc = pc;        // Program counter in that function
    this.env = env;      // Environment at return point
  }

  toString() {
    return `<RetAddr pc=${this.pc}>`;
  }
}

// Export for both Node.js and browser
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { Fn, RetAddr };
}
