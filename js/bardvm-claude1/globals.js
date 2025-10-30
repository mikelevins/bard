/**
 * globals.js - Global variable management
 * Based on Norvig's PAIP compile3.lisp
 * 
 * In Norvig's Lisp implementation, global variables are stored as properties
 * on symbols using (get var 'global-val). In JavaScript, we use a Map to
 * avoid prototype chain issues.
 */

// Global variables storage
const globalVars = new Map();

/**
 * Get a global variable's value
 * Corresponds to: (get (arg1 instr) 'global-val)
 */
function getGlobalVar(varName) {
  if (!globalVars.has(varName)) {
    throw new Error(`Unbound global variable: ${varName}`);
  }
  return globalVars.get(varName);
}

/**
 * Set a global variable's value
 * Corresponds to: (setf (get (arg1 instr) 'global-val) (top stack))
 */
function setGlobalVar(varName, value) {
  globalVars.set(varName, value);
  return value;
}

/**
 * Check if a global variable is defined
 */
function hasGlobalVar(varName) {
  return globalVars.has(varName);
}

/**
 * Clear all global variables (for testing)
 */
function clearGlobalVars() {
  globalVars.clear();
}

/**
 * Get all global variable names
 */
function globalVarNames() {
  return Array.from(globalVars.keys());
}

// Export for both Node.js and browser
if (typeof module !== 'undefined' && module.exports) {
  module.exports = {
    getGlobalVar,
    setGlobalVar,
    hasGlobalVar,
    clearGlobalVars,
    globalVarNames,
    globalVars // Export the map itself for advanced use
  };
}
