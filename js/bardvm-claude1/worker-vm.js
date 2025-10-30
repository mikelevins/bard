/**
 * worker-vm.js - Worker thread wrapper for Bard VM
 * 
 * This enables running multiple VMs concurrently in separate threads.
 * Can be used with Node.js worker_threads or browser Web Workers.
 */

// Import VM components (works in both Node.js workers and browser workers via importScripts)
if (typeof module !== 'undefined' && module.exports) {
  // Node.js worker_threads
  const { parentPort, workerData } = require('worker_threads');
  const { Fn } = require('./structs.js');
  const { machine } = require('./vm.js');
  const { setGlobalVar } = require('./globals.js');

  parentPort.on('message', (msg) => {
    try {
      switch (msg.type) {
        case 'run': {
          // Initialize globals if provided
          if (msg.globals) {
            for (const [name, value] of Object.entries(msg.globals)) {
              setGlobalVar(name, value);
            }
          }

          // Create function from bytecode
          const fn = new Fn({
            code: msg.code,
            name: msg.name || 'main',
            args: msg.args || []
          });

          // Run the VM
          const result = machine(fn);

          // Send result back
          parentPort.postMessage({
            type: 'result',
            value: result,
            id: msg.id
          });
          break;
        }

        case 'stop': {
          // Clean shutdown
          process.exit(0);
        }

        default:
          parentPort.postMessage({
            type: 'error',
            error: `Unknown message type: ${msg.type}`,
            id: msg.id
          });
      }
    } catch (error) {
      parentPort.postMessage({
        type: 'error',
        error: error.message,
        stack: error.stack,
        id: msg.id
      });
    }
  });
} else {
  // Browser Web Worker
  importScripts('./structs.js', './opcodes.js', './globals.js', './vm.js');

  self.onmessage = function(e) {
    const msg = e.data;
    
    try {
      switch (msg.type) {
        case 'run': {
          // Initialize globals if provided
          if (msg.globals) {
            for (const [name, value] of Object.entries(msg.globals)) {
              setGlobalVar(name, value);
            }
          }

          // Create function from bytecode
          const fn = new Fn({
            code: msg.code,
            name: msg.name || 'main',
            args: msg.args || []
          });

          // Run the VM
          const result = machine(fn);

          // Send result back
          self.postMessage({
            type: 'result',
            value: result,
            id: msg.id
          });
          break;
        }

        case 'stop': {
          // Clean shutdown
          self.close();
          break;
        }

        default:
          self.postMessage({
            type: 'error',
            error: `Unknown message type: ${msg.type}`,
            id: msg.id
          });
      }
    } catch (error) {
      self.postMessage({
        type: 'error',
        error: error.message,
        stack: error.stack,
        id: msg.id
      });
    }
  };
}
