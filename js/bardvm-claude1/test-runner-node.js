#!/usr/bin/env node
/**
 * test-runner-node.js - Node.js test runner for Bard VM
 * 
 * Usage:
 *   node test-runner-node.js [bytecode-file ...]
 *   node test-runner-node.js tests/*.json
 */

const fs = require('fs');
const path = require('path');
const { Worker } = require('worker_threads');
const { Fn } = require('./structs.js');
const { machine } = require('./vm.js');
const { setGlobalVar, clearGlobalVars } = require('./globals.js');

/**
 * Load bytecode from a JSON file
 */
function loadBytecode(filepath) {
  const data = JSON.parse(fs.readFileSync(filepath, 'utf8'));
  return data;
}

/**
 * Run bytecode directly in main thread
 */
function runDirect(bytecodeData) {
  // Clear globals
  clearGlobalVars();

  // Initialize globals if present
  if (bytecodeData.globals) {
    for (const [name, value] of Object.entries(bytecodeData.globals)) {
      setGlobalVar(name, value);
    }
  }

  // Create function
  const fn = new Fn({
    code: bytecodeData.code,
    name: bytecodeData.name || 'main',
    args: bytecodeData.args || []
  });

  // Run
  const result = machine(fn);
  return result;
}

/**
 * Run bytecode in a worker thread
 */
function runInWorker(bytecodeData) {
  return new Promise((resolve, reject) => {
    const worker = new Worker(path.join(__dirname, 'worker-vm.js'));
    
    worker.on('message', (msg) => {
      if (msg.type === 'result') {
        worker.terminate();
        resolve(msg.value);
      } else if (msg.type === 'error') {
        worker.terminate();
        reject(new Error(msg.error));
      }
    });

    worker.on('error', (err) => {
      worker.terminate();
      reject(err);
    });

    worker.postMessage({
      type: 'run',
      code: bytecodeData.code,
      name: bytecodeData.name || 'main',
      args: bytecodeData.args || [],
      globals: bytecodeData.globals || {},
      id: 1
    });
  });
}

/**
 * Run a test file
 */
async function runTest(filepath, useWorker = false) {
  console.log(`\n${'='.repeat(70)}`);
  console.log(`Running: ${path.basename(filepath)}`);
  console.log('='.repeat(70));

  try {
    const bytecodeData = loadBytecode(filepath);
    
    console.log(`Name: ${bytecodeData.name || '(unnamed)'}`);
    console.log(`Description: ${bytecodeData.description || '(none)'}`);
    console.log(`Mode: ${useWorker ? 'Worker Thread' : 'Direct'}`);
    console.log();

    const startTime = Date.now();
    let result;

    if (useWorker) {
      result = await runInWorker(bytecodeData);
    } else {
      result = runDirect(bytecodeData);
    }

    const endTime = Date.now();
    const elapsed = endTime - startTime;

    console.log(`Result: ${JSON.stringify(result)}`);
    console.log(`Time: ${elapsed}ms`);

    // Check expected result if present
    if (bytecodeData.expected !== undefined) {
      const passed = JSON.stringify(result) === JSON.stringify(bytecodeData.expected);
      console.log(`Expected: ${JSON.stringify(bytecodeData.expected)}`);
      console.log(`Status: ${passed ? '✓ PASS' : '✗ FAIL'}`);
      return passed;
    } else {
      console.log('Status: ✓ COMPLETED (no expected result)');
      return true;
    }

  } catch (error) {
    console.error(`✗ ERROR: ${error.message}`);
    if (error.stack) {
      console.error(error.stack);
    }
    return false;
  }
}

/**
 * Main entry point
 */
async function main() {
  const args = process.argv.slice(2);

  if (args.length === 0) {
    console.log('Usage: node test-runner-node.js [options] <bytecode-file ...>');
    console.log('');
    console.log('Options:');
    console.log('  --worker    Run tests in worker threads');
    console.log('  --direct    Run tests in main thread (default)');
    console.log('');
    console.log('Examples:');
    console.log('  node test-runner-node.js test.json');
    console.log('  node test-runner-node.js --worker tests/*.json');
    process.exit(1);
  }

  let useWorker = false;
  const files = [];

  for (const arg of args) {
    if (arg === '--worker') {
      useWorker = true;
    } else if (arg === '--direct') {
      useWorker = false;
    } else {
      files.push(arg);
    }
  }

  if (files.length === 0) {
    console.error('No bytecode files specified');
    process.exit(1);
  }

  let passed = 0;
  let failed = 0;

  for (const file of files) {
    const result = await runTest(file, useWorker);
    if (result) {
      passed++;
    } else {
      failed++;
    }
  }

  console.log(`\n${'='.repeat(70)}`);
  console.log('Summary');
  console.log('='.repeat(70));
  console.log(`Total: ${passed + failed}`);
  console.log(`Passed: ${passed}`);
  console.log(`Failed: ${failed}`);

  process.exit(failed === 0 ? 0 : 1);
}

// Run main
if (require.main === module) {
  main().catch(err => {
    console.error('Fatal error:', err);
    process.exit(1);
  });
}

module.exports = { loadBytecode, runDirect, runInWorker, runTest };
