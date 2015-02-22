// Copyright (c) 2008 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** Support for code block that one may be exited.
 * You may optionally exit with a value, and/or run intervening finally blocks.
 *
 * <p>Typical usage::
 * <blockquote><pre>
 * CodeAttr code = ...;
 * Type retType = ...; // a Type or null
 * ExitableBlock block = code.startExitableBlock(retType, true);
 * ...
 * ... block.exit() ...; // conditionally
 * ...
 * code.endExitableBlock();
 * </pre></blockquote>
 *
 * <p>The <code>block.exit()</code> compiles to a transfer to the end
 * of the <code>block</code>, executing any <code>finally</code>-blocks
 * within the block that surround the call to <code>exit</code>.
 *
 * <p>If the <code>ExitableBlock</code> should leave a result on
 * the stack, then specify the type of the result as the <code>retType</code>.
 * The block itself must push a result before calling
 * <code>endExitableBlock</code> (unless <code>code.reachableHere()</code>
 * is false), and must also push a result before <code>block.exit().</code>.
 */

public class ExitableBlock
{
  Variable resultVariable;
  CodeAttr code;
  Type resultType;
  TryState initialTryState;
  Label endLabel;
  ExitableBlock outer;
  // The innermost current TryState which contains an exit to the block.
  TryState currentTryState;
  // Next ExitableBlock in list headed by currentTryState.exitCases.
  ExitableBlock nextCase;
  int switchCase;
  int startStackSize;

  ExitableBlock (Type resultType, CodeAttr code, boolean runFinallyBlocks)
  {
    this.code = code;
    this.resultType = resultType;
    this.startStackSize = code.SP;
    initialTryState = code.try_stack;
    if (runFinallyBlocks && resultType != null)
      {
        code.pushScope();
        Variable var = code.addLocal(resultType);
        resultVariable = var;
        // We need to initialize the variable for the sake of the verifier.
        code.emitStoreDefaultValue(var);
        switchCase = ++code.exitableBlockLevel;
      }
    endLabel = new Label(code);
  }

  void finish ()
  {
    boolean reachable = code.reachableHere();
    if (resultVariable != null && reachable)
      code.emitStore(resultVariable);
    endLabel.define(code);
    if (! reachable && ! endLabel.needsStackMapEntry)
      code.setUnreachable();
    else if (resultVariable != null)
      code.emitLoad(resultVariable);
    if (resultVariable != null)
      {
        code.popScope();
        --code.exitableBlockLevel;
      }
  }

  /** Exit this surrounding block, executing finally blocks as needed.
   * Return a value as the result of this ExitableBlock. */
  public void exit ()
  {
    if (resultVariable != null)
      code.emitStore(resultVariable);
    exit(TryState.outerHandler(code.try_stack, initialTryState));
  }

  /** If an exit is simple, return the label for block end.
   * The exit is simple if there is no intervening finally blocks.
   */
  public Label exitIsGoto ()
  {
    if (TryState.outerHandler(code.try_stack, initialTryState) == initialTryState)
      return endLabel;
    else
      return null;
  }

    private void popStack(CodeAttr code) {
        int retSize = resultVariable != null || resultType == null ? 0
            : resultType.size > 4 ? 2 : 1;
        if (code.SP == startStackSize + retSize)
            return;
        Variable resultVar;
        if (retSize > 0) {
            code.pushScope();
            resultVar = code.addLocal(resultType);
            code.emitStore(resultVar);
        }
        else
            resultVar = null;
        code.emitPop(code.SP - startStackSize);
        if (resultVar != null) {
            code.emitLoad(resultVar);
            code.popScope();
        }
    }

  /** Exit this surrounding block, executing finally blocks as needed. */
  void exit (TryState activeTry)
  {
    popStack(code);
    if (activeTry == initialTryState)
      code.emitGoto(endLabel);
    else if (code.useJsr())
      {
        for (TryState stack = code.try_stack;
             stack != initialTryState; stack = stack.previous)
          {
            if (stack.finally_subr != null         // there is a finally block
                && stack.finally_ret_addr == null) // 'return' is not inside it
              {
                code.emitJsr(stack.finally_subr);
              }
          }
        code.emitGoto(endLabel);
      }
    else
      {
        if (currentTryState == null)
          linkCase(activeTry);
        if (activeTry.saved_result != null)
          code.emitStoreDefaultValue(activeTry.saved_result);
        code.emitPushInt(switchCase);
        code.emitPushNull(); // No caught Throwable.
        code.emitGoto(activeTry.finally_subr);
      }
  }

  void linkCase (TryState tryState)
  {
    if (currentTryState != tryState)
      {
        if (currentTryState != null)
          throw new Error();
        nextCase = tryState.exitCases;
        tryState.exitCases = this;
        currentTryState = tryState;
      }
  }
}
