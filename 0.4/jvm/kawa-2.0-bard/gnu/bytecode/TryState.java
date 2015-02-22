// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** The state of a try statement. */

public class TryState {
  /** The surrounding TryState, if any. */
  TryState previous;

  /** The label for the code following the entire try-statement.
   * Allocated lazily: If null, the following code is unreachable.
   */
  Label end_label;

  /** If this "try" has a "finally", the Label of the "finally" sub-routine. */
  Label finally_subr;

  /** Used for the return address of the finally subroutine (if any). */
  Variable finally_ret_addr;

  /** Non-null if we need a temporary to save the result. */
  Variable saved_result;

  /** If the {@code SP > 0} when we entered the try, the stack is saved here. */
  Variable[] savedStack;

  Label start_try;
  Label end_try;

  /** If we are inside a try, the type of variable matched. */
  ClassType try_type;

  /** Only used in emitWithCleanupStart mode. */
  Type[] savedTypes;

  ExitableBlock exitCases;

  Variable exception;

  boolean tryClauseDone;

  public TryState (CodeAttr code)
  {
    previous = code.try_stack;
    code.try_stack = this;
    start_try = code.getLabel();
  }

  /* Skip TryStates without finally blocks, or if we're no longer in
   * the try-clause. */
  static TryState outerHandler (TryState innerTry, TryState outerTry)
  {
    while (innerTry != outerTry
           && (innerTry.finally_subr == null || innerTry.tryClauseDone))
      innerTry = innerTry.previous;
    return innerTry;
  }
}
