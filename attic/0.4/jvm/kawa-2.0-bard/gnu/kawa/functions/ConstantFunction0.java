// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.expr.QuoteExp;

/** A 0-argument function that returns a constant value.
 * Used for false() and true() in XQuery. */

public class ConstantFunction0 extends Procedure0
{
  final Object value;
  final QuoteExp constant;

  public ConstantFunction0(String name, Object value)
  {
    this(name, QuoteExp.getInstance(value));
  }

  public ConstantFunction0(String name, QuoteExp constant)
  {
    super(name);
    this.value = constant.getValue();
    this.constant = constant;
     setProperty(Procedure.validateApplyKey,
                 "gnu.kawa.functions.CompileMisc:validateApplyConstantFunction0");
  }

  public Object apply0() { return value; }
}
