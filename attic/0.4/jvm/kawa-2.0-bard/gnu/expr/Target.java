// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;

import gnu.bytecode.Type;

/** This represents where a compiler can put the result of an expression. */

public abstract class Target
{
  public abstract Type getType();

  public abstract void compileFromStack(Compilation comp, Type stackType);

  /** A Target which means that the result is ignored. */
  public static final Target Ignore = new IgnoreTarget();

  /** A Target which means to push an Object on the JVM stack. */
  public static final Target pushObject = new StackTarget(Type.pointer_type);

  /** Return a Target to push a value of specified type on JCM stack. */
  public static Target pushValue(Type type)
  {
    return type.isVoid() ? Target.Ignore : StackTarget.getInstance(type);
  }
}
