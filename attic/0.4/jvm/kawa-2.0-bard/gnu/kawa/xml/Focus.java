// Copyright (C) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.math.IntNum;

/** This is only used for XSLT, which should be fixed. */

public final class Focus extends TreePosition
{
  /* #ifdef JAVA2 */
  static ThreadLocal current = new ThreadLocal();
  /* #endif */
  /* #ifndef JAVA2 */
  // static Focus current = new Focus();
  /* #endif */

  public static Focus getCurrent()
  {
    /* #ifdef JAVA2 */
    Object obj = current.get();
    if (obj == null)
      {
	obj = new Focus();
	current.set(obj);
      }
    return (Focus) obj;
    /* #endif */
    /* #ifndef JAVA2 */
    // return current;
    /* #endif */
  }

  public long position;
  IntNum contextPosition;

  public static void compileGetCurrent(Compilation comp)
  {
    // FIXME This should be optimized so it only done once per method.
    CodeAttr code = comp.getCode();
    code.emitInvoke(getCurrentFocusMethod);
  }

  public static final ClassType TYPE = ClassType.make("gnu.kawa.xml.Focus");
  static final Method getCurrentFocusMethod
    = TYPE.getDeclaredMethod("getCurrent", 0);
}
