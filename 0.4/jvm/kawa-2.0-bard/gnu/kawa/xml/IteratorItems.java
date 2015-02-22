// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
/* #ifdef JAVA2 */
import java.util.Iterator;
/* #endif */

/* A function that maps an Iterator into the sequence of ite elements. */

public class IteratorItems extends MethodProc
{
  public static IteratorItems iteratorItems = new IteratorItems();

  public void apply (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    Object arg = ctx.getNextArg();
    ctx.lastArg();

    /* #ifdef JAVA2 */
    Iterator iter = (Iterator) arg;
    /* #endif */
    /* #ifndef JAVA2 */
    // SeqPosition iter = (SeqPosition) arg;
    /* #endif */
    while (iter.hasNext())
      {
	Object val = iter.next();
	Values.writeValues(val, out);
      }
  }
}
