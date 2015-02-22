// Copyright (c) 2001, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.math.IntNum;
import gnu.mapping.*;
import gnu.kawa.xml.KNode;
import gnu.kawa.xml.UntypedAtomic;

public class IntegerRange
{
  public static final IntNum MIN_INT = IntNum.make(Integer.MIN_VALUE);
  public static final IntNum MAX_INT = IntNum.make(Integer.MAX_VALUE);

  /*
  public static void integerRange(int first, int last, Consumer out)
  {
    int step = first > last ? -1 : 1;
    for (;;)
      {
	out.writeInt(first);
	if (first == last)
	  break;
	first += step;
      }
  }
  */

  public static void integerRange(IntNum first, IntNum last, Consumer out)
  {
    if (IntNum.compare(first, MIN_INT) >= 0
	&& IntNum.compare(last, MAX_INT) <= 0)
      {
	int fst = first.intValue();
	int lst = last.intValue();
	if (fst <= lst)
	  {
	    for (;;)
	      {
		out.writeInt(fst);
		if (fst == lst)
		  break;
		fst++;
	      }
	  }
	return;
      }
    while (IntNum.compare(first, last) <= 0)
      {
	out.writeObject(first);
	first = IntNum.add(first, 1);
      }
  }

  public static void integerRange$X(Object first, Object last, CallContext ctx)
  {
    first = KNode.atomicValue(first);
    last = KNode.atomicValue(last);
    if (first == Values.empty || first == null
        || last == Values.empty || last == null)
      return;
    if (first instanceof UntypedAtomic)
      first = IntNum.valueOf(first.toString().trim(), 10);
    if (last instanceof UntypedAtomic)
      last = IntNum.valueOf(last.toString().trim(), 10);
    integerRange((IntNum) first, (IntNum) last, ctx.consumer);
  }
}
