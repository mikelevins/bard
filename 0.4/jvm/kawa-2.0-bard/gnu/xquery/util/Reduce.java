// Copyright (c) 2002  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.IntNum;
import gnu.kawa.functions.AddOp;

public class Reduce
{
  public static Object sum (Object arg)
    throws Throwable
  {
    return sum(arg, IntNum.zero());
  }

  public static Object sum (Object arg, Object zero)
    throws Throwable
  {
    if (arg instanceof Values)
      {
	Values tlist = (Values) arg;
	int pos = 0;
        Object next = tlist.getPosNext(pos);
        if (next == Sequence.eofValue)
          return zero;
        Object result = MinMax.convert(next);
	for (;;)
	  {
            pos = tlist.nextPos(pos);
	    next = tlist.getPosNext(pos);
	    if (next == Sequence.eofValue)
	      return result;
            next = MinMax.convert(next);
	    result = AddOp.apply2(1, result, next);
	  }
      }
    else
      return (Number) MinMax.convert(arg);
  }
}
