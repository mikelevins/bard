// Copyright (c) 2002, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.TextUtils;
import gnu.kawa.xml.*;
import gnu.kawa.functions.*;

public class MinMax
{
  public static Object min (Object arg, NamedCollator collation)
  {
    return minMax(arg, false, collation);
  }

  public static Object max (Object arg, NamedCollator collation)
  {
    return minMax(arg, true, collation);
  }

  public static Object minMax (Object arg, boolean returnMax,
                               NamedCollator collation)
  {
    if (arg instanceof Values)
      {
	Values tlist = (Values) arg;
	int pos = 0;
        int flags = returnMax ? Compare.TRUE_IF_GRT :  Compare.TRUE_IF_LSS;
        Object cur = tlist.getPosNext(pos);
        if (cur == Sequence.eofValue)
          return Values.empty;
        Object result = convert(cur);
	for (;;)
	  {
            pos = tlist.nextPos(pos);
	    cur = tlist.getPosNext(pos);
	    if (cur == Sequence.eofValue)
	      return result;
            cur = convert(cur);

            if (result instanceof Number || cur instanceof Number)
              {
                int code1 = Arithmetic.classifyValue(result);
                int code2 = Arithmetic.classifyValue(cur);
                int rcode = NumberCompare.compare(result, code1,
                                                 cur, code2, false);
                 if (rcode == -3)
                   throw new IllegalArgumentException("values cannot be compared");
                 int code = code1 < code2 ? code2 : code1;
                 boolean castNeeded;
                 if (rcode == -2)
                   {
                     result = NumberValue.NaN;
                     castNeeded = true; // For simplicity.
                   }
                 else if (! NumberCompare.checkCompareCode(rcode, flags))
                   {
                     castNeeded = code != code2;
                     result = cur;
                   }
                 else
                   {
                     castNeeded = code != code1;
                   }
                 if (castNeeded)
                   result = Arithmetic.convert(result, code);
              }
            else
              {
                if (! Compare.atomicCompare(flags, result, cur, collation))
                  result = cur;
              }
	  }
      }
    else
      {
        arg = convert(arg);
        // An easy way to check that arg has a valid type.
        Compare.atomicCompare(Compare.TRUE_IF_GRT, arg, arg, collation);
        return arg;
      }
  }

  static Object convert (Object arg)
  {
    arg =  KNode.atomicValue(arg);
    if (arg instanceof UntypedAtomic)
      arg = (Double)
        XDataType.doubleType.valueOf(TextUtils.stringValue(arg));
    return arg;
  }
}
