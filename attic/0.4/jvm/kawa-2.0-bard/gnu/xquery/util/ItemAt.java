// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;

/** Returns a value at a given index in a sequence of values.
 * Implements XQuery 'item-at'. */

public class ItemAt extends Procedure2
{
  public static final ItemAt itemAt = new ItemAt();

  static public Object itemAt (Object seq, int index)
  {
    if (seq instanceof Values)
      {
	Values vals = (Values) seq;
	if (vals.isEmpty())
	  return Values.empty;
	return vals.get(index - 1);
      }
    else
      {
	if (index != 1)
	  throw new IndexOutOfBoundsException();
	return seq;
      }
  }

  public Object apply2(Object arg1, Object arg2)
  {
    return itemAt(arg1, ((Number) arg2).intValue());
  }
}
