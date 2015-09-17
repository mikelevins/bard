// Copyright (c) 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;

public class ArrayRef extends ProcedureN
{
  public static final ArrayRef arrayRef = new ArrayRef();

  public static Object arrayRef(Array array, Sequence index)
  {
    int dims = index.size();
    int[] indexes = new int[dims];
    for (int i = 0;  i < dims;  i++)
      {
	indexes[i] = ((Number) index.get(i)).intValue();
      }
    return array.get(indexes);
  }

  public Object apply2 (Object arg0, Object arg1)
    throws Throwable
  {
    if (arg1 instanceof Sequence)
      return arrayRef((Array) arg0, (Sequence) arg1);
    return super.apply2(arg0, arg1);
  }

  public Object applyN (Object[] args)
    throws Throwable
  {
    Array array = (Array) args[0];

    if (args.length == 2)
      {
	Object arg1 = args[1];
	if (arg1 instanceof Sequence)
	  return arrayRef(array, (Sequence) arg1);
      }
    int[] indexes = new int[args.length-1];
    for (int i = args.length-1;  --i >= 0; )
      {
	indexes[i] = ((Number) args[i+1]).intValue();
      }
    return array.get(indexes);
  }
}
