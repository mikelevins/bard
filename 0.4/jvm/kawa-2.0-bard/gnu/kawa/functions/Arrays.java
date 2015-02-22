// Copyright (c) 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.math.IntNum;

/** Static methods for implementing Scheme (SRFI-25) arrays. */

public class Arrays
{
  static final int[] shapeStrides = { 2, 1} ;
  static final int[] zeros2 = new int[2];

  public static Array shape (Object[] vals)
  {
    int len = vals.length;
    if ((len & 1) != 0)
      throw new RuntimeException("shape: not an even number of arguments");
    int d = len >> 1;
    int[] dims = { d, 2 };
    return new FVector(vals).transpose(zeros2, dims, 0, shapeStrides);
  }

  public static Array make(Array shape, Object value)
  {
    int rank = shape.getSize(0);
    int[] dimensions = new int[rank];
    int[] lowBounds = null;
    int total = 1;
    for (int i = rank;  --i >= 0; )
      {
	int lo = ((Number) shape.getRowMajor(2*i)).intValue();
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	int size = hi - lo;
	dimensions[i] = size;
	if (lo != 0)
	  {
	    if (lowBounds == null)
	      lowBounds = new int[rank];
	    lowBounds[i] = lo;
	  }
	total *= size;
      }
    return GeneralArray.makeSimple(lowBounds, dimensions, new FVector(total, value));
  }

  public static Array makeSimple(Array shape, SimpleVector base)
  {
    int rank = shape.getSize(0);
    int[] dimensions = new int[rank];
    int[] lowBounds = null;
    for (int i = rank;  --i >= 0; )
      {
	int lo = ((Number) shape.getRowMajor(2*i)).intValue();
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	dimensions[i] = hi - lo;
	if (lo != 0)
	  {
	    if (lowBounds == null)
	      lowBounds = new int[rank];
	    lowBounds[i] = lo;
	  }
      }
    return GeneralArray.makeSimple(lowBounds, dimensions, base);
  }

  public static int effectiveIndex (Array array, Procedure proc,
				    Object[] args, int[] work)
    throws Throwable
  {
    Object mapval = proc.applyN(args);
    if (mapval instanceof Values)
      {
	Values mapvals = (Values) mapval;
	for (int i = 0, j = 0;  (i = mapvals.nextPos(i)) != 0;  j++)
	  {
	    work[j] = ((Number) mapvals.getPosPrevious(i)).intValue();
	  }
      }
    else
      work[0] = ((Number) mapval).intValue();
    return array.getEffectiveIndex(work);
  }

  public static Array shareArray(Array array, Array shape,
				 Procedure proc)
    throws Throwable
  {
    int rank = shape.getSize(0);
    Object[] args = new Object[rank];
    int[] dimensions = new int[rank];
    int[] lowBounds = new int[rank]; // null - FIXME
    boolean empty = false;
    for (int i = rank;  --i >= 0; )
      {
	Object low = shape.getRowMajor(2*i);
	args[i] = low;
	int lo = ((Number) low).intValue();
	lowBounds[i] = lo;
	int hi = ((Number) shape.getRowMajor(2*i+1)).intValue();
	int size = hi - lo;
	dimensions[i] = size;
	if (size <= 0)
	  empty = true;
      }
    int arank = array.rank();
    int[] offsets = new int[rank];
    int offset0;
    if (empty)
      offset0 = 0;
    else
      {
	int[] work = new int[arank];
	offset0 = effectiveIndex (array, proc, args, work);
	for (int i = rank;  --i >= 0; )
	  {
	    int size = dimensions[i];
	    int lo = lowBounds[i];
	    if (size <= 1)
	      offsets[i] = 0;
	    else
	      {
		Object low = args[i];
		args[i] = IntNum.make(lo + 1);
		offsets[i] = (effectiveIndex (array, proc, args, work)
			      - offset0);
		args[i] = low;
	      }
	  }
      }
    return array.transpose(lowBounds, dimensions, offset0, offsets);
  }
}
