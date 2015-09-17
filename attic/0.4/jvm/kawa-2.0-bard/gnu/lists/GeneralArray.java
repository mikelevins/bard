// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;

/** A class to handle general multi-dimensional arrays.
 * This class is <strong>unfinished</strong>.
 * If the number of dimensions (the "rank") is one, should use
 * a class that implements Sequence.
 * GeneralArray uses a SimpleVector 'base' to store the actual data, and
 * provides general linear mapping from the array indexes to an
 * element index in the 'base' SimpleVector.  Thus such uperations as
 * transposing an array can be implement as just creating a simple
 * re-mapping of the indexes. */

public class GeneralArray extends AbstractSequence
implements Array //, Consumable
// Should implement Collection?
// Note this intentionally does not implement Sequence.
{
  SimpleVector base;
  int[] dimensions;
  int[] strides;
  int[] lowBounds;
  static final int[] zeros = new int[8];
  int offset;
  boolean simple = true;
  
  protected int nextIndex(int ipos) { return ipos>>>1; }

  public static Array makeSimple(int[] lowBounds, int[] dimensions,
				 SimpleVector base)
  {
    int d = dimensions.length;
    if (lowBounds == null)
      {
	lowBounds = zeros;
	if (d > lowBounds.length)
	  lowBounds = new int[d];
      }
    if (d == 1 && lowBounds[0] == 0)
      return base;
    GeneralArray array = new GeneralArray();
    int[] strides = new int[d];
    int n = 1;
    for (int i = d;  --i >= 0; )
      {
	strides[i] = n;
	n *= dimensions[i];
      }
    array.strides = strides;
    array.dimensions = dimensions;
    array.lowBounds = lowBounds;
    array.base = base;
    return array;
  }

  public GeneralArray()
  {
  }

  public GeneralArray(int[] dimensions)
  {
    int total = 1;
    int rank = dimensions.length;
    if (rank <= zeros.length)
      lowBounds = zeros;
    else
      lowBounds = new int[rank]; 
    int[] strides = new int[rank];
    for (int i = rank;  --i >= 0; )
      {
	strides[i] = total;
	total *= dimensions[i];
      }
    base = new FVector(total);
    this.dimensions = dimensions;
    this.offset = 0;
  }

  public int rank() { return dimensions.length; }

  /** Calculate corresponding index in base array. */
  public int getEffectiveIndex(int[] indexes)
  {
    int result = offset;
    for (int i = dimensions.length;  --i >= 0; )
      {
	int index = indexes[i];
	int low = lowBounds[i];
	if (index < low || (index -= low) >= dimensions[i])
	  throw new IndexOutOfBoundsException();
	result += strides[i] * index;
      }
    return result;
  }

  public Object get (int index)
  {
    return getRowMajor(index);
  }

  public int createPos(int index, boolean isAfter)
  {
    int total = offset;
    for (int i = dimensions.length;  --i >= 0; )
      {
	int dim = dimensions[i];
	int cur = index % dim;
	index = index / dim;
	total = total + strides[i] * cur;
      }
    return (total << 1) | (isAfter ? 1 : 0);
  }

  public Object getRowMajor(int index)
  {
    if (simple)
      return base.get(index);
    int total = offset;
    for (int i = dimensions.length;  --i >= 0; )
      {
	int dim = dimensions[i];
	int cur = index % dim;
	index = index / dim;
	total = total + strides[i] * cur;
      }
    return base.get(total);
  }

  public Object get(int[] indexes)
  {
    return base.get(getEffectiveIndex(indexes));
  }

  public Object set(int[] indexes, Object value)
  {
    return base.set(getEffectiveIndex(indexes), value);
  }

  /** See java.util.Collection. */
  public int size()
  {
    int total = 1;
    for (int i = dimensions.length;  --i >= 0; )
      total *= dimensions[i];
    return total;
  }

  public int getLowBound(int dim)
  {
    return lowBounds[dim];
  }

  public int getSize(int dim)
  {
    return dimensions[dim];
  }

  public Array transpose(int[] lowBounds, int[] dimensions,
			 int offset0, int[] factors)
  {
    GeneralArray array =
      dimensions.length == 1 && lowBounds[0] == 0 ? new GeneralArray1()
      : new GeneralArray();
    array.offset = offset0;
    array.strides = factors;
    array.dimensions = dimensions;
    array.lowBounds = lowBounds;
    array.base = base;
    array.simple = false;
    return array;
  }

  public static void toString (Array array, StringBuffer sbuf)
  {
    sbuf.append("#<array");
    int r = array.rank();
    for (int i = 0;  i < r;  i++)
      {
	sbuf.append(' ');
	int lo = array.getLowBound(i);
	int sz = array.getSize(i);
	if (lo != 0)
	  {
	    sbuf.append(lo);
	    sbuf.append(':');
	  }
	sbuf.append(lo+sz);
      }
    sbuf.append('>');
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer();
    toString(this, sbuf);
    return sbuf.toString();
  }
}
