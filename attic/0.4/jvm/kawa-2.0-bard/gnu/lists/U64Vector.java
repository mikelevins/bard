// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 64-bit integers (longs). */

public class U64Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  long[] data;

  public U64Vector ()
  {
    data = S64Vector.empty;
  }

  public U64Vector(int size, long value)
  {
    long[] array = new long[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U64Vector(int size)
  {
    this.data = new long[size];
    this.size = size;
  }

  public U64Vector (long[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U64Vector(Sequence seq)
  {
    data = new long[seq.size()];
    addAll(seq);
  }

  /** Get the allocated length of the data buffer. */
  public int getBufferLength()
  {
    return data.length;
  }

  public void setBufferLength(int length)
  {
    int oldLength = data.length;
    if (oldLength != length)
      {
	long[] tmp = new long[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final int intAtBuffer(int index)
  {
    return (int) data[index];
  }

  public final long longAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final long longAtBuffer(int index)
  {
    return data[index];
  }

  public final Object get(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return Convert.toObjectUnsigned(data[index]);
  }

  public final Object getBuffer(int index)
  {
    return Convert.toObjectUnsigned(data[index]);
  }

  @Override
  public void setBuffer(int index, Object value)
  {
    data[index] = Convert.toLongUnsigned(value);
  }

  public final void setLongAt(int index, long value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setLongAtBuffer(int index, long value)
  {
    data[index] = value;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  public int getElementKind()
  {
    return INT_U64_VALUE;
  }

  public String getTag() { return "u64"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeLong(data[i]);
  }

  public int compareTo(Object obj)
  {
    U64Vector vec2 = (U64Vector) obj;
    long[] arr1 = data;
    long[] arr2 = vec2.data;
    int n1 = size;
    int n2 = vec2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	long v1 = arr1[i];
	long v2 = arr2[i];
	if (v1 != v2)
	  return (v1^0x8000000000000000L) > (v2^0x8000000000000000L) ? 1 : -1;
      }
    return n1 - n2;
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeLong).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeLong(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    long[] data = new long[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readLong();
    this.data = data;
    this.size = size;
  }
}
