// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 64-bit integers (longs). */

public class S64Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  long[] data;
  protected static long[] empty = new long[0];

  public S64Vector ()
  {
    data = empty;
  }

  public S64Vector(int size, long value)
  {
    long[] array = new long[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public S64Vector(int size)
  {
    this.data = new long[size];
    this.size = size;
  }

  public S64Vector (long[] data)
  {
    this.data = data;
    size = data.length;
  }

  public S64Vector(Sequence seq)
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
    return Convert.toObject(data[index]);
  }

  public final Object getBuffer(int index)
  {
    return Convert.toObject(data[index]);
  }

  public final int intAtBuffer(int index)
  {
    return (int) data[index];
  }

  @Override
  public void setBuffer(int index, Object value)
  {
    data[index] = Convert.toLong(value);
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
    return INT_S64_VALUE;
  }

  public String getTag() { return "s64"; }

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
    return compareToLong(this, (S64Vector) obj);
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
