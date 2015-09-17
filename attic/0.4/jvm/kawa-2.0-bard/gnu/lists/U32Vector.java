// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 32-bit integers (ints). */

public class U32Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  int[] data;

  public U32Vector ()
  {
    data = S32Vector.empty;
  }

  public U32Vector(int size, int value)
  {
    int[] array = new int[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U32Vector(int size)
  {
    this.data = new int[size];
    this.size = size;
  }

  public U32Vector (int[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U32Vector(Sequence seq)
  {
    data = new int[seq.size()];
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
	int[] tmp = new int[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final int intAtBuffer(int index)
  {
    return data[index];
  }

  public final long longAtBuffer(int index)
  {
    return (long) data[index] & 0xffffffffL;
  }

  public final long longAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return longAtBuffer(index);
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
    data[index] = Convert.toIntUnsigned(value);
  }

  public final void setIntAt(int index, int value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setIntAtBuffer(int index, int value)
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
    return INT_U16_VALUE;
  }

  public String getTag() { return "u32"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(data[i]);
  }

  public int compareTo(Object obj)
  {
    return compareToLong(this, (U32Vector) obj);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeInt).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeInt(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    int[] data = new int[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readInt();
    this.data = data;
    this.size = size;
  }
}
