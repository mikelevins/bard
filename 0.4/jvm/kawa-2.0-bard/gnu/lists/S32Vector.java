// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 32-bit integers (ints). */

public class S32Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  int[] data;
  protected static int[] empty = new int[0];

  public S32Vector ()
  {
    data = empty;
  }

  public S32Vector(int size, int value)
  {
    int[] array = new int[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public S32Vector(int size)
  {
    this.data = new int[size];
    this.size = size;
  }

  public S32Vector (int[] data)
  {
    this.data = data;
    size = data.length;
  }

  public S32Vector(Sequence seq)
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

  public final int intAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final int intAtBuffer(int index)
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

  @Override
  public void setBuffer(int index, Object value)
  {
    data[index] = Convert.toInt(value);
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
    return INT_S32_VALUE;
  }

  public String getTag() { return "s32"; }

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
    return compareToInt(this, (S32Vector) obj);
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
