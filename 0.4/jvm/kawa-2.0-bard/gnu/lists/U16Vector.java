// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 16-bit integers (shorts). */

public class U16Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  short[] data;

  public U16Vector ()
  {
    data = S16Vector.empty;
  }

  public U16Vector(int size, short value)
  {
    short[] array = new short[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U16Vector(int size)
  {
    this.data = new short[size];
    this.size = size;
  }

  public U16Vector (short[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U16Vector(Sequence seq)
  {
    data = new short[seq.size()];
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
	short[] tmp = new short[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final short shortAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final short shortAtBuffer(int index)
  {
    return data[index];
  }

  public final int intAtBuffer(int index)
  {
    return data[index] & 0xffff;
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
    data[index] = Convert.toShortUnsigned(value);
  }

  public final void setShortAt(int index, short value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setShortAtBuffer(int index, short value)
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

  public String getTag() { return "u16"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(data[i] & 0xffff);
  }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (U16Vector) obj);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeShort).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeShort(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    short[] data = new short[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readShort();
    this.data = data;
    this.size = size;
  }
}
