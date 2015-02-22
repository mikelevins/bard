// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of boolean values. */

public class BitVector extends SimpleVector implements Externalizable
{
  boolean[] data;
  protected static boolean[] empty = new boolean[0];

  public BitVector ()
  {
    data = empty;
  }

  public BitVector(int size, boolean value)
  {
    boolean[] array = new boolean[size];
    data = array;
    this.size = size;
    if (value)
      {
	while (--size >= 0)
	  array[size] = true;
      }
  }

  public BitVector(int size)
  {
    this.data = new boolean[size];
    this.size = size;
  }

  public BitVector (boolean[] data)
  {
    this.data = data;
    size = data.length;
  }

  public BitVector(Sequence seq)
  {
    data = new boolean[seq.size()];
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
	boolean[] tmp = new boolean[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final boolean booleanAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final boolean booleanAtBuffer(int index)
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
    data[index] = Convert.toBoolean(value);
  }

  public final void setBooleanAt(int index, boolean value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setBooleanAtBuffer(int index, boolean value)
  {
    data[index] = value;
  }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = false;
  }

  public int getElementKind()
  {
    return BOOLEAN_VALUE;
  }

  public String getTag() { return "b"; }

  public void consumePosRange(int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    for (;  i < end;  i++)
      out.writeBoolean(data[i]);
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeBoolean).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeBoolean(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    boolean[] data = new boolean[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readBoolean();
    this.data = data;
    this.size = size;
  }
}
