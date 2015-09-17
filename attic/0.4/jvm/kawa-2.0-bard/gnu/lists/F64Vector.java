// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector whose elements are 64-bit floats. */

public class F64Vector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  double[] data;
  protected static double[] empty = new double[0];

  public F64Vector ()
  {
    data = empty;
  }

  public F64Vector(int size, double value)
  {
    double[] array = new double[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public F64Vector(int size)
  {
    this.data = new double[size];
    this.size = size;
  }

  public F64Vector (double[] data)
  {
    this.data = data;
    size = data.length;
  }

  public F64Vector(Sequence seq)
  {
    data = new double[seq.size()];
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
	double[] tmp = new double[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  protected Object getBuffer() { return data; }

  public final double doubleAt(int index)
  {
    if (index >= size)
      throw new ArrayIndexOutOfBoundsException();
    return data[index];
  }

  public final double doubleAtBuffer(int index)
  {
    return data[index];
  }

  public final Object get (int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return Convert.toObject(data[index]);
  }

  public final Object getBuffer (int index)
  {
    return Convert.toObject(data[index]);
  }

  public final int intAtBuffer(int index)
  {
    return (int) data[index];
  }

  public final void setDoubleAt(int index, double value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setDoubleAtBuffer(int index, double value)
  {
    data[index] = value;
  }

  @Override
  public final void setBuffer(int index, Object value)
  {
    data[index] = Convert.toDouble(value);
  }

  /*
  public final void setElementAt(Object value, int index)
  {
    data[index] = ((Number) value).doubleValue();
  }
  */

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

  public int getElementKind()
  {
    return DOUBLE_VALUE;
  }

  public String getTag() { return "f64"; }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    for (;  i < end;  i++)
      out.writeDouble(data[i]);
  }

  /*
  public final void print(int index, java.io.PrintWriter ps)
  {
    ps.print(getDouble(index));
  }
  */

  public int compareTo(Object obj)
  {
    F64Vector vec2 = (F64Vector) obj;
    double[] arr1 = data;
    double[] arr2 = vec2.data;
    int n1 = size;
    int n2 = vec2.size;
    int n = n1 > n2 ? n2 : n1;
    for (int i = 0;  i < n;  i++)
      {
	double v1 = arr1[i];
	double v2 = arr2[i];
	if (v1 != v2)
	  return v1 > v2 ? 1 : -1;
      }
    return n1 - n2;
  }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeDouble).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeDouble(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    double[] data = new double[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readDouble();
    this.data = data;
    this.size = size;
  }
}
