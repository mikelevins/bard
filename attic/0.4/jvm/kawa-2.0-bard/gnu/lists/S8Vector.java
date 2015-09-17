// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of signed 8-bit integers (bytes). */

public class S8Vector extends ByteVector
{

  public S8Vector ()
  {
    data = ByteVector.empty;
  }

  public S8Vector(int size, byte value)
  {
    byte[] array = new byte[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public S8Vector(int size)
  {
    this.data = new byte[size];
    this.size = size;
  }

  public S8Vector (byte[] data)
  {
    this.data = data;
    size = data.length;
  }

  public S8Vector(Sequence seq)
  {
    data = new byte[seq.size()];
    addAll(seq);
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
    data[index] = Convert.toByte(value);
  }

  public int getElementKind()
  {
    return INT_S8_VALUE;
  }

  public String getTag() { return "s8"; }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (S8Vector) obj);
  }
}
