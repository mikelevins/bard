// Copyright (c) 2001, 2002, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Simple adjustable-length vector of unsigned 8-bit integers (bytes). */

public class U8Vector extends ByteVector
{
  public U8Vector ()
  {
    data = ByteVector.empty;
  }

  public U8Vector(int size, byte value)
  {
    byte[] array = new byte[size];
    data = array;
    this.size = size;
    while (--size >= 0)
      array[size] = value;
  }

  public U8Vector(int size)
  {
    this.data = new byte[size];
    this.size = size;
  }

  public U8Vector (byte[] data)
  {
    this.data = data;
    size = data.length;
  }

  public U8Vector(Sequence seq)
  {
    data = new byte[seq.size()];
    addAll(seq);
  }

  /** Copy constructor. */
  public U8Vector(U8Vector seq)
  {
    size = seq.size;
    data = new byte[size];
    System.arraycopy(seq.data, 0, data, 0, size);
  }

  public U8Vector(U8Vector seq, int offset, int length)
  {
    size = length;
    data = new byte[length];
    if (offset + length > seq.size)
      throw new IndexOutOfBoundsException();
    System.arraycopy(seq.data, offset, data, 0, length);
  }

  public U8Vector(byte[] buffer, int offset, int length)
  {
    size = length;
    data = new byte[length];
    System.arraycopy(buffer, offset, data, 0, length);
  }

  public final int intAtBuffer(int index)
  {
    return data[index] & 0xff;
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
    data[index] = Convert.toByteUnsigned(value);
  }

  public int getElementKind()
  {
    return INT_U8_VALUE;
  }

  public String getTag() { return "u8"; }

  public int compareTo(Object obj)
  {
    return compareToInt(this, (U8Vector) obj);
  }

    /** Covert bytes, interpreted as UTF-8 characters, to a String. */
    public String toUtf8(int start, int length) {
      if (start+length>size) throw new IndexOutOfBoundsException();
      /* #ifdef JAVA7 */  
      return new String(data, start, length, java.nio.charset.StandardCharsets.UTF_8);
      /* #else */
      // try {
      //   return new String(data, start, length, "UTF-8");
      // } catch (UnsupportedEncodingException ex) {
      //     throw new RuntimeException(ex);
      // }
      /* #endif */
  }
}
