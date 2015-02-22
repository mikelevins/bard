// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/* Represents a generic uninterpreted or unknown attribute.
 * @author      Per Bothner
 */

public class MiscAttr extends Attribute
{
  byte[] data;
  int offset;
  int dataLength;

  public MiscAttr(String name, byte[] data, int offset, int length)
  {
    super(name);
    this.data = data;
    this.offset = offset;
    this.dataLength = length;
  }
  
  public MiscAttr(String name, byte[] data)
  {
    this(name, data, 0, data.length);
  }
  
  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
  public int getLength() { return dataLength; }

  protected int u1(int offset)
  {
    return data[offset] & 0xFF;
  }

  protected int u2(int offset)
  {
    return ((data[offset]  & 0xFF) << 8) + (data[offset+1] & 0xFF);
  }

  protected int u1()
  {
    return u1(offset++);
  }

  protected int u2()
  {
    int v = u2(offset);
    offset += 2;
    return v;
  }

  protected void put1(int val)
  {
    if (data == null)
      data = new byte[20];
    else if (dataLength >= data.length)
      {
        byte[] tmp = new byte[2 * data.length];
        System.arraycopy(data, 0, tmp, 0, dataLength);
        data = tmp;
      }
    data[dataLength++] = (byte) val;
  }

  protected void put2(int val)
  {
    put1((byte) (val >> 8));
    put1((byte) (val));
  }

  protected void put2(int offset, int val)
  {
    data[offset] = (byte) (val >> 8);
    data[offset+1] = (byte) val;
  }

  /** Write out the contents of the Attribute.
    * Does not write the 6-byte attribute header. */
  public void write (DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.write(data, offset, dataLength);
  }

  public void print (ClassTypeWriter dst)
  {
    super.print(dst);
    int len = getLength();
    for (int i = 0;  i < len;  )
      {
        int b = data[i];
        if ((i % 20) == 0)
          dst.print(' ');
        dst.print(' ');
        dst.print(Character.forDigit((b >> 4) & 15, 16));
        dst.print(Character.forDigit(b & 15, 16));
        i++;
        if ((i % 20) == 0 || i == len)
          dst.println();
      }
  }

}

