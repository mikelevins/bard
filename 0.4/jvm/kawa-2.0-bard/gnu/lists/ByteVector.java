package gnu.lists;
import java.io.*;

public abstract class ByteVector extends SimpleVector
  implements Externalizable
  /* #ifdef JAVA2 */
  , Comparable
  /* #endif */
{
  byte[] data;
  protected static byte[] empty = new byte[0];

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
	byte[] tmp = new byte[length];
	System.arraycopy(data, 0, tmp, 0,
			 oldLength < length ? oldLength : length);
	data = tmp;
      }
  }

  public byte[] getBuffer() { return data; }

  public final byte byteAt(int index)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    return data[index];
  }

  public final byte byteAtBuffer(int index)
  {
    return data[index];
  }

  public void consumePosRange (int iposStart, int iposEnd, Consumer out)
  {
    if (out.ignoring())
      return;
    int i = iposStart >>> 1;
    int end = iposEnd >>> 1;
    if (end > size)
      end = size;
    for (;  i < end;  i++)
      out.writeInt(intAtBuffer(i));
  }

  public final void setByteAt(int index, byte value)
  {
    if (index > size)
      throw new IndexOutOfBoundsException();
    data[index] = value;
  }

  public final void setByteAtBuffer(int index, byte value)
  {
    data[index] = value;
  }

    public void copyFrom(byte[] src, int soffset, int doffset, int length) {
        if (doffset + length > size)
            throw new IndexOutOfBoundsException();
        System.arraycopy(src, soffset, data, doffset, length);
    }

  protected void clearBuffer(int start, int count)
  {
    while (--count >= 0)
      data[start++] = 0;
  }

    public void copyFrom (int index, ByteVector src, int start, int end) {
        int count = end-start;
        if (count < 0 || index+count > size || end > src.size)
            throw new ArrayIndexOutOfBoundsException();
        System.arraycopy(src.data, start, data, index, count);
    }

  /**
   * @serialData Write 'size' (using writeInt),
   *   followed by 'size' elements in order (using writeByte).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    int size = this.size;
    out.writeInt(size);
    for (int i = 0;  i < size;  i++)
      out.writeByte(data[i]);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    int size = in.readInt();
    byte[] data = new byte[size];
    for (int i = 0;  i < size;  i++)
      data[i] = in.readByte();
    this.data = data;
    this.size = size;
  }

    public InputStream getInputStream() {
        return new ByteArrayInputStream(data, 0, size);
    }

    public int readFrom(int start, int count, InputStream in)
        throws IOException {
        return in.read(data, start, count);
    }

    public void writeTo(int start, int count, OutputStream out)
        throws IOException {
        out.write(data, start, count);
    }

}
