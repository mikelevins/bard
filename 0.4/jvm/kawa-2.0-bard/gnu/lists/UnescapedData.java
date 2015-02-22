// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

/** Used for text that is supposed to be written out verbatim.
 * For example, if the output format is XML, can be used to write
 * a literal {@code '<'} as a plain {@code "<"}, instead of being
 * escaped as {@code "&lt;"}.
 */

public class UnescapedData implements
                           /* #ifdef use:java.lang.CharSequence */
                           CharSequence, 
                           /* #endif */
                           Externalizable
{
  String data;

  public UnescapedData ()
  {
  }

  public UnescapedData (String data)
  {
    this.data = data;
  }

  public final String getData() { return data; }

  public final String toString() { return data; }

  public final boolean equals(Object other)
  {
    return other instanceof UnescapedData
      && data.equals(other.toString());
  }

  public final int hashCode() { return data == null ? 0 : data.hashCode(); }

  public int length()
  {
    return data.length();
  }

  public char charAt(int index)
  {
    return data.charAt(index);
  }

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  {
    return new UnescapedData(data.substring(start, end));
  }
  /* #endif */

  /**
   * @serialData Write 'data' (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(data);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    data = (String) in.readObject();
  }
}
