// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.lists;
import java.io.*;

public class EofClass implements Externalizable
{
  /** Special magic end-of-file marker. */
  public static final EofClass eofValue = new EofClass();

  public final String toString()
  {
    return "#!eof";
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
  }

  public Object readResolve() throws ObjectStreamException
  {
    return Sequence.eofValue;
  }
}
