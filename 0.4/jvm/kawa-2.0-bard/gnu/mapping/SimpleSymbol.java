// Copyright (c) 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

/** A Symbol in the EmptyNamespace. */

public class SimpleSymbol extends Symbol
{
  public SimpleSymbol ()
  {
  }

  public SimpleSymbol (String key)
  {
    super(key, Namespace.EmptyNamespace);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    /* #ifdef JAXP-QName */
    // throw new Error("Symbol.readExternal not implemented"); // FIXME!
    /* #else */
    name = ((String) in.readObject()).intern();
    /* #endif */
  }

  public Object readResolve() throws ObjectStreamException
  {
    return Namespace.EmptyNamespace.getSymbol(getName().intern());
  }
}
