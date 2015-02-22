package kawa.lang;
import gnu.mapping.*;
import java.io.*;
import gnu.text.*;
import gnu.lists.Consumer;

/**
 * A pattern that requires an exact match (using equal?).
 */

public class EqualPat extends Pattern implements Printable, Externalizable
{

  Object value;

  public EqualPat () { }

  public EqualPat (Object obj) { value = obj; }

  static public EqualPat make (Object obj) { return new EqualPat (obj); }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    // We should be using Translator's matches routine, but the current
    // Translator isn't available, so here is a special-purpose kludge.
    if (value instanceof String && obj instanceof Symbol)
      obj = ((Symbol) obj).getName();
    return value.equals (obj);
  }

  public int varCount () { return 0; }

  public void print (Consumer out)
  {
    out.write("#<equals: ");
    ReportFormat.print(value, out);
    out.write('>');
  }

  /**
   * @serialData Write the value (using writeObject).
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(value);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    value = in.readObject();
  }
}
