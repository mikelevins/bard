package kawa.lang;
import gnu.mapping.*;
import java.io.*;
import gnu.text.Printable;
import gnu.lists.Consumer;

/**
 * A pattern that matches anything.
 */

public class AnyPat extends Pattern implements Printable, Externalizable
{
  public AnyPat () { }

  public static AnyPat make () { return new AnyPat (); }

  public void print (Consumer out)
  {
    out.write("#<match any>");
  }

  public boolean match (Object obj, Object[] vars, int start_vars)
  {
    vars[start_vars] = obj;
    return true;
  }

  public int varCount () { return 1; }

  /**
   * @serialData Write nothing.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
  }
}
