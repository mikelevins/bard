package kawa.lang;
import gnu.mapping.*;

/**
 * A Continuation "represents an entire (default) future for the computation.
 * This implemementation is based on Java exceptions, and is restricted
 * to "upward" (?) continuation (i.e. catch/throw-type uses).
 * @author	Per Bothner
 */

public class Continuation extends MethodProc
{
  public boolean invoked;
  static int counter;
  int id;

  public Continuation (CallContext ctx)
  {
  }

  public void apply (CallContext ctx)
  {
    if (invoked)
      throw new GenericError
	("implementation restriction: continuation can only be used once");
    throw new CalledContinuation (ctx.values, this, ctx);
  }

  public static void handleException$X (Throwable ex, Continuation cont,
                                        CallContext ctx)
    throws Throwable
  {
    CalledContinuation cex;
    if (! (ex instanceof CalledContinuation)
        || (cex = (CalledContinuation) ex).continuation != cont)
      throw ex;
    cont.invoked = true;
    Object[] values = cex.values;
    int nvalues = values.length;
    for (int i = 0;  i < nvalues;  i++)
      ctx.consumer.writeObject(values[i]);
  }

  public static Object handleException (Throwable ex, Continuation cont)
    throws Throwable
  {
    CalledContinuation cex;
    if (! (ex instanceof CalledContinuation)
        || (cex = (CalledContinuation) ex).continuation != cont)
      throw ex;
    cont.invoked = true;
    return Values.make(cex.values);
  }

  public final String toString()
  {
    return "#<continuation " + id + (invoked ? " (invoked)>" : ">");
  }
}

