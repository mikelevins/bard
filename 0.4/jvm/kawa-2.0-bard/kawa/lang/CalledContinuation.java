package kawa.lang;
import gnu.mapping.CallContext;

public class CalledContinuation extends RuntimeException
{
  public Object[] values;
  public Continuation continuation;
  public CallContext ctx;

  CalledContinuation (Object[] values, Continuation continuation, CallContext ctx)
  {
    super ("call/cc called");
    this.values = values;
    this.continuation = continuation;
    this.ctx = ctx;
  }
}
