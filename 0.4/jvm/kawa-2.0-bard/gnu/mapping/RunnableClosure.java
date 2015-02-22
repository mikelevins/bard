package gnu.mapping;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;

public class RunnableClosure<T>
  implements
  /* #ifdef JAVA5 */
  java.util.concurrent.Callable<T>,
  /* #endif */
  Runnable
{
  T result;
  CallContext context;

  // These are only used to when we need to override the parents' in/out/err
  // in the child.  This is not needed for normal RunnableClosure objects, but (say)
  // when starting a repl in a new window.  In that case we could do te
  // in/out/err override in the 'action'.  FIXME.
  private InPort in;
  private OutPort out;
  private OutPort err;
  Throwable exception;

  Procedure action;
  String name;

  static int nrunnables=0;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name=name;
  }

  public RunnableClosure (Procedure action, CallContext parentContext)
  {
    setName("r"+nrunnables++);
    this.action = action;
  }

  public RunnableClosure (Procedure action,
			  InPort in, OutPort out, OutPort err)
  {
    this(action, CallContext.getInstance());
    this.in = in;
    this.out = out;
    this.err = err;
  }

  public RunnableClosure (Procedure action)
  {
    this(action, CallContext.getInstance());
  }

  /** Get the CallContext we use for this Thread. */
  public final CallContext getCallContext() { return context; }

  public void run ()
  {
    try
      {
        Environment env = Environment.getCurrent();
        String name = getName();
        if (env != null && env.getSymbol() == null && name != null)
          {
            env.setName(name);
          }
        if (context == null)
          context = CallContext.getInstance();
        else
          CallContext.setInstance(context);
	if (in != null)
	  InPort.setInDefault(in);
	if (out != null)
	  OutPort.setOutDefault(out);
	if (err != null)
	  OutPort.setErrDefault(err);
	result = (T) action.apply0 ();
      }
    catch (Error ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
	exception = ex;
      }
  }

  /** Get the result of running this {@code Runnable}.
   * The result is a value or a thrown exception.
   * Should be called after {#code run} finishes. */
  Object getResult () throws Throwable
  {
    Throwable ex = exception;
    if (ex != null)
      throw ex;
    return result;
  }

  public T call()
    throws Exception
  {
    run();
    Throwable ex = exception;
    if (ex != null)
      {
        if (ex instanceof Exception)
          throw (Exception) ex;
        else if (ex instanceof Error)
          throw (Error) ex;
        else
          throw new RuntimeException(ex);
      }
    return result;
  }

  public String toString() {
    StringBuffer buf = new StringBuffer();
    buf.append ("#<runnable ");
    buf.append(getName());
    buf.append(">");
    return buf.toString();
  }
}
