package gnu.expr;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.kawa.reflect.ClassMemberLocation;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.WriterManager;
import gnu.kawa.util.ExitCalled;

/**
 * Class for the dummy top-level function of a module.
 */

public abstract class ModuleBody extends Procedure0 implements RunnableModule
{
  public void apply (CallContext ctx)  throws Throwable
  {
    if (ctx.pc == 0)
      run(ctx);
  }

  protected boolean runDone;

  public void run (CallContext ctx)  throws Throwable
  {
  }

  public void run ()
  {
    synchronized (this)
      {
        if (runDone)
          return;
        runDone = true;
      }
    run (VoidConsumer.instance);
  }

  public void run (Consumer out)
  {
    // This should match the "run" method generated in Compilation.
    CallContext ctx = CallContext.getInstance();
    Consumer save = ctx.consumer;
    ctx.consumer = out;
    Throwable th;
    try
      {
	run(ctx);
	th = null;
      }
    catch (Throwable ex)
      {
	th = ex;
      }
    runCleanup(ctx, th, save);
  }

  public static void runCleanup (CallContext ctx, Throwable th, Consumer save)
  {
    if (th == null)
      {
	try
	  {
	    ctx.runUntilDone();
	  }
	catch (Throwable ex)
	  {
	    th = ex;
	  }
      }
    ctx.consumer = save;
    if (th != null)
      {
        WrappedException.rethrow(th);
      }
  }

  public Object apply0 () throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    match0(ctx);
    return ctx.runUntilValue();
  }

  private static boolean mainPrintValues;

  /** True if runAsMain should print values (in top-level expressions). */
  public static boolean getMainPrintValues()
  {
    return mainPrintValues;
  }

  public static void setMainPrintValues(boolean value)
  {
    mainPrintValues = value;
  }

  /** Number of times exitDecrement calls before we exit. */
  private static int exitCounter;
  /** See exitDecrement. */
  public static synchronized void exitIncrement()
  {
    if (exitCounter == 0)
      exitCounter++;
    exitCounter++;
  }

  /** Work around an AWT bug, where AWT threads are non-daemon.
   * Thus if you start up AWT, the JVM will wait for the AWT to finish,
   * even if there are no other non-daemon threads.
   * So call exitIncrement() each time a Freme is created,
   * and call exitDecrement() when a Frame is closed. */
  public static synchronized void exitDecrement()
  {
    int counter = exitCounter;
    if (counter > 0)
      {
	counter--;
	if (counter == 0)
	  {
	    System.exit(0);
	  }
	else
	  exitCounter = counter;
      }
  }

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public final void runAsMain ()
  {
    runAsMain(this);
  }

  /** This is invoked by main when ModuleBody is compiled with --main. */
  public static void runAsMain (RunnableModule module)
  {
    boolean registered = WriterManager.instance.registerShutdownHook();
    try
      {
        ExitCalled.push();
	CallContext ctx = CallContext.getInstance();
	if (getMainPrintValues())
	  {
	    OutPort out = OutPort.outDefault();
	    ctx.consumer = kawa.Shell.getOutputConsumer(out);
	    module.run(ctx);
	    ctx.runUntilDone();
	    out.freshLine();
	  }
	else
	  {
            ctx.consumer = VoidConsumer.instance;
            module.run(ctx);
	    ctx.runUntilDone();
	  }
        if (! registered)
          gnu.kawa.io.OutPort.runCleanups();
	exitDecrement();
      }
    catch (ExitCalled ex)
      {
         throw ex; // handled by ExitCalled.pop below.
      }
    catch (Throwable ex)
      {
	ex.printStackTrace();
	gnu.kawa.io.OutPort.runCleanups();
	System.exit(-1);
      }
    finally
      {
        ExitCalled.pop();
      }
  }

  /**
   * A subclass will typically override this like:
   * switch (method.selector) {
   *   case 3:  return function3();
   *   case 5:  return function5();
   *   default:  super.apply0(method);
   * }
   */

  public Object apply0(ModuleMethod method)
    throws Throwable
  {
    return applyN(method, Values.noArgs);
  }

  public Object apply1(ModuleMethod method, Object arg1)
    throws Throwable
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN(method, args);
  }

  public Object apply2(ModuleMethod method, Object arg1, Object arg2)
    throws Throwable
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN(method, args);
  }

  public Object apply3(ModuleMethod method,
                       Object arg1, Object arg2, Object arg3)
    throws Throwable
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN(method, args);
  }

  public Object apply4(ModuleMethod method,
                       Object arg1, Object arg2, Object arg3, Object arg4)
    throws Throwable
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return applyN(method, args);
  }

  public Object applyN(ModuleMethod method, Object[] args)
    throws Throwable
  {
    int count = args.length;
    int num = method.numArgs();
    if (count >= (num & 0xFFF)
	&& (num < 0 || count <= (num >> 12)))
      {
        switch (count)
          {
          case 0:
            return apply0(method);
          case 1:
            return apply1(method, args[0]);
          case 2:
            return apply2(method, args[0], args[1]);
          case 3:
            return apply3(method, args[0], args[1], args[2]);
          case 4:
            return apply4(method, args[0], args[1], args[2], args[3]);
          }
      }
    throw new WrongArguments(method, count);
  }

  public int match0 (ModuleMethod proc, CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (min > 0)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num < 0)
      return matchN(proc, ProcedureN.noArgs, ctx);
    ctx.count = 0;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = proc;
    return 0;
  }

  public int match1 (ModuleMethod proc, Object arg1, CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (min > 1)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 1)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.count = 1;
	ctx.where = CallContext.ARG_IN_VALUE1;
	ctx.next = 0;
	ctx.proc = proc;
	//ctx.proc = this; 	ctx.pc = proc.selector;
	return 0;
      }
    ctx.where = 0;
    Object[] args = { arg1 };
    return matchN(proc, args, ctx);
  }

  public int match2 (ModuleMethod proc, Object arg1, Object arg2,
		     CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (min > 2)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 2)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.count = 2;
	ctx.where = CallContext.ARG_IN_VALUE1
	  |(CallContext.ARG_IN_VALUE2<<4);
	ctx.next = 0;
	ctx.proc = proc;
	return 0;
      }
    ctx.where = 0;
    Object[] args = { arg1, arg2 };
    return matchN(proc, args, ctx);
  }

  public int match3 (ModuleMethod proc, Object arg1, Object arg2, Object arg3,
		     CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (min > 3)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 3)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.value3 = arg3;
	ctx.count = 3;
	ctx.where = CallContext.ARG_IN_VALUE1
	  |(CallContext.ARG_IN_VALUE2<<4)
	  |(CallContext.ARG_IN_VALUE3<<8);
	ctx.next = 0;
	ctx.proc = proc;
	// ctx.proc = this; ctx.pc = proc.selector;
	return 0;
      }
    ctx.where = 0;
    Object[] args = { arg1, arg2, arg3 };
    return matchN(proc, args, ctx);
  }

  public int match4 (ModuleMethod proc, Object arg1, Object arg2,
		     Object arg3, Object arg4, CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (min > 4)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = num >> 12;
	if (max < 4)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.value3 = arg3;
	ctx.value4 = arg4;
	ctx.count = 4;
	ctx.where = (CallContext.ARG_IN_VALUE1
		     |(CallContext.ARG_IN_VALUE2<<4)
		     |(CallContext.ARG_IN_VALUE3<<8)
		     |(CallContext.ARG_IN_VALUE4<<12));
	ctx.next = 0;
	ctx.proc = proc;
	//ctx.proc = this;	ctx.pc = proc.selector;
	return 0;
      }
    ctx.where = 0;
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(proc, args, ctx);
  }

  public int matchN (ModuleMethod proc, Object[] args, CallContext ctx)
  {
    int num = proc.numArgs();
    int min = num & 0xFFF;
    if (args.length < min)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
	switch (args.length)
	  {
	  case 0:
	    return match0(proc, ctx);
	  case 1:
	    return match1(proc, args[0], ctx);
	  case 2:
	    return match2(proc, args[0], args[1], ctx);
	  case 3:
	    return match3(proc, args[0], args[1], args[2], ctx);
	  case 4:
	    return match4(proc, args[0], args[1], args[2], args[3], ctx);
	  default:
	    int max = num >> 12;
	    if (args.length > max)
	      return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	  }
      }
    ctx.values = args;
    ctx.count = args.length;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = proc;
    // The following doesn't work if this does not pass contexts
    //ctx.proc = this;    ctx.pc = proc.selector;
    return 0;
  }
}
