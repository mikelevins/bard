package gnu.kawa.functions;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Implement the Scheme standard function "call-with-current-continuation".
 * This is a restricted version, that only works for escape-like applications.
 * @author Per Bothner
 */

public class CallCC extends MethodProc implements Inlineable
{
  public static final CallCC callcc = new CallCC();

  CallCC ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileMisc:validateApplyCallCC");
  }

  public int numArgs() { return 0x1001; }

  public int match1 (Object proc, CallContext ctx)
  {
    if (! (proc instanceof Procedure))
      return NO_MATCH_BAD_TYPE;
    return super.match1(proc, ctx);
  }

  public void apply (CallContext ctx)  throws Throwable
  {
    Procedure proc = (Procedure) ctx.value1;
    Continuation cont = new Continuation(ctx);
    proc.check1(cont, ctx);
    proc = ctx.proc;
    ctx.proc = null;
    try
      {
	proc.apply(ctx);
	ctx.runUntilDone();
	cont.invoked = true;
      }
    catch (Exception ex)
      {
        Continuation.handleException$X(ex, cont, ctx);
      }
  }

  /*
  public void apply (CallContext stack)
  {
    kawa.lang.Continuation cont = new Continuation ();
    cont.frame = stack.proc;
    cont.pc = stack.pc;
    stack.value = cont;
  }
  */

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    CompileMisc.compileCallCC(exp, comp, target, this);
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}

/*
class Continuation extends MethodProc
{
  Procedure frame;
  int pc;

  public void apply (CallContext stack)
  {
    Object result = Values.make(stack.args);
    stack.pc = pc;
    stack.proc = frame;
    stack.result = result;
  }
}
*/
