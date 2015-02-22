package gnu.kawa.functions;
import gnu.expr.Language;
import gnu.mapping.*;
import gnu.kawa.reflect.Invoke;

/** Implement the standard Scheme function "apply".
 * This has been generalized so that the last (list argument)
 * can be any sequence, or any primitive array coercible to Object[]. */

public class ApplyToArgs extends ProcedureN
{
  public int match1 (Object arg1, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match0(ctx);
    else
      return super.match1(arg1, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match1(arg2, ctx);
    else
      return super.match2(arg1, arg2, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match2(arg2, arg3, ctx);
    else
      return super.match3(arg1, arg2, arg3, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
                     CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      return ((Procedure) arg1).match3(arg2, arg3, arg4, ctx);
    else
      return super.match4(arg1, arg2, arg3, arg4, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int n = args.length;
    if (n > 0 && args[0] instanceof Procedure)
      {
        Procedure proc = (Procedure) args[0];
        switch (n)
          {
          case 1:
            return proc.match0(ctx);
          case 2:
            return proc.match1(args[1], ctx);
          case 3:
            return proc.match2(args[1], args[2], ctx);
          case 4:
            return proc.match3(args[1], args[2], args[3], ctx);
          case 5:
            return proc.match4(args[1], args[2], args[3], args[4], ctx);
          default:
            Object[] xargs = new Object[n-1];
            System.arraycopy(args, 1, xargs, 0, n-1);
            return proc.matchN(xargs, ctx);
          }
      }
    return super.matchN(args, ctx);
  }

  public void check1 (Object arg1, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check0(ctx);
    else
      super.check1(arg1, ctx);
  }

  public void check2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check1(arg2, ctx);
    else
      super.check2(arg1, arg2, ctx);
  }

  public void check3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check2(arg2, arg3, ctx);
    else
      super.check3(arg1, arg2, arg3, ctx);
  }

  public void check4 (Object arg1, Object arg2, Object arg3, Object arg4,
                     CallContext ctx)
  {
    if (arg1 instanceof Procedure)
      ((Procedure) arg1).check3(arg2, arg3, arg4, ctx);
    else
      super.check4(arg1, arg2, arg3, arg4, ctx);
  }

  public void checkN (Object[] args, CallContext ctx)
  {
    int code = matchN(args, ctx);
    if (code != 0)
      {
        Procedure proc = this;
        if (args.length > 0 && args[0] instanceof Procedure)
          {
            proc = (Procedure) args[0];
            Object[] xargs = new Object[args.length-1];
            System.arraycopy(args, 1, xargs, 0, xargs.length);
            args = xargs;
          }
	throw MethodProc.matchFailAsException(code, proc, args);
      }
  }

  public ApplyToArgs (String name, Language language)
  {
    super(name);
    this.language = language;
    setProperty(Procedure.validateXApplyKey,
                "gnu.kawa.functions.CompilationHelpers:validateApplyToArgs");
  }

  Language language;

    public Object applyN (Object[] args) throws Throwable {
        Object proc = Promise.force(args[0]);
        if (proc instanceof Procedure) {
            Object[] rargs = new Object[args.length-1];
            System.arraycopy(args, 1, rargs, 0, rargs.length);
            return ((Procedure) proc).applyN(rargs);
        }
        if (proc instanceof gnu.bytecode.Type
            || proc instanceof Class) {
            return gnu.kawa.reflect.Invoke.make.applyN(args);
        }
        if (proc instanceof java.util.List) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            int index = ((Number) Promise.force(args[1])).intValue();
            return ((java.util.List) proc).get(index);
        }
        /*
          What should happen if key has no associated value?
          Throw an exception?  Return null?
        if (proc instanceof java.util.Map) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            Object key = Promise.force(args[1]);
            
        }
        */
        Class pclass = proc.getClass();
        if (pclass.isArray()) {
            if (args.length != 2)
                throw new WrongArguments(this, args.length); // FIXME
            return java.lang.reflect.Array.get(proc, ((Number) args[1]).intValue());
        }
        throw new WrongType(this, 0, proc, "procedure");
    }
}
