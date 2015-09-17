// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/**
 * The abstract parent for all Scheme functions.
 * @author  Per Bothner
 */

public abstract class Procedure extends PropertySet
{
  private static final String sourceLocationKey = "source-location";
  private static final Symbol setterKey = Namespace.EmptyNamespace.getSymbol("setter");

  /** Key for a property used by gnu.expr.Inlinecalls.
   * The property value is either a String of the form "CLASSNAME:METHODNAME",
   * or a java.lang.reflect.Method (or FUTURE: MethodHandle) for a static
   * method whose parameters are
   * {@code (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)} and returns a re-written/validated {@code Expression}.
   */
  public static final Symbol validateApplyKey =
    Namespace.EmptyNamespace.getSymbol("validate-apply");
    /** Same as validateApplyKey but handles splice args. */
  public static final Symbol validateXApplyKey =
    Namespace.EmptyNamespace.getSymbol("validate-xapply");
  public static final Symbol compilerXKey =
    Namespace.EmptyNamespace.getSymbol("compile-apply");

  // This should be a LazyPropertyKey<gnu.expr.Inlineable>, but we want
  // to avoid any strict dependency on gnu.expr for run-time classes.
  public static final LazyPropertyKey<?> compilerKey
  = new LazyPropertyKey("compiler");

  public void setSourceLocation (String file, int line)
  {
    setProperty(sourceLocationKey, file + ":" + line);
  }

  public String getSourceLocation ()
  {
    Object value = getProperty(sourceLocationKey, null);
    return value == null ? null : value.toString();
  }

  public Procedure()
  {
  }

  public Procedure(String n)
  {
    setName(n);
  }

  public abstract Object applyN (Object[] args) throws Throwable;

  public abstract Object apply0 () throws Throwable;

  public abstract Object apply1 (Object arg1) throws Throwable;

  public abstract Object apply2 (Object arg1,Object arg2) throws Throwable;

  public abstract Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable;

  public abstract Object apply4(Object arg1,Object arg2,
				Object arg3,Object arg4) throws Throwable;

  /** Minimum number of arguments required. */
  public final int minArgs() { return minArgs(numArgs()); }

  /** Maximum number of arguments allowed, or -1 for unlimited.
   * (May also return -1 if there are keyword arguments, for implementation
   * reasons.) */
  public final int maxArgs() { return maxArgs(numArgs()); }

  /** Return {@code minArgs()|(maxArgs<<12)}.
   * We use a single virtual function to reduce the number of methods
   * in the system, as well as the number of virtual method table entries.
   * We shift by 12 so the number can normally be represented using a
   * sipush instruction, without requiring a constant pool entry.
   */
  public int numArgs() { return 0xfffff000; }

  /** Extract minimum number of arguments from {@code numArgs()} encoding. */
  public static int minArgs (int num) { return num & 0xFFF; }
  /** Extract maximum number of arguments from {@code numArgs()} encoding. */
  public static int maxArgs (int num) { return num >> 12; }

  /** Check that the number of arguments in a call is valid.
    * @param proc the Procedure being called
    * @param argCount the number of arguments in the call
    * @exception WrongArguments there are too many or too
    *     few actual arguments
    */
  public static void checkArgCount(Procedure proc, int argCount)
  {
    int num = proc.numArgs();
    if (argCount < minArgs(num)
	|| (num >= 0 && argCount > maxArgs(num)))
      throw new WrongArguments(proc, argCount);
  }

  /* CPS: ??
  public void apply1(Object arg, CallContext stack, CallFrame rlink, int rpc)
  {
    context.value = apply1(arg);
    context.frame = rlink;
    context.pc = rpc;
  }
  */

  /** Call this Procedure using the explicit-CallContext-convention.
   * The input arguments are (by default) in stack.args;
   * the result is written to ctx.consumer. */

  public void apply (CallContext ctx) throws Throwable
  {
    apply(this, ctx);
  }

  public static void apply (Procedure proc, CallContext ctx) throws Throwable
  {
    Object result;
    int count = ctx.count;
    if (ctx.where == 0 && count != 0)
      result = proc.applyN(ctx.values);
    else
      {
	switch (count)
	  {
	  case 0:
	    result = proc.apply0();
	    break;
	  case 1:
	    result = proc.apply1(ctx.getNextArg());
	    break;
	  case 2:
	    result = proc.apply2(ctx.getNextArg(), ctx.getNextArg());
	    break;
	  case 3:
	    result = proc.apply3(ctx.getNextArg(), ctx.getNextArg(),
				 ctx.getNextArg());
	    break;
	  case 4:
	    result = proc.apply4(ctx.getNextArg(), ctx.getNextArg(),
				 ctx.getNextArg(), ctx.getNextArg());
	    break;
	  default:
	    result = proc.applyN(ctx.getArgs());
	    break;
	  }
      }
    ctx.writeValue(result);
  }

  /** Pass zero arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match0 (CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (min > 0)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num < 0)
      return matchN(ProcedureN.noArgs, ctx);
    ctx.count = 0;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = this;
    return 0;
  }

  /** Pass one argument.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match1 (Object arg1, CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (min > 1)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = maxArgs(num);
	if (max < 1)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.count = 1;
	ctx.where = CallContext.ARG_IN_VALUE1;
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1 };
    return matchN(args, ctx);
  }

  /** Pass two arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (min > 2)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = maxArgs(num);
	if (max < 2)
          return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	ctx.value1 = arg1;
	ctx.value2 = arg2;
	ctx.count = 2;
	ctx.where = CallContext.ARG_IN_VALUE1
	  |(CallContext.ARG_IN_VALUE2<<4);
	ctx.next = 0;
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2 };
    return matchN(args, ctx);
  }

  /** Pass three arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (min > 3)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = maxArgs(num);
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
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2, arg3 };
    return matchN(args, ctx);
  }

  /** Pass four arguments.
   * @return non-negative if the match succeeded, else negative.
   */
  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (min > 4)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
        int max = maxArgs(num);
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
	ctx.proc = this;
	return 0;
      }
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(args, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    int num = numArgs();
    int min = minArgs(num);
    if (args.length < min)
      return MethodProc.NO_MATCH_TOO_FEW_ARGS|min;
    if (num >= 0)
      {
	switch (args.length)
	  {
	  case 0:
	    return match0(ctx);
	  case 1:
	    return match1(args[0], ctx);
	  case 2:
	    return match2(args[0], args[1], ctx);
	  case 3:
	    return match3(args[0], args[1], args[2], ctx);
	  case 4:
	    return match4(args[0], args[1], args[2], args[3], ctx);
	  default:
	    int max = maxArgs(num);
	    if (args.length > max)
	      return MethodProc.NO_MATCH_TOO_MANY_ARGS|max;
	  }
      }
    ctx.values = args;
    ctx.count = args.length;
    ctx.where = 0;
    ctx.next = 0;
    ctx.proc = this;
    return 0;
  }

  /** Does match0, plus throws exception on argument mismatch. */
  public void check0 (CallContext ctx)
  {
    int code = match0(ctx);
    if (code != 0)
      {
	throw MethodProc.matchFailAsException(code, this, ProcedureN.noArgs);
      }
  }

  /** Does match1, plus throws exception on argument mismatch. */
  public void check1 (Object arg1, CallContext ctx)
  {
    int code = match1(arg1, ctx);
    if (code != 0)
      {
	Object[] args = { arg1 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does match, plus throws exception on argument mismatch. */
  public void check2 (Object arg1, Object arg2, CallContext ctx)
  {
    int code = match2(arg1, arg2, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }
 
  /** Does match3, plus throws exception on argument mismatch. */
  public void check3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    int code = match3(arg1, arg2, arg3, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2, arg3 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does match4, plus throws exception on argument mismatch. */
  public void check4 (Object arg1, Object arg2, Object arg3, Object arg4,
		      CallContext ctx)
  {
    int code = match4(arg1, arg2, arg3, arg4, ctx);
    if (code != 0)
      {
	Object[] args = { arg1, arg2, arg3, arg4 };
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  /** Does matchN, plus throws exception on argument mismatch. */
  public void checkN (Object[] args, CallContext ctx)
  {
    int code = matchN(args, ctx);
    if (code != 0)
      {
	throw MethodProc.matchFailAsException(code, this, args);
      }
  }

  public Procedure getSetter()
  {
    if (! (this instanceof HasSetter))
      {
	Object setter = getProperty(setterKey, null);
	if (setter instanceof Procedure)
	  return (Procedure) setter;
	throw new RuntimeException("procedure '"+getName()+ "' has no setter");
      }
    int num_args = numArgs();
    if (num_args == 0x0000)
      return new Setter0(this);
    if (num_args == 0x1001)
      return new Setter1(this);
    return new Setter(this);
  }

  public void setSetter (Procedure setter)
  {
    if (this instanceof HasSetter)
      throw new RuntimeException("procedure '"+getName()+
				 "' has builtin setter - cannot be modified");
    setProperty(Procedure.setterKey, setter);
  }

  /** If HasSetter, the Procedure is called in the LHS of an assignment. */
  public void set0(Object result) throws Throwable
  {
    getSetter().apply1(result);
  }

  public void set1(Object arg1, Object value) throws Throwable
  {
    getSetter().apply2(arg1, value);
  }

  public void setN (Object[] args) throws Throwable
  {
    getSetter().applyN(args);
  }

  /** True if this Procedure (definitely) has no side-effects.
   * Note side-effect-free does not imply idempotent if this
   * allocates an object with "identity".
   */
  public boolean isSideEffectFree ()
  {
    return false;
  }
  
  /** Semi-deprecated - instead should be set at Inline time. FIXME */
  public gnu.bytecode.Type getReturnType (gnu.expr.Expression[] args)
  {
    return gnu.bytecode.Type.objectType;
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append ("#<procedure ");
    String n = getName();
    if (n == null)
      n = getSourceLocation();
    if (n == null)
      n = getClass().getName();
    sbuf.append(n);
    sbuf.append('>');
    return sbuf.toString();
  }
}
