// Copyright (c) 2003, 2004, 2006, 2014  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;

import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.functions.MakeSplice;
import gnu.kawa.reflect.CompileArrays;
import gnu.kawa.io.OutPort;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/** This class is used to represent "combination" or "application".
 * A function and arguments are evaluated, and then the function applied.
 * @author	Per Bothner
 */

public class ApplyExp extends Expression
{
  Expression func;
  Expression[] args;

  public static final int TAILCALL = NEXT_AVAIL_FLAG;
  public static final int INLINE_IF_CONSTANT = NEXT_AVAIL_FLAG << 1;
  public static final int MAY_CONTAIN_BACK_JUMP = NEXT_AVAIL_FLAG << 2;

  /** Containing LambdaExp. */
  LambdaExp context;

  /** The next ApplyExp in ((ReferenceExp)func).binding.firstCall list. */
  public ApplyExp nextCall;

    /** Index of argument for first keyword argument.
     * If zero, no keyword arguments.  If non-zero, then
     * {@code firstKeywordArgIndex-1} is the index in the {@code args}
     * array of the first keyword.
     */
    public int firstKeywordArgIndex;
    public int numKeywordArgs;

    /** Index of first argument that is a MakeSplice.
     * The value is -1 is no argument is a splice.
     */
    public int firstSpliceArg = -1;

  public final Expression getFunction() { return func; }
  public final Expression[] getArgs() { return args; }
  public final int getArgCount() { return args.length; }
    public void setFunction(Expression func) { this.func = func; }
  public void setFunction(Procedure proc) { this.func = new QuoteExp(proc); }
  public void setArgs(Expression[] args) { this.args = args; }
  public Expression getArg(int i) { return args[i]; }
  public void setArg(int i, Expression arg) { args[i] = arg; }
  public final boolean isTailCall() { return getFlag(TAILCALL); }
  public final void setTailCall(boolean tailCall)
  { setFlag(tailCall, TAILCALL); }

  public ApplyExp setFuncArgs (Expression func, Expression[] args)
  {
    setFunction(func);
    setArgs(args);
    setFlag(false, Expression.VALIDATED);
    return this;
  }

  public ApplyExp setFuncArgs (Procedure proc, Expression[] args)
  {
    return setFuncArgs(new QuoteExp(proc), args);
  }

  /** If getFunction() is constant, return its value; otherwise null. */
  public final Object getFunctionValue()
  {
    return func instanceof QuoteExp ? ((QuoteExp) func).getValue() : null;
  }

    /** Copy over splice and keyword start indexes.
     * @param src orginal ApplyExp (may be the same as this)
     * @aram amount to adjust indexes by
     */
    public void adjustSplice(ApplyExp src, int delta) {
        if (src.firstSpliceArg >= 0)
            this.firstSpliceArg = src.firstSpliceArg + delta;
        if (src.firstKeywordArgIndex > 0)
            this.firstKeywordArgIndex = src.firstKeywordArgIndex + delta;
    }

    public int spliceCount() {
        int count = 0;
        if (firstSpliceArg >= 0) {
            Expression[] args = this.args;
            int nargs = args.length;
            for (int i = firstSpliceArg; i < nargs; i++) {
                if (MakeSplice.argIfSplice(args[i]) != null)
                    count++;
            }
        }
        return count;
    }

    public boolean isSimple() {
        return firstSpliceArg < 0 && firstKeywordArgIndex == 0;
    }

    public boolean isSimple(int min) {
        return isSimple() && getArgCount() >= min;
    }

    public boolean isSimple(int min, int max) {
        int ac = getArgCount();
        return isSimple() && ac >= min && ac <= max;
    }

    public boolean isAppendValues() {
        return func instanceof QuoteExp
            && (((QuoteExp) func).getValue()
                == gnu.kawa.functions.AppendValues.appendValues);
    }

  public ApplyExp (Expression f, Expression... a) { func = f; args = a; }

  public ApplyExp (Procedure p, Expression... a) { this(new QuoteExp(p), a); }

  public ApplyExp (Method m, Expression... a)
  {
    this(new QuoteExp(new PrimProcedure(m)), a);
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object proc = func.eval(ctx);
    int n = args.length;
    Object[] vals = new Object[n];
    for (int i = 0; i < n; i++)
      vals[i] = args[i].eval(ctx);
    ((Procedure) proc).checkN(vals, ctx);
  }

  private static void compileToArray(Expression[] args, int start, Compilation comp)
  {
    CodeAttr code = comp.getCode();
    int argslength = args.length;
    int nargs = argslength - start;
    if (nargs == 0)
      {
	code.emitGetStatic(Compilation.noArgsField);
	return;
      }
    code.emitPushInt(nargs);
    code.emitNewArray(Type.pointer_type);
    for (int i = start; i < argslength; ++i)
      {
	Expression arg = args[i];
	if (comp.usingCPStyle()
	    && ! (arg instanceof QuoteExp) && ! (arg instanceof ReferenceExp))
	  {
	    // If the argument involves a CPStyle function call, we will
	    // have to save and restore anything on the JVM stack into
	    // fields in the CallFrame.  This is expensive, so defer
	    // pushing the duplicated argument array and the index
	    // until *after* we've calculated the argument.  The downside
	    // is that we have to do some extra stack operations.
	    // However, these are cheap (and get compiled away when
	    // compiling to native code).
	    arg.compileWithPosition(comp, Target.pushObject);
	    code.emitSwap();
	    code.emitDup(1, 1);
	    code.emitSwap();
	    code.emitPushInt(i);
	    code.emitSwap();
	  }
	else
	  {
	    code.emitDup(Compilation.objArrayType);
	    code.emitPushInt(i);
	    arg.compileWithPosition(comp, Target.pushObject);
	  }
	code.emitArrayStore(Type.pointer_type);
      }
  }

  public void compile (Compilation comp, Target target)
  {
    compile(this, comp, target, true);
  }

  public static void compile (ApplyExp exp, Compilation comp, Target target)
  {
    compile(exp, comp, target, false);
  }

    static void compile (ApplyExp exp, Compilation comp, Target target,
                         boolean checkInlineable) {
        int args_length = exp.args.length;
        Expression exp_func = exp.func;
        LambdaExp func_lambda = null;
        String func_name = null;
        Declaration owner = null;
        Object quotedValue = null;
        if (exp_func instanceof LambdaExp) {
            func_lambda = (LambdaExp) exp_func;
            func_name = func_lambda.getName();
            if (func_name == null)
                func_name = "<lambda>";
        } else if (exp_func instanceof ReferenceExp) { 
            ReferenceExp func_ref = (ReferenceExp) exp_func;
            owner = func_ref.contextDecl();
            Declaration func_decl = func_ref.binding;
            Expression dval;
            while (func_decl != null && func_decl.isAlias()
                   && (dval = func_decl.getValueRaw()) instanceof ReferenceExp) {
                func_ref = (ReferenceExp) dval;
            if (owner != null || func_decl.needsContext()
                || func_ref.binding == null)
                break;
            func_decl = func_ref.binding;
            owner = func_ref.contextDecl();
            }
            if (! func_decl.getFlag(Declaration.IS_UNKNOWN)) {
                Expression value = func_decl.getValue();
                func_name = func_decl.getName();
                if (value != null && value instanceof LambdaExp) 
                    func_lambda = (LambdaExp) value;
                if (value != null && value instanceof QuoteExp) 
                    quotedValue = ((QuoteExp) value).getValue();
            }
        } else if (exp_func instanceof QuoteExp) {
            quotedValue = ((QuoteExp) exp_func).getValue();
        }
        if (checkInlineable && quotedValue instanceof Procedure) {
            Procedure proc = (Procedure) quotedValue;
            if (target instanceof IgnoreTarget && proc.isSideEffectFree()) {
                for (int i = 0; i < args_length;  i++)
                    exp.args[i].compile(comp, target);
                return;
            }
            try {
                if (inlineCompile(proc, exp, comp, target))
                    return;
            } catch (Error ex) {
                throw ex;
            } catch (Throwable ex) {
                SourceMessages msg = comp.getMessages();
                msg.error('f',
                          "caught exception in inline-compiler for "
                          +quotedValue+" - "+ex, ex);
                throw new SyntaxException(msg);
            }
        }

        CodeAttr code = comp.getCode();
        Method method;

    // Check for tail-recursion.
    boolean tail_recurse
      = exp.isTailCall()
      && func_lambda != null && func_lambda == comp.curLambda
      // No keyword or optional arguments.
      && func_lambda.opt_args == 0 && func_lambda.keywords == null;

    int spliceCount = exp.spliceCount();
    int nonSpliceCount = args_length - spliceCount;
        
    if (func_lambda != null)
      {
	if ((func_lambda.max_args >= 0 && nonSpliceCount > func_lambda.max_args)
	    || (nonSpliceCount < func_lambda.min_args) && spliceCount == 0)
	  // This is supposed to get caught by InlineCalls.
	  throw new Error ("internal error - wrong number of parameters for "
			   + func_lambda);
	int conv = func_lambda.getCallConvention();
        if (func_lambda.primMethods==null && ! func_lambda.isClassGenerated()
            && ! func_lambda.getInlineOnly())
            func_lambda.allocMethod(func_lambda.outerLambda(), comp);
        // Mostly duplicates logic with LambdaExp.validateApply.
        // See comment there.
	if (comp.inlineOk(func_lambda)
            && ! tail_recurse
	    && (conv <= Compilation.CALL_WITH_CONSUMER
		|| (conv == Compilation.CALL_WITH_TAILCALLS
		    && ! exp.isTailCall()))
	    && (method = func_lambda.getMethod(nonSpliceCount, spliceCount)) != null
            && (exp.firstSpliceArg < 0
                || (PrimProcedure.takesVarArgs(method)
                    && func_lambda.min_args <= exp.firstSpliceArg)))
	  {
	    PrimProcedure pproc = new PrimProcedure(method, func_lambda);
	    boolean is_static = method.getStaticFlag();
	    boolean extraArg = false;
	    // ?? Procedure.checkArgCount(this, args.length); // FIXME
	    if (! is_static || func_lambda.declareClosureEnv() != null)
	      {
		if (is_static)
		  extraArg = true;
		if (comp.curLambda == func_lambda)  // Recursive call.
		  code.emitLoad(func_lambda.closureEnv != null
				? func_lambda.closureEnv
				: func_lambda.thisVariable);
		else if (owner != null)
                  owner.load(null, 0, comp, Target.pushObject);
                else
                  func_lambda.getOwningLambda().loadHeapFrame(comp);
	      }

	    pproc.compile(extraArg ? Type.voidType : null,
			  exp, comp, target);
	    return;
	  }
      }

    /*
    if (comp.usingCPStyle())
      {
	  {
	    Label l = new Label(code);
	    gnu.bytecode.SwitchState fswitch = comp.fswitch;
	    int pc = fswitch.getMaxValue() + 1;
	    fswitch.addCase(pc, code, l);
            exp_func.compile(comp, new StackTarget(Compilation.typeProcedure));
	    comp.loadCallContext();

	    // Emit: context->pc = pc.
	    comp.loadCallContext();
	    code.emitPushInt(pc);
	    code.emitPutField(Compilation.pcCallContextField);
	    code.emitInvokeVirtual(Compilation.applyCpsMethod);

	    // emit[save java stack, if needed]
	    Type[] stackTypes = code.saveStackTypeState(false);
	    java.util.Stack stackFields = new java.util.Stack(); 
	    if (stackTypes != null)
	      {
		for (int i = stackTypes.length;  --i >= 0; )
		  {
		    Field fld = comp.allocLocalField (stackTypes[i], null);
		    code.emitPushThis();
		    code.emitSwap();
		    code.emitPutField(fld);
		    stackFields.push(fld);
		  }
	      }

	    code.emitReturn();
	    l.define(code);

	    // emit[restore java stack, if needed]
	    if (stackTypes != null)
	      {
		for (int i = stackTypes.length;  --i >= 0; )
		  {
		    Field fld = (Field) stackFields.pop();
		    code.emitPushThis();
		    code.emitGetField(fld);
		    comp.freeLocalField(fld);
		  }
	      }

	    // FIXME
	    // Load result from stack.value to target.
	    comp.loadCallContext();
	    code.emitGetField(comp.valueCallContextField);
	    target.compileFromStack(comp, Type.pointer_type);
	  }
	return;
      }
    */
    if (func_lambda != null && func_lambda.getInlineOnly() && !tail_recurse
	&& func_lambda.min_args == nonSpliceCount)
      {
        pushArgs(func_lambda, exp.args, exp.args.length, null, comp);
        if (func_lambda.getFlag(LambdaExp.METHODS_COMPILED))
          {
            code.emitTailCall(false, func_lambda.startForInlining);
            return;
          }
        func_lambda.flags |= LambdaExp.METHODS_COMPILED;
	LambdaExp saveLambda = comp.curLambda;
	comp.curLambda = func_lambda;
	func_lambda.allocChildClasses(comp);
	func_lambda.allocParameters(comp);
        Label startForInlining = new Label(code);
        startForInlining.define(code);
        func_lambda.startForInlining = startForInlining;
	popParams (code, func_lambda, null, false);
	func_lambda.enterFunction(comp);
	func_lambda.body.compileWithPosition(comp, target);
	func_lambda.compileEnd(comp);
	func_lambda.generateApplyMethods(comp);
        code.popScope(); // Matches enterScope in allocParameters.
	comp.curLambda = saveLambda;
	return;
      }

    if (comp.curLambda.isHandlingTailCalls()
	&& (exp.isTailCall() || target instanceof ConsumerTarget)
	&& ! comp.curLambda.getInlineOnly())
      {
	ClassType typeContext = Compilation.typeCallContext;
	exp_func.compile(comp, new StackTarget(Compilation.typeProcedure));
	// evaluate args to frame-locals vars;  // may recurse!
	if (args_length <= 4 && exp.isSimple())
	  {
	    for (int i = 0; i < args_length; ++i)
	      exp.args[i].compileWithPosition(comp, Target.pushObject);
	    comp.loadCallContext();
	    code.emitInvoke(Compilation.typeProcedure
			    .getDeclaredMethod("check"+args_length,
					       args_length+1));
	  }
	else
	  {
            if (exp.firstSpliceArg >= 0)
                CompileArrays.createArray(Type.objectType, comp,
                                          exp.args, 0, args_length);
            else
                compileToArray (exp.args, 0, comp);
	    comp.loadCallContext();
	    code.emitInvoke(Compilation.typeProcedure
			    .getDeclaredMethod("checkN", 2));
	  }
	if (exp.isTailCall())
	  {
	    code.emitReturn();
	  }
	else if (((ConsumerTarget) target).isContextTarget())
	  {
	    comp.loadCallContext();
	    code.emitInvoke(typeContext.getDeclaredMethod("runUntilDone", 0));
	  }
	else
	  {
	    comp.loadCallContext();
	    code.emitLoad(((ConsumerTarget) target).getConsumerVariable());
	    code.emitInvoke(typeContext.getDeclaredMethod("runUntilValue", 1));
	  }
	return;
      }

        if (exp.firstSpliceArg >= 0) {
            Expression[] args = exp.getArgs();
            exp_func.compile(comp, Target.pushObject);
            CompileArrays.createArray(Type.objectType, comp,
                                      args, 0, args.length);
            code.emitInvoke(Compilation.typeProcedure
                            .getDeclaredMethod("applyN", 1));
            target.compileFromStack(comp, Type.pointer_type);
            return;
        }

    if (!tail_recurse)
      exp_func.compile (comp, new StackTarget(Compilation.typeProcedure));

    boolean toArray
      = (tail_recurse ? func_lambda.max_args < 0
         : args_length > 4);
    int[] incValues = null; // Increments if we can use iinc.
    if (tail_recurse)
      {
        int fixed = func_lambda.min_args;
        incValues = new int[fixed];
        pushArgs(func_lambda, exp.args, fixed, incValues, comp);
        if (toArray)
          compileToArray(exp.args, fixed, comp);
        method = null;
      }
    else if (toArray)
      {
        compileToArray(exp.args, 0, comp);
	method = Compilation.applyNmethod;
      }
    else
      {
	for (int i = 0; i < args_length; ++i)
          {
            exp.args[i].compileWithPosition(comp, Target.pushObject);
            if (! code.reachableHere())
              break;
          }
        method = Compilation.applymethods[args_length];
      }
    if (! code.reachableHere())
      {
        if (comp.warnUnreachable())
          comp.error('w', "unreachable code");
        return;
      }
    if (tail_recurse)
      {
        Label startLabel = func_lambda.startForInlining;
        boolean mustStore = startLabel == null;
        if (incValues != null && ! mustStore)
          {
            for (int i = incValues.length;  --i >= 0; )
              if (incValues[i] != SetExp.BAD_SHORT)
                {
                  mustStore = true;
                  break;
                }
          }
        if (mustStore)
          {
            popParams(code, func_lambda, incValues, toArray);
            startLabel = func_lambda.getVarScope().getStartLabel();
          }
        code.emitTailCall(false, startLabel);
	return;
      }
    code.emitInvokeVirtual(method);
    target.compileFromStack(comp, Type.pointer_type);
  }

  public Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    Expression f = deepCopy(func, mapper);
    Expression[] a = deepCopy(args, mapper);
    if ((f == null && func != null) || (a == null && args != null))
      return null;
    ApplyExp copy = new ApplyExp(f, a);
    copy.flags = getFlags();
    return copy;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitApplyExp(this, d);
  }

    public void visitArgs(InlineCalls visitor) {
        int nargs = args.length;
        args = visitor.visitExps(args, nargs,
                                 isAppendValues() ? null
                                 : InlineCalls.ValueNeededType.instance);
    }

  protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d)
  {
    func = visitor.visitAndUpdate(func, d);
    if (visitor.exitValue == null)
      args = visitor.visitExps(args, args.length, d);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Apply", ")", 2);
    if (isTailCall())
      out.print (" [tailcall]");
    if (type != null && type != Type.pointer_type)
      {
        out.print(" => ");
        out.print(type);
      }
    out.writeSpaceFill();
    printLineColumn(out);
    func.print(out);
    for (int i = 0; args != null && i < args.length; ++i)
      {
	out.writeSpaceLinear();
	args[i].print(out);
      }
    out.endLogicalBlock(")");
  }

  /** Only used for inline- and tail-calls. */
  private static void pushArgs (LambdaExp lexp,
                                Expression[] args, int args_length,
                                int[] incValues, Compilation comp)
  {
    Declaration param = lexp.firstDecl();
    for (int i = 0; i < args_length; ++i)
      {
        Expression arg = args[i];
        if (param.ignorable())
          arg.compile(comp, Target.Ignore);
        else if (incValues != null
            && (incValues[i] = SetExp.canUseInc(arg, param)) != SetExp.BAD_SHORT)
          ;
        else
          arg.compileWithPosition(comp,
                                  StackTarget.getInstance(param.getType()));
        param = param.nextDecl();
      }
  }

  private static void popParams (CodeAttr code, LambdaExp lexp,
                                 int[] incValues, boolean toArray)
  {
    Variable vars = lexp.getVarScope().firstVar();
    Declaration decls = lexp.firstDecl();
    if (vars != null && vars.getName() == "this")
      vars = vars.nextVar();
    if (vars != null && vars.getName() == "$ctx")
      vars = vars.nextVar();
    popParams (code, 0, lexp.min_args, toArray, incValues, decls, vars);
  }

  // Recursive helper function.
  private static void popParams (CodeAttr code, int paramNo,
                                 int count, boolean toArray,
                                 int[] incValues,
                                 Declaration decl, Variable vars)
  {
    if (count > 0)
      {
        count--;
	popParams (code, paramNo+1, count, toArray, incValues, decl.nextDecl(),
                   decl.getVariable() == null ? vars : vars.nextVar());
        if (! decl.ignorable())
          {
            if (incValues != null && incValues[paramNo] != SetExp.BAD_SHORT)
              code.emitInc(vars, (short) incValues[paramNo]);
            else
              code.emitStore(vars);
          }
      }
    else if (toArray)
      code.emitStore(vars); // rest array
  }

  public boolean side_effects ()
  {
    Object value = derefFunc(func).valueIfConstant();
    if (value instanceof Procedure
        && ((Procedure) value).isSideEffectFree())
      {
        Expression[] a = args;
        int alen = a.length;
        for (int i = 0;  i < alen;  i++)
          {
            if (a[i].side_effects())
              return true;
          }
        return false;
      }
    return true;
  }

  static Expression derefFunc(Expression afunc)
  {
    if (afunc instanceof ReferenceExp)
      {
	Declaration func_decl = ((ReferenceExp) afunc).binding;
	func_decl = Declaration.followAliases(func_decl);
	if (func_decl != null && ! func_decl.getFlag(Declaration.IS_UNKNOWN))
	  afunc = func_decl.getValue();
      }
    return afunc;
  }

  protected gnu.bytecode.Type calculateType()
  {
    Expression afunc = derefFunc(func);
    if (afunc instanceof QuoteExp)
      {
        Object value = ((QuoteExp) afunc).getValue();
        // This logic is deprecated - instead set type during InlineCalls.
        if (value instanceof Procedure)
          type = ((Procedure) value).getReturnType(args);
      }
    else if (afunc instanceof LambdaExp)
      {
	type = ((LambdaExp) afunc).getReturnType();
      }
    return type;
  }

    public static boolean isInlineable(Procedure proc) {
        return
            proc instanceof Inlineable
            || Procedure.compilerKey.get(proc) != null
            || proc.getProperty(Procedure.compilerXKey, null) != null;
    }

    static boolean inlineCompile(Procedure proc, ApplyExp exp,
                                 Compilation comp, Target target)
            throws Throwable {
        if (proc instanceof PrimProcedure)
            return ((PrimProcedure) proc).compile(exp, comp, target);
        Object propval = proc.getProperty(Procedure.compilerXKey, null);
        if (propval instanceof String) {
            Object method = InlineCalls.resolveInliner(proc, (String) propval,
                                                       compilerMethodType);
            if (method != null)
                propval = method;
        }
        /* #ifdef use:java.lang.invoke */
        if (propval instanceof MethodHandle) {
            return (boolean) ((MethodHandle) propval).invokeExact(exp, comp, target, proc);
        }
        /* #else */
        // if (propval instanceof java.lang.reflect.Method)
        //    return (Boolean) ((java.lang.reflect.Method) propval)
        //        .invoke(null, new Object[] { exp, comp, target, proc });
        /* #endif */
        if (propval != null) {
            comp.error('e', "compiler property string for "+proc
                       +" is not of the form CLASS:METHOD");
            return false;
        }
        if (! exp.isSimple())
            return false;
        Inlineable compiler;
        if (proc instanceof Inlineable)
            compiler = (Inlineable) proc;
        else if ((propval = Procedure.compilerKey.get(proc)) != null)
            compiler = (Inlineable) Procedure.compilerKey.get(proc);
        else
            compiler = null;
        if (compiler == null)
            return false;
        compiler.compile(exp, comp, target);
        return true;
    }

  public final Expression inlineIfConstant(Procedure proc, InlineCalls visitor)
  {
    return inlineIfConstant(proc, visitor.getMessages());
  }

  /** Inline this ApplyExp if parameters are constant.
   * @param proc the procedure bound to this.func.
   * @return the constant result (as a QuoteExp) if inlining was possible;
   *   otherwise this ApplyExp.
   * If applying proc throws an exception, print a warning on walker.messages.
   */
  public final Expression inlineIfConstant(Procedure proc, SourceMessages messages)
  {
    int len = args.length;
    Object[] vals = new Object[len];
    for (int i = len;  --i >= 0; )
      {
	Expression arg = args[i];
	if (arg instanceof ReferenceExp)
	  {
	    Declaration decl = ((ReferenceExp) arg).getBinding();
	    if (decl != null)
	      {
		arg = decl.getValue();
		if (arg == QuoteExp.undefined_exp)
		  return this;
	      }
	  }
	if (! (arg instanceof QuoteExp))
	  return this;
	vals[i] = ((QuoteExp) arg).getValue();
      }
    try
      {
	return new QuoteExp(proc.applyN(vals), type).setLine(this);
      }
    catch (Error ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
	if (messages != null)
	  messages.error('w', "call to " + proc +
				" throws " + ex);
	return this;
      }
  }

  public String toString ()
  {
    if (this==LambdaExp.unknownContinuation)
      return "ApplyExp[unknownContinuation]";
    return "ApplyExp/"+(args == null?0:args.length)+'['+func+']';
  }

    /* #ifdef use:java.lang.invoke */
    static final MethodType compilerMethodType =
        MethodType.methodType(boolean.class,
                              gnu.expr.ApplyExp.class,
                              gnu.expr.Compilation.class,
                              gnu.expr.Target.class,
                              gnu.mapping.Procedure.class);
    /* #else */
    // private static final Class[] compilerMethodType =
    //     new Class[] { gnu.expr.ApplyExp.class,
    //                   gnu.expr.Compilation.class,
    //                   gnu.expr.Target.class,
    //                   gnu.mapping.Procedure.class };
    /* #endif */
}
