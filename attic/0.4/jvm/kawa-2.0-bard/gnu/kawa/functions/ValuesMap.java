// Copyright (c) 2001, 2003, 2004, 2008  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.math.IntNum;

/** Map a function over a value sequence, yielding a new sequence.
 * Normally, the function takes one argument, the item in the sequence.
 * If startCounter is non-negative, a position index is also passed.
 * Used to implement XQuery's 'for' form.
 */

public class ValuesMap extends MethodProc implements Inlineable
{
  public static final ValuesMap valuesMap = new ValuesMap(-1);
  public static final ValuesMap valuesMapWithPos = new ValuesMap(1);

  private ValuesMap (int startCounter)
  {
    this.startCounter = startCounter;
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.functions.CompileMisc:validateApplyValuesMap");
  }

  /** If non-negative also define a counter variable.
   * Used for XQuery's 'at' clause in a FLWOR expression. */
  private final int startCounter;

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Procedure proc = (Procedure) ctx.getNextArg();
    Consumer out = ctx.consumer;
    Object val = ctx.getNextArg();
    Procedure.checkArgCount(proc, 1);
    if (val instanceof Values)
      {
	int ipos = 0;
	int count = startCounter;
	Values values = (Values) val;
	while ((ipos = values.nextPos(ipos)) != 0)
	  {
	    Object v = values.getPosPrevious(ipos);
	    if (startCounter >= 0)
	      proc.check2(v, IntNum.make(count++), ctx);
	    else
	      proc.check1(v, ctx);
	    ctx.runUntilDone();
	  }
      }
    else
      {
	if (startCounter >= 0)
	  proc.check2(val, IntNum.make(startCounter), ctx);
	else
	  proc.check1(val, ctx);
	ctx.runUntilDone();
      }
  }

  /** If we can inline, return LambdaExp for first arg; otherwise null. */
  static LambdaExp canInline (ApplyExp exp, ValuesMap proc)
  {
    Expression[] args = exp.getArgs();
    Expression arg0;
    // FIXME Could if needed wrap expr in LambdaExp:
    if (args.length == 2 && (arg0 = args[0]) instanceof LambdaExp)
      {
	LambdaExp lexp = (LambdaExp) arg0;
	if (lexp.min_args == lexp.max_args
	    && 	lexp.min_args == (proc.startCounter >= 0 ? 2 : 1))
	  return lexp;
      }
    return null;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    LambdaExp lambda = canInline(exp, this);
    if (lambda == null)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Expression[] args = exp.getArgs();
    if (! (target instanceof IgnoreTarget
	   || target instanceof ConsumerTarget))
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	return;
      }
    Expression vals = args[1];
    compileInlined(lambda, vals, startCounter, null, comp, target);
  }

  public static void compileInlined(LambdaExp lambda, Expression vals,
                                    int startCounter, Method matchesMethod,
                                    Compilation comp, Target target)
  {
    Declaration param = lambda.firstDecl();
    CodeAttr code = comp.getCode();
    Scope scope = code.pushScope();
    Variable counter;
    Declaration counterDecl;
    Type paramType = param.getType();
    if (startCounter >= 0)
      {
	counter = scope.addVariable(code, Type.intType, "position");
	code.emitPushInt(startCounter);
	code.emitStore(counter);
        counterDecl = new Declaration(counter);
      }
    else
      {
        counter = null;
        counterDecl = null;
      }
    // If the param Declaration is captured, then it gets messy initializing
    // it.  So just cheat and create a helper variable.
    if (param.isSimple() && matchesMethod == null)
      param.allocateVariable(code);
    else
    {
      String pname = Compilation.mangleNameIfNeeded(param.getName());
      param = new Declaration(code.addLocal(paramType.getImplementationType(), pname));
    }
    Expression[] args;
    if (startCounter >= 0)
      {
	args = new Expression[] { new ReferenceExp(param),
                                  new ReferenceExp(counterDecl) };
      }
    else
      args = new Expression[] { new ReferenceExp(param) };
    Expression app = new ApplyExp(lambda, args);
    if (matchesMethod != null)
      {
        // Major kludge - used by ValuesFilter.
        if (app.getType().getImplementationType() != Type.booleanType)
          app = new ApplyExp(matchesMethod,
                             new Expression[] {
                               app,
                               new ReferenceExp(counterDecl) });
        app = new IfExp(app, new ReferenceExp(param), QuoteExp.voidExp);
      }

    /* emit the following:
       int index = 0;
       for (;;)
       {
         int next = Values.nextIndex(values, index);
	 if (next < 0)
	   goto done;
	 Values.nextValue(values, index);
	 compileFromStackSimple(comp, Type.pointerType);
	 index = next;
       }
    */
    Variable indexVar = code.addLocal(Type.intType);
    Variable valuesVar = code.addLocal(Type.pointer_type);
    Variable nextVar = code.addLocal(Type.intType); 

    vals.compileWithPosition(comp, Target.pushObject);
    code.emitStore(valuesVar);
    code.emitPushInt(0);
    code.emitStore(indexVar);

    Label top = new Label(code);
    Label doneLabel = new Label(code);
    top.define(code);
    code.emitLoad(valuesVar);
    code.emitLoad(indexVar);
    code.emitInvokeStatic(Compilation.typeValues.getDeclaredMethod("nextIndex", 2));
    code.emitDup(Type.intType);
    code.emitStore(nextVar);

    code.emitGotoIfIntLtZero(doneLabel);

    code.emitLoad(valuesVar);
    code.emitLoad(indexVar);
    code.emitInvokeStatic(Compilation.typeValues.getDeclaredMethod("nextValue", 2));

    StackTarget.convert(comp, Type.objectType, paramType);
    param.compileStore(comp);

    app.compile(comp, target);

    if (startCounter >= 0)
      {
	code.emitInc(counter, (short) 1);
      }

    code.emitLoad(nextVar);
    code.emitStore(indexVar);
    code.emitGoto(top);

    doneLabel.define(code);

    code.popScope();
  }

  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}
