// Copyright (c) 2001, 2002  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class AppendValues extends MethodProc implements Inlineable
{
  public static final AppendValues appendValues = new AppendValues();

  public AppendValues ()
  {
    super();
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.functions.CompileMisc:validateApplyAppendValues");
  }

  public void apply (CallContext ctx)
  {
    Object endMarker = Special.dfault;
    for (;;)
      {
	Object arg = ctx.getNextArg(endMarker);
	if (arg == endMarker)
	  break;
	if (arg instanceof Consumable)
	  ((Consumable) arg).consume(ctx.consumer);
	else
	  ctx.writeValue(arg);
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (target instanceof ConsumerTarget || target instanceof IgnoreTarget)
      {
	for (int i = 0;  i < nargs;  i++)
	  args[i].compileWithPosition(comp, target);
      }
    else
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	/*
	CodeAttr code = comp.getCode();
	Scope scope = code.pushScope();
	Variable values = scope.addVariable(code, comp.typeValues, null);
	ConsumerTarget ctarget = new ConsumerTarget(values);
	code.emitInvokeStatic(comp.typeValues.getDeclaredMethod("make", 0));
	code.emitStore(values);
	for (int i = 0;  i < nargs;  i++)
	  args[i].compile(comp, ctarget);
	code.emitLoad(values);
	code.popScope();
	target.compileFromStack(comp, Compilation.typeValues);
	*/
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }
}
