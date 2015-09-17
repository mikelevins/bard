// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.mapping.*;

/** Implement 'typeswitch' (as in XQuery) or 'typecase'.
 * Usage: (typeswitch SELECTOR CASE-LAMBDA ... DEFAULT-LAMBDA)
 * Each CASE-LAMBDA is a 1-argument MethodProc, while DEFAULT-LAMBDA
 * is a 0-argument Procedure.  Calls the first CASE-LAMBDA such that
 * SELECTOR is a valid argument; if there is none, calls DEFAULT-LAMBDA.
 * In the current implementation, all of CASE-LAMBDA and DEFAULT-LAMBDA
 * must be LambdaExps, and the call must be inlined.
 */

public class TypeSwitch extends MethodProc implements Inlineable
{
  public static final TypeSwitch typeSwitch = new TypeSwitch("typeswitch");

  public TypeSwitch(String name)
  {
    setName(name);
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.reflect.CompileReflect:validateApplyTypeSwitch");
  }

  public int numArgs() { return 0xfffff002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    Object selector = args[0];
    int n = args.length-1;
    for (int i = 1;  i < n;  i++)
      {
	MethodProc caseProc = (MethodProc) args[i];
        int m = caseProc.match1(selector, ctx);
	if (m >= 0)
	  return;
      }
    Procedure defaultProc = (Procedure) args[n];
    defaultProc.check1(selector, ctx);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();

    CodeAttr code = comp.getCode();
    code.pushScope();
    Variable selector = code.addLocal(Type.pointer_type);
    args[0].compile(comp, Target.pushObject);
    code.emitStore(selector);

    for (int i = 1;  i < args.length;  )
      {
	if (i > 1)
	  code.emitElse();

        Expression arg = args[i++];

	if (arg instanceof LambdaExp)
	  {
	    LambdaExp lambda = (LambdaExp) arg;
	    Declaration param = lambda.firstDecl();
	    Type type = param.getType();
            if (! param.getCanRead())
              param = null;
            else
              param.allocateVariable(code);

	    if (type instanceof TypeValue)
	      ((TypeValue) type).emitTestIf(selector, param, comp);
	    else
	      {
                if (i < args.length)
                  {
                    code.emitLoad(selector);
                    type.emitIsInstance(code);
                    code.emitIfIntNotZero();
                  }
                if (param != null)
                  {
                    code.emitLoad(selector);
                    param.compileStore(comp);
                  }
	      }
	    lambda.allocChildClasses(comp);
	    lambda.body.compileWithPosition(comp, target);
	  }
	else
	  {
	    throw new Error("not implemented: typeswitch arg not LambdaExp");
	  }
      }
    for (int i = args.length - 2; --i >= 0; )
      code.emitFi();
    
    code.popScope();
  }


  public Type getReturnType (Expression[] args)
  {
    return Type.pointer_type;
  }
}
