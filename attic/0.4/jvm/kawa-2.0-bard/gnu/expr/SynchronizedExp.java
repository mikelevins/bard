package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.*;

public class SynchronizedExp extends Expression
{
  Expression object;
  Expression body;

  public SynchronizedExp (Expression object, Expression body)
  {
    this.object = object;
    this.body = body;
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object value = object.eval(ctx);
    Object result;
    synchronized (value)
      {
	result = body.eval(ctx);
      }
    ctx.writeValue(result);
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    object.compile (comp, Target.pushObject);
    code.emitDup(1);
    Scope scope = code.pushScope();
    Variable objvar = scope.addVariable(code, Type.pointer_type, null);
    code.emitStore(objvar); 
    code.emitMonitorEnter();
    code.emitTryStart(false,
		      (target instanceof IgnoreTarget
		       || target instanceof ConsumerTarget) ? null
		      : target.getType());

    body.compileWithPosition(comp, target); 
    code.emitLoad(objvar);
    code.emitMonitorExit();
    code.emitTryEnd();
    code.emitCatchStart(null);
    code.emitLoad(objvar);
    code.emitMonitorExit(); 
    code.emitThrow(); 
    code.emitCatchEnd();
    code.emitTryCatchEnd();
    code.popScope();
 }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitSynchronizedExp(this, d);
  }

  protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d)
  {
    object = visitor.visitAndUpdate(object, d);
    if (visitor.exitValue == null)
      body = visitor.visitAndUpdate(body, d);
  }

  public void print (OutPort ps)
  {
    ps.print("(Synchronized ");
    object.print(ps);
    ps.print(" ");
    body.print(ps);
    ps.print(")");
  }
}
