package gnu.expr;
import gnu.bytecode.*;

/** An expression that evaluated to an instance of an anonymous class.
 * It's conceptually questionable that this inherits from ClassExp
 * - it should perhaps inherit from ApplyExp.
 */

public class ObjectExp extends ClassExp
{
  public ObjectExp ()
  {
    super(true, new ClassType());
  }

  protected Type calculateType() { return compiledType; }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitObjectExp(this, d);
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    code.emitNew(compiledType);
    code.emitDup(1);
    Method init = Compilation.getConstructor(compiledType, this);
    if (closureEnvField != null)
      {
	LambdaExp caller = outerLambda();
	Variable closureEnv =
          ! comp.usingCallContext() ? getOwningLambda().heapFrame
	  : caller.heapFrame != null ? caller.heapFrame	: caller.closureEnv;
	if (closureEnv == null)
	  code.emitPushThis();
	else
	  code.emitLoad(closureEnv);
      }
    code.emitInvokeSpecial(init);

    target.compileFromStack(comp, getCompiledClassType(comp));
  }

}
