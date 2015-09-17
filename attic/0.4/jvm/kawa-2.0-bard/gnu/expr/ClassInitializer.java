// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

/** Cause a class value from a ClassExp to be initialized. */

public class ClassInitializer extends Initializer
{
  ClassExp cexp;

    public ClassInitializer(ClassExp cexp, Field field, Compilation comp) { 
        this.field = field;
        this.cexp = cexp;
        if (field.getStaticFlag()) {
            next = comp.clinitChain;
            comp.clinitChain = this;
        } else {
            LambdaExp heapLambda = cexp.getOwningLambda();
            next = heapLambda.initChain;
            heapLambda.initChain = this;
        }
    }

  @Override
  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();
    if (comp.immediate && field.getStaticFlag()
            && cexp.type != Type.javalangClassType)
        comp.compileConstant(cexp.compiledType);
    else
        cexp.compilePushClass(comp, Target.pushValue(field.getType()));
    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
    if (cexp.compiledType == comp.mainClass && cexp.clinitMethod != null)
      cexp.clinitMethod.body.compileWithPosition(comp, Target.Ignore);
  }
}
