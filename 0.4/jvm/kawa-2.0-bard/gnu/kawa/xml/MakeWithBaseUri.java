// Copyright (c) 2006  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.xml.*;

/** A Procedure to create an included entity object, or
 * set the base-uri property for a document or fragment.
 * This procedure takes two paramaters: The base-uri, and the "contents".
 */

public class MakeWithBaseUri extends NodeConstructor
{
  public static final MakeWithBaseUri makeWithBaseUri = new MakeWithBaseUri();

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = NodeConstructor.pushNodeContext(ctx);
    Object baseUri = ctx.getNextArg();
    Object node = ctx.getNextArg();
    if (out instanceof XConsumer)
      ((XConsumer) out).beginEntity(baseUri);
    try
      {
        Values.writeValues(node, out);
      }
    finally
      {
        if (out instanceof XConsumer)
          ((XConsumer) out).endEntity();
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }


  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    args[0].compile(comp, Target.pushObject);
    code.emitInvokeInterface(beginEntityMethod);
    compileChild(args[1], stringIsText, comp, target);
    code.emitLoad(consumer);
    code.emitInvokeInterface(endEntityMethod);
  }

  static final ClassType typeXConsumer = ClassType.make("gnu.lists.XConsumer");
  static final Method beginEntityMethod
    = typeXConsumer.getDeclaredMethod("beginEntity", 1);
  static final Method endEntityMethod
    = typeXConsumer.getDeclaredMethod("endEntity", 0);
}
