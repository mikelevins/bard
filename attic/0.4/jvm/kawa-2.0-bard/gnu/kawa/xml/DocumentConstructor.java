// Copyright (c) 2003  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.lists.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class DocumentConstructor extends NodeConstructor
{
  public static final DocumentConstructor documentConstructor
    = new DocumentConstructor();

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
	Object endMarker = Location.UNBOUND;
	out.startDocument();
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    if (arg instanceof Consumable)
	      ((Consumable) arg).consume(out);
	    else
	      out.writeObject(arg);
	  }
	out.endDocument();
      }
    finally
      {
	popNodeContext(saved, ctx);
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
    code.emitInvokeInterface(startDocumentMethod);
    for (int i = 0;  i < nargs;  i++)
      compileChild(args[i], stringIsText, comp, target);
    code.emitLoad(consumer);
    code.emitInvokeInterface(endDocumentMethod);
  }

  static final Method startDocumentMethod
    = Compilation.typeConsumer.getDeclaredMethod("startDocument", 0);
  static final Method endDocumentMethod
    = Compilation.typeConsumer.getDeclaredMethod("endDocument", 0);

}
