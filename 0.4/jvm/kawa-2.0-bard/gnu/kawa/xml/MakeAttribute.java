// Copyright (c) 2001, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class MakeAttribute extends NodeConstructor
{
  public static final MakeAttribute makeAttribute = new MakeAttribute();
  public static final MakeAttribute makeAttributeS = new MakeAttribute();
    static { makeAttributeS.setStringIsText(true); }
  public static final QuoteExp makeAttributeExp = new QuoteExp(makeAttribute);

  public int numArgs() { return 0xFFFFF001; }

  public static void startAttribute(Consumer out, Symbol type)
  {
    out.startAttribute(type);
  }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
	Object type = ctx.getNextArg();
	startAttribute(out, (Symbol) type);
	Object endMarker = Special.dfault;
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
            if (stringIsText)
                writeContentS(arg, out);
            else
                writeContent(arg, out);
	  }
	out.endAttribute();
      }
    finally
      {
	popNodeContext(saved, ctx);
      }
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = ((ConsumerTarget) target).getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    code.emitDup();
    args[0].compile(comp, CheckedTarget.getInstance(Compilation.typeSymbol));
    // Stack:  consumer, consumer, tagtype
    code.emitInvokeStatic(startAttributeMethod);
    for (int i = 1;  i < nargs;  i++)
      compileChild(args[i], stringIsText, comp, target);
    code.emitInvokeInterface(endAttributeMethod);
  }

  static final ClassType typeMakeAttribute
    = ClassType.make("gnu.kawa.xml.MakeAttribute");
  static final Method startAttributeMethod
    = typeMakeAttribute.getDeclaredMethod("startAttribute", 2);
  static final Method endAttributeMethod
    = Compilation.typeConsumer.getDeclaredMethod("endAttribute", 0);

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }
}
