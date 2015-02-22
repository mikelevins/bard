// Copyright (c) 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.TextUtils;

public class MakeCDATA extends MethodProc // NodeConstructor
{
  public static final MakeCDATA makeCDATA
    = new MakeCDATA();

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    XConsumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
	StringBuffer sbuf = new StringBuffer();
	Object endMarker = Location.UNBOUND;
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    TextUtils.stringValue(arg, sbuf);
	  }
	int n = sbuf.length();
	char[] chars = new char[n];
	sbuf.getChars(0, n, chars, 0);
	out.writeCDATA(chars, 0, n);
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }

  /*
  public void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    for (int i = 0;  i < nargs;  i++)
      // FIXME needs to coerce to string value.
      compileChild(args[i], comp, target);
  }
  */
}
