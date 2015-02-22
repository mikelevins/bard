// Copyright (c) 2004  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.xml.TextUtils;

public class CommentConstructor extends MethodProc // NodeConstructor
{
  public static final CommentConstructor commentConstructor
    = new CommentConstructor();

  public int numArgs() { return 0x1001; }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    XConsumer out = NodeConstructor.pushNodeContext(ctx);
    try
      {
	StringBuffer sbuf = new StringBuffer();
	Object endMarker = Location.UNBOUND;
        boolean first = true;
	for (int i = 0; ; i++)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
            if (arg instanceof Values)
              {
                Values vals = (Values) arg;
                for (int it = 0;  (it = vals.nextPos(it)) != 0; )
                  {
                    if (! first)
                      sbuf.append(' ');
                    first = false;
                    TextUtils.stringValue(vals.getPosPrevious(it), sbuf);
                  }
              }
            else
              {
                if (! first)
                  sbuf.append(' ');
                first = false;
                TextUtils.stringValue(arg, sbuf);
              }
	  }
	int len = sbuf.length();
	char[] buf = new char[len];
	sbuf.getChars(0, len, buf, 0);
	out.writeComment(buf, 0, len);
      }
    finally
      {
	NodeConstructor.popNodeContext(saved, ctx);
      }
  }
}
