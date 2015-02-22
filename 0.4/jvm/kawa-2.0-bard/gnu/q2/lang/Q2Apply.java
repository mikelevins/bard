package gnu.q2.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import java.util.Vector;

public class Q2Apply extends MethodProc
{
  public static Q2Apply q2Apply = new Q2Apply();

  public void apply (CallContext ctx)  throws Throwable
  {
    Object endMarker = Special.dfault;
    Object arg = ctx.getNextArg(endMarker);
    if (arg instanceof Procedure || arg instanceof gnu.bytecode.Type || arg instanceof Class)
      {
	Procedure proc;
	Vector vec = new Vector();
        if (arg instanceof Procedure)
          proc = (Procedure) arg;
        else
          {
            vec.add(arg);
            proc = gnu.kawa.reflect.Invoke.make;
          }
	for (;;)
	  {
	    arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
	    if (arg instanceof Values)
	      {
		Object[] vals = ((Values) arg).getValues();
		for (int i = 0;  i < vals.length;  i++)
		  vec.add(vals[i]);
	      }
	    else
	      vec.add(arg);  // requires collections FIXME
	  }
	arg = proc.applyN(vec.toArray());
        Values.writeValues(arg, ctx.consumer);
        return;
      }
    for (;;)
      {
	if (arg == endMarker)
	  break;
	if (arg instanceof Consumable)
	  ((Consumable) arg).consume(ctx.consumer);
	else
	  ctx.writeValue(arg);
	arg = ctx.getNextArg(endMarker);
      }
  }

  /*
  public Object applyN (Object[] args)  throws Throwable
  {
    if (args.length == 0)
      return Values.empty;
    Object arg0 = args[0];
    if (arg0 instanceof Procedure)
      {
	Object[] xargs = new Object[args.length-1];
	System.arraycopy(args, 1, xargs, 0, xargs.length);
	return ((Procedure) arg0).applyN(xargs);
      }
    return gnu.kawa.functions.AppendValues.appendValues.applyN(args);
  }
  */
}
