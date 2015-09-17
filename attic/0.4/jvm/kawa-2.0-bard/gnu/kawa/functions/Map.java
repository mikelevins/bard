package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.SlotGet;

/** Implement the Scheme standard functions "map" and "for-each".
 * @author Per Bothner
 */

public class Map extends gnu.mapping.ProcedureN
{
  /** True if we should collect the result into a list. */
  boolean collect;

  final ApplyToArgs applyToArgs;
  final IsEq isEq;

  public Map (boolean collect, ApplyToArgs applyToArgs, IsEq isEq)
  {
    super (collect ? "map" : "for-each");
    this.collect = collect;
    this.applyToArgs = applyToArgs;
    this.isEq = isEq;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileMisc:validateApplyMap");
  }

  /** An optimized single-list version of map. */
  static public Object map1 (Procedure proc, Object list) throws Throwable
  {
    Object result = LList.Empty;
    Pair last = null;
    while (list != LList.Empty)
      {
	Pair pair = (Pair) list;
	Pair new_pair = new Pair (proc.apply1(pair.getCar()), LList.Empty);
	if (last == null)
	  result = new_pair;
	else
	  last.setCdr(new_pair);
	last = new_pair;
	list = pair.getCdr();
      }
    return result;
  }

  /** An optimized single-list version of for-each. */
  static public void forEach1 (Procedure proc, Object list) throws Throwable
  {
    while (list != LList.Empty)
      {
	Pair pair = (Pair) list;
	proc.apply1(pair.getCar());
	list = pair.getCdr();
      }
  }

  public Object apply2 (Object arg1, Object arg2) throws Throwable
  {
    if (arg1 instanceof Procedure)
      {
        Procedure proc = (Procedure) arg1;
        if (collect)
          return map1 (proc, arg2);
        forEach1 (proc, arg2);
        return Values.empty;
      }
    return applyN(new Object[] { arg1, arg2 });
  }

  public Object applyN (Object[] args) throws Throwable
  {
    int arity = args.length - 1;
    if (arity == 1 && args[0] instanceof Procedure)
      {
        Procedure proc = (Procedure) (args[0]);
	if (collect)
	  return map1 (proc, args[1]);
	forEach1 (proc, args[1]);
	return Values.empty;
      }
    Object result;
    Pair last = null;
    if (collect)
      result = LList.Empty;
    else
      result = Values.empty;;
    Object[] rest = new Object [arity];
    System.arraycopy (args, 1, rest, 0, arity);
    Procedure proc;
    int need_apply;
    Object[] each_args;
    if (args[0] instanceof Procedure)
      {
        need_apply = 0;
        each_args = new Object[arity];
        proc = (Procedure) args[0];
      }
    else
      {
        need_apply = 1;
        each_args = new Object[arity+1];
        each_args[0] = args[0];
        proc = applyToArgs;
      }
    for (;;)
      {
	for (int i = 0;  i < arity;  i++)
	  {
	    Object list = rest[i];
	    if (list == LList.Empty)
	      return result;
	    Pair pair = (Pair) list;
	    each_args[need_apply+i] = pair.getCar();
	    rest[i] = pair.getCdr();
	  }
	Object value = proc.applyN (each_args);
	if (collect)
	  {
	    Pair new_pair = new Pair (value, LList.Empty);
	    if (last == null)
	      result = new_pair;
	    else
	      last.setCdr(new_pair);
	    last = new_pair;
	  }
      }
  }

  public int numArgs() { return (-1 << 12) | 2; }
}
