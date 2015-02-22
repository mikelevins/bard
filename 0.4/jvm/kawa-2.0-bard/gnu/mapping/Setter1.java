package gnu.mapping;

/** A special case of Setter, retricted to one argument (plus the RHS). */

public class Setter1 extends Setter
{
  public Setter1(Procedure getter) { super(getter); }

  public int numArgs() { return 0x2002; }

  public Object apply2(Object arg, Object value) throws Throwable
  { getter.set1(arg, value);  return Values.empty; }

  public Object applyN(Object[] args) throws Throwable
  {
    int nargs = args.length;
    if (nargs != 2)
      throw new WrongArguments(this, nargs);
    getter.set1(args[0], args[1]);
    return Values.empty;
  }
}
