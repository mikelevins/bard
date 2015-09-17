package gnu.mapping;

/** A special case of Setter, retricted to no arguments, except the RHS. */

public class Setter0 extends Setter
{
  public Setter0(Procedure getter) { super(getter); }

  public int numArgs() { return 0x1001; }

  public Object apply1(Object result) throws Throwable
  { getter.set0(result);  return Values.empty; }

  public Object applyN(Object[] args) throws Throwable
  {
    int nargs = args.length;
    if (nargs != 1)
      throw new WrongArguments(this, nargs);
    getter.set0(args[0]);
    return Values.empty;
  }
}
