package gnu.mapping;

/**
 * Abstract class for 1- or 2-argument Scheme procedures.
 * Extensions must provide apply1 and apply2.
 * @author	Per Bothner
 */

public abstract class Procedure1or2 extends Procedure
{

  public Procedure1or2 ()
  {
    super();
  }

  public Procedure1or2 (String n)
  {
    super(n);
  }

  public int numArgs() { return 0x2001; }

  public Object apply0 ()
  {
    throw new WrongArguments(this, 0);
  }

  public abstract Object apply1 (Object arg1) throws Throwable;

  public abstract Object apply2 (Object arg1,Object arg2) throws Throwable;

  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    throw new WrongArguments(this, 3);
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
  {
    throw new WrongArguments(this, 4);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length == 1)
      return apply1 (args[0]);
    else if (args.length == 2)
      return apply2 (args[0], args[1]);
    else
      throw new WrongArguments(this, args.length);
  }
}
