package gnu.mapping;

/**
 * Abstract class for 0- or 1-argument Scheme procedures.
 * Extensions must provide apply0 and apply1.
 * @author	Per Bothner
 */

public abstract class Procedure0or1 extends Procedure
{

  public Procedure0or1 ()
  {
    super();
  }

  public Procedure0or1 (String n)
  {
    super(n);
  }

  public int numArgs() { return 0x1000; }

  public abstract Object apply0 () throws Throwable;

  public abstract Object apply1 (Object arg1) throws Throwable;

  public Object apply2 (Object arg1,Object arg2)
  {
    throw new WrongArguments(this, 2);
  }

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
    if (args.length == 0)
      return apply0 ();
    else if (args.length == 1)
      return apply1 (args[0]);
    else
      throw new WrongArguments(this, args.length);
  }
}
