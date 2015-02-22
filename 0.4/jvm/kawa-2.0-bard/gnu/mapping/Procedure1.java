package gnu.mapping;

/**
 * Abstract class for 1-argument Scheme procedures.
 * @author	Per Bothner
 */

public abstract class Procedure1 extends Procedure
{
  public Procedure1 ()
  {
    super();
  }

  public Procedure1(java.lang.String n)
  {
    super(n);
  }

  public int numArgs() { return 0x1001; }

  public Object apply0 () throws Throwable
  {
    throw new WrongArguments(this, 0);
  }

  public abstract Object apply1 (Object arg1) throws Throwable;

  public Object apply2 (Object arg1,Object arg2) throws Throwable
  {
    throw new WrongArguments(this, 2);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable
  {
    throw new WrongArguments(this, 3);
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4) throws Throwable
  { 
    throw new WrongArguments(this, 4);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length != 1)
      throw new WrongArguments(this, args.length);
    return apply1 (args[0]);
  }
}
