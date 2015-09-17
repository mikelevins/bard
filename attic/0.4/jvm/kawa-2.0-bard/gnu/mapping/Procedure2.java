package gnu.mapping;

/**
 * Abstract class for 2-argument Scheme procedures.
 * Extensions must provide apply2.
 * @author	Per Bothner
 */

public abstract class Procedure2 extends Procedure
{

  public Procedure2(java.lang.String n)
  {
    super(n);
  }
  public Procedure2()
  {
    super();
  }

  public int numArgs() { return 0x2002; }

  public Object apply0 () throws Throwable
  {
    throw new WrongArguments(this.getName(), 2, "(?)");
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    throw new WrongArguments(this, 1);
  }

  public abstract Object apply2 (Object arg1,Object arg2) throws Throwable;

  public Object apply3 (Object arg1, Object arg2, Object arg3)
  {
    throw new WrongArguments(this, 3);
  }

  public Object apply4 (Object arg1, Object arg2, Object arg3, Object arg4)
     throws Throwable
  {
    throw new WrongArguments(this, 4);
  }

  public Object applyN (Object[] args) throws Throwable
  {
    if (args.length != 2)
      throw new WrongArguments(this, args.length);
    return apply2 (args[0], args[1]);
  }
}
