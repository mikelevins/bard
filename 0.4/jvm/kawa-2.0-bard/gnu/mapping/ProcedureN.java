package gnu.mapping;

/**
 * Abstract class for "N-argument" Scheme procedures, where N>4 or variable.
 * @author	Per Bothner
 */

public abstract class ProcedureN extends Procedure
{
  public ProcedureN ()
  {
    super();
  }

  public ProcedureN (String n)
  {
      super(n);
  }

  public static final Object[] noArgs = new Object[0];

  public Object apply0 () throws Throwable
  {
    return applyN(noArgs);
  }

  public Object apply1 (Object arg1) throws Throwable
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return applyN (args);
  }

   public Object apply2 (Object arg1,Object arg2) throws Throwable
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return applyN (args);
  }

  public Object apply3 (Object arg1, Object arg2, Object arg3) throws Throwable
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return applyN (args);
  }

  public Object apply4 (Object arg1, Object arg2,
			Object arg3, Object arg4)  throws Throwable
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return applyN (args);
  }

  public abstract Object applyN (Object[] args) throws Throwable;
}
