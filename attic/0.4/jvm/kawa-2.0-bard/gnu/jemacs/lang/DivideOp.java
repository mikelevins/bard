package gnu.jemacs.lang;
import gnu.math.*;
import gnu.mapping.*;

public class DivideOp extends ProcedureN
{
  public DivideOp(String name)
  {
    setName(name);
  }

  public static final DivideOp $Sl = new DivideOp("/");

  public static Object $Sl(Object arg1)
  {
    return $Sl(IntNum.one(), arg1);
  }

  public static Object $Sl(Object arg1, Object arg2)
  {
    Numeric num1 = ELisp.asNumber(arg1);
    Numeric num2 = ELisp.asNumber(arg2);
    if (num1 instanceof IntNum && num2 instanceof IntNum)
      return IntNum.quotient((IntNum) num1, (IntNum) num2, Numeric.TRUNCATE);
    return num1.div(num2);
  }

  public static Object $Sl$V (Object arg1, Object arg2,
			      Object arg3, Object[] rest)
  {
    return applyN($Sl($Sl(arg1, arg2), arg3), rest);
  }

  public static Object applyN(Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = $Sl(result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len <= 1)
      return $Sl(args[0]);
    Object result = args[0];
    for (int i = 1; i < len; i++)
      result = $Sl(result, args[i]);
    return result;
  }
}
