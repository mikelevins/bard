package gnu.ecmascript;
import gnu.mapping.*;

public class BinaryOp extends Procedure2
{
  int op;
  public BinaryOp(String name, int op)
  {
    setName(name);
    this.op = op;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if (op == Reserved.LESS_OP)
      {
	return Convert.toNumber(arg1) < Convert.toNumber(arg2)
	  ? Boolean.TRUE : Boolean.FALSE;
      }
    return new Double(apply(Convert.toNumber(arg1), Convert.toNumber(arg2)));
  }
  
  public double apply (double arg1, double arg2)
  {
    switch (op)
      {
      case Reserved.PLUS_OP:  return arg1+arg2;
      case Reserved.MINUS_OP: return arg1-arg2;
      case Reserved.TIMES_OP: return arg1*arg2;
      case Reserved.LSHIFT_OP: return (double)((int)arg1 << ((int)arg2 & 31));
      }
    return Double.NaN;
  }
}
