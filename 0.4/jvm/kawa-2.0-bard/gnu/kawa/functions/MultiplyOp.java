// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard function "*".
 * @author Per Bothner
 */

public class MultiplyOp extends ArithOp
{
  public static final MultiplyOp $St = new MultiplyOp("*");

  public MultiplyOp(String name)
  {
    super(name, MUL);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileArith:validateApplyArithOp");
    Procedure.compilerKey.set(this, "*gnu.kawa.functions.CompileArith:forMul");
  }

  public Object defaultResult ()
  {
    return IntNum.one();
  }

  public static Object apply (Object arg1, Object arg2)
  {
    return ((Numeric) arg1).mul(arg2);
  }

  public Object applyN (Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.one ();
    Number result = (Number) Promise.force(args[0]);
    int code = Arithmetic.classifyValue(result);
    for (int i = 1; i < len; i++)
      {
	Object arg2 = args[i];
	int code2 = Arithmetic.classifyValue(arg2);
	code = code < code2 ? code2 : code;

	switch (code)
	  {
	  case Arithmetic.INT_CODE:
	    int i1 = Arithmetic.asInt(result);
	    int i2 = Arithmetic.asInt(arg2);
	    result = new Integer(i1 * i2);
	    break;
	  case Arithmetic.LONG_CODE:
	    long l1 = Arithmetic.asLong(result);
	    long l2 = Arithmetic.asLong(arg2);
	    result = new Long(l1 * l2);
	    break;
	  case Arithmetic.BIGINTEGER_CODE:
	    BigInteger bi1 = Arithmetic.asBigInteger(result);
	    BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	    result = bi1.multiply(bi2);
	    break;
	  case Arithmetic.INTNUM_CODE:
	    result = IntNum.times(Arithmetic.asIntNum(result),
				  Arithmetic.asIntNum(arg2));
	    break;
	  case Arithmetic.BIGDECIMAL_CODE:
	    BigDecimal bd1 = Arithmetic.asBigDecimal(result);
	    BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	    result = bd1.multiply(bd2);
	    break;
	  case Arithmetic.RATNUM_CODE:
	    result = RatNum.times(Arithmetic.asRatNum(result),
				  Arithmetic.asRatNum(arg2));
	    break;
	  case Arithmetic.FLOAT_CODE:
	    float f1 = Arithmetic.asFloat(result);
	    float f2 = Arithmetic.asFloat(arg2);
	    result = new Float(f1 * f2);
	    break;
	  case Arithmetic.DOUBLE_CODE:
	    double d1 = Arithmetic.asDouble(result);
	    double d2 = Arithmetic.asDouble(arg2);
	    result = new Double(d1 * d2);
	    break;
	  case Arithmetic.FLONUM_CODE:
	    d1 = Arithmetic.asDouble(result);
	    d2 = Arithmetic.asDouble(arg2);
	    result = new DFloNum(d1 * d2);
	    break;
	  default:
	    result = Arithmetic.asNumeric(result)
	      .mul(Arithmetic.asNumeric(arg2));
	  }
      }
    return result;
   }

}
