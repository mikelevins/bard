// Copyright (c) 2000, 2001, 2003, 2005, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.*;
import java.math.*;
import gnu.mapping.*;

/**
 * Implement the Scheme standard functions "+" and "-".
 * @author Per Bothner
 */

public class AddOp extends ArithOp
{
  int plusOrMinus = 1;

  public AddOp(String name, int plusOrMinus)
  {
    super(name, plusOrMinus > 0 ? ADD : SUB);
    this.plusOrMinus = plusOrMinus;
    String compiler = plusOrMinus > 0
      ? "gnu.kawa.functions.CompileArith:$Pl"
      : "gnu.kawa.functions.CompileArith:$Mn";
    Procedure.compilerKey.set(this, compiler);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileArith:validateApplyArithOp");
 }

  public static final AddOp $Pl = new AddOp("+", 1);
  public static final AddOp $Mn = new AddOp("-", -1);

  public static Object apply2(int plusOrMinus, Object arg1, Object arg2)
  {
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    /*
    if (code1 < 0 || code2 < 0)
    throw new ClasscastException(); // FIXME
    */
    int code = code1 < code2 ? code2 : code1;
    switch (code)
      {
      case Arithmetic.INT_CODE:
	int i1 = Arithmetic.asInt(arg1);
	int i2 = Arithmetic.asInt(arg2);
	return new Integer(plusOrMinus > 0 ? i1 + i2 : i1 - i2);
      case Arithmetic.LONG_CODE:
	long l1 = Arithmetic.asLong(arg1);
	long l2 = Arithmetic.asLong(arg2);
	return new Long(plusOrMinus > 0 ? l1 + l2 : l1 - l2);
      case Arithmetic.BIGINTEGER_CODE:
	BigInteger bi1 = Arithmetic.asBigInteger(arg1);
	BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	return plusOrMinus > 0 ? bi1.add(bi2) : bi1.subtract(bi2);
      case Arithmetic.INTNUM_CODE:
	return IntNum.add(Arithmetic.asIntNum(arg1), Arithmetic.asIntNum(arg2),
			  plusOrMinus);
      case Arithmetic.BIGDECIMAL_CODE:
	BigDecimal bd1 = Arithmetic.asBigDecimal(arg1);
	BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	return plusOrMinus > 0 ? bd1.add(bd2) : bd1.subtract(bd2);
      case Arithmetic.RATNUM_CODE:
	return RatNum.add(Arithmetic.asRatNum(arg1), Arithmetic.asRatNum(arg2),
			  plusOrMinus);
      case Arithmetic.FLOAT_CODE:
	float f1 = Arithmetic.asFloat(arg1);
	float f2 = Arithmetic.asFloat(arg2);
	return new Float(plusOrMinus > 0 ? f1 + f2 : f1 - f2);
      case Arithmetic.DOUBLE_CODE:
	double d1 = Arithmetic.asDouble(arg1);
	double d2 = Arithmetic.asDouble(arg2);
	return new Double(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      case Arithmetic.FLONUM_CODE:
	d1 = Arithmetic.asDouble(arg1);
	d2 = Arithmetic.asDouble(arg2);
	return new DFloNum(plusOrMinus > 0 ? d1 + d2 : d1 - d2);
      default:
	Numeric num1 = Arithmetic.asNumeric(arg1);
	Numeric num2 = Arithmetic.asNumeric(arg2);
	return num1.add(num2, plusOrMinus);
      }
  }

  public static Object $Pl(Object arg1, Object arg2)
  {
    return apply2(1, arg1, arg2);
  }

  public static Object $Mn(Object arg1, Object arg2)
  {
    return apply2(-1, arg1, arg2);
  }

  public static Object $Mn(Object arg1)
  {
    int code = Arithmetic.classifyValue(arg1);
    switch (code)
      {
      case Arithmetic.INT_CODE:
	return new Integer(- Arithmetic.asInt(arg1));
      case Arithmetic.LONG_CODE:
	return new Long(- Arithmetic.asLong(arg1));
      case Arithmetic.BIGINTEGER_CODE:
	return Arithmetic.asBigInteger(arg1).negate();
      case Arithmetic.INTNUM_CODE:
	return IntNum.neg(Arithmetic.asIntNum(arg1));
      case Arithmetic.BIGDECIMAL_CODE:
	return Arithmetic.asBigDecimal(arg1).negate();
      case Arithmetic.RATNUM_CODE:
	return RatNum.neg(Arithmetic.asRatNum(arg1));
      case Arithmetic.FLOAT_CODE:
	return new Float(- Arithmetic.asFloat(arg1));
      case Arithmetic.DOUBLE_CODE:
	return new Double(- Arithmetic.asDouble(arg1));
      case Arithmetic.FLONUM_CODE:
	return new DFloNum(- Arithmetic.asDouble(arg1));
      default:
        return Arithmetic.asNumeric(arg1).neg();
      }

  }

  public static Object $Pl$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(1, apply2(1,apply2(1, arg1, arg2), arg3), rest);
  }

  public static Object $Mn$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return applyN(-1, apply2(-1,apply2(-1, arg1, arg2), arg3), rest);
  }

  public static Object applyN(int plusOrMinus, Object[] args)
  {
    int len = args.length;
    if (len == 0)
      return IntNum.zero ();
    Object result = args[0];
    if (len == 1 && plusOrMinus < 0)
      return $Mn(result);
    for (int i = 1; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public static Object applyN(int plusOrMinus, Object init, Object[] args)
  {
    int len = args.length;
    Object result = init;
    for (int i = 0; i < len; i++)
      result = apply2(plusOrMinus, result, args[i]);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return applyN(plusOrMinus, args);
  }
}
