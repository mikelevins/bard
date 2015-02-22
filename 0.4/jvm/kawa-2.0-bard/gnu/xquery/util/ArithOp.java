package gnu.xquery.util;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.xml.TextUtils;
import gnu.kawa.xml.*;
import gnu.kawa.functions.*;
import java.math.*;
import gnu.math.*;

public class ArithOp extends Procedure1or2
{
  char op;

  static final BigInteger TEN = BigInteger.valueOf(10);

  public static final ArithOp add = new ArithOp("+", '+', 2);
  public static final ArithOp sub = new ArithOp("-", '-', 2);
  public static final ArithOp mul = new ArithOp("*", '*', 2);
  public static final ArithOp div = new ArithOp("div", 'd', 2);
  public static final ArithOp idiv = new ArithOp("idiv", 'i', 2);
  public static final ArithOp mod = new ArithOp("mod", 'm', 2);
  public static final ArithOp plus = new ArithOp("+", 'P', 1);
  public static final ArithOp minus = new ArithOp("-", 'M', 1);

  ArithOp (String name, char op, int nargs)
  {
    super(name);
    setProperty(Procedure.validateApplyKey,
                "gnu.xquery.util.CompileMisc:validateArithOp");
    this.op = op;
  }

  public Object apply1 (Object arg1)
    throws java.lang.Throwable
  {
    if (arg1 == Values.empty || arg1 == null)
      return arg1;
    if (arg1 instanceof KNode || arg1 instanceof UntypedAtomic)
      arg1 = XDataType.doubleType.valueOf(TextUtils.stringValue(arg1));
    switch (op)
      {
      case 'P':
        return AddOp.apply2(1, gnu.math.IntNum.zero(), arg1);
      case 'M':
        int code1 = Arithmetic.classifyValue(arg1);
        switch (code1)
          {
          case Arithmetic.FLOAT_CODE:
            return XDataType.makeFloat(- Arithmetic.asFloat(arg1));
          case Arithmetic.DOUBLE_CODE:
            return XDataType.makeDouble(- Arithmetic.asDouble(arg1));
          default:
            if (arg1 instanceof Numeric)
              return ((Numeric) arg1).neg();
            return AddOp.apply2(-1, gnu.math.IntNum.zero(), arg1);
          }
      }
    throw new UnsupportedOperationException(getName());
  }

  public static BigDecimal div (BigDecimal d1, BigDecimal d2)
  {
    /* #ifdef JAVA5 */
    return d1.divide(d2, MathContext.DECIMAL128);
    /* #else */
    // BigDecimal d = d1.divide(d2, 18, BigDecimal.ROUND_HALF_EVEN);
    /* #ifdef JAVA2 */
    // BigInteger unscaled = d.unscaledValue();
    // if (unscaled.signum() == 0)
    //   return BigDecimal.valueOf(0);
    // int sc = 0;
    // while (sc < 18)
    //   {
    //     BigInteger[] divmod = unscaled.divideAndRemainder(TEN);
    //     if (divmod[1].signum() != 0)
    //       break;
    //     sc++;
    //     unscaled = divmod[0];
    //   }
    // return sc == 0 ? d : new BigDecimal(unscaled, d.scale() - sc);
    /* #endif */
    /* #endif */
  }

  public Object apply2 (Object arg1, Object arg2)
    throws java.lang.Throwable
  {
    if (arg1 == Values.empty || arg1 == null)
      return arg1;
    if (arg2 == Values.empty || arg2 == null)
      return arg2;
    if (arg1 instanceof KNode || arg1 instanceof UntypedAtomic)
      arg1 = XDataType.doubleType.valueOf(TextUtils.stringValue(arg1));
    if (arg2 instanceof KNode || arg2 instanceof UntypedAtomic)
      arg2 = XDataType.doubleType.valueOf(TextUtils.stringValue(arg2));
    switch (op)
      {
      case '+':
        return AddOp.apply2(1, arg1, arg2);
      case '-':
        return AddOp.apply2(-1, arg1, arg2);
      case '*':
        return MultiplyOp.$St.apply2(arg1, arg2);
      }
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    int code = code1 < code2 ? code2 : code1;
    switch (op)
      {
      case 'd': // 'div'
        if (code1 < 0 || code2 < 0)
          break;
        else if (code <= Arithmetic.RATNUM_CODE)
          {
            BigDecimal d1 = (BigDecimal) XDataType.decimalType.cast(arg1);
            // OR: d1 = Arithmetic.asBigDecimal(arg1);
            BigDecimal d2 = (BigDecimal) XDataType.decimalType.cast(arg2);
            // OR: d2 = Arithmetic.asBigDecimal(arg2);
            return div(d1, d2);
          }
        else if (code == Arithmetic.FLOAT_CODE)
          {
            return new Float(((Number) arg1).floatValue()
                             / ((Number) arg2).floatValue());
          }
        else if (code == Arithmetic.DOUBLE_CODE)
          {
            return new Double(((Number) arg1).doubleValue()
                             / ((Number) arg2).doubleValue());
          }
        else if (arg1 instanceof Duration && arg2 instanceof Duration)
          {
            Duration dur1 = (Duration) arg1;
            Duration dur2 = (Duration) arg2;
            if (dur1.unit() == Unit.second && dur2.unit() == Unit.second)
              {
                long s1 = dur1.getTotalSeconds();
                long s2 = dur2.getTotalSeconds();
                int n1 = dur1.getNanoSecondsOnly();
                int n2 = dur2.getNanoSecondsOnly();
                BigDecimal sec1 = TimeUtils.secondsBigDecimalFromDuration(s1, n1);
                BigDecimal sec2 = TimeUtils.secondsBigDecimalFromDuration(s2, n2);
                return div(sec1, sec2);
              }
            if (dur1.unit() == Unit.month && dur2.unit() == Unit.month)
              {
                BigDecimal m1 = BigDecimal.valueOf(dur1.getTotalMonths());
                BigDecimal m2 = BigDecimal.valueOf(dur2.getTotalMonths());
                return div(m1, m2);
              }
            throw new ArithmeticException("divide of incompatible durations");
          }
        else if (code >= 0)
          return Arithmetic.asNumeric(arg1).div(Arithmetic.asNumeric(arg2));
      case 'i': // 'idiv'
        if (code1 < 0 || code2 < 0)
          break;
        else if (code <= Arithmetic.INTNUM_CODE)
          {
            IntNum i1 = Arithmetic.asIntNum(arg1);
            IntNum i2 = Arithmetic.asIntNum(arg2);
            return IntNum.quotient(i1, i2);
          }
        else if (code <= Arithmetic.RATNUM_CODE)
          {
            BigDecimal d1 = (BigDecimal) XDataType.decimalType.cast(arg1);
            // OR: d1 = Arithmetic.asBigDecimal(arg1);
            BigDecimal d2 = (BigDecimal) XDataType.decimalType.cast(arg2);
            // OR: d2 = Arithmetic.asBigDecimal(arg2);
            return Arithmetic.asIntNum(d1.divide(d2, 0,
                                                 BigDecimal.ROUND_DOWN));
          }
        else if (code <= Arithmetic.FLOAT_CODE)
          {
            float f
              = ((Number) arg1).floatValue() / ((Number) arg2).floatValue();
            return RealNum.toExactInt((double) f, RealNum.TRUNCATE); 
          }
        else
          {
            double d
              = ((Number) arg1).doubleValue() / ((Number) arg2).doubleValue();
            return RealNum.toExactInt(d, RealNum.TRUNCATE); 
          }
      case 'm': // 'mod'
        if (code1 < 0 || code2 < 0)
          break;
        else if (code <= Arithmetic.INTNUM_CODE)
          {
            IntNum i1 = Arithmetic.asIntNum(arg1);
            IntNum i2 = Arithmetic.asIntNum(arg2);
            return IntNum.remainder(i1, i2);
          }
        else if (code <= Arithmetic.RATNUM_CODE)
          {
            return sub.apply2(arg1, mul.apply2(idiv.apply2(arg1, arg2), arg2));
          }
        else if (code <= Arithmetic.FLOAT_CODE)
          {
            float f1 = Arithmetic.asFloat(arg1);
            float f2 = Arithmetic.asFloat(arg2);
            return gnu.kawa.xml.XDataType.makeFloat(f1 % f2);
          }
        else if (code <= Arithmetic.FLONUM_CODE)
          {
            double d1 = Arithmetic.asDouble(arg1);
            double d2 = Arithmetic.asDouble(arg2);
            double d = d1 % d2;
            if (code == Arithmetic.FLONUM_CODE)
              return DFloNum.make(d);
            else
              return gnu.kawa.xml.XDataType.makeDouble(d);
          }
      }
    throw new UnsupportedOperationException(getName());
  }
}
