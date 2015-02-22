// Copyright (c) 2001, 2008  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.math.*;
import gnu.mapping.*;
import java.math.BigDecimal;
import gnu.kawa.functions.Arithmetic;
import gnu.kawa.xml.*;
import gnu.xml.TextUtils;

public class NumberValue extends Procedure1
{
  public static final NumberValue numberValue = new NumberValue();
 
  public static final Double NaN = new Double(Double.NaN);

  public static boolean isNaN (Object arg)
  {
    return ((arg instanceof Double || arg instanceof Float
             || arg instanceof DFloNum)
            && Double.isNaN(((Number) arg).doubleValue()));
  }

  public Object apply1(Object arg)
  {
    if (arg != Values.empty && arg != null)
      {
        try
          {
            return numberValue(arg);
          }
        catch (Exception ex)
          {
            // fall through to return NaN;
          }
      }
    return NaN;
  }

  public static Number numberCast (Object value)
  {
    if (value == Values.empty || value == null)
      return null;
    if (value instanceof Values)
      {
        Values vals = (Values) value;
        int ipos = vals.startPos();
        int count = 0;
        while ((ipos = vals.nextPos(ipos)) != 0)
          {
            if (count > 0)
              throw new ClassCastException("non-singleton sequence cast to number");
            value = vals.getPosPrevious(ipos);
            count++;
          }
      }
    if (value instanceof KNode || value instanceof UntypedAtomic)
      return (Double) XDataType.doubleType.valueOf(TextUtils.stringValue(value));
    return (Number) value;
  }

  public static Object numberValue (Object value)
  {
    value = KNode.atomicValue(value);
    double d;
    if (value instanceof UntypedAtomic || value instanceof String)
      {
        try
          {
            return XDataType.doubleType
              .valueOf(TextUtils.stringValue(value));
          }
        catch (Exception ex)
          {
            d = Double.NaN;
          }
      }
    else if (value instanceof Number
             && (value instanceof RealNum || ! (value instanceof Numeric)))
      d = (((Number) value).doubleValue());
    else
      d = Double.NaN;
    return XDataType.makeDouble(d);
  }

  public static Object abs (Object value)
  {
    if (value == null || value == Values.empty)
      return value;
    value = numberCast(value);
    if (value instanceof Double)
      {
        Double d = (Double) value;
        double x = d.doubleValue();
        long bits = Double.doubleToRawLongBits(x);
        if (bits >= 0)
          return d;
        bits &= 0x7fffffffffffffffL;
        x = Double.longBitsToDouble(bits);
        /* #ifdef JAVA5 */
        return Double.valueOf(x);
        /* #else */
        // return new Double(x);
        /* #endif */
      }
    if (value instanceof Float)
      {
        Float d = (Float) value;
        float x = d.floatValue();
        int bits = Float.floatToRawIntBits(x);
        if (bits >= 0)
          return d;
        bits &= 0x7fffffff;
        x = Float.intBitsToFloat(bits);
        /* #ifdef JAVA5 */
        return Float.valueOf(x) ;
        /* #else */
        // return new Float(x);
        /* #endif */
      }
    if (value instanceof BigDecimal)
      {
        BigDecimal dec = (BigDecimal) value;
        if (dec.signum() < 0)
          dec = dec.negate();
        return dec;
      }
    return ((Numeric) value).abs();
  }

  public static Object floor (Object val)
  {
    Number value = numberCast(val);
    if (value == null)
      return val;
    if (value instanceof Double)
      return XDataType.makeDouble(Math.floor(((Double) value).doubleValue()));
    if (value instanceof Float)
      return XDataType.makeFloat((float) Math.floor(((Float) value).floatValue()));
    if (value instanceof BigDecimal)
      {
        BigDecimal dec = (BigDecimal) value;
        return Arithmetic.asIntNum(dec.divide(XDataType.DECIMAL_ONE, 0, BigDecimal.ROUND_FLOOR).toBigInteger());
      }
    return ((RealNum) value).toInt(Numeric.FLOOR);
  }

  public static Object ceiling (Object val)
  {
    Number value = numberCast(val);
    if (value == null)
      return val;
    if (value instanceof Double)
      return XDataType.makeDouble(Math.ceil(((Double) value).doubleValue()));
    if (value instanceof Float)
      return XDataType.makeFloat((float) Math.ceil(((Float) value).floatValue()));
    if (value instanceof BigDecimal)
      {
        BigDecimal dec = (BigDecimal) value;
        return Arithmetic.asIntNum(dec.divide(XDataType.DECIMAL_ONE, 0, BigDecimal.ROUND_CEILING).toBigInteger());
      }
    return ((RealNum) value).toInt(Numeric.CEILING);
  }

  public static Object round (Object arg)
  {
    Number value = numberCast(arg);
    if (value == null)
      return arg;
    if (value instanceof Double)
      {
        double val = ((Double) value).doubleValue();
        if (val >= -0.5 && val <= 0.0
            && (val < 0.0 || Double.doubleToLongBits(val) < 0))
          val = -0.0;
        else
          val = Math.floor(val+0.5);
        return XDataType.makeDouble(val);
      }
    if (value instanceof Float)
      {
        float val = ((Float) value).floatValue();
        if (val >= -0.5 && val <= 0.0
            && (val < 0.0 || Float.floatToIntBits(val) < 0))
          val = (float) (-0.0);
        else
          val = (float) Math.floor(val+0.5);
        return XDataType.makeFloat(val);
      }
    if (value instanceof BigDecimal)
      {
        BigDecimal dec = (BigDecimal) value;
        int mode = dec.signum() >= 0 ? BigDecimal.ROUND_HALF_UP
          : BigDecimal.ROUND_HALF_DOWN;
        dec = dec.divide(XDataType.DECIMAL_ONE, 0, mode);
        return Arithmetic.asIntNum(dec.toBigInteger());
      }
    return ((RealNum) value).toInt(Numeric.ROUND);
  }

  public static Object roundHalfToEven (Object value, IntNum precision)
  {
    Number number = numberCast(value);
    if (number == null)
      return value;
    if (value instanceof Double || value instanceof Float)
      {
        double v = ((Number) value).doubleValue();
        if (v == 0 || Double.isInfinite(v) || Double.isNaN(v))
          return value;
      }
    BigDecimal dec = (BigDecimal) XDataType.decimalType.cast(number);
    int prec = precision.intValue();
    /* #ifndef JAVA5 */
    // if (prec < 0)
    //   {
    //     BigDecimal power = null;
    //     int shift = -prec;
    //     if (shift >= 6)
    //       {
    //         BigDecimal million = BigDecimal.valueOf(1000000);
    //         power = million;
    //         while ((shift -= 6) >= 6)
    //           power = power.multiply(million);
    //       }
    //     if (shift > 0)
    //       {
    //         int i = 10;
    //         while (--shift > 0)
    //           i = 10 * i;
    //         BigDecimal tens = BigDecimal.valueOf(i);
    //         power = power == null ? tens : power.multiply(tens);
    //       }
    //     dec = dec.divide(power, 0, BigDecimal.ROUND_HALF_EVEN);
    //     dec = dec.multiply(power);
    //   }
    // else
    /* #endif */
      dec = dec.setScale(prec, BigDecimal.ROUND_HALF_EVEN);
    if (number instanceof Double)
      return XDataType.makeDouble(dec.doubleValue());
    if (number instanceof Float)
      return XDataType.makeFloat(dec.floatValue());
    if (number instanceof IntNum)
      return XIntegerType.integerType.cast(dec);
    return dec;
  }

  public static Object roundHalfToEven (Object value)
  {
    return roundHalfToEven(value, IntNum.zero());
  }
}
