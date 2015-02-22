// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;
import gnu.math.*;
import gnu.bytecode.*;
import java.math.*;
import gnu.kawa.functions.Arithmetic;

/** A restriction (sub-range) of the integer type.
 * Implements built-in XML Schema types derived from {@code xs:integer}.
 */

public class XIntegerType extends XDataType
{
  /** The lower bound, inclusive. of the value range of this type.
   * If there is no lower bound then {@code minValue} is {@code null}. */
  public final IntNum minValue;
  /** The upper bound, inclusive. of the value range of this type.
   * If there is no upper bound then {@code maxValue} is {@code null}. */
  public final IntNum maxValue;

  static ClassType typeIntNum = ClassType.make("gnu.math.IntNum");

  boolean isUnsignedType;

  public boolean isUnsignedType ()
  {
    return isUnsignedType;
  }

  public static final XIntegerType integerType = 
    new XIntegerType ("integer", decimalType, INTEGER_TYPE_CODE,
                      null, null);
  public static final XIntegerType longType = 
    new XIntegerType("long", integerType, LONG_TYPE_CODE,
                     IntNum.make(Long.MIN_VALUE), IntNum.make(Long.MAX_VALUE));
  public static final XIntegerType intType = 
    new XIntegerType("int", longType, INT_TYPE_CODE,
                     IntNum.make(Integer.MIN_VALUE), 
                     IntNum.make(Integer.MAX_VALUE));
  public static final XIntegerType shortType = 
    new XIntegerType("short", intType, SHORT_TYPE_CODE,
                     IntNum.make(Short.MIN_VALUE), 
                     IntNum.make(Short.MAX_VALUE));
  public static final XIntegerType byteType = 
    new XIntegerType("byte", shortType, BYTE_TYPE_CODE,
                     IntNum.make(Byte.MIN_VALUE), 
                     IntNum.make(Byte.MAX_VALUE));
  public static final XIntegerType nonPositiveIntegerType =
    new XIntegerType("nonPositiveInteger", integerType,
                     NON_POSITIVE_INTEGER_TYPE_CODE,
                     null, IntNum.zero());
  public static final XIntegerType negativeIntegerType =
    new XIntegerType("negativeInteger", nonPositiveIntegerType,
                     NEGATIVE_INTEGER_TYPE_CODE,
                     null, IntNum.minusOne());
  public static final XIntegerType nonNegativeIntegerType =
    new XIntegerType("nonNegativeInteger", integerType,
                     NONNEGATIVE_INTEGER_TYPE_CODE,
                     IntNum.zero(), null);
  public static final XIntegerType unsignedLongType =
    new XIntegerType("unsignedLong", nonNegativeIntegerType,
                     UNSIGNED_LONG_TYPE_CODE,
                     IntNum.zero(), IntNum.valueOf("18446744073709551615"));
  public static final XIntegerType unsignedIntType =
    new XIntegerType("unsignedInt", unsignedLongType,
                     UNSIGNED_INT_TYPE_CODE,
                     IntNum.zero(), IntNum.make(4294967295L));
  public static final XIntegerType unsignedShortType =
    new XIntegerType("unsignedShort", unsignedIntType,
                     UNSIGNED_SHORT_TYPE_CODE,
                     IntNum.zero(), IntNum.make(65535));
  public static final XIntegerType unsignedByteType =
    new XIntegerType("unsignedByte", unsignedShortType,
                     UNSIGNED_BYTE_TYPE_CODE,
                     IntNum.zero(), IntNum.make(255));
  public static final XIntegerType positiveIntegerType =
    new XIntegerType("positiveInteger", nonNegativeIntegerType,
                     POSITIVE_INTEGER_TYPE_CODE,
                     IntNum.one(), null);

  public XIntegerType (String name, XDataType base, int typeCode,
                       IntNum min, IntNum max)
  {
    // FIXME Should convert NAME to xs:NAME.
    this((Object) name, base, typeCode, min, max);
    isUnsignedType = name.startsWith("unsigned");
  }

  public XIntegerType (Object name, XDataType base, int typeCode,
                       IntNum min, IntNum max)
  {
    super(name, typeIntNum, typeCode);
    minValue = min;
    maxValue = max;
    baseType = base;
  }

  public boolean isInstance (Object obj)
  {
    if (! (obj instanceof IntNum))
      return false;
    if (this == integerType)
      return true;
    XDataType objType
      = (obj instanceof XInteger ? ((XInteger) obj).getIntegerType()
         : integerType);
    while (objType != null)
      {
        if (objType == this)
          return true;
        objType = objType.baseType;
      }
    return false;
  }

 public Object coerceFromObject (Object obj)
  {
    IntNum ival = IntNum.asIntNumOrNull(obj);
    if (ival == null)
      throw new ClassCastException("cannot cast "+obj+" to "+name);
    return valueOf(ival);
  }

  public IntNum valueOf (IntNum value)
  {
    if (this != integerType)
      {
        if ((minValue != null && IntNum.compare(value, minValue) < 0)
            || (maxValue != null && IntNum.compare(value, maxValue) > 0))
          throw new ClassCastException("cannot cast "+value+" to "+name);
        return new XInteger(value, this);
      }
    return value;
  }

  public Object cast (Object value)
  {
    if (value instanceof Boolean)
      return valueOf(((Boolean)value).booleanValue() ? IntNum.one()
                     : IntNum.zero());
    if (value instanceof IntNum)
      return valueOf((IntNum) value);
    if (value instanceof BigDecimal)
      return valueOf(Arithmetic.asIntNum((BigDecimal) value));
    if (value instanceof RealNum)
      return valueOf(((RealNum) value).toExactInt(RealNum.TRUNCATE));
    if (RealNum.isReal(value))
      return valueOf(RealNum.toExactInt(((Number) value).doubleValue(), RealNum.TRUNCATE));
    return super.cast(value);
  }

  public Object valueOf (String value)
  {
    return valueOf(IntNum.valueOf(value.trim(), 10));
  }

  public IntNum valueOf (String value, int radix)
  {
    return valueOf(IntNum.valueOf(value.trim(), radix));
  }
}
