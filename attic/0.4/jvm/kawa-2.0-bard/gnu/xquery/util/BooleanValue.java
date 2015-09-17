// Copyright (c) 2002, 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.Numeric;
import gnu.math.RealNum;
import gnu.kawa.xml.UntypedAtomic;
import gnu.kawa.xml.XDataType;

public class BooleanValue extends Procedure1
{
  public static final BooleanValue booleanValue =
    new BooleanValue("boolean-value");

  public BooleanValue (String name)
  {
    super(name);
    setProperty(Procedure.validateApplyKey,
                "gnu.xquery.util.CompileMisc:validateBooleanValue");
  }

  public static boolean booleanValue(Object value)
  {
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue();
    if (value instanceof Number
        && (value instanceof RealNum || ! (value instanceof Numeric)))
      {
        double d = ((Number) value).doubleValue();
        return d != 0 && ! Double.isNaN(d);
      }
    if (value instanceof SeqPosition)
      return true;
    if (value instanceof String
        || value instanceof gnu.kawa.io.Path
        || value instanceof UntypedAtomic)
      return value.toString().length() > 0;
    if (value instanceof Values)
      {
	Values values = (Values) value;
        int sz = values.size();
	if (sz == 0)
	  return false;
	Object value1 = values.get(0);
        if (sz == 1)
	  return booleanValue(value1);
        if (value1 instanceof SeqPosition)
          return true;
      }
    throw new WrongType("fn:boolean", 1, value, "boolean-convertible-value");
  }

  public static boolean not(Object value)
  {
    return ! booleanValue(value);
  }

  public Object apply1(Object arg)
  {
    return booleanValue(arg) ? Boolean.TRUE : Boolean.FALSE;
  }

}
