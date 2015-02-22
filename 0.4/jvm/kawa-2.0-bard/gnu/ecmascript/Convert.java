package gnu.ecmascript;

public class Convert
{
  public static double toNumber(Object x)
  {
    if (x instanceof java.lang.Number)
      return ((java.lang.Number)x).doubleValue();
    //if (x == ECMAScript.UNDEFINED) return Double.NaN;
    // if (x == ECMAScript.NULL) return 0;
    if (x instanceof Boolean)
      return ((Boolean)x).booleanValue() ? 1 : 0;
    if (x instanceof String)
      {
	try
	  {
	    // FIXME - is Java grammar correct for ECMAScript?
	    return Double.valueOf((String)x).doubleValue();
	  }
	catch (NumberFormatException ex)
	  {
	    return Double.NaN;
	  }
      }
    // if (x instanceof JSObject)  { FIXME } 
    return Double.NaN;
  }

  public static double toInteger(double x)
  {
    if (Double.isNaN(x))
      return 0.0;
    return x < 0.0 ? Math.ceil (x) : Math.floor (x);
  }

  public static double toInteger(Object x)
  {
    return toInteger(toNumber(x));
  }

  public int toInt32 (double x)
  {
    if (Double.isNaN(x) || Double.isInfinite(x))
      return 0;
    // FIXME - does not handle overflow correctly!
    return (int) x;
  }

  public int toInt32 (Object x)
  {
    return toInt32(toNumber(x));
  }
}
