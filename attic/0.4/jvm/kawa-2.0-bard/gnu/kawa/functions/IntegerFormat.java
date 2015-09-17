// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.text.EnglishIntegerFormat;
import gnu.text.RomanIntegerFormat;
import gnu.math.*;

public class IntegerFormat extends gnu.text.IntegerFormat
{

  public IntegerFormat ()
  {
  }

  private static IntegerFormat plainDecimalFormat;

  public static IntegerFormat getInstance()
  {
    if (plainDecimalFormat == null)
      plainDecimalFormat = new IntegerFormat();
    return plainDecimalFormat;
  }

  public static java.text.Format
  getInstance (int base, int minWidth, int padChar,
	       int commaChar, int commaInterval, int flags)
  {
    if (base == PARAM_UNSPECIFIED)
      {
	if (padChar == PARAM_UNSPECIFIED
	    && padChar == PARAM_UNSPECIFIED
	    && commaChar == PARAM_UNSPECIFIED
	    && commaInterval == PARAM_UNSPECIFIED)
	  {
            // Common Lisp ~R format:
            boolean seenColon = (flags&SHOW_GROUPS) != 0;
	    if ((flags & SHOW_PLUS) != 0)
	      return RomanIntegerFormat.getInstance(seenColon);
	    else
	      return EnglishIntegerFormat.getInstance(seenColon);
	  }
	base = 10;
      }
    if (minWidth == PARAM_UNSPECIFIED)  minWidth = 1;
    if (padChar == PARAM_UNSPECIFIED)  padChar = ' ';
    if (commaChar == PARAM_UNSPECIFIED)  commaChar = ',';
    if (commaInterval == PARAM_UNSPECIFIED)  commaInterval = 3;
    if (base == 10 && minWidth == 1 && padChar == ' '
	&& commaChar == ',' && commaInterval == 3
	&& flags == 0)
      return getInstance();
    IntegerFormat fmt = new IntegerFormat();
    fmt.base = base;
    fmt.minWidth = minWidth;
    fmt.padChar = padChar;
    fmt.commaChar = commaChar;
    fmt.commaInterval = commaInterval;
    fmt.flags = flags;
    return fmt;
  }

  public String convertToIntegerString(Object arg, int radix)
  {
    if (arg instanceof RealNum)
      return ((RealNum) arg).toExactInt(Numeric.ROUND).toString(radix);
    else
      return super.convertToIntegerString(arg, radix);
  }
}
