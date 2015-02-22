package gnu.text;
import java.text.FieldPosition;

public class RomanIntegerFormat extends java.text.NumberFormat
{
  private static RomanIntegerFormat newRoman;
  private static RomanIntegerFormat oldRoman;

  public boolean oldStyle;

  public RomanIntegerFormat(boolean oldStyle)
  {
    this.oldStyle = oldStyle;
  }

  public RomanIntegerFormat()
  {
  }

  public static RomanIntegerFormat getInstance (boolean oldStyle)
  {
    if (oldStyle)
      {
	if (oldRoman == null)
	  oldRoman = new RomanIntegerFormat(true);
	return oldRoman;
      }
    else
      {
	if (newRoman == null)
	  newRoman = new RomanIntegerFormat(false);
	return newRoman;
      }
  }

  static final String codes = "IVXLCDM";

  public static String format(int num, boolean oldStyle)
  {
    if (num <= 0 || num >= 4999)
      return Integer.toString(num);
    StringBuffer sbuf = new StringBuffer(20);
    int power = 3;
    int unit = 1000;
    for ( ;  power >= 0;  unit = unit / 10,  power--)
      {
	int digit = num / unit;
	num -= digit * unit;
	if (digit == 0)
	  continue;
	if (! oldStyle && (digit == 4 || digit == 9))
	  {
	    sbuf.append(codes.charAt(2 * power));
	    sbuf.append(codes.charAt(2 * power + (digit + 1) / 5));
	    continue;
	  }
	int rest = digit;
	if (rest >= 5)
	  {
	    sbuf.append(codes.charAt(2 * power + 1));
	    rest -= 5;
	  }
	while (--rest >= 0)
	  {
	    sbuf.append(codes.charAt(2 * power));
	  }
      }
    return sbuf.toString();
  }

  public static String format(int num)
  {
    return format(num, false);
  }

  public StringBuffer format(long num, StringBuffer sbuf, FieldPosition fpos)
  {
    String str;
    if (num > 0 && num < (oldStyle ? 4999 : 3999))
      str = format((int) num, oldStyle);
    else
      str = Long.toString(num);
    if (fpos != null)
      {
	// Kludge because FieldPosition fields not settable (in JDK 1.1)!
	long tval = 1;
	int len = str.length();
	for (int i = len;  --i > 0; )
	  tval = 10 * tval + 9;
	// tval now has same number of digits as str
	StringBuffer tbuf = new StringBuffer(len);
	new java.text.DecimalFormat("0").format(tval, tbuf, fpos);
      }
    sbuf.append(str);
    return sbuf;
  }

  public StringBuffer format(double num, StringBuffer sbuf, FieldPosition fpos)
  {
    long inum = (long) num;
    if ((double) inum == num)
      return format(inum, sbuf, fpos);
    sbuf.append(Double.toString(num));
    return sbuf;
  }

  public java.lang.Number parse(String text, java.text.ParsePosition status)
  {
    throw new Error("RomanIntegerFormat.parseObject - not implemented");
  }
}
