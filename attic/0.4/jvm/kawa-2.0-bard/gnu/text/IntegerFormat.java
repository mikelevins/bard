// Copyright (c) 2001, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import java.text.FieldPosition;
import java.io.Writer;

/** Handle formatting of integers.
 * Used to implement the Common Lisp ~D (Decimal), ~X (Hexadecimal),
 * ~O (Octal), ~B (Binary), and ~R (Radix) Common Lisp formats operators. */

public class IntegerFormat extends ReportFormat
{
  public int base;

  /** Minimal width of the result, including sign, commas, etc.
   * However, if the MIN_DIGITS flag is given, it's the minimum number
   * of digits instead.  This is used for printf-style "precision".  */
  public int minWidth;

  /** The padding characters, by default ' '. */
  public int padChar;

  public int commaChar;
  public int commaInterval;

  public int flags;
  /** Do groups (for example thousands, using commas). */
  public static final int SHOW_GROUPS = 1;

  /** If value is non-negative, emit a '+'. */
  public static final int SHOW_PLUS = 2;

  /** If value is non-negative, emit an initial ' '. */
  public static final int SHOW_SPACE = 4;

  /** Add "0x" (hex) or "0" (octal) prefix. */
  public static final int SHOW_BASE = 8;

  public static final int PAD_RIGHT = 16;

  public static final int UPPERCASE = 32;

  /** The minWidth is minimum number of digits, not minimum total width. */
  public static final int MIN_DIGITS = 64;

  public IntegerFormat ()
  {
    base = 10;
    minWidth = 1;
    padChar = (int) ' ';
    commaChar = (int) ',';
    commaInterval = 3;
    flags = 0;
  }

  public int format(Object[] args, int start, Appendable dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    return format((Object) args, start, dst, fpos);
  }

  public int format(Object arg, int start, Appendable dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    Object[] args = arg instanceof Object[] ? (Object[]) arg : null;
    int minWidth =  getParam(this.minWidth, 1, args, start);
    if (this.minWidth == PARAM_FROM_LIST)  start++;
    char padChar = getParam(this.padChar, ' ', args, start);
    if (this.padChar == PARAM_FROM_LIST)  start++;
    char commaChar = getParam(this.commaChar, ',', args, start);
    if (this.commaChar == PARAM_FROM_LIST)  start++;
    int commaInterval = getParam(this.commaInterval, 3, args,start);
    if (this.commaInterval == PARAM_FROM_LIST)  start++;
    boolean printCommas = (flags & SHOW_GROUPS) != 0;
    boolean padRight = (flags & PAD_RIGHT) != 0;
    boolean padInternal = padChar == '0';
    if (args != null)
      {
	if (start >= args.length)
	  {
	    dst.append("#<missing format argument>");
	    return start;
	  }
	arg = args[start];
      }
    String sarg = convertToIntegerString(arg, base);
    if (sarg != null)
      {
        char sarg0 = sarg.charAt(0);
	boolean neg = sarg0 == '-';
	int slen = sarg.length();
	int ndigits = neg ? slen - 1 : slen;
	int numCommas = printCommas ? (ndigits-1)/commaInterval : 0;
	int unpadded_len = ndigits + numCommas;
	if (neg || (flags & (SHOW_PLUS|SHOW_SPACE)) != 0)
	  unpadded_len++;

        if ((flags & SHOW_BASE) != 0)
          {
            if (base == 16)
              unpadded_len += 2;
            else if (base == 8 && sarg0 != '0')
              unpadded_len += 1;
          }
	if ((flags & MIN_DIGITS) != 0)
	  {
	    unpadded_len = ndigits;
	    if (slen == 1 && sarg0 == '0' && minWidth == 0)
	      slen = 0;
	  }
        if (! padRight && ! padInternal)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.append(padChar);
	int i = 0;
	if (neg)
	  {
	    dst.append('-');
	    i++;
	    slen--;
	  }
	else if ((flags & SHOW_PLUS) != 0)
	  dst.append('+');
	else if ((flags & SHOW_SPACE) != 0)
	  dst.append(' ');
        boolean uppercase = base > 10 && (flags & UPPERCASE) != 0;
        if ((flags & SHOW_BASE) != 0)
          {
            if (base == 16)
              {
                dst.append('0');
                dst.append(uppercase ? 'X' : 'x');
              }
            else if (base == 8 && sarg0 != '0')
              dst.append('0');
          }
        if (padInternal)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.append(padChar);
	for (;;)
	  {
	    if (slen == 0)
	      break;
            char ch = sarg.charAt(i++);
            if (uppercase)
              ch = Character.toUpperCase(ch);
	    dst.append(ch);
	    --slen;
	    if (printCommas && slen > 0 && (slen % commaInterval) == 0)
	      dst.append(commaChar);
	  }
        if (padRight)
          for (; minWidth > unpadded_len;  --minWidth)
            dst.append(padChar);
      }
    else
      dst.append(arg.toString());
    return start + 1;
  }

  public String convertToIntegerString(Object x, int radix)
  {
    if (! (x instanceof Number))
      return null;
    else if (x instanceof java.math.BigInteger)
      return ((java.math.BigInteger) x).toString(radix);
    else
      return Long.toString(((Number) x).longValue(), radix);
  }
}
