// Copyright (c) 1999, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.text.FieldPosition;

// Can't use NumberFormat, because its format(Object, StringBuffer,
// FieldPosition) method is final - and does the wrong thing.
// (It ends up converting gnu.math number types to long!)

/** Format a real number using a fixed-point format.
 * Used for Common Lisp specs ~F and ~$;  also C-style %f.
 */

public class FixedRealFormat extends java.text.Format
{
  private int i, d;
  public int getMaximumFractionDigits() { return d; }
  public int getMinimumIntegerDigits() { return i; }
  public void setMaximumFractionDigits(int d) { this.d = d; }
  public void setMinimumIntegerDigits(int i) { this.i = i; }

  // These should not be public.  FIXME. 
  public int width;
  public int scale;
  public char padChar;
  public boolean showPlus;
  public boolean internalPad;
  public char overflowChar;

  public void format(RealNum number, StringBuffer sbuf, FieldPosition fpos)
  {
    int decimals;
    if (number instanceof RatNum
        && (decimals = getMaximumFractionDigits()) >= 0)
      {
        RatNum ratnum = (RatNum) number;
        boolean negative = ratnum.isNegative();
        if (negative)
          ratnum = ratnum.rneg();
        int oldSize = sbuf.length();
        int signLen = 1;
        if (negative)
          sbuf.append('-');
        else if (showPlus)
          sbuf.append('+');
        else
          signLen = 0;
        String string = RealNum.toScaledInt(ratnum, decimals+scale)
          .toString();
        sbuf.append(string);
        int length = string.length();
        int digits = length - decimals;
        format(sbuf, fpos, length, digits, decimals, signLen, oldSize);
      }
    else
      format(number.doubleValue(), sbuf, fpos);
  }

  public StringBuffer format(long num, StringBuffer sbuf, FieldPosition fpos)
  {
    format(IntNum.make(num), sbuf, fpos);
    return sbuf;
  }

  public StringBuffer format(double num, StringBuffer sbuf, FieldPosition fpos)
  {
    if (Double.isNaN(num) || Double.isInfinite(num))
      return sbuf.append(num);
    if (getMaximumFractionDigits() >= 0)
      format(DFloNum.toExact(num), sbuf, fpos);
    else
      {
        boolean negative;
        if (num < 0)
          {
            negative = true;
            num = -num;
          }
        else
          negative = false;
        int oldSize = sbuf.length();
        int signLen = 1;
        if (negative)
          sbuf.append('-');
        else if (showPlus)
          sbuf.append('+');
        else
          signLen = 0;

        String string = Double.toString(num);
        int cur_scale = scale;
        int seenE = string.indexOf('E');
        if (seenE >= 0)
          {
            int expStart = seenE+1;
            if (string.charAt(expStart) == '+')
              expStart++;
            cur_scale += Integer.parseInt(string.substring(expStart));
            string = string.substring(0, seenE);
          }
        int seenDot = string.indexOf('.');
        int length = string.length();
        if (seenDot >= 0) // Should always be true.
          {
            cur_scale -= length - seenDot - 1;
            length--;
            string = string.substring(0, seenDot) + string.substring(seenDot+1);
          }

	int i = string.length();

        // Skip leading zeros.  May happen if scale != 0.
        int initial_zeros = 0;
        while (initial_zeros < i - 1 && string.charAt(initial_zeros) == '0')
          initial_zeros++;
        if (initial_zeros > 0)
          {
            string = string.substring(initial_zeros);
            i -= initial_zeros;
          }

        int decimals;
        int digits = i + cur_scale;
        if (width > 0)
          {
            // Add zeros after the decimal point.  This could be made implicit,
            // but that would complicate rounding to fit in the field width.
            while (digits < 0)
              {
                sbuf.append('0');
                digits++;
                i++;
              }
            decimals = width - signLen - 1 - digits;
          }
	else
	  decimals = (i > 16 ? 16 : i) - digits;
	if (decimals < 0)
	  decimals = 0;
	sbuf.append(string);
        while (cur_scale > 0)
          {
            sbuf.append('0');
            cur_scale--;
            i++;
          }
	int digStart = oldSize + signLen;
	int digEnd = digStart + digits + decimals;
	i = sbuf.length();
	char nextDigit;
	if (digEnd >= i)
	  {
	    digEnd = i;
	    nextDigit = '0';
	  }
	else
	  nextDigit = sbuf.charAt(digEnd);
	boolean addOne = nextDigit >= '5';
	char skip = addOne ? '9' : '0';
	while (digEnd > digStart + digits && sbuf.charAt(digEnd - 1) == skip)
	  digEnd--;
	length = digEnd - digStart;
	decimals = length - digits;
	if (addOne)
	  {
	    if (ExponentialFormat.addOne(sbuf, digStart, digEnd))
	      {
		digits++;
		decimals = 0;
		length = digits;
	      }
	  }
	if (decimals == 0 && (width <= 0
			      || signLen + digits + 1 < width))
	  {
	    decimals = 1;
	    length++;
	    // This is only needed if number==0.0:
	    sbuf.insert(digStart+digits, '0');
	  }
	sbuf.setLength(digStart + length);

        format(sbuf, fpos, length, digits, decimals,
               negative ? 1 : 0,
               oldSize);
      }
    return sbuf;
  }

  public StringBuffer format(Object num, StringBuffer sbuf, FieldPosition fpos)
  {
    RealNum rnum = RealNum.asRealNumOrNull(num);
    if (rnum == null)
      {
        if (num instanceof Complex)
          {
            // Common Lisp says if value is non-real, print as if with ~wD.
            String str = num.toString();
            int padding = width - str.length();
            while (--padding >= 0)
              sbuf.append(' ');
            sbuf.append(str);
            return sbuf;
          }
        rnum = (RealNum) num;
      }
    return format(rnum.doubleValue(), sbuf, fpos);
  }

  /** Do padding and similar adjustments on the converted number. */
  private void format (StringBuffer sbuf, FieldPosition fpos, int length, int digits, int decimals, int signLen, int oldSize)
  {
    int total_digits = digits + decimals;
    // Number of initial zeros to add.
    int zero_digits = getMinimumIntegerDigits();
    if (digits >= 0 && digits > zero_digits)
      zero_digits = 0;
    else
      zero_digits -= digits;
    // If there are no integer digits, add an initial '0', if there is room.
    if (digits + zero_digits <= 0
	&& (width <= 0 || width > decimals + 1 + signLen))
      zero_digits++;
    int needed = signLen + length + zero_digits + 1;  /* Add 1 for '.'. */
    int padding = width - needed;
    for (int i = zero_digits;  --i >= 0; )
      sbuf.insert(oldSize + signLen, '0');
    if (padding >= 0)
      {
	int i = oldSize;
	if (internalPad && signLen > 0)
	  i++;
	while (--padding >= 0)
	  sbuf.insert(i, padChar);
      }
    else if (overflowChar != '\0')
      {
	sbuf.setLength(oldSize);
	for (i = width;  --i >= 0; )
	  sbuf.append(overflowChar);
	return;
     }
    int newSize = sbuf.length();
    sbuf.insert(newSize - decimals, '.');

    /* Requires JDK1.2 FieldPosition extensions:
    if (fpos == null)
      {
	newSize++;
	if (fpos.getField() == FRACTION_FIELD)
	  {
	    fpos.setBeginIndex(newSize-decimals);
	    fpos.setEndIndex(newSize);
	  }
	else if (fpos.getField() == INTEGER_FIELD)
	  {
	    fpos.setBeginIndex(newSize-decimals);
	    fpos.setEndIndex(newSize-length-zero_digits-1);
	  }
      }
    */
  }

    public java.lang.Number parse(String text, java.text.ParsePosition status) {
        throw new UnsupportedOperationException
            ("RealFixedFormat.parse - not implemented");
    }
    public Object parseObject(String text, java.text.ParsePosition status) {
        throw new UnsupportedOperationException
            ("RealFixedFormat.parseObject - not implemented");
    }

}
