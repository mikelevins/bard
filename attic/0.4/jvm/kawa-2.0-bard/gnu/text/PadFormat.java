package gnu.text;
import java.text.*;
import java.text.FieldPosition;
import java.io.Writer;

/** Given a Format, pad the formatted result to a specified width. */

public class PadFormat extends ReportFormat
{
  /** Minimum width. */
  int minWidth;
  char padChar;
  /** What percentage of padding appears after the data.
   * -1 means internal padding (after initial '-' or '+' or '0x' or '0X'). */
  int where;

  Format fmt;

  public PadFormat(Format fmt, int minWidth, char padChar, int where)
  {
    this.fmt = fmt;
    this.minWidth = minWidth;
    this.padChar = padChar;
    this.where = where;
  }

  public PadFormat(Format fmt, int minWidth)
  {
    this(fmt, minWidth, ' ', 100);
  }

  public int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
    throws java.io.IOException
  {
    return format(fmt, args, start, dst, padChar, minWidth, 1, 0, where, fpos);
  }

  public static int padNeeded(int actualWidth,
			      int minWidth, int colInc, int minPad)
  {
    int total = actualWidth + minPad;
    if (colInc <= 1)
      colInc = minWidth - total;
    while (total < minWidth)
      total += colInc;
    return total - actualWidth;
  }

  public static int format(Format fmt, Object[] args, int start, Appendable dst, char padChar, int minWidth, int colInc, int minPad, int where, FieldPosition fpos)
    throws java.io.IOException
  {
    /*
    if (where == 100)
      {
	int oldLength = sbuf.length();
	fmt.format(obj, sbuf, fpos);
	int newLength = sbuf.length();
	int pad = padNeeded(newLength - oldLength, minWidth, colInc, minPad);
	while (--pad >= 0)
	  sbuf.append(padChar);
	return start + ?;
      }
    */

      // FIXME: Should use a CharArrayOutPort instead of a StringBuffer;
      // dst is already a CharArrayOutPort, re-use it.
    StringBuffer tbuf = new StringBuffer(200);
    if (fmt instanceof ReportFormat)
      start = ((ReportFormat)fmt).format(args, start, tbuf, fpos);
    else if (fmt instanceof MessageFormat)
      {
	// FIXME - only correct if start == 0.
	fmt.format(args, tbuf, fpos);
	start = args.length;
      }
    else
      {
	fmt.format(args[start], tbuf, fpos);
	start++;
      }
    int len = tbuf.length();
    int pad = padNeeded(len, minWidth, colInc, minPad);
    int prefix = 0;
    String text = tbuf.toString();
    if (pad > 0)
      {
	if (where == -1)
	  {
	    if (len > 0)
	      {
		char ch = text.charAt(0);
		if (ch == '-' || ch == '+')
		  {
		    prefix++;
		    dst.append(ch);
		  }
		if (len - prefix > 2 && text.charAt(prefix) == '0')
		  {
		    dst.append('0');
		    ch = text.charAt(++prefix);
		    if (ch == 'x' || ch == 'X')
		      {
			prefix++;
			dst.append(ch);
		      }
		  }
		if (prefix > 0)
		  text = text.substring(prefix);
	      }
	    where = 0;
	  }
	int padAfter = (pad * where) / 100;
	int padBefore = pad - padAfter;
	/*
	if (fpos != null && padBefore > 0)
	  {
	    // This is still broken in JDK 1.2 beta2.  Check beta3.  FIXME.
	    fpos.setBeginIndex(fpos.getBeginIndex() + padBefore);
	    fpos.setEndIndex(fpos.getEndIndex() + padBefore);
	  }
	*/
	while (--padBefore >= 0)
	  dst.append(padChar);
	dst.append(text);
	while (--padAfter >= 0)
	  dst.append(padChar);
      }
    else
      {
	dst.append(text);
      }
    return start;
  }
}
