package gnu.kawa.functions;
import java.text.ParseException;
import java.text.Format;
import java.util.ArrayList;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.InPort;
import gnu.mapping.*;
import gnu.math.FixedRealFormat;
import gnu.lists.*;
import gnu.text.*;

public class ParseFormat extends Procedure1
{
  public static final ParseFormat parseFormat = new ParseFormat(false);

  boolean emacsStyle = true;
  public static final int PARAM_UNSPECIFIED = LispFormat.PARAM_UNSPECIFIED;
  public static final int PARAM_FROM_LIST = LispFormat.PARAM_FROM_LIST;

  public ParseFormat (boolean emacsStyle)
  {
    this.emacsStyle = emacsStyle;
  }

  public static final int SEEN_MINUS = 1;
  public static final int SEEN_PLUS  = 2;
  public static final int SEEN_SPACE = 4;
  public static final int SEEN_ZERO  = 8;
  public static final int SEEN_HASH = 16;

  public ReportFormat parseFormat(InPort fmt)
    throws java.text.ParseException, java.io.IOException
  {
    return parseFormat(fmt, emacsStyle ? '?' : '~');
  }

  public static ReportFormat parseFormat(InPort fmt, char magic)
    throws java.text.ParseException, java.io.IOException
  {
    StringBuffer fbuf = new StringBuffer(100);
    int position = 0;
    ArrayList<Format> formats = new ArrayList<Format>();
    Format format;
    for (;;)
      {
	int ch = fmt.read();
	if (ch >= 0)
	  {
	    if (ch != magic)
	      {
		// FIXME - quote special characters!
		fbuf.append((char) ch);
		continue;
	      }
	    ch = fmt.read();
	    if (ch == magic)
	      {
		fbuf.append((char) ch);
		continue;
	      }
	  }
	int len = fbuf.length();

        // Note we create a LiteralFormat even when fbuf is empty.
        // This is to make sure there are string-valued separators between
        // specifiers (as well as before and after).  Otherwise
        // ($sprintf$ "%s%s" 3 4) would return "3 4" rather than "34".
        if (len == 0)
            format = LiteralFormat.separator;
        else
          {
            char[] text = new char[len];
            fbuf.getChars(0, len, text, 0);
            fbuf.setLength(0);
            format = new LiteralFormat(text);
          }
        formats.add(format);

	if (ch < 0)
	  break;
	int digit;
	if (ch == '$')
	  {
	    ch = fmt.read();
	    position = Character.digit((char) ch, 10);
	    if (position < 0)
	      throw new ParseException("missing number (position) after '%$'",
				       -1);
	    for (;;)
	      {
		ch = fmt.read();
		digit = Character.digit((char) ch, 10);
		if (digit < 0)
		  break;
		position = 10 * position + digit;
	      }
	    position--;  /* Convert to zero-origin index. */
	  }

	int flags = 0;
	for (;; ch = fmt.read())
	  {
	    switch ((char) ch)
	      {
	      case '-':  flags |= SEEN_MINUS;  continue;
	      case '+':  flags |= SEEN_PLUS;   continue;
	      case ' ':  flags |= SEEN_SPACE;  continue;
	      case '0':  flags |= SEEN_ZERO;   continue;
	      case '#':  flags |= SEEN_HASH;   continue;
	      }
	    break;
	  }

	int width;
        if (ch == '*')
          {
            width = PARAM_FROM_LIST;
            ch = fmt.read();
          }
	else if ((digit = Character.digit((char) ch, 10)) >= 0)
	  {
	    width = digit;
	    for (;;)
	      {
		ch = fmt.read();
		digit = Character.digit((char) ch, 10);
		if (digit < 0)
		  break;
		width = 10 * width + digit;
	      }
	  }
        else
          width = PARAM_UNSPECIFIED;

	int precision = PARAM_UNSPECIFIED;
	if (ch == '.')
	  {
            ch = fmt.read();
	    if (ch == '*')
              {
                precision = PARAM_FROM_LIST;
                ch = fmt.read();
              }
	    else if ((digit = Character.digit((char) ch, 10)) >= 0)
	      {
                precision = digit;
                for (;;)
                  {
                    ch = fmt.read();
                    digit = Character.digit((char) ch, 10);
                    if (digit < 0)
                        break;
                    precision = 10 * precision + digit;
                  }
              }
          }

        char padChar
            = (flags & (SEEN_ZERO+SEEN_MINUS)) == SEEN_ZERO ? '0' : ' ';
	switch (ch)
	  {
	  case 's':
	  case 'S':
	    format = new ObjectFormat(ch == 'S', precision);
	    break;

	  case 'x':
	  case 'X':
	  case 'i':
	  case 'd':
	  case 'o':
	    int base;
            int fflags = 0;
	    if (ch == 'd' || ch == 'i')
              base = 10;
	    else if (ch == 'o')
              base = 8;
	    else
              { /* if (ch == 'x' || ch == 'X') */
                base = 16;
                if (ch == 'X') fflags = IntegerFormat.UPPERCASE;
              }
            boolean seenColon = false;
            boolean seenAt = false;
            if ((flags & SEEN_HASH) != 0)
              fflags |= IntegerFormat.SHOW_BASE;
            if ((flags & SEEN_PLUS) != 0)
              fflags |= IntegerFormat.SHOW_PLUS;
            if ((flags & SEEN_MINUS) != 0)
              fflags |= IntegerFormat.PAD_RIGHT;
            if ((flags & SEEN_SPACE) != 0)
              fflags |= IntegerFormat.SHOW_SPACE;
	    if (precision != PARAM_UNSPECIFIED)
	      {
		flags &= ~ SEEN_ZERO;
		fflags |= IntegerFormat.MIN_DIGITS;
		format = IntegerFormat.getInstance(base, precision,
						   '0', PARAM_UNSPECIFIED,
						   PARAM_UNSPECIFIED, fflags);
	      }
	    else
	      format = IntegerFormat.getInstance(base, width,
						 padChar, PARAM_UNSPECIFIED,
						 PARAM_UNSPECIFIED, fflags);
            break;
	  case 'f':
	  case 'e':
	  case 'E':
	  case 'g':
	  case 'G':
              LispRealFormat dfmt = new LispRealFormat();
              dfmt.op = (char) ch;
              dfmt.style = 'P';
              dfmt.arg1 = width;
              if (precision == PARAM_UNSPECIFIED)
                  precision = 6;
              dfmt.arg2 = precision;
              dfmt.showPlus = (flags & SEEN_PLUS) != 0;
              if (ch == 'e' || ch == 'E' || ch == 'g' || ch == 'G') {
                  dfmt.arg3 = 2;
                  dfmt.arg4 = 1; // intDigits
                  dfmt.arg5 = '\0'; // overflowChar
                  dfmt.arg6  = padChar;
                  // set exponentChar
                  dfmt.arg7 = ch == 'E' || ch == 'G' ? 'E' : 'e';
              }
              else {
                  dfmt.arg3 = '\0'; // overflowChar
                  dfmt.arg5 = padChar;
              }
              dfmt.internalPad = true;
              format = dfmt.resolve(null, 0);
	    break;
	  default:
	    throw new ParseException ("unknown format character '"+ch+"'", -1);
	  }
	if (width > 0)
	  {
	    padChar = (flags & SEEN_ZERO) != 0 ? '0' : ' ';
	    int where;
	    if ((flags & SEEN_MINUS) != 0)
	      where = 100;
	    else if (padChar == '0')
	      where = -1;
	    else
	      where = 0;
	    format = new gnu.text.PadFormat(format, width, padChar, where);
	  }
	// FIXME handle re-positioning
	//fbuf.append('{');
        // fbuf.append(position);
	//fbuf.append('}');
	formats.add(format);
	position++;
      }
    int fcount = formats.size();
    if (fcount == 1)
      {
	Object f = formats.get(0);
	if (f instanceof ReportFormat)
	  return (ReportFormat) f;
      }
    return new CompoundFormat(formats.toArray(new Format[fcount]));
  }

  public Object apply1 (Object arg)
  {
    return asFormat(arg, emacsStyle ? '?' : '~');
  }

  public static ReportFormat asFormat (Object arg, char style)
  {
    try
      {
	if (arg instanceof ReportFormat)
	  return (ReportFormat) arg;
	if (style == '~')
	  return new LispFormat(arg.toString());
	else
	  {
	    InPort iport;
	    if (arg instanceof FString)
	      iport = new CharArrayInPort((FString) arg);
	    else 
	      iport = new CharArrayInPort(arg.toString()); 
	    try
	      {
		return parseFormat(iport, style);
	      }
	    finally
	      {
		iport.close();
	      }
	  }
      }
    catch (java.io.IOException ex)
      {
	throw new RuntimeException("Error parsing format ("+ex+")");
      }
    catch (ParseException ex)
      {
	throw new RuntimeException("Invalid format ("+ex+")");
      }
    catch (IndexOutOfBoundsException ex)
      {
	throw new RuntimeException("End while parsing format");
      }
  }
}
