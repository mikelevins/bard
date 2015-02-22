package gnu.text;
import java.text.Format;
import java.text.FieldPosition;
import java.io.Writer;
import java.io.CharArrayWriter;
import gnu.lists.Consumer;

public abstract class ReportFormat extends Format
{
  /** Some Formats use this to indicate a parameter that is the
   * extracted from the argment list. */
  public static final int PARAM_FROM_LIST = 0xA0000000;

  /** Some Formats use this to indicate a parameter that is the
   * number of remaining paramaters. */
  public static final int PARAM_FROM_COUNT = 0xB0000000;

  /** Some Formats use this to indicate an unspecified parameter. */
  public static final int PARAM_UNSPECIFIED = 0xC0000000;

  public static int result(int resultCode, int nextArg)
  {
    return (resultCode << 24) | nextArg;
  }
  public static int nextArg(int result) { return result & 0xffffff; }
  public static int resultCode(int result) { return result >>> 24; }

    /**
     * Format an array of arguments, and write out the result.
     *
     * @param args the objects to be formatted
     * @param start the index (in args) of the argument to start with
     * @param dst where to write the result
     * @return an integer result(resultCode, nextArg), where
     * nextArg is the index following the last argument processed, and
     * code is a result code (normally 0, or negative if early termination)
     */
    
  public abstract int format(Object[] args, int start, Appendable dst, FieldPosition fpos)
    throws java.io.IOException;

  public int format(Object arg, int start, Appendable dst, FieldPosition fpos)
    throws java.io.IOException
  {
    if (arg instanceof Object[])
      return format((Object[]) arg, start, dst, fpos);
    else
      {
        Object[] args = { arg };
        return format(args, start, dst, fpos);
      }
  }

  public StringBuffer format(Object obj, StringBuffer sbuf, FieldPosition fpos)
  {
    format((Object[]) obj, 0, sbuf, fpos);
    return sbuf;
  }

  public int format(Object[] args, int start,
		    StringBuffer sbuf, FieldPosition fpos)
  {
    CharArrayWriter wr = new CharArrayWriter();
    try
      {
	start = format(args, start, wr, fpos);
	if (start < 0)
	  return start;
      }
    catch (java.io.IOException ex)
      {
	throw new Error("unexpected exception: "+ex);
      }
    sbuf.append(wr.toCharArray());
    return start;
  }

  public static int format(Format fmt, Object[] args, int start, Appendable dst, FieldPosition fpos) 
    throws java.io.IOException
  {
    if (fmt instanceof ReportFormat)
      return ((ReportFormat) fmt).format(args, start, dst, fpos);
    StringBuffer sbuf = new StringBuffer();
    if (fmt instanceof java.text.MessageFormat)
      start = format(fmt, args, start, sbuf, fpos);
    else
      fmt.format(args[start++], sbuf, fpos);
    dst.append(sbuf);
    return start;
  }

  public static int format(Format fmt, Object[] args, int start, 
			   StringBuffer sbuf, FieldPosition fpos) 
  {
    if (fmt instanceof ReportFormat)
      return ((ReportFormat) fmt).format(args, start, sbuf, fpos);
    int nargs;
    Object arg;
    if (fmt instanceof java.text.MessageFormat)
      {
	nargs = args.length - start;
	if (start > 0)
	  {
	    Object[] subarr = new Object[args.length - start];
	    System.arraycopy(args, start, subarr, 0, subarr.length);
	    arg = subarr;
	  }
	else
	  arg = args;
      }
    else
      {
	arg = args[start];
	nargs = 1;
      }
    fmt.format(arg, sbuf, fpos);
    return start + nargs;
  }

  /** (Parameters in non-standard order.) */
  public static void print (Writer dst, String str)
    throws java.io.IOException
  {
    if (dst instanceof java.io.PrintWriter)
      ((java.io.PrintWriter) dst).print(str);
    else
      dst.write(str);
  }

  public static void print (Object value, Consumer out)
  {
    if (value instanceof Printable)
      ((Printable) value).print(out);
    else
      // Should we use out.writeObject?
      // We need make consistent rules to avoid infinite recursion.
      out.write(value == null ? "null" : value.toString());
  }

  public Object parseObject(String text, java.text.ParsePosition status)
  {
    throw new Error("ReportFormat.parseObject - not implemented");
  }

  public static int getParam(Object arg, int defaultValue)
  {
    if (arg instanceof Number)
      return ((Number) arg).intValue();
    if (arg instanceof Character)
      return ((Character) arg).charValue();
    if (arg instanceof Char)
      return ((Char) arg).charValue();
    //if (arg == null || arg == Boolean.FALSE || arg == Special.dfault)
    return defaultValue;
  }

  protected static int getParam(int param, int defaultValue, Object[] args, int start)
  {
    if (param == PARAM_FROM_COUNT)
      return args.length - start;
    if (param == PARAM_FROM_LIST)
      return args == null ? defaultValue : getParam(args[start], defaultValue);
    if (param == PARAM_UNSPECIFIED)
      return defaultValue;
    // Need to mask off flags etc?
    return param;
  }

  protected static char getParam(int param, char defaultValue, Object[] args, int start)
  {
    return (char) getParam (param, (int) defaultValue, args, start);
  }

  /** Get the index'th parameter for the conversion specification specs[speci].
   * Note that parameters are numbered from 1 to numParams(speci).
   * The list of arguments to be converted is args, with the current index
   * (as of the start of this conversion, i.e. not taking into account
   * earlier PARAM_FROM_LIST paramaters for this conversion) in start.
   * The default value (used if PARAM_UNSPECIFIED) is defaultValue.
   */
  /*
  int getParam(int speci, int index, int defaultValue, Object[] args, int start)
  {
    int num_params = numParams(speci);
    int param = index <= num_params ? specs[speci+index] : PARAM_UNSPECIFIED;
    if (param == PARAM_FROM_LIST || param == PARAM_FROM_COUNT)
      start += adjustArgsStart(speci, index);
    return getParam(param, defaultValue, args, start);
  }
  */
}
