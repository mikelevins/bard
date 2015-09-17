package gnu.kawa.functions;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.OutPort;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.text.ReportFormat;
import java.io.OutputStream;
import java.io.Writer;
import java.text.MessageFormat;

public class Format extends ProcedureN
{
  public static final Format format = new Format();
  static {
    format.setName("format");
    format.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileMisc:validateApplyFormat");
  }

  public static void format (Writer dst, Object[] args, int arg_offset)
  {
    Object format = args[arg_offset++];
    Object[] vals = new Object[args.length - arg_offset];
    System.arraycopy(args, arg_offset, vals, 0, vals.length);
    formatToWriter(dst, format, vals);
  }

  public static void formatToWriter (Writer dst, Object format, Object... vals)
  {
    if (dst == null)
      dst = OutPort.outDefault();
    try
      {
        if (format instanceof MessageFormat)
          {
            String out = ((MessageFormat) format).format(vals);
            dst.write(out);
          }
        else
          {
            if (! (format instanceof ReportFormat))
              format = ParseFormat.asFormat(format, '~');
	    ((ReportFormat) format).format(vals, 0, dst, null);
	  }
      }
    catch (java.io.IOException ex)
      {
        throw new RuntimeException("Error in format: "+ ex);
      }
  }

  public static void formatToOutputStream (OutputStream dst, Object format, Object... vals)
  {
    OutPort port = new OutPort(dst);
    format(port, format, vals);
    port.closeThis();
  }

  public static String formatToString (int arg_offset, Object... args)
  {
    CharArrayOutPort port = new CharArrayOutPort();
    format(port, args, arg_offset);
    String str = port.toString();
    port.close ();
    return str;
  }

    public static String sprintfToString(Object fmt, Object... args) {
        ReportFormat rfmt = ParseFormat.asFormat(fmt, '%');
        CharArrayOutPort port = new CharArrayOutPort();
        try {
            rfmt.format(args, 0, port, null);
        } catch (java.io.IOException ex) {
            WrappedException.rethrow(ex);
        }
        String str = port.toString();
        port.close();
        return str;
    }

  /**
   * Apply format and argument, yielding an FString.
   * @param style either '%' (C/Emacs-style format specifiers), or
   *   '~' (Common Lisp-style format specifiers).
   * @param fmt the format string or specification
   * @param args the arguments to be formatted
   */
  public static FString formatToFString (char style, Object fmt, Object[] args)
  {
    ReportFormat rfmt = ParseFormat.asFormat(fmt, style);
    CharArrayOutPort port = new CharArrayOutPort();
    try
      {
	rfmt.format(args, 0, port, null);
      }
    catch (java.io.IOException ex)
      {
        WrappedException.rethrow(ex);
      }
    char[] chars = port.toCharArray();
    port.close ();
    return new FString(chars);
  }

  public Object applyN (Object[] args)
  {
    /* #ifdef JAVA5 */
    return format(args);
    /* #else */
    // return format$V(args);
    /* #endif */
  }

  /* #ifdef JAVA5 */
  public static Object format (Object... args)
  /* #else */
  // public static Object format$V (Object[] args)
  /* #endif */
  {
    Object port_arg = args[0];
    if (port_arg == Boolean.TRUE)
      {
	format(OutPort.outDefault(), args, 1);
	return Values.empty;
      }
    else if (port_arg == Boolean.FALSE)
      {
	return formatToString(1, args);
      }
    else if (port_arg instanceof MessageFormat
             /* #ifdef use:java.lang.CharSequence */
             || port_arg instanceof CharSequence
             /* #else */
             // || port_arg instanceof String || port_arg instanceof CharSeq
             /* #endif */
	     || port_arg instanceof ReportFormat)
      {
	return formatToString(0, args);
      }
    else if (port_arg instanceof Writer)
      {
	format((Writer) port_arg, args, 1);
	return Values.empty;
      }
    else if (port_arg instanceof OutputStream)
      {
        formatToOutputStream((OutputStream) port_arg,
                             args[1], drop2(args));
	return Values.empty;
      }
    else
      throw new RuntimeException("bad first argument to format");
  }

  static Object[] drop2 (Object[] vals)
  {
    int xlen = vals.length - 2;
    Object[] xvals = new Object[xlen];
    System.arraycopy(vals, 2, xvals, 0, xlen);
    return xvals;
  }
}
