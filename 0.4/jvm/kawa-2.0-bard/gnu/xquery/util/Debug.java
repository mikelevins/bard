package gnu.xquery.util;
import gnu.mapping.*;
import gnu.kawa.io.OutPort;
import gnu.xml.XMLPrinter;

public class Debug
{
  public static String tracePrefix = "XQuery-trace: ";
  public static OutPort tracePort = null;
  public static String traceFilename = "XQuery-trace.log";
  public static boolean traceShouldFlush = true;
  public static boolean traceShouldAppend = false;

  public static synchronized Object trace (Object value, Object message)
  {
    OutPort out = tracePort;
    if (out == null)
      {
        try
          {
            out = new OutPort(new java.io.FileOutputStream(traceFilename,
                                                           traceShouldAppend));
          }
        catch (Exception ex)
          {
            throw new WrappedException("Could not open '"+traceFilename
                                 +"' for fn:trace output", ex);
          }
        tracePort = out;
      }
    out.print(tracePrefix);
    out.print(message);
    out.print(' ');
    XMLPrinter xout = new XMLPrinter(out, false);
    xout.writeObject(value);
    out.println();
    if (traceShouldFlush)
      out.flush();
    return value;
  }
}
