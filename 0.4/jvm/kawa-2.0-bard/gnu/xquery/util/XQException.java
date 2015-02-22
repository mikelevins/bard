package gnu.xquery.util;
import gnu.mapping.*;

public class XQException extends RuntimeException
{

  public Symbol code;
  public String description;
  public Object errorValue;

  public XQException (Symbol code, String description, Object errorValue)
  {
    super(description);
    this.code = code;
    this.description = description;
    this.errorValue = errorValue;
  }

  public static Symbol FOER0000_QNAME
  = Symbol.make("http://www.w3.org/2005/xqt-errors", "FOER0000", "err");

  public static void error ()
  {
    throw new XQException(FOER0000_QNAME, null, null);
  }

  public static void error (Symbol error)
  {
    throw new XQException(error, null, null);
  }

  public static void error (Object error, String description)
  {
    if (error == null || error == Values.empty)
      error = FOER0000_QNAME;
    throw new XQException((Symbol) error, description, null);
  }

  public static void error (Object error, String description, Object errorValue)
  {
    if (error == null || error == Values.empty)
      error = FOER0000_QNAME;
     throw new XQException((Symbol) error, description, errorValue);
  }

  public String getMessage()
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (description == null)
      sbuf.append("XQuery-error");
    else
      sbuf.append(description);
    if (code != null)
      {
        sbuf.append(" [");
        String prefix = code.getPrefix();
        if (prefix != null && prefix.length() > 0)
          {
            sbuf.append(prefix);
            sbuf.append(':');
          }
        sbuf.append(code.getLocalName());
        sbuf.append(']');
      }
    if (errorValue != null && errorValue != Values.empty)
      {
        sbuf.append(" value: ");
        sbuf.append(errorValue);
      }
    return sbuf.toString();
  }
}
