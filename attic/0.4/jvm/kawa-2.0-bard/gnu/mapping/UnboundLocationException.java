package gnu.mapping;
import gnu.text.SourceLocator;

/** An undefined symbol was evaluated. */

public class UnboundLocationException extends RuntimeException
{
  public Object symbol;
  Location location;
  String filename;
  int line, column;

  public UnboundLocationException ()
  {
  }

  public UnboundLocationException (Object symbol)
  {
    this.symbol = symbol;
  }

  public UnboundLocationException (Object symbol, String filename,
                                   int line, int column)
  {
    this.symbol = symbol;
    this.filename = filename;
    this.line = line;
    this.column = column;
  }

  public UnboundLocationException (Object symbol, SourceLocator location)
  {
    this.symbol = symbol;
    if (location != null)
      {
        this.filename = location.getFileName();
        this.line = location.getLineNumber();
        this.column = location.getColumnNumber();
      }
  }

  public UnboundLocationException (Location loc)
  {
    this.location = loc;
  }

  public UnboundLocationException (Object symbol, String message)
  {
    super (message);
    this.symbol = symbol;
  }

  public void setLine (String filename, int line, int column)
  {
    this.filename = filename;
    this.line = line;
    this.column = column;
  }
  
  public String getMessage()
  {
    String msg = super.getMessage();
    if (msg != null)
      return msg;
    StringBuffer sbuf = new StringBuffer();
    if (filename != null || line > 0)
      {
        if (filename != null)
          sbuf.append(filename);
        if (line >= 0)
          {
            sbuf.append(':');
            sbuf.append(line);
            if (column > 0)
              {
                sbuf.append(':');
                sbuf.append(column);
              }
          }
        sbuf.append(": ");
      }
    Symbol name = location == null ? null : location.getKeySymbol();
    if (name != null)
      {
	sbuf.append("unbound location: ");
	sbuf.append(name);
	Object property = location.getKeyProperty();
	if (property != null)
	  {
	    sbuf.append(" (property ");
	    sbuf.append(property);
	    sbuf.append(')');
	  }
      }
    else if (symbol != null)
      {
	sbuf.append("unbound location: ");
	sbuf.append(symbol);
      }
    else
      sbuf.append("unbound location");
    return sbuf.toString();
  }

  public String toString()
  {
    return getMessage();
  }
}
