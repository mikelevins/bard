package gnu.jemacs.lang;
import gnu.kawa.functions.DisplayFormat;
import gnu.lists.Consumer;

public class Print extends DisplayFormat
{
  public Print (boolean readable)
  {
    super(readable, 'E');
  }

  public static String escapeChar(int ch)
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append('?');
    if (ch < ' ')
      {
	sbuf.append('\\');
	switch (ch)
	  {
	  case '\t':  sbuf.append('t');  break;
	  case '\n':  sbuf.append('n');  break;
	  case '\r':  sbuf.append('r');  break;
	  default:
	    sbuf.append('^');
	    sbuf.append((char) (ch + 64));
	    if ((ch + 64) == '\\')
	      sbuf.append('\\');
	  }
      }
    else if (ch < 127)
      {
	/* syntactically special characters should be escaped. */
	switch (ch)
	  {
	  case ' ':
	  case '"':
	  case '#':
	  case '\'':
	  case '(':
	  case ')':
	  case ',':
	  case '.':
	  case ';':
	  case '?':
	  case '[':
	  case '\\':
	  case ']':
	  case '`':
	    sbuf.append('\\');
	  }
	sbuf.append((char) ch);
      }
    else if (ch == 127)
      sbuf.append("\\^?");
    else
      {
	if (ch < 160)
	  sbuf.append("\\^");
	sbuf.append(ch);
      }
    return sbuf.toString();
  }

  public void write (int v, Consumer out)
  {
    if (getReadableOutput ())
      write(Print.escapeChar(v), out);
    else
      out.write(v);
  }

}
