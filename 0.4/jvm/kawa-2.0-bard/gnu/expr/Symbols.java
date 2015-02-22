package gnu.expr;
import gnu.kawa.io.OutPort;
import gnu.mapping.*;
import gnu.lists.Consumer;

/** Utility class containing various routines to manipulate Scheme symbols.
  * Note Scheme symbols are represented using java.lang.String objects,
  * and there are no Symbol objects. */

public class Symbols
{
  /** There are no instances of this class. */
  private Symbols ()
  {
  }

  private static int gensym_counter;

  static synchronized int generateInt()
  {
    return ++gensym_counter;
  }

  /* DEBUGGING:
  static java.util.Vector gensyms = new java.util.Vector();

  public static String show (String str)
  {
    StringBuffer buf = new StringBuffer(str);
    if (str.intern() == str)
      buf.append("<I>");
    else
      {
	for (int i = gensyms.size();  ; )
	  {
	    if (--i < 0)
	      {
		buf.append("<?>");
		break;
	      }
	    else if (gensyms.elementAt(i) == str)
	      {
		buf.append('<');
		buf.append(i);
		buf.append('>');
		break;
	      }
	  }
      }
    return buf.toString();
  }
  */

  /**
   * Generate a new (interned) symbol with a unique name.
   * @return the new symbol
   */
  public static final SimpleSymbol gentemp ()
  {
    return SimpleSymbol.valueOf("GS." + Integer.toString(generateInt()));
  }

  /**
   * Create or find a Symbol with a given name.
   * @param name the print-name of the desired Symbol
   * @return a Symbol with the given name, newly created iff none such exist
   */
  static public String make (String name)
  {
    return name.intern();
  }

  static public final String intern (String name)
  {
    return make (name);
  }

  public static void print(String name, Consumer out)
  {
    boolean readable = (out instanceof OutPort)
      && ((OutPort)out).printReadable;
    if (readable)
      {
	int len = name.length ();
	for (int i = 0;  i < len;  i++)
	  {
	    char ch = name.charAt (i);
	    if (!(Character.isLowerCase (ch)
		  || ch == '!' || ch == '$' || ch == '%' || ch == '&'
		  || ch == '*' || ch == '/' || ch == ':' || ch == '<'
		  || ch == '=' || ch == '>' || ch == '?' || ch == '~'
		  || ch == '_' || ch == '^'
		  || ((ch == '+' || ch == '-') && (i > 0 || len == 1))
		  || (Character.isDigit (ch) && i > 0)
		  || (ch == '.' && (i == 0 || name.charAt (i - 1) == '.'))))
	      out.write('\\');
	    out.write(ch);
	  }
      }
    else
      out.write(name);
  }

}
