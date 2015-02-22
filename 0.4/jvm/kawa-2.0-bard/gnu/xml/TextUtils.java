package gnu.xml;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.xml.KNode;
import gnu.lists.*;
import gnu.text.Char;
import java.math.BigDecimal;

public class TextUtils
{
  public static String asString (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    else if (node instanceof Values)
      throw new ClassCastException();
    StringBuffer sbuf = new StringBuffer(100);
    stringValue(node, sbuf);
    return sbuf.toString();
  }

  public static String stringValue (Object node)
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (node instanceof Values)
      {
	Values vals = (Values) node;
	int index = 0;
        for (int ipos = 0; (ipos = vals.nextPos(ipos)) != 0; )
          {
            stringValue(vals.getPosPrevious(ipos), sbuf);
          }
      }
    else
      stringValue(node, sbuf);
    return sbuf.toString();
  }

    public static void stringValue (Object node, StringBuffer sbuf) {
        if (node instanceof KNode) {
            KNode pos = (KNode) node;
            NodeTree tlist = (NodeTree) pos.sequence;
            tlist.stringValue(tlist.posToDataIndex(pos.ipos), sbuf);
        } else if (node instanceof Char) {
            Char.print(((Char) node).intValue(), sbuf);
        } else if (node instanceof Character) {
            sbuf.append(((Character) node).charValue());
        } else {
            if (node instanceof BigDecimal)
                node = XMLPrinter.formatDecimal((BigDecimal) node);
            else if (node instanceof Double || node instanceof gnu.math.DFloNum)
                node = XMLPrinter.formatDouble(((Number) node).doubleValue());
            else if (node instanceof Float)
                node = XMLPrinter.formatFloat(((Number) node).floatValue());
            if (node != null && node != Values.empty)
                sbuf.append(node);
        }
    }

  public static void textValue (Object arg, Consumer out)
  {
    if (arg == null || (arg instanceof Values && ((Values) arg).isEmpty()))
      return;
    String str;
    if (arg instanceof String)
      str = (String) arg;
    else
      {
        StringBuffer sbuf = new StringBuffer();
        if (arg instanceof Values)
          {
            Values vals = (Values) arg;
            int count = -1;
            for (int ipos = 0; (ipos = vals.nextPos(ipos)) != 0; )
              {
                if (++count > 0)
                  sbuf.append(' ');
                stringValue(vals.getPosPrevious(ipos), sbuf);
              }
          }
        else
          stringValue(arg, sbuf);
        str = sbuf.toString();
      }
    out.write(str);
  }

  /** Create a normalized string.
   * @return the original string if it was normalized; otherwise a fresh one.
   * (XStringType.matcyhes assumes the above.)
   */
  public static String replaceWhitespace (String str, boolean collapse)
  {
    /* #ifdef JAVA5 */
    StringBuilder sbuf = null;
    /* #else */
    // StringBuffer sbuf = null;
    /* #endif */
    int len = str.length();
    // 1: previous was single space.
    // 2: previous was multiple spaces or other whitespace.
    int prevSpace = collapse ? 1 : 0;
    for (int i = 0;  i < len;  )
      {
        char ch = str.charAt(i++);
        int isSpace = ch == ' ' ? 1
        : ch == '\t' || ch == '\r' || ch == '\n' ? 2 : 0;
        if (sbuf == null
            && (isSpace == 2
                || (isSpace == 1 && prevSpace > 0 && collapse)
                || (isSpace == 1 && i == len && collapse)))
          {
            /* #ifdef JAVA5 */
            sbuf = new StringBuilder();
            /* #else */
            // sbuf = new StringBuffer();
            /* #endif */
            int k = prevSpace > 0 ? i - 2 : i - 1;
            for (int j = 0;  j < k;  j++)
              sbuf.append(str.charAt(j));
            ch = ' ';
           }
        if (collapse)
          {
            if (prevSpace > 0 && isSpace == 0)
              {
                if (sbuf != null && sbuf.length() > 0)
                  sbuf.append(' ');
                prevSpace = 0;
              }
            else if (isSpace == 2 || (isSpace == 1 && prevSpace > 0))
              prevSpace = 2;
            else if (isSpace > 0)
              prevSpace = 1;
            else
              prevSpace = 0;
            if (prevSpace > 0)
              continue;
          }
        if (sbuf != null)
          sbuf.append(ch);
      }
    if (sbuf != null)
      return sbuf.toString();
    else
      return str;

  }
}
