// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xml;
import gnu.mapping.*;
import java.io.*;

/** A QName with namespace nodes [and future optional type annotation]. */

public class XName extends Symbol implements Externalizable
{
  NamespaceBinding namespaceNodes;

  public XName ()
  {
  }

  public XName (Symbol symbol, NamespaceBinding namespaceNodes)
  {
    super(symbol.getName(), symbol.getNamespace());
    this.namespaceNodes = namespaceNodes;
  }

  /** Namespace nodes associated with an element.
   * These are in inverse document/parse order.
   */
  public final NamespaceBinding getNamespaceNodes () { return namespaceNodes; }
  public final void setNamespaceNodes (NamespaceBinding nodes)
  { this.namespaceNodes = nodes; }

  String lookupNamespaceURI (String prefix)
  {
    for (NamespaceBinding ns = namespaceNodes;  ns != null;  ns = ns.next)
      {
	if (prefix == ns.prefix)
	  return ns.uri;
      }
    return null;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    super.writeExternal(out);
    out.writeObject(namespaceNodes);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    super.readExternal(in);
    namespaceNodes = (NamespaceBinding) in.readObject();
  }

  public static boolean isNameStart(int ch)
  {
    /* #ifdef JAVA5 */
    return Character.isUnicodeIdentifierStart(ch)
    /* #else */
    // return ch >= 0x10000 || Character.isUnicodeIdentifierStart((char) ch)
    /* #endif */
      || ch == '_';
  }

  public static boolean isNamePart(int ch)
  {
    /* #ifdef JAVA5 */
    return Character.isUnicodeIdentifierPart(ch)
    /* #else */
    // return ch >= 0x10000 || Character.isUnicodeIdentifierPart((char) ch)
    /* #endif */
      || ch == '-' || ch == '.';
  }

  public static boolean isNmToken (String value)
  {
    return checkName(value) >= 0;
  }

  public static boolean isName (String value)
  {
    return checkName(value) > 0;
  }

  public static boolean isNCName (String value)
  {
    return checkName(value) > 1;
  }

  /** Check if a string is a valid NMTOKEN, Name, or NCName.
   * @return 2 if string is an NCName; otherwise 1 if string is a Name;
   *  otherwise 0 if string is an NMTOKEN; otherwise -1.
   */

  public static int checkName (String value)
  {
    int len = value.length();
    if (len == 0)
      return -1;
    int result = 2;
    for (int i = 0;  i < len;  )
      {
        boolean first = i == 0;
        int ch = value.charAt(i++);
        if (ch >= 0xD800 && ch < 0xDC00 && i < len)
          ch = (ch - 0xD800) * 0x400 + (value.charAt(i++) - 0xDC00) + 0x10000;
        if (ch == ':')
          {
            if (result == 2)
              result = 1;
          }
        else if (! XName.isNamePart(ch))
          return -1;
        else if (first && ! XName.isNameStart(ch))
          result = 0;
      }
    return result;
  }
}
