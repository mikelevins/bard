package gnu.xquery.util;

import gnu.kawa.io.Path;
import gnu.kawa.io.URIPath;
import gnu.kawa.xml.*;
import gnu.mapping.*;
import gnu.xml.*;

public class QNameUtils
{
  public static Object resolveQNameUsingElement (Object qname, KElement node)
  {
    qname = KNode.atomicValue(qname);
    if (qname == Values.empty || qname == null)
      return qname;
    if (qname instanceof Values
        || ! (qname instanceof String || qname instanceof UntypedAtomic))
      throw new RuntimeException("bad argument to QName");
    String name = TextUtils.replaceWhitespace(qname.toString(), true);
    int colon = name.indexOf(':');
    String prefix, localPart, uri;
    if (colon < 0)
      {
	prefix = null;
	localPart = name;
      }
    else
      {
	prefix = name.substring(0, colon).intern();
	localPart = name.substring(colon+1);
      }
    uri = node.lookupNamespaceURI(prefix);
    if (uri == null)
      {
        if (prefix == null)
          uri = "";
        else
          throw new RuntimeException("unknown namespace for '"+name+"'");
      }
    if (! validNCName(localPart)
	|| (prefix != null && ! validNCName(prefix)))
      {
	throw new RuntimeException("invalid QName syntax '"+name+"'");
      }
    return Symbol.make(uri, localPart, prefix == null ? "" : prefix);
  }

  /** Method called from compiled code to "cast" to a QName.
   * @param qname The value to cast to QName.
   * @param constructorNamespaces Namespace bindings from namespace
   *   attributes in direct element constructors.
   * @param prologNamespaces Namespac bindings from query prolog,
   *   as well as builtin namespace prefixes.
   */
  public static Object resolveQName (Object qname,
				     NamespaceBinding constructorNamespaces,
				     NamespaceBinding prologNamespaces)
  {
    qname = KNode.atomicValue(qname);
    if (qname instanceof Symbol)
      return qname;
    if (qname instanceof Values
        || ! (qname instanceof String || qname instanceof UntypedAtomic))
      throw new RuntimeException("bad argument to QName");
    String name = TextUtils.replaceWhitespace(qname.toString(), true);
    int colon = name.indexOf(':');
    String prefix, localPart;
    if (colon < 0)
      {
	localPart = name;
	prefix = null;
      }
    else
      {
	prefix = name.substring(0, colon).intern();
	localPart = name.substring(colon+1);
      }
    if (! validNCName(localPart)
	|| (prefix != null && ! validNCName(prefix)))
      {
	throw new RuntimeException("invalid QName syntax '"+name+"'");
      }
    String uri = resolvePrefix(prefix, constructorNamespaces, prologNamespaces);
    return Symbol.make(uri, localPart, prefix == null ? "" : prefix);
  }

  /** Search for a uri matching the given prefix.
   * @return uri or null if there is no binding for prefix.
   */
  public static String lookupPrefix (String prefix,
                                     NamespaceBinding constructorNamespaces,
                                     NamespaceBinding prologNamespaces)
  {
    String uri;

    for (NamespaceBinding ns = constructorNamespaces; ; ns = ns.getNext())
      {
        if (ns == null)
          {
            uri = prologNamespaces.resolve(prefix);
            break;
          }
	if (ns.getPrefix() == prefix)
          {
            uri = ns.getUri();
            break;
          }
      }
    if (uri == null && prefix == null)
      uri = "";
    return uri;
  }

  /** Search for a uri matching the given prefix.
   * Throw exception if there is no binding and the prefix is non-empty.
   */
  public static String resolvePrefix (String prefix,
                                      NamespaceBinding constructorNamespaces,
                                      NamespaceBinding prologNamespaces)
  {
    String uri = lookupPrefix(prefix, constructorNamespaces, prologNamespaces);
    if (uri == null)
      throw new RuntimeException("unknown namespace prefix '"+prefix+"'");
    return uri;
  }

  public static boolean validNCName (String name)
  {
    return XName.isName(name);
  }

  /** This implements the <code>fn:QName</code> standard function. */

  public static Symbol makeQName (Object paramURI, String paramQName)
  {
    if (paramURI == null || paramURI == Values.empty)
      paramURI = "";
    int colon = paramQName.indexOf(':');
    String namespaceURI = (String) paramURI, localPart, prefix;
    if (colon < 0)
      {
	localPart = paramQName;
	prefix = "";
      }
    else
      {
	localPart = paramQName.substring(colon+1);
	prefix = paramQName.substring(0, colon).intern();
      }
    if (! validNCName(localPart)
	|| (colon >= 0 && ! validNCName(prefix)))
      throw new IllegalArgumentException("invalid QName syntax '"+paramQName+"'");
    if (colon >= 0 && namespaceURI.length() == 0)
      throw new IllegalArgumentException("empty uri for '"+paramQName+"'");
    return Symbol.make(namespaceURI, localPart, prefix);
  }

  public static Object localNameFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (! (name instanceof Symbol))
      throw new WrongType("local-name-from-QName", 1, name, "xs:QName");
    return XStringType.makeNCName(((Symbol) name).getName());
  }

  public static Object prefixFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    if (name instanceof Symbol)
      {
        String prefix = ((Symbol) name).getPrefix();
        if (prefix == null || prefix.length() == 0)
          return Values.empty;
        return XStringType.makeNCName(prefix);
      }
    throw new WrongType("prefix-from-QName", 1, name, "xs:QName");
  }

  public static Object namespaceURIFromQName (Object name)
  {
    if (name == Values.empty || name == null)
      return name;
    try
      {
        return URIPath.makeURI(((Symbol) name).getNamespaceURI());
      }
    catch (ClassCastException ex)
      {
        throw new WrongType("namespace-uri", 1, name, "xs:QName");
      }
  }

  public static Object namespaceURIForPrefix (Object prefix,
					      Object element)
  {
    KNode el = KNode.coerce(element);
    if (el == null)
      throw new WrongType("namespace-uri-for-prefix", 2, element, "node()");
    String str;
    if (prefix == null || prefix == Values.empty)
      str = null;
    else if (! (prefix instanceof String || prefix instanceof UntypedAtomic))
      throw new WrongType("namespace-uri-for-prefix", 1, element, "xs:string");
    else
      {
        str = prefix.toString().intern();
        if (str == "")
          str = null;
      }
    String uri = el.lookupNamespaceURI(str);
    if (uri == null)
      return Values.empty;
    else
      return uri;
  }

  public static Object resolveURI (Object relative, Object base)
    throws java.net.URISyntaxException
  {
    if (relative instanceof KNode)
      relative = KNode.atomicValue(relative);
    if (base instanceof KNode)
      base = KNode.atomicValue(base);
    if (relative == Values.empty || relative == null)
      return relative;
    if (relative instanceof UntypedAtomic)
      relative = relative.toString();
    if (base instanceof UntypedAtomic)
      base = base.toString();
    Path baseP = base instanceof Path ? (Path) base : URIPath.makeURI(base);
    if (relative instanceof Path)
      return baseP.resolve((Path) relative);
    else
      return baseP.resolve(relative.toString());
  }
}
