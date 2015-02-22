// Copyright (c) 2005, 2006, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.mapping.*;
import gnu.xml.*;
import gnu.kawa.io.Path;
import gnu.kawa.xml.*;
import gnu.lists.*;
import java.util.Stack;
import java.net.*;
import gnu.bytecode.ClassType;
import gnu.xquery.lang.XQuery;

public class NodeUtils
{
  public static Object nodeName (Object node)
  {
    if (node == Values.empty || node == null)
      return node;
    if (! (node instanceof KNode))
      throw new WrongType("node-name", 1, node, "node()?");
    Object sym = ((KNode) node).getNodeSymbol();
    if (sym == null)
      return Values.empty;
    else
      return sym;
  }

  public static String name (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    Object name = ((KNode) node).getNodeNameObject();
    if (name == null || name == Values.empty)
      return "";
    return name.toString();
  }

  public static String localName (Object node)
  {
    if (node == Values.empty || node == null)
      return "";
    if (! (node instanceof KNode))
      throw new WrongType("local-name", 1, node, "node()?");
    Object name = ((KNode) node).getNodeNameObject();
    if (name == null || name == Values.empty)
      return "";
    if (name instanceof Symbol)
      return ((Symbol) name).getName();
    return name.toString();
  }

  public static Object namespaceURI (Object node)
  {
    if (node != Values.empty && node != null)
      {
        if (! (node instanceof KNode))
          throw new WrongType("namespace-uri", 1, node, "node()?");
        Object name = ((KNode) node).getNodeNameObject();
        if (name instanceof Symbol)
          return QNameUtils.namespaceURIFromQName(name);
      }
    return "";
  }

  public static void prefixesFromNodetype (XName name, Consumer out)
  {
    NamespaceBinding bindings = ((XName) name).getNamespaceNodes();
    for (NamespaceBinding ns = bindings;
         ns != null;
         ns = ns.getNext())
      {
        String uri = ns.getUri();
        if (uri == null)
          continue; // An "undeclare-namespace" binding.
        String prefix = ns.getPrefix();
        // Check for duplicates.  This is an O(n^2) algorthm, but these
        // lists are usually quite short ...
        for (NamespaceBinding ns2 = bindings;  ; ns2 = ns2.getNext())
           {
             if (ns2 == ns)
               {
                 out.writeObject(prefix == null ? "" : prefix);
                 break;
               }
             if (ns2.getPrefix() == prefix)
               {
                 // Previously written.
                 break;
               }
           }
      }
  }

  public static void inScopePrefixes$X (Object node, CallContext ctx)
  {
    //if (node instanceof KElement)
      {
        KElement element = (KElement) node;
        Object type = element.getNodeNameObject();
        if (type instanceof XName)
          prefixesFromNodetype((XName) type, ctx.consumer);
        else
          ctx.consumer.writeObject("xml");
      }
  }

  public static void data$X (Object arg, CallContext ctx)
  {
    Consumer out = ctx.consumer; 
    if (arg instanceof Values)
      {
        Values vals = (Values) arg;
        int ipos = vals.startPos();
        while ((ipos = vals.nextPos(ipos)) != 0)
          out.writeObject(KNode.atomicValue(vals.getPosPrevious(ipos)));
      }
    else
      out.writeObject(KNode.atomicValue(arg));
  }

  /** Return the root node of the argument. */
  public static Object root (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("root", 1, arg, "node()?");
    KNode node = (KNode) arg;
    return Nodes.root((NodeTree) node.sequence, node.getPos());
  }

  /** Return root node, coerced to a document node.
   * Used to implement '/'-rooted path expressions.
   */
  public static KDocument rootDocument (Object arg)
  {
    if (! (arg instanceof KNode))
      throw new WrongType("root-document", 1, arg, "node()?");
    KNode node = (KNode) arg;
    node = Nodes.root((NodeTree) node.sequence, node.getPos());
    if (! (node instanceof KDocument))
      throw new WrongType("root-document", 1, arg, "document()");
    return (KDocument) node;
  }

  public static String getLang (KNode node)
  {
    NodeTree seq = (NodeTree) node.sequence;
    int attr = seq.ancestorAttribute(node.ipos,
                                     gnu.xml.NamespaceBinding.XML_NAMESPACE,
                                     "lang");
    if (attr == 0)
      return null;
    else
      return KNode.getNodeValue(seq, attr);
  }

  public static boolean lang (Object testlang, Object node)
  {
    String teststr;
    if (testlang == null || testlang == Values.empty)
      teststr = "";
    else
      teststr = TextUtils.stringValue(testlang);
    String lang = getLang((KNode) node);
    if (lang == null)
      return false;
    int langlen = lang.length();
    int testlen = teststr.length();
    if (langlen > testlen && lang.charAt(testlen) == '-')
      lang = lang.substring(0, testlen);
    return lang.equalsIgnoreCase(teststr);
  }

  public static Object documentUri (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("xs:document-uri", 1, arg, "node()?");
    KNode node = (KNode) arg;
    Object uri = ((NodeTree) node.sequence).documentUriOfPos(node.ipos);
    return uri == null ? Values.empty : uri;
  }

  public static Object nilled (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("nilled", 1, arg, "node()?");
    if (! (arg instanceof KElement))
      return Values.empty;
    return Boolean.FALSE;
  }

  public static Object baseUri (Object arg)
  {
    if (arg == null || arg == Values.empty)
      return arg;
    if (! (arg instanceof KNode))
      throw new WrongType("base-uri", 1, arg, "node()?");
    Path uri = ((KNode) arg).baseURI();
    if (uri == null)
      return Values.empty;
    else
      return uri;
  }

  /* #ifdef JAVA5 */
  @SuppressWarnings("unchecked")
  /* #endif */
  /** Extract canditate IDREFs from arg.
   * @return {@code null} (if no {@code IDREF}s);
   *   a {@code String} (if a single {@code IDREF});
   *   or a {@code Stack<String>} (if more than one {@code IDREF}s).
   */
  static Object getIDs (Object arg, Object collector)
  {
    if (arg instanceof KNode)
      arg = KNode.atomicValue(arg);
    if (arg instanceof Values)
      {
        Object[] ar = ((Values) arg).getValues();
        for (int i = ar.length; --i >= 0; )
          collector = getIDs(ar[i], collector);
      }
    else
      {
        String str = StringUtils.coerceToString(arg, "fn:id", 1, "");
        int len = str.length();
        int i = 0;
        while (i < len)
          {
            char ch = str.charAt(i++);
            if (Character.isWhitespace(ch))
              continue;
            int start = XName.isNameStart(ch) ? i - 1 : len;
            while (i < len)
              {
                ch = str.charAt(i);
                if (Character.isWhitespace(ch))
                  break;
                i++;
                if (start < len && ! XName.isNamePart(ch))
                  start = len;
              }
            if (start < len)
              {
                String ref = str.substring(start, i);
                if (collector == null)
                  collector = ref;
                else
                  {
                    Stack st;
                    if (collector instanceof Stack)
                      st = (Stack) collector;
                    else
                      {
                        st = new Stack();
                        st.push(collector);
                        collector = st;
                      }
                    st.push(ref);
                  }
              }
            i++;
          }
      }
    return collector;
  }

  public static void id$X (Object arg1, Object arg2, CallContext ctx)
  {
    KNode node = (KNode) arg2;
    NodeTree ntree = (NodeTree) node.sequence;
    KDocument root
      = (KDocument) Nodes.root(ntree, node.ipos);
    Consumer out = ctx.consumer;
    Object idrefs = getIDs(arg1, null);
    if (idrefs == null)
      return;
    ntree.makeIDtableIfNeeded();
    if (out instanceof PositionConsumer
        && (idrefs instanceof String || out instanceof SortedNodes))
      idScan(idrefs, ntree, (PositionConsumer) out);
    else if (idrefs instanceof String)
      {
        int pos = ntree.lookupID((String) idrefs);
        if (pos != -1)
          out.writeObject(KNode.make(ntree, pos));
      }
    else
      {
        SortedNodes nodes = new SortedNodes();
        idScan(idrefs, ntree, nodes);
        Values.writeValues(nodes, out);
      }
  }

  private static void idScan (Object ids, NodeTree seq, PositionConsumer out)
  {
    if (ids instanceof String)
      {
        int pos = seq.lookupID((String) ids);
        if (pos != -1)
          out.writePosition(seq, pos);
      }
    else if (ids instanceof Stack)
      {
        Stack st = (Stack) ids;
        int n = st.size();
        for (int i = 0;  i < n;  i++)
          idScan(st.elementAt(i), seq, out);
      }
  }

  public static Object idref (Object arg1, Object arg2)
  {
    KNode node = (KNode) arg2;
    KDocument root
      = (KDocument) Nodes.root((NodeTree) node.sequence, node.getPos());
    return Values.empty;
  }

  /** Internal namespace used to manage cached collections. */
  static String collectionNamespace = "http://gnu.org/kawa/cached-collections";

  /** Add a uri-to-value binding that setSavedCollection can later return. */
  public static void setSavedCollection (Object uri, Object value,
                                         Environment env)
  {
    if (uri == null)
      uri = "#default";
    Symbol sym = Symbol.make(collectionNamespace, uri.toString());
    env.put(sym, null, value);
  }

  /** Add a uri-to-value binding that setSavedCollection can later return. */
  public static void setSavedCollection (Object uri, Object value)
  {
    setSavedCollection(uri, value, Environment.getCurrent());
  }

  /** Default resolver for fn:collection.
   * Return nodes previously bound using setSavedCollection.
   */
  public static Object getSavedCollection (Object uri, Environment env)
  {
    if (uri == null)
      uri = "#default";
    Symbol sym = Symbol.make(collectionNamespace, uri.toString());
    Object coll = env.get(sym, null, null);
    if (coll == null)
      throw new RuntimeException("collection '"+uri+"' not found");
    return coll;
  }

  /** Default resolver for fn:collection.
   * Return nodes previously bound using setSavedCollection.
   */
  public static Object getSavedCollection (Object uri)
  {
    return getSavedCollection(uri, Environment.getCurrent());
  }

  /** Symbol used to bind a collection resolver. */
  public static final Symbol collectionResolverSymbol = 
    Symbol.make(XQuery.LOCAL_NAMESPACE, "collection-resolver", "qexo");

  public static Object collection (Object uri, Object base)
    throws Throwable
  {
    uri = resolve(uri, base, "collection");
    Environment env = Environment.getCurrent();
    Symbol rsym = NodeUtils.collectionResolverSymbol;
    Object rvalue = env.get(rsym, null, null);
    if (rvalue == null)
      {
        rvalue = env.get(Symbol.makeWithUnknownNamespace(rsym.getLocalName(),
                                                           rsym.getPrefix()),
                           null, null);
      }
    String str;
    int colon;
    if (rvalue == null)
      {
        return getSavedCollection(uri);
      }
    else if ((rvalue instanceof String || rvalue instanceof UntypedAtomic)
             && (colon = (str = rvalue.toString()).indexOf(':')) > 0)
      {
        String cname = str.substring(0, colon);
        String mname = str.substring(colon+1);
        Class rclass;
        try
          {
            rclass = Class.forName(cname);
          }
        catch (ClassNotFoundException ex)
          {
            throw new RuntimeException("invalid collection-resolver: class "+cname+" not found");
          }
        catch (Exception ex)
          {
            throw new RuntimeException("invalid collection-resolver: "+ex);
          }
        ClassType rclassType = (ClassType) ClassType.make(rclass);
        rvalue = gnu.kawa.reflect.ClassMethods.apply(rclassType, mname, '\0', XQuery.instance);
        if (rvalue == null)
          throw new RuntimeException("invalid collection-resolver: no method "+mname+" in "+cname);
      }
    if (! (rvalue instanceof Procedure))
      throw new RuntimeException("invalid collection-resolver: "+rvalue);
    return ((Procedure) rvalue).apply1(uri);
  }

  static Object resolve (Object uri, Object base, String fname)
    throws Throwable
  {
    if (! (uri instanceof java.io.File)
        && ! (uri instanceof Path)
        /* #ifdef use:java.net.URI */
        && ! (uri instanceof URI)
        /* #endif */
        && ! (uri instanceof URL))
      uri = StringUtils.coerceToString(uri, fname, 1, null);
    if (uri == Values.empty || uri == null)
      return null;
    return Path.currentPath().resolve(Path.valueOf(uri));
  }

  /** Parse an XML document, caching the result.
   * Only positive results are cached; failures are not.)
   * This implements the standard XQuery <code>fn:doc</code> function.
   */
  public static Object docCached (Object uri, Object base)
    throws Throwable
  {
    uri = resolve(uri, base, "doc");
    if (uri == null)
      return Values.empty;
    return Document.parseCached(uri);
  }

  /** Check if an XML document is available, caching the result.
   * Only positive results are cached; failures are not.  Thus it is possible
   * for a false result to be followed by a true result, but not vice versa.
   * This implements the standard XQuery <code>fn:doc-available</code> function.
   */
  public static boolean availableCached (Object uri, Object base)
    throws Throwable
  {
    uri = resolve(uri, base, "doc-available");
    if (uri == null)
      return false;
    try
      {
        Document.parseCached(uri);
        return true;
      }
    catch (Exception ex)
      {
        return false;
      }
  }
}
