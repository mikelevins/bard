// Copyright (c) 1996-2000, 2002, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.io.*;

// Enable JAXP-QName if Symbol should extend javax.xml.namespace.QName.
// This is not the default, even when JAXP-1.3 is defined, because it makes
// symbols bigger and some operations (such as equals and handling of
// uninterned symbols) slower, without much benefit.
// This option needs some work.
/* #ifdef JAXP-QName */
// import javax.xml.namespace.QName;
/* #endif */

/** A Symbol is a name, usually in a specific Namespace.
 * A Symbol is stateless:  Common Lisp-style "value", "function" and
 * "property list" bindings are not part of the Symbol itself, but
 * looked up in the current Environment.
 * A <code>Symbol</code> may be viewed as an <code>EnvironmentKey</code>
 * with a <code>null</code> property component.
 */

public class Symbol
  /* #ifdef JAXP-QName */
  // extends QName
  /* #endif */
  implements
  EnvironmentKey,
  /* #ifdef JAVA2 */
  Comparable,
  /* #endif */
  Externalizable
{
  /* #ifndef JAXP-QName */
  protected String name;
  /* #endif */
  Namespace namespace;

  public final Symbol getKeySymbol () { return this; }
  public final Object getKeyProperty () { return null; }
  public boolean matches (EnvironmentKey key)
  {
    return equals(key.getKeySymbol(), this) && key.getKeyProperty() == null;
  }
  public boolean matches (Symbol symbol, Object property)
  {
    return equals(symbol, this) && property == null;
  }

  /* #ifndef JAXP-QName */
  public final String getNamespaceURI()
  {
    Namespace ns = getNamespace();
    String uri = ns == null ? null : ns.getName();
    return uri == Namespace.UNKNOWN_NAMESPACE ? "" : uri;
  }

  public final String getLocalPart()
  {
    return name;
  }

  public final String getPrefix ()
  {
    Namespace ns = namespace;
    return ns == null ? "" : ns.prefix;
  }
  /* #endif */

  public final boolean hasEmptyNamespace ()
  {
    Namespace ns = getNamespace();
    String nsname;
    return (ns == null
	    || (nsname = ns.getName()) == null || nsname.length() == 0);
  }

    public final boolean hasUnknownNamespace() {
        Namespace ns = getNamespace();
        return ns != null && ns.isUnknownNamespace();
    }

  /** Synonym for getName - the "print name" of the symbol without Namespace.
   * Useful when thinking of a Symbol as an XML QName. */
  public final String getLocalName()
  {
    /* #ifdef JAXP-QName */
    // return getLocalPart();
    /* #else */
    return name;
    /* #endif */
  }

  public final String getName()
  {
    /* #ifdef JAXP-QName */
    // return getLocalPart();
    /* #else */
    return name;
    /* #endif */
  }

  /** Find or create a symbol in a specificed namespace.
   * @param uri a namespace uri.
   * @param name The "local name" or "print name" of the desired symbol.
   * @param prefix namespace prefix, or {@code ""}
   */
  public static Symbol make (String uri, String name, String prefix)
  {
    return Namespace.valueOf(uri, prefix).getSymbol(name.intern());
  }

  /** Find or create a symbol in a specificed namespace.
   * @param namespace can be an Namespace, or a namespace/environment name
   *   (resolved using Namespace.getInstance), or null (in which case
   *   an uninterned symbol is created).
   * @param name The "local name" or "print name" of the desired symbol.
   */
  public static Symbol make (Object namespace, String name)
  {
    Namespace ns = namespace instanceof String
      ? Namespace.valueOf((String) namespace)
      : (Namespace) namespace;
    if (ns == null || name == null)
      return makeUninterned(name);
    return ns.getSymbol(name.intern());
  }

  public static SimpleSymbol valueOf (String name)
  {
    return (SimpleSymbol) Namespace.EmptyNamespace.getSymbol(name.intern());
  }

  public static Symbol valueOf (String name, Object spec)
  {
    if (spec == null || spec == Boolean.FALSE)
      return makeUninterned(name);
    Namespace ns;
    if (spec instanceof Namespace)
      ns = (Namespace) spec;
    else if (spec == Boolean.TRUE)
      ns = Namespace.EmptyNamespace;
    else
      {
        /* #ifdef use:java.lang.CharSequence */
        spec = (CharSequence) spec;
        /* #else */
        //spec = (CharSeq) spec;
        /* #endif */
        ns = Namespace.valueOf(spec.toString());
      }
    return ns.getSymbol(name.intern());
  }

  /* Redundant, and cause method invocation to pick the wrong method.
  public static Symbol valueOf (String name, Namespace namespace)
  {
    return namespace.getSymbol(name.intern());
  }
  public static Symbol valueOf (String name, String namespace)
  {
    return Namespace.valueOf(namespace).getSymbol(name.intern());
  }
  */

  public static Symbol valueOf (String name, String namespace, String prefix)
  {
    return Namespace.valueOf(namespace, prefix).getSymbol(name.intern());
  }

  /** Parse a String as a Symbol.
   * Recognizes:
   * <ul>
   * <li>{@code "{namespace-uri}:local-name"} - which creates a
   * symbol with that namespace-uri and an empty prefix;
   * <li>{@code "{namespace-uri}local-name"} - which is the same as above
   * <li>{@code "prefix{namespace-uri}:local-name"} - which creates a
   * symbok with that prefix and namespace-uri
   * </li>
   * <li>{@code "prefix:local-name"}- which creates a symbol with that prefix
   * and an "unknown" namespace-uri, using {@link #makeWithUnknownNamespace};
   * </li>
   * <li>and plain {@code "local-name"} - which creates a symbol in
   * {@link Namespace#EmptyNamespace}.
   * </li></ul>
   */
  public static Symbol parse (String symbol)
  {
    int slen = symbol.length();
    int lbr = -1, rbr = -1;
    int braceCount = 0;
    int mainStart = 0;
    int prefixEnd = 0;
    for (int i = 0;  i < slen; i++)
      {
        char ch = symbol.charAt(i);
        if (ch == ':' && braceCount == 0)
          {
            prefixEnd = i;
            mainStart = i+1;
            break;
          }
        if (ch == '{')
          {
            if (lbr < 0)
              {
                prefixEnd = i;
                lbr = i;
              }
            braceCount++;
          }
        if (ch == '}')
          {
            braceCount--;
            if (braceCount == 0)
              {
                rbr = i;
                mainStart = (i < slen && symbol.charAt(i+1) == ':') ? i+2 : i+1;
                break;
              }
            if (braceCount < 0) // error
              {
                mainStart = prefixEnd;
                break;
              }
          }
      }
    if (lbr >= 0 && rbr > 0)
      {
        String uri = symbol.substring(lbr+1, rbr);
        String prefix = prefixEnd > 0 ? symbol.substring(0, prefixEnd) : null;
        return Symbol.valueOf(symbol.substring(mainStart), uri, prefix);
      }
    else if (prefixEnd > 0)
      {
        return Symbol.makeWithUnknownNamespace(symbol.substring(mainStart),
                                               symbol.substring(0, prefixEnd));
      }
    else
      {
        return Symbol.valueOf(symbol);
      }
  }

  /** Make a placeholder symbol with a known prefix and unknown namespace-uri.
   * This is convenient for processing definition commands like
   * {@code "prefix:name=value"} - such as on the Kawa command-line -
   * where we don't yet know the namespace-uri.  Code that later looks
   * for a value should look both under the true namespace-uri and
   * the prefix combined with {@link Namespace#makeUnknownNamespace(String)}.
   */
  public static Symbol makeWithUnknownNamespace (String local, String prefix)
  {
    return Namespace.makeUnknownNamespace(prefix).getSymbol(local.intern());
  }

  public Symbol ()
  {
    /* #ifdef JAXP-QName */
    // super("");
    /* #endif */
  }

  public static Symbol makeUninterned (String name)
  {
    /* #ifdef JAXP-QName */
    // Namespace ns = Namespace.getInstance("kawa.gensym");
    // String sname = name;
    // int i = 0;
    // for (;;)
    //   {
    //     int hash = sname.hashCode();
    //     synchronized (ns)
    //       {
    //         Symbol sym = ns.lookup(sname, hash, false);
    //         if (sym == null)
    //           return ns.add(new Symbol(sname.intern(), ns), hash);
    //       }
    //     sname = name + '.' + ++i;
    //   }
    /* #else */
    return new Symbol(name, null);
    /* #endif */
  }

  public static Symbol makeUninterned (String name, Namespace namespace)
  {
    return new Symbol(name, namespace);
  }


  /** Create new Symbol in a given namespace.
   * Does not enter the result in the namespace's symbol table.
   * @param name an interned String
   */
  protected Symbol (String name, Namespace ns)
  {
    /* #ifdef JAXP-QName */
    // super(ns == null ? "" : ns.getName(), name, ns == null ? "" : ns.prefix);
    /* #else */
    this.name = name;
    /* #endif */
    this.namespace = ns;
  }

  public int compareTo(Object o)
  {
    Symbol other = (Symbol) o;
    if (getNamespaceURI() != other.getNamespaceURI())
      throw new IllegalArgumentException("comparing Symbols in different namespaces");
    return getLocalName().compareTo(other.getLocalName());
  }

  public static boolean equals (Symbol sym1, Symbol sym2)
  {
    if (sym1 == sym2)
      return true;
    if (sym1 == null || sym2 == null)
      return false;
    /* #ifdef JAXP-QName */
    // if (sym1.getLocalPart() == sym2.getLocalPart())
    /* #else */
    if (sym1.name == sym2.name)
    /* #endif */
      {
        Namespace namespace1 = sym1.namespace;
        Namespace namespace2 = sym2.namespace;
        // If a namespace is null, it means an uninterned symbol,
        // which is only equals to the same Symbol instance.
        if (namespace1 != null && namespace2 != null)
          return namespace1.name == namespace2.name;
      }
    return false;
  }

  /* #ifndef JAXP-QName */
  /** Just tests for identity.
   * Otherwise hashTables that have Symbols as keys will break. */
  public final boolean equals (Object o)
  {
    return o instanceof Symbol && equals(this, (Symbol) o);
  }

  public int hashCode ()
  {
    return name == null ? 0 : name.hashCode();
  }
  /* #endif */

  public final Namespace getNamespace()
  {
    return namespace;
  }

  public final void setNamespace (Namespace ns)
  {
    namespace = ns;
  }

  /** Conventional value used as a property key for function bindings. */
  public static final Symbol FUNCTION = makeUninterned("(function)");

  /** Conventional value used as a <code>Symbol</code> name to
   * access an <code>Object</code>'s property list.
   * A <dfn>property list</dfn> is a list with a even number of
   * <code>Pair</code>s, containing alternating keys and values.
   * They are used in Common Lisp and Emacs Lisp.
   * Kawa (following XEmacs) allows arbitrary objects to have property lists,
   * thus the PLIST as used as the name and the object as the property.
   * (In the future we'll do somethingg clever so that get(SYMBOL, KEY)
   * as the same as getf(get(PLIST, SYMBOL), KEY) - but much faster.)
   */
  public static final Symbol PLIST = makeUninterned("(property-list)");

  public String toString()
  {
    return toString('P');
  }

  /** Convert a Symbol to a printable String.
   * @param style if 'P' then print prefix if available (otherwise Uri),
                  if 'U' then print Uri if available (otherwise prefix),
                  if '+' then print both if available.
  */
  public String toString(char style)
  {
    // String h = "@"+Integer.toHexString(System.identityHashCode(this));
    String uri = getNamespaceURI();
    String prefix = getPrefix();
    boolean hasUri = uri != null && uri.length() > 0;
    boolean hasPrefix = prefix != null && prefix.length() > 0;
    String name = getName();
    if (hasUri || hasPrefix)
      {
        StringBuilder sbuf = new StringBuilder();
        if (hasPrefix && (style != 'U' || ! hasUri))
          sbuf.append(prefix);
        if (hasUri && (style != 'P' || ! hasPrefix))
          {
            sbuf.append('{');
            sbuf.append(getNamespaceURI());
            sbuf.append('}');
          }
        sbuf.append(':');
        sbuf.append(name);
        return sbuf.toString();
      }
    else
      return name;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    Namespace ns = getNamespace();
    out.writeObject(ns);
    out.writeObject(getName());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    /* #ifdef JAXP-QName */
    // throw new Error("Symbol.readExternal not implemented"); // FIXME!
    /* #else */
    namespace = (Namespace) in.readObject();
    name = (String) in.readObject();
    /* #endif */
  }

  public Object readResolve() throws ObjectStreamException
  {
    if (namespace == null)
      return this;
    return make(namespace, getName());
  }
}
