// Copyright (c) 1996-2000, 2001, 2002, 2004, 2012  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.kawa.util.*;
import java.util.*;
import java.io.*;

/** A mapping from strings ("print names") to <code>Symbol</code>s.
 * Namespaces are normally named and can be accessed from a global table.
 * They correspond to Common Lisp "packages" (which are implemented
 * using <code>gnu.kawa.lispexpr.LispPackage</code>,
 * which extends <code>Namespace</code>).
 * A <code>Namespace</code> is a "weak" mapping in the sense that a
 * <code>Symbol</code> can be garbage collected even though it is
 * referenced from a <code>Namespace</code>.
 * @author	Per Bothner
 */

public class Namespace
  extends AbstractHashTable<SymbolRef, String, Symbol>
  implements Externalizable, HasNamedParts
{
  /** Map namepsace names (and nick-names) to Namespaces. */
  protected static final Hashtable nsTable = new Hashtable(50);

  /** The Namespace with the empty name. */
  public static final Namespace EmptyNamespace = valueOf("");

  /** Should be interned. */
  String name;

  protected String prefix = "";

  /** Get the name of this Namespace. */
  public final String getName () { return name; }
  /** Set the name of this Namespace. */
  public final void setName (String name) { this.name = name; }

  public final String getPrefix () { return prefix; }

  public Namespace()
  {
    this(64);
  }

  protected Namespace (int capacity)
  {
    super(capacity);
  }

  public static Namespace create (int capacity)
  {
    return new Namespace(capacity);
  }

  public static Namespace create ()
  {
    return new Namespace(64);
  }

  public static Namespace getDefault ()
  {
    return EmptyNamespace;
  }

  public static Symbol getDefaultSymbol (String name)
  {
    return EmptyNamespace.getSymbol(name);
  }

  public static Namespace valueOf ()
  {
    return EmptyNamespace;
  }

  /** Return Namespace with the given name (namespace-URI).
   *  Create it if needed.
   */
  public static Namespace valueOf (String name)
  {
    if (name == null)
      name = "";
    synchronized (nsTable)
      {
	Namespace ns = (Namespace) nsTable.get(name);
	if (ns != null)
	  return ns;
	ns = new Namespace ();
	ns.setName(name.intern());
	nsTable.put(name, ns);
	return ns;
      }
  }

    /** Return Namespace with the given name (namespace-URI), if it exists.
     * Return null if no such namespace exists.
     */
    public static Namespace valueOfNoCreate(String name) {
        if (name == null)
            name = "";
        return (Namespace) nsTable.get(name);
    }

  public static Namespace valueOf (String uri, String prefix)
  {
    if (prefix == null || prefix.length() == 0)
      return valueOf(uri);
    String xname = prefix + " -> "+ uri;
    synchronized (nsTable)
      {
	Object old = nsTable.get(xname);
	if (old instanceof Namespace)
	  return (Namespace) old;
	Namespace ns = new Namespace();
        if (uri != UNKNOWN_NAMESPACE)
            uri = uri.intern();
        ns.setName(uri);
        ns.prefix = prefix.intern();
	nsTable.put(xname, ns);
	return ns;
      }
  }

  public static Namespace valueOf (String uri, SimpleSymbol prefix)
  {
    return valueOf(uri, prefix == null ? null : prefix.getName());
  }

  public static final String UNKNOWN_NAMESPACE = new String("$unknown$");

  /** A namespace with known prefix but unknown uri. */
  public boolean isUnknownNamespace() {
      return name == UNKNOWN_NAMESPACE;
    }

    /** Create a "placeholder" for a namespace with a known prefix
     * but unknown uri.
     * @see Symbol#makeWithUnknownNamespace
     */
    public static Namespace makeUnknownNamespace (String prefix) {
        String uri = prefix == null || prefix == "" ? ""
            : UNKNOWN_NAMESPACE;
        return Namespace.valueOf(uri, prefix);
    }

  public Object get (String key)
  {
    return Environment.getCurrent().get(getSymbol(key));
  }

  public boolean isConstant (String key)
  {
    return false;
  }

  /** Get a Symbol matching the given name.
   * Creates a new Symbol if one is not found.
   * Equivalent to Common Lisp's "intern" function.
   */
  public Symbol getSymbol (String key)
  {
    return lookup(key, key.hashCode(), true);
  }

  /** Get a Symbol matching the given name.
   * Returns null if one is not found.
   */
  public Symbol lookup(String key)
  {
    return lookup(key, key.hashCode(), false);
  }

  /** Search for an existing Symbol with the give name.
   * @param key String - does not need to be interned.
   */
  protected final Symbol lookupInternal(String key, int hash)
  {
    int index = hashToIndex(hash);
    SymbolRef prev = null;
    for (SymbolRef ref = table[index];  ref != null;  )
      {
	SymbolRef next = ref.next;
	Symbol sym = ref.getSymbol();
	if (sym == null)
	  {
	    // Weakly referenced object has been collected.
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = next;
	    num_bindings--;
	  }
	else
	  {
	    if (sym.getLocalPart().equals(key))
	      return sym;
	    prev = ref;
	  }
	ref = next;
      }
    return null;
  }

  public Symbol add(Symbol sym, int hash)
  {
    put(sym.getName(), hash, sym);
    return sym;
  }

  public Symbol get (Object key, Symbol defaultValue)
  {
    if (key instanceof String)
      {
        Symbol sym = lookup((String) key, key.hashCode(), false);
        if (sym != null)
          return sym;
      }
      return defaultValue;
  }

  public Symbol lookup(String key, int hash, boolean create)
  {
    synchronized (this)
      {
	Symbol sym = lookupInternal(key, hash);
	if (sym != null)
	  return sym;

	/*
	// Do we need to synchronize on nsTable as well?  FIXME
	for (NamespaceUse used = imported;  used != null;
	     used = used.nextImported)
	  {
	    el = lookup(key, hash, false);
	    if (el != null)
	      return el;
	  }
	*/
	if (create)
          {
            if (this == EmptyNamespace)
              sym = new SimpleSymbol(key);
            else
              sym = new Symbol(key, this);
            return add(sym, hash);
          }
	else
	  return null;
      }
  }

  public boolean remove (Symbol symbol)
  {
    synchronized (this)
      {
	String name = symbol.getLocalPart();
        return remove(name) != null;
      }
  }

    protected int getEntryHashCode (SymbolRef entry) { return entry.hashCode(); }
    /** Extract next Entry in same hash-bucket. */
    protected SymbolRef getEntryNext (SymbolRef entry) { return entry.next; }
    /** Set next Entry in same hash-bucket. */
    protected void setEntryNext (SymbolRef entry, SymbolRef next) { entry.next = next; }
    /** Allocate Entry[n]. */
    protected SymbolRef[] allocEntries(int n) { return new SymbolRef[n]; }

    protected SymbolRef makeEntry (String key, int hash, Symbol value) {
        return new SymbolRef(value);
    }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(prefix);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    name = ((String) in.readObject()).intern();
    prefix = (String) in.readObject();
  }

  public Object readResolve() throws ObjectStreamException
  {
    String name = getName();
    if (name != null)
      {
        String xname = (prefix == null || prefix.length() == 0 ? name
                        : (prefix + " -> "+ name));
	Namespace ns = (Namespace) nsTable.get(xname);
	if (ns != null)
	  return ns;
	nsTable.put(xname, this);
      }
    return this;
   
  }

  public String toString()
  {
    StringBuilder sbuf = new StringBuilder("#,(namespace \"");
    sbuf.append(name);
    sbuf.append('\"');
    if (prefix != null && prefix != "")
      {
        sbuf.append(' ');
        sbuf.append(prefix);
      }
    sbuf.append(')');
    return sbuf.toString();
  }
}

/** A week reference to a <code>Symbol</code>.
 * This is to allow <code>Symbol</code>s to be garbage collected,
 * even though they're referenced from a <code>Namespace</code>. */

class SymbolRef
  extends java.lang.ref.WeakReference<Symbol>
  implements Map.Entry<String,Symbol>
{
    SymbolRef next;

    String getName () {
        Symbol sym = getSymbol();
        return sym == null ? null : sym.getName();
    }

    SymbolRef (Symbol sym) {
        super(sym);
    }

    public String getKey() {
        Symbol sym = getSymbol();
        return sym == null ? null : sym.getName();
    }

    public Symbol getValue() {
        return getSymbol();
    }

    public int hashCode() {
        Symbol sym = getSymbol();
        return sym == null ? 0 : sym.hashCode();
    }

    public Symbol setValue(Symbol value) {
        throw new UnsupportedOperationException();
    }

    public boolean equals (Object o) {
        return o instanceof SymbolRef
            && get() == ((SymbolRef) o).get();
    }

    Symbol getSymbol() {
        return get();
    }

    public String toString() {
        return "SymbolRef["+getSymbol()+"]";
    }
}
