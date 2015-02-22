// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import java.util.Hashtable;

/** A mapping from <code>EnvironmentKey</code> to <code>Location</code>s.
 * An <code>EnvironmentKey</code> is either a <code>Symbol</code> or
 * a (<code>Symbol</code>, property)-pair.
 */

public abstract class Environment
  extends PropertySet
  // implements java.util.Map<EnvironmentKey, Object>
{
  static Environment global;

  public static void setGlobal (Environment env)
  {
    global = env;
  }

  public static Environment getGlobal ()
  {
    return global;
  }

  static final int CAN_DEFINE = 1;
  static final int CAN_REDEFINE = 2;

  /** If 'put' can implicitly define an unbound location. */
  static final int CAN_IMPLICITLY_DEFINE = 4;

  /** May be shared by multiple threads. */
  static final int THREAD_SAFE = 8;

  /** If this flag is on, set DIRECT_ON_SET for inherited locations. */
  static final int DIRECT_INHERITED_ON_SET = 16;

  /** Newly defined locations are created in inherited parent environment. */
  public static final int INDIRECT_DEFINES = 32;

  int flags = (CAN_DEFINE|CAN_REDEFINE|CAN_IMPLICITLY_DEFINE
	       |DIRECT_INHERITED_ON_SET);

  public int getFlags () { return flags; }

  public void setFlag (boolean setting, int flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  /** True if new bindings (non-unbound Locations) can be added. */
  public boolean getCanDefine() { return ( flags & CAN_DEFINE) != 0; }
  public void setCanDefine(boolean canDefine)
  { if (canDefine) flags |= CAN_DEFINE; else flags &= ~CAN_DEFINE; }

  /** True if bindings can be removed or replaced by other kinds of Location.*/
  public boolean getCanRedefine() { return ( flags & CAN_REDEFINE) != 0; }
  public void setCanRedefine(boolean canRedefine)
  { if (canRedefine) flags |= CAN_REDEFINE; else flags &= ~CAN_REDEFINE; }

  /** True if this environment is locked - bindings cannot be added or removed. */
  public final boolean isLocked()
  {
    return (flags & (CAN_DEFINE|CAN_REDEFINE)) == 0;
  }

  public void setLocked ()
  {
    flags &= ~(CAN_DEFINE|CAN_REDEFINE|CAN_IMPLICITLY_DEFINE);
  }

  public final void setIndirectDefines ()
  {
    flags |= Environment.INDIRECT_DEFINES;
    ((InheritingEnvironment) this).baseTimestamp = 0x7fffffff;
  }

  /** Return a location bound to (key, property).
   * Create new unbound Location if no such Location exists. */
  public final Location getLocation (Symbol key, Object property)
  {
    return getLocation(key, property, true);
  }

  /** Return a location bound to key (and null property).
   * Create new unbound Location if no such Location exists. */
  public final Location getLocation (Symbol key)
  {
    return getLocation(key, null, true);
  }

  /** Return a location bound to (key, property).
   * Return null if no such Location exists. */
  public final Location lookup (Symbol key, Object property)
  {
    return getLocation(key, property, false);
  }

  public abstract NamedLocation
  lookup (Symbol name, Object property, int hash);

  public final Location lookup (Symbol key)
  {
    return getLocation(key, null, false);
  }

  public abstract NamedLocation getLocation (Symbol key, Object property,
                                             int hash, boolean create);

  public final NamedLocation
  getLocation (Symbol name, Object property, boolean create)
  {
    int hash = name.hashCode() ^ System.identityHashCode(property);
    return getLocation(name, property, hash, create);
  }

  public final Location getLocation (Object key, boolean create)
  {
    Object property = null;
    if (key instanceof EnvironmentKey)
      {
	EnvironmentKey k = (EnvironmentKey) key;
	key = k.getKeySymbol();
	property = k.getKeyProperty();
      }
    Symbol sym = key instanceof Symbol ? (Symbol) key
      : getSymbol((String) key);
    return getLocation(sym, property, create);
  }

  public boolean isBound(Symbol key, Object property)
  {
    Location loc = lookup(key, property);
    if (loc == null)
      return false;
    return loc.isBound();
  }

  public final boolean isBound(Symbol key)
  {
    return isBound(key, null);
  }

  public final boolean containsKey (Object key)
  {
    Object property = null;
    if (key instanceof EnvironmentKey)
      {
	EnvironmentKey k = (EnvironmentKey) key;
	key = k.getKeySymbol();
	property = k.getKeyProperty();
      }
    Symbol sym = key instanceof Symbol ? (Symbol) key
      : getSymbol((String) key);
    return isBound(sym, property);
  }

  /** Get the value bound to the given name.
   * @exception gnu.mapping.UnboundLocationException the name has no binding
   * @see Environment#get(Object)
   */
  public final Object getChecked(String name)
  {
    Object value = get(name, Location.UNBOUND);
    if (value == Location.UNBOUND)
      throw new UnboundLocationException(name+" in "+this);
    return value;
  }

  public Object get (Symbol key, Object property, Object defaultValue)
  {
    Location loc = lookup(key, property);
    if (loc == null)
      return defaultValue;
    return loc.get(defaultValue);
  }

  public final Object get (EnvironmentKey key, Object defaultValue)
  {
    Symbol symbol = key.getKeySymbol();
    Object property = key.getKeyProperty();
    return get(symbol, property, defaultValue);
  }

  public final Object get(String key, Object defaultValue)
  {
    return get(getSymbol(key), null, defaultValue);
  }

  public Object get (Symbol sym)
  {
    Object unb = Location.UNBOUND;
    Object val = get(sym, null, unb);
    if (val == unb)
      throw new UnboundLocationException(sym);
    return val;
  }

  public final Object getFunction (Symbol key, Object defaultValue)
  {
    return get(key, EnvironmentKey.FUNCTION, defaultValue);
  }

  public final Object getFunction (Symbol sym)
  {
    Object unb = Location.UNBOUND;
    Object val = get(sym, EnvironmentKey.FUNCTION, unb);
    if (val == unb)
      throw new UnboundLocationException(sym);
    return val;
  }

  /** Get the value bound to the given name.
   * Returns null if the name has no binding
   * (for compatibility with Java2 Collections framework).
   * @see Environment#getChecked(String)
   */
  public final Object get (Object key)
  {
    Object property = null;
    if (key instanceof EnvironmentKey)
      {
	EnvironmentKey k = (EnvironmentKey) key;
	key = k.getKeySymbol();
	property = k.getKeyProperty();
      }
    Symbol sym = key instanceof Symbol ? (Symbol) key
      : getSymbol((String) key);
    return get(sym, property, null);
  }

  public void put(Symbol key, Object property, Object newValue)
  {
    Location loc = getLocation(key, property);
    if (loc.isConstant()) // FIXME - is this helpful?
      define(key, property, newValue);
    else
      loc.set(newValue);
  }

  public abstract void define (Symbol key, Object property, Object newValue);

  public final void put (Symbol key, Object newValue)
  {
    put(key, null, newValue);
  }

  public final Object put(Object key, Object newValue)
  {
    Location loc = getLocation(key, true);
    Object oldValue = loc.get(null);
    loc.set(newValue);
    return oldValue;
  }

  public final void putFunction(Symbol key, Object newValue)
  {
    put(key, EnvironmentKey.FUNCTION, newValue);
  }

  public final Object put (String key, Object value)
  {
    return put((Object) key, value);
  }

  /** Remove Location from this Environment.
   * Does not explicitly undefine the location itself.
   */
  public Location unlink (Symbol key, Object property, int hash)
  {
    throw new RuntimeException("unsupported operation: unlink (aka undefine)");
  }

  /** Remove Location from this Environment and undefine it. */
  public Object remove (Symbol key, Object property, int hash)
  {
    Location loc = unlink(key, property, hash);
    if (loc == null)
      return null;
    Object value = loc.get(null);
    loc.undefine();
    return value;
 }

  /** Remove and undefine binding.
   * @return Old value
   */
  public final Object remove (EnvironmentKey key)
  {
    Symbol symbol = key.getKeySymbol();
    Object property = key.getKeyProperty();
    int hash = symbol.hashCode() ^ System.identityHashCode(property);
    return remove(symbol, property, hash);
  }

  public final Object remove (Symbol symbol, Object property)
  {
    int hash = symbol.hashCode() ^ System.identityHashCode(property);
    return remove(symbol, property, hash);
  }

  public final void remove (Symbol sym)
  {
    remove(sym, null, sym.hashCode());
  }

  public final void removeFunction (Symbol sym)
  {
    remove(sym, EnvironmentKey.FUNCTION);
  }

  public final Object remove (Object key)
  {
    Object property = null;
    if (key instanceof EnvironmentKey)
      {
	EnvironmentKey k = (EnvironmentKey) key;
	return remove(k.getKeySymbol(), k.getKeyProperty());
      }
    Symbol symbol = key instanceof Symbol ? (Symbol) key
      : getSymbol((String) key);
    int hash = symbol.hashCode() ^ System.identityHashCode(property);
    return remove(symbol, property, hash);
  }

  public Namespace defaultNamespace()
  {
    // FIXME: return get("*package*, Namespace.getDefault())
    return Namespace.getDefault();
  }

  public Symbol getSymbol (String name)
  {
    return defaultNamespace().getSymbol(name);
  }

  static final Hashtable envTable = new Hashtable(50);

  public static Environment getInstance(String name)
  {
    if (name == null)
      name = "";
    synchronized (envTable)
      {
	Environment env = (Environment) envTable.get(name);
	if (env != null)
	  return env;
	env = new SimpleEnvironment ();
	env.setName(name);
	envTable.put(name, env);
	return env;
      }
  }

  /** Does not enumerate inherited Locations. */
  public abstract LocationEnumeration enumerateLocations();

  /** Does enumerate inherited Locations. */
  public abstract LocationEnumeration enumerateAllLocations();

  protected abstract boolean hasMoreElements (LocationEnumeration it);

  /**
    * @deprecated
    */
  public static Environment current () { return getCurrent(); }

  protected static final InheritedLocal curEnvironment = new InheritedLocal();

  public static Environment getCurrent ()
  {
    Environment env = curEnvironment.get();
    if (env == null)
      {
        if (Environment.global == null)
          throw new Error("Environment.global not set - need to do Scheme.registerEnvironment() or similar");
	env = Environment.make(Thread.currentThread().getName(), Environment.global);
        env.flags |= Environment.THREAD_SAFE;
        curEnvironment.set(env);
      }
    return env;
  }

  public static void setCurrent (Environment env)
  {
    curEnvironment.set(env);
  }

  public static Environment setSaveCurrent (Environment env)
  {
    Environment save = curEnvironment.get();;
    curEnvironment.set(env);
    return save;
  }

  public static void restoreCurrent (Environment saved)
  {
    curEnvironment.set(saved);
  }

  public static Environment user () { return getCurrent(); }

  public final void addLocation (NamedLocation loc)
  {
    addLocation(loc.getKeySymbol(), loc.getKeyProperty(), loc);
  }

  public abstract NamedLocation addLocation (Symbol name, Object prop, Location loc);

  public final void addLocation (EnvironmentKey key, Location loc)
  {
    addLocation(key.getKeySymbol(), key.getKeyProperty(), loc);
  }

  public static SimpleEnvironment make ()
  {
    return new SimpleEnvironment();
  }

  public static SimpleEnvironment make (String name)
  {
    return new SimpleEnvironment(name);
  }

  public static InheritingEnvironment make (String name, Environment parent)
  {
    return new InheritingEnvironment(name, parent);
  }

  public String toString ()
  {
    return "#<environment "+getName()+'>';
  }

  /** Overridden in sub-classes - useful for more verbose debug output. */
  public String toStringVerbose ()
  {
    return toString();
  }

  SimpleEnvironment cloneForThread ()
  {
    InheritingEnvironment env = new InheritingEnvironment(null, this);

    // There is no point for a lookup to search this Environment,
    // since its bindings will be cloned.  Instead patch the new Environment
    // so it inherits from its grandparents instead.
    if (this instanceof InheritingEnvironment)
      {
        InheritingEnvironment p = (InheritingEnvironment) this;
        int numInherited = p.numInherited;
        env.numInherited = numInherited;
        env.inherited = new Environment[numInherited];
        for (int i = 0;  i < numInherited;  i++)
          env.inherited[i] = p.inherited[i];
      }

    LocationEnumeration e = enumerateLocations();
    while (e.hasMoreElements())
      {
        Location loc = e.nextLocation();

        Symbol name = loc.getKeySymbol();
        Object property = loc.getKeyProperty();
        if (name != null && loc instanceof NamedLocation)
          {
            NamedLocation nloc = (NamedLocation) loc;
            if (nloc.base == null)
              {
                SharedLocation sloc = new SharedLocation(name, property, 0);
                sloc.value = nloc.value;
                nloc.base = sloc;
                nloc.value = null;
                nloc = sloc;
              }
            int hash = name.hashCode() ^ System.identityHashCode(property);
            NamedLocation xloc = env.addUnboundLocation(name, property, hash);
            xloc.base = nloc;
          }
      }
    return env;
  }

  static class InheritedLocal extends InheritableThreadLocal<Environment>
  {
    protected Environment childValue(Environment parentValue)
    {
      if (parentValue == null)
        parentValue = getCurrent();
      SimpleEnvironment env = parentValue.cloneForThread();
      env.flags |= Environment.THREAD_SAFE;
      env.flags &= ~Environment.DIRECT_INHERITED_ON_SET;
      return env;
    }
  }
}
