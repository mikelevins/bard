package gnu.mapping;
import gnu.lists.*;

/** Used to implement Lisp-style "property lists".
 * A <code>PropertyLocation</code> is a location whose value is the
 * <code>car</code> of a property list.
 * This class also contains a number of static methods useful for
 * working with property lists.
 */

public class PropertyLocation extends Location<Object>
{
  //  Location plist;

  Pair pair;

  public final Object get ()
  {
    return pair.getCar();
  }

  public boolean isBound ()
  {
    return true;
  }

  public final void set (Object newValue)
  {
    pair.setCar(newValue);
  }

  /** Get the property list assocated with an object in a given Environment.
   * @param symbol Usually but not necessarily a Symbol.
   * (A String is <em>not </em> converted a Symbol by this method.)
   */
  public static Object getPropertyList (Object symbol, Environment env)
  {
    return env.get(Symbol.PLIST, symbol, LList.Empty);
  }

  /** Get the property list associated with object in the current Environment.
   * Corresponds to Common Lisp's <code>symbol-plist</code> function.
   * @param symbol Usually but not necessarily a Symbol.
   * (A String is <em>not </em> converted a Symbol by this method.)
   */
  public static Object getPropertyList (Object symbol)
  {
    return Environment.getCurrent().get(Symbol.PLIST, symbol, LList.Empty);
  }

  /** Set the property list assocated with an object in a given Environment.
   * This function should be avoided, since a Symbol's property list may
   * be used by unknown classes.  It also can be slow.
   * @param symbol Usually but not necessarily a Symbol.
   * (A String is <em>not </em> converted a Symbol by this method.)
   */
  public static void setPropertyList (Object symbol, Object plist,
				      Environment env)
  {
    synchronized (env)
      {
	Location lloc = env.lookup(Symbol.PLIST, symbol);
	if (symbol instanceof Symbol)
	  {
	    Symbol sym = (Symbol) symbol;
	    Object old = lloc.get(LList.Empty);
	    Object p = old;
	    // Remove old PropertyLocation bindings not in plist.
	    for (;;)
	      {
		if (! (p instanceof Pair))
		  break;
		Pair pair = (Pair) p;
		Object property = pair.getCar();
		if (plistGet(plist, property, null) != null)
		  env.remove(sym, property);
		p = ((Pair) pair.getCdr()).getCdr();
	      }
	    // Add/update PropertyLocation bindings from plist.
	    p = plist;
	    for (;;)
	      {
		if (! (p instanceof Pair))
		  break;
		Pair pair = (Pair) p;
		Object property = pair.getCar();
		Location loc = env.lookup(sym, property);
		PropertyLocation ploc;
		if (loc != null
		    && (loc = loc.getBase()) instanceof PropertyLocation)
		  {
		    ploc = (PropertyLocation) loc;
		  }
		else
		  {
		    ploc = new PropertyLocation();
		    env.addLocation(sym, property, ploc);
		  }
		Pair valuePair = (Pair) pair.getCdr();
		ploc.pair = valuePair;
		//ploc.plist = plist;
		p = valuePair.getCdr();
	      }
	  }
	lloc.set(plist);
      }
  }

  /** Set the property list assocated with an object in a given Environment.
   * Corresponds to Common Lisp's <code>(setf symbol-plist)</code> function.
   * @see #setPropertyList(Object, Object, Environment).
   */
  public static void setPropertyList (Object symbol, Object plist)
  {
    setPropertyList(symbol, plist, Environment.getCurrent());

  }

  /** Gets a property value associated with an object.
   * @param symbol Usually a <code>Symbol</code>, but can be any
   *   <code>Object>/code>.  A <code>String</code> is converted to a
   *   <code>Symbol</code> using <code>env.getSymbol()</code>.
   *   Symbols require a constant-type hash lookup; other object
   *   are serached linearly.
   */
  public static Object getProperty (Object symbol, Object property,
				    Object defaultValue, Environment env)
  {
    if (! (symbol instanceof Symbol))
      {
	if (symbol instanceof String)
	  symbol = Namespace.getDefaultSymbol((String) symbol);
	else
	  return plistGet(env.get(Symbol.PLIST, symbol, LList.Empty),
			  property, defaultValue);
      }
    return env.get((Symbol) symbol, property, defaultValue);
  }

  /** Gets a property value associated with an object.
   * Corresponds to Common Lisp's <code>get</code> function.
   * @see #getProperty(Object, Object, Object, Environment).
   */
  public static Object getProperty (Object symbol, Object property,
				    Object defaultValue)
  {
    return getProperty(symbol, property,
		       defaultValue, Environment.getCurrent());
  }

  public static void putProperty (Object symbol, Object property,
				  Object newValue, Environment env)
  {
    if (! (symbol instanceof Symbol))
      {
	if (symbol instanceof String)
	  symbol = Namespace.getDefaultSymbol((String) symbol);
	else
	  {
	    Location lloc = env.getLocation(Symbol.PLIST, symbol);
	    lloc.set(plistPut(lloc.get(LList.Empty), property, newValue));
	    return;
	  }
      }
    Location loc = env.lookup((Symbol) symbol, property);
    if (loc != null && (loc = loc.getBase()) instanceof PropertyLocation)
      ((PropertyLocation) loc).set(newValue);
    else
      {
	Location lloc = env.getLocation(Symbol.PLIST, symbol);
	Object plist = lloc.get(LList.Empty);
	Pair pair = null;
	/*
	pair = null;
	if (plist instanceof Pair)
	  {
	    pair = (Pair) plist;
	    for (;;)
	      {
		if (pair.getCar() == property)
		  break;
		if (pair.getCdr() instanceof Pair)
		  pair = (Pair) pair.getCdr();
		else
		  {
		    pair = null;
		    break;
		  }
	      }
	  }
	if (pair == null)
	  plist = pair = new Pair(property, new Pair(newValue, plist));
	*/
	pair = new Pair(newValue, plist);
	plist = new Pair(property, pair);
	lloc.set(plist);
	PropertyLocation ploc = new PropertyLocation();
	//ploc.plist = lloc;
	ploc.pair = pair;
	env.addLocation((Symbol) symbol, property, ploc);
      }
  }

  /** Sets a property value associated with an object.
   * Corresponds to Common Lisp's <code>(setf get)</code> function.
   */
  public static void putProperty (Object symbol, Object property,
				  Object newValue)
  {
    putProperty(symbol, property, newValue, Environment.getCurrent());
  }

  /** Remove a properaty assocatied with an object.
   */
  public static boolean removeProperty (Object symbol, Object property,
					Environment env)
  {
    Location ploc = env.lookup(Symbol.PLIST, symbol);
    if (ploc == null)
      return false;
    Object plist = ploc.get(LList.Empty);
    if (! (plist instanceof Pair))
      return false;
    Pair pair = (Pair) plist;
    Pair prev = null;
    for (;;)
      {
	if (pair.getCar() == property)
	  break;
	Object next = pair.getCdr();
	if (! (next instanceof Pair))
	  return false;
	prev = pair;
	pair = (Pair) next;
      }
    Object tail = ((Pair) pair.getCdr()).getCdr();
    if (prev == null)
      {
	plist = tail;
	ploc.set(plist);
      }
    else
      prev.setCdr(tail);
    if (symbol instanceof Symbol)
      env.remove((Symbol) symbol, property);
    return true;
  }

  /** Remove a property associated with an object.
   * Corresponds to Common Lisp's <code>remprop</code> function.
   */
  public static boolean removeProperty (Object symbol, Object property)
  {
    return removeProperty(symbol, property, Environment.getCurrent());
  }

  /**
   * Given a property list and a key, find the corresponding property value.
   */
  public static Object plistGet(Object plist, Object prop, Object dfault)
  {
    while (plist instanceof Pair)
      {
	Pair pair = (Pair) plist;
	if (pair.getCar() == prop)
	  return ((Pair) pair.getCdr()).getCar();
      }
    return dfault;
  }

  /** Modify and add a property binding to a property list.
   * @return The updated property list.
   */
  public static Object plistPut(Object plist, Object prop, Object value)
  {
    for (Object p = plist; p instanceof Pair; )
      {
	Pair pair = (Pair) p;
	Pair next = (Pair) pair.getCdr();
	if (pair.getCar() == prop)
	  {
	    next.setCar(value);
	    return plist;
	  }
	p = next.getCdr();
      }
    return new Pair(prop, new Pair(value, plist));
  }

  /** Remove a property binding from a property list.
   * @return The updated property list.
   */
  public static Object plistRemove(Object plist, Object prop)
  {
    Pair prev = null;
    for (Object p = plist; p instanceof Pair; )
      {
	Pair pair = (Pair) p;
	Pair next = (Pair) pair.getCdr();
	p = next.getCdr();
	if (pair.getCar() == prop)
	  {
	    if (prev == null)
	      return p;
	    prev.setCdr(p);
	    return plist;
	  }
	prev = next;
      }
    return plist;
  }
}
