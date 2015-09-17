// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location is an abstract cell/location/variable with a value of type T. */

public abstract class Location<T>
{
  /* DEBUGGING
  static int counter;
  public int id=++counter;
  */

  public Location ()
  {
  }

  public Symbol getKeySymbol ()
  {
    return null;
  }

  public Object getKeyProperty ()
  {
    return null;
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(getClass().getName());
    Symbol sym = getKeySymbol();
    sbuf.append('[');
    if (sym != null)
      {
	sbuf.append(sym);
	Object property = getKeyProperty();
	// For a ThreadLocation the property defaults to "this".
	// In that case we'd better not print the property ...
	if (property != null && property != this)
	  {
	    sbuf.append('/');
	    sbuf.append(property);
	  }
      }
    /* DEBUGGING:
    sbuf.append(" #:");
    sbuf.append(id);
    */
    sbuf.append("]");
    return sbuf.toString();
  }

  /** Magic value used to indicate there is no property binding. */
  public static final String UNBOUND = new String("(unbound)");

  public T get (T defaultValue)
  {
    return isBound() ? get() : defaultValue;
  }

  /** Get the current value of this location.
   * @exception UnboundLocationException the location does not have a value. */
  public abstract T get ();

  public abstract void set (T value);

  public void undefine ()
  {
    throw new UnsupportedOperationException ();
  }

  /** Set a value, but return cookie so old value can be restored.
   * This is intended for fluid-let where (in the case of multiple threads)
   * a simple save-restore isn't always the right thing. */
  public Object setWithSave (T newValue)
  {
    Object old = isBound() ? get() : UNBOUND;
    set(newValue);
    return old;
  }

  /** Restore an old value.
   * @param oldValue the return value from a prior setWithSave. */
  public void setRestore (Object oldValue)
  {
    if (oldValue == UNBOUND)
      undefine();
    else
      set((T) oldValue);
  }

  public abstract boolean isBound ();

  public boolean isConstant ()
  {
    return false;
  }

  public Location getBase ()
  {
    return this;
  }

  public final T getValue ()
  {
    return get(null);
  }

  public final T setValue (T newValue)
  {
    T value = get(null);
    set(newValue);
    return value;
  }

  /** True if directly entered in an Environment.  (Only if NamedLocation.) */
  public boolean entered ()
  {
    return false;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print ("#<location ");
    Symbol name = getKeySymbol();
    if (name != null)
      ps.print(name);
    if (isBound())
      {
	ps.print(" -> ");
        try
          {
            ps.print(get());
          }
        catch (Exception ex)
          {
            ps.print("<caught "+ex+">");
          }
      }
    else
      ps.print("(unbound)");
    ps.print ('>');
  }

  // The compiler emits calls to this method.
  public static Location make (Object init, String name)
  {
    ThreadLocation loc = new ThreadLocation(name);
    loc.setGlobal(init);
    return loc;
  }

  // The compiler emits calls to this method.
  public static IndirectableLocation make (String name)
  {
    Symbol sym = Namespace.EmptyNamespace.getSymbol(name.intern());
    PlainLocation loc = new PlainLocation(sym, null);
    loc.base = null;
    loc.value = UNBOUND;
    return loc;
  }

  public static IndirectableLocation make (Symbol name)
  {
    PlainLocation loc = new PlainLocation(name, null);
    loc.base = null;
    loc.value = UNBOUND;
    return loc;
  }

    /** Implement top-level 'define' for Scheme in interactive mode.
     * I.e. if there is no binding, create it;
     * if the existing binding is non-constant, set its value;
     * if it is constant, create a new binding initialized to the old value
     * (though we actually change the binding to non-constant).
     */
    public static Location define(Symbol name) {
        Environment env = Environment.getCurrent();
        int hash = name.hashCode();
        NamedLocation loc = env.getLocation(name, null, hash, true);
        if (loc.isConstant()) {
            Object value = loc.get(Location.UNBOUND);
            loc.value = value;
            loc.base = null;
        }
        return loc;
    }
}
