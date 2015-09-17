// Copyright (c) 2005, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location that forwards to a thread-specific Location.
 */

public class ThreadLocation<T> extends NamedLocation<T> implements Named {
    static int counter;
    private static synchronized int nextCounter() { return ++counter; }

    /** Used for a binding that was explicitly set to null. */
    static final Object NULL_PROXY = new Object();

    private ThreadLocal thLocal;

    private boolean stringName;

    private boolean importedThreadLocal;
    private boolean importedThreadLocal() { return importedThreadLocal; }

    /** A new anonymous fluid location. */
    public ThreadLocation() {
        this("param#"+nextCounter());
    }

    /** A new anonymous fluid location but used a given name for printing.
     * However, the binding is not bound to the name as a visible binding. */
    public ThreadLocation(String name) {
        this(Symbol.makeUninterned(name));
    }

    public ThreadLocation(Symbol name) {
        super(name, null);
        thLocal = new ThreadLocalWithDefault<Object>(null);
    }

    public ThreadLocation(Symbol name, ThreadLocal<T> thLocal) {
        super(name, null);
        this.thLocal = thLocal;
        importedThreadLocal = true;
    }

    /** Create a fresh ThreadLocation, independent of other ThreadLocations.
     * @param name used for printing, but not identification.
     */
    public static ThreadLocation makeAnonymous(String name) {
        ThreadLocation loc = new ThreadLocation(name);
        loc.stringName = true;
        return loc;
    }

    /** Create a fresh ThreadLocation, independent of other ThreadLocations.
     * @param name used for printing, but not identification.
     */
    public static ThreadLocation makeAnonymous(Symbol name) {
        return new ThreadLocation(name);
    }

    /** Set the default/global value. */
    public void setGlobal(T value) {
        synchronized (this) {
            ((ThreadLocalWithDefault) thLocal).setDefault(value);
        }
    }

    public T get() {
        Object value = thLocal.get();
        if (importedThreadLocal())
            return (T) value;
        if (value == Location.UNBOUND)
            throw new UnboundLocationException(this);
        return value == NULL_PROXY ? null : (T) value;
    }

    public T get(T defaultValue) {
        Object value = thLocal.get();
        if (importedThreadLocal())
            return (T) value;
        if (value == UNBOUND)
            return defaultValue;
        return value == NULL_PROXY ? null : (T) value;
    }

    public boolean isBound() {
        return importedThreadLocal() || thLocal.get() != Location.UNBOUND;
    }

    public void set(T value) {
        thLocal.set(value == null && ! importedThreadLocal() ? NULL_PROXY
                    : value);
    }

    public Object setWithSave (T newValue) {
        Object old = thLocal.get();
        // Don't inline set, since set may be overridden.
        set(newValue);
        return old;
    }

    public void setRestore(Object oldValue) {
        thLocal.set(oldValue);
    }

    public void undefine() {
        if (importedThreadLocal())
            thLocal.remove();
        else
            thLocal.set(UNBOUND); // FIXME - maybe use remove?
    }

    public String getName () { return name == null ? null : name.toString(); }

    public Object getSymbol () { // Implements Named
        // If this was allocated using makeAnonymous(String) it is better
        // to return the original String, rather than a generated Symbol.
        // One motivation is when a module is imported with a specified
        // namespace URI (only in XQuery at this point); we want to use
        // the latter namespace.
        if (stringName)
            return name.toString();
    return name;
  }
  public void setName (String name)
  { throw new RuntimeException("setName not allowed"); }

    static class ThreadLocalWithDefault<T> extends InheritableThreadLocal<T> {
        T defaultValue;

        public ThreadLocalWithDefault(T defaultValue) {
            this.defaultValue = defaultValue;
        }

        public void setDefault(T value) { defaultValue = value; }

        protected T initialValue() { return defaultValue; }
    }
}
