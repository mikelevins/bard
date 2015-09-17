package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.jemacs.lang.ELisp;

/**
 * A buffer-local variable (Location).
 */

public class BufferLocal extends IndirectableLocation<Object>
{
  boolean all;

  final Symbol name;

  Buffer cachedBuffer;

  /** Index in <code>cachedBuffer</code>'s <code>localBindings</code> array. */
  int cachedIndex;

  BufferLocal (Symbol name, boolean all)
  {
    this.name = name;
    this.all = all;
  }

  public final Symbol getKeySymbol ()
  {
    return name;
  }

  public static BufferLocal make(Symbol symbol, boolean all)
  {
    Environment env = Environment.getCurrent();
    NamedLocation loc = env.getLocation(symbol, null, true);
    Location base = loc.getBase();
    BufferLocal bloc;
    if (base instanceof BufferLocal)
      {
        bloc = (BufferLocal) base;
	if (all)
	  bloc.all = true;
      }
    else
      {
        bloc = new BufferLocal(symbol, all);
        // Not sure if this is 100% correct.  FIXME.
        // We have to be careful to avoid cycles, handle INDIERCT_DEFINES, etc.
        bloc.base = loc.getBaseForce();
        bloc.setAlias(loc);
      }
    return bloc;
  }

  public Object get ()
  {
    Object v = get(Buffer.getCurrent(), Location.UNBOUND);
    if (v == Location.UNBOUND)
        throw new UnboundLocationException(this);
    return v;
  }

  public Object get (Object defaultValue)
  {
    return get(Buffer.getCurrent(), defaultValue);
  }

  public Object get (Buffer buffer, Object defaultValue)
  {
    if (buffer == null)
      return base.get(defaultValue);
    if (this == readOnlyVar)
      return buffer.getReadOnly() ? ELisp.TRUE : ELisp.FALSE;
    Object[] localBindings = buffer.localBindings;
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i > 0)
	  return localBindings[i];
      }
    else if (localBindings != null)
      {
	Symbol n = this.getKeySymbol();
	int len = localBindings.length;
	cachedBuffer = buffer;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (localBindings[i] == n)
	      {
		cachedIndex = ++i;
		return localBindings[i];
	      }
	  }
	cachedIndex = 0;
      }
    return base != null ? base.get(defaultValue)
      : value == Location.UNBOUND ? defaultValue : value;
  }

  public boolean isBound ()
  {
    return isBound(Buffer.getCurrent());
  }

  public boolean isBound (Buffer buffer)
  {
    if (buffer == null)
      return base.isBound();
    if (this == readOnlyVar)
      return true;
    Object[] localBindings = buffer.localBindings;
    Object unb = Location.UNBOUND;
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i >= 0)
	  return localBindings[i] != unb;
      }
    else if (localBindings != null)
      {
	Symbol n = this.getKeySymbol();
	int len = localBindings.length;
	cachedBuffer = buffer;
	for (int i = 0;  i < len;  i += 2)
	  {
	    if (localBindings[i] == n)
	      {
		cachedIndex = ++i;
		return localBindings[i] != unb;
	      }
	  }
	cachedIndex = 0;
      }
    return get(unb) != unb;
  }

  public synchronized final void set (Object newValue)
  {
    set(Buffer.getCurrent(), newValue);
  }

  public synchronized final void set (Buffer buffer, Object newValue)
  {
    if (buffer == null)
      {
        base.set(newValue);
        return;
      }
    if (this == readOnlyVar)
      {
        buffer.setReadOnly(newValue != ELisp.FALSE);
        return;
      }
    Object[] localBindings = buffer.localBindings;
    int avail = -1;
    Symbol n = this.getKeySymbol();
    if (buffer == cachedBuffer)
      {
	int i = cachedIndex;
	if (i >= 0)
	  {
	    localBindings[i] = newValue;
	    return;
	  }
      }
    else if (localBindings != null)
      {
	int len = localBindings.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    Object key = localBindings[i];
	    if (key == n)
	      {
		cachedBuffer = buffer;
		cachedIndex = ++i;
		localBindings[i] = newValue;
		return;
	      }
	    if (key == null)
	      avail = i;
	  }
	cachedIndex = 0;
      }
    if (all)
      {
	if (avail < 0)
	  {
	    if (localBindings == null)
	      {
		localBindings = new Object[20];
		buffer.localBindings = localBindings;
		avail = 0;
	      }
	    else
	      {
		avail = localBindings.length;
		Object[] newBindings = new Object[2 * avail];
		System.arraycopy(localBindings, 0, newBindings, 0, avail);
		buffer.localBindings = localBindings = newBindings;
	      }
	  }
	localBindings[avail] = n;
	cachedBuffer = buffer;
	cachedIndex = ++avail;
	localBindings[avail] = newValue;
      }
    else if (base == null)
      value = newValue;
    else
      base.set(newValue);
  }

  static final BufferLocal readOnlyVar
  = BufferLocal.make(ELisp.getInstance().getSymbol("buffer-read-only"), true);
  static { readOnlyVar.set(null, ELisp.FALSE); }
}
