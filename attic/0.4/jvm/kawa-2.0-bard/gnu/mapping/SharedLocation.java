// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A Location suitable when Environment or Location can be access by
 * multiple threads.  Accesses are synchronized. */

public class SharedLocation<T> extends NamedLocation<T>
{
  int timestamp;

  public SharedLocation (Symbol symbol, Object property, int timestamp)
  {
    super(symbol, property);
    this.timestamp = timestamp;
  }

  public synchronized final T get ()
  {
    if (base != null) return base.get();
    if (value == Location.UNBOUND) throw new UnboundLocationException(this);
    return (T) value;
  }

  public synchronized final T get (T defaultValue)
  {
    return base != null ? base.get(defaultValue)
        : value == Location.UNBOUND ? defaultValue : (T) value;
  }

  public synchronized boolean isBound ()
  {
    return base != null ? base.isBound() : value != Location.UNBOUND;
  }

  public synchronized final void set (T newValue)
  {
    if (base == null)
      value = newValue;
    else if (value == DIRECT_ON_SET)
      {
	base = null;
	value = newValue;
      }
    else if (base.isConstant())
      getEnvironment().put(getKeySymbol(), getKeyProperty(), newValue);
    else
      base.set(newValue);
  }

}
