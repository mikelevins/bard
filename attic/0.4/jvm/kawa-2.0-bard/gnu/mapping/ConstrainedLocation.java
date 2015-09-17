// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public class ConstrainedLocation<T> extends Location<T>
{
  protected Location<T> base;

  protected Procedure converter;

  public static <T> ConstrainedLocation<T> make (Location<T> base,
					  Procedure converter)
  {
    ConstrainedLocation<T> cloc = new ConstrainedLocation<T>();
    cloc.base = base;
    cloc.converter = converter;
    return cloc;
  }

  public Symbol getKeySymbol ()
  {
    return base.getKeySymbol();
  }

  public Object getKeyProperty ()
  {
    return base.getKeyProperty();
  }

  public boolean isConstant ()
  {
    return base.isConstant();
  }

  public final T get ()
  {
    return base.get();
  }
  
  public final T get (T defaultValue)
  {
    return base.get(defaultValue);
  }
  
  public boolean isBound ()
  {
    return base.isBound();
  }

  protected T coerce (T newValue)
  {
    try
      {
          return (T) converter.apply1(newValue);
      }
    catch (Throwable ex)
      {
	throw WrappedException.rethrow(ex);
      }
  }

  public final void set (T newValue)
  {
    base.set(coerce(newValue));
  }

  public void undefine ()
  {
    base.undefine();
  }

  public Object setWithSave (T newValue)
  {
    return base.setWithSave(coerce(newValue));
  }

  public void setRestore (Object oldValue)
  {
    base.setRestore(oldValue);
  }
}

