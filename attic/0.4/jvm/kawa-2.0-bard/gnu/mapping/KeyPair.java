// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A simple concrete implemementation of <code>EnvironmentKey</code>. */

public class KeyPair implements EnvironmentKey
{
  Symbol name;
  Object property;

  public KeyPair (Symbol name, Object property)
  {
    this.name = name;
    this.property = property;
  }

  public Symbol getKeySymbol () { return name; }
  public Object getKeyProperty () { return property; }

  public final boolean matches (EnvironmentKey key)
  {
    return Symbol.equals(key.getKeySymbol(), this.name)
      && key.getKeyProperty() == this.property;
  }

  public final boolean matches (Symbol symbol, Object property)
  {
    return Symbol.equals(symbol, this.name) && property == this.property;
  }

  public boolean equals (Object x)
  {
    if (! (x instanceof KeyPair))
      return false;
    KeyPair e2 = (KeyPair) x;
    return property == e2.property
      && (name == null ? e2.name == null : name.equals(e2.name));
  }

  public int hashCode ()
  {
    return name.hashCode() ^ System.identityHashCode(property);
  }

  public String toString () { return "KeyPair[sym:"+name+" prop:"+property+"]"; }
}
