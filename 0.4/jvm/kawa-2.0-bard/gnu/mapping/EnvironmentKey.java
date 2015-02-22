// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** A pair of a name (a <code>Symbol</code>) and a property (any <code>Object</code>).
 * A <code>Symbol</code> is an <code>EnvironmentKey</code> with
 * a null property component.
 * An <code>Environment</code> is a map from <code>EnvironmentKey</code>s
 * to <code>Location</code>s.
 */

public interface EnvironmentKey
{
  public Symbol getKeySymbol ();
  public Object getKeyProperty ();

  public boolean matches (EnvironmentKey key);
  public boolean matches (Symbol symbol, Object property);

  /** Conventional value used as a property key for function bindings. */
  public static final Object FUNCTION = Symbol.FUNCTION;
}
