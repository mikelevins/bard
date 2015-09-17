// Copyright (c) 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

public abstract class IndirectableLocation<T> extends Location<T>
{
  /** If <code>value==DIRECT_ON_SET</code>, break indirection on a <code>set</code>. */
  protected static final Object DIRECT_ON_SET = new String("(direct-on-set)");

  /** If <code>value</code> has this value, force indirection even
   * for the <code>setWithSave</code> operation.
   * Ignoring the restore aspect of a <code>fluid-let</code>, it is normally
   * treated as closer to a <code>define</code> than to a <code>set</code>,
   * in that we break the sharing with another <code>Environment</code>.
   * Setting <code>value</code> to <code>INDIRECT_FLUIDS</code> means we do
   * <em>not</em> want to break the indirection in this case. */
  protected static final Object INDIRECT_FLUIDS = new String("(indirect-fluids)");

  /** If non-null, operations are forwarded to the base location. */
  protected Location<T> base;

  /** If <code>base</code> is null, the current value stored in
   * this <code>Location</code>.
   * If <code>base</code> is non-null, then <code>value</code> is generally
   * ignored.  However, the special value <code>DIRECT_ON_SET</code> means that
   * writes change <code>value</code> directly, instead of setting
   * the value of <code>base</code>.
   */
  protected Object value;

  public Symbol getKeySymbol ()
  {
    return base != null ? base.getKeySymbol() : null;
  }

  public Object getKeyProperty ()
  {
    return base != null ? base.getKeyProperty() : null;
  }

  public boolean isConstant ()
  {
    return base != null && base.isConstant();
  }

  public Location getBase ()
  {
    return base == null ? this : base.getBase();
  }

  public Location getBaseForce ()
  {
    if (base == null)
      return new PlainLocation(getKeySymbol(), getKeyProperty(), value);
    else
      return base;
  }

  public void setBase (Location base)
  { 
    this.base = base;
    this.value = null;
  }

  /** Define this Location as an alias for some other Location. */
  public void setAlias (Location base)
  { 
    this.base = base;
    this.value = INDIRECT_FLUIDS;
  }

  public void undefine ()
  {
    base = null;
    value = UNBOUND;
  }

  public Environment getEnvironment ()
  {
    return (base instanceof NamedLocation
	    ? ((NamedLocation) base).getEnvironment()
	    : null);
  }
}
