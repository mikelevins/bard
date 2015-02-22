package gnu.mapping;

/** PropertySet keys that provide statically-typeable values. */

public class PropertyKey<T>
{
  String name;

  public PropertyKey(String name)
  {
    this.name = name;
  }

  /** Get the value associated with this key in a given {@code PropertySet}.
   * Return {@code defaultValue} if there is no association for this key.
   */
  @SuppressWarnings("unchecked")
  public T get(PropertySet container, T defaultValue)
  {
    return (T) container.getProperty(this, defaultValue);
  }

  /** Get the value associated with this key in a given {@code PropertySet}.
   * Return null if there is no association for this key.
   */
  public final T get(PropertySet container)
  {
    return get(container, null);
  }

  public void set (PropertySet container, T value)
  {
    container.setProperty(this, value);
  }
}

