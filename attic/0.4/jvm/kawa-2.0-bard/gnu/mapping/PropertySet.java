package gnu.mapping;

public abstract class PropertySet implements Named
{
  /** If non-null, a sequence of (key, value)-pairs. */
  private Object[] properties;

  public static final Symbol nameKey = Namespace.EmptyNamespace.getSymbol("name");

  public String getName()
  {
    Object symbol = getProperty(nameKey, null);
    return symbol == null ? null
      : symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }

  public Object getSymbol()
  {
    return getProperty(nameKey, null);
  }

  public final void setSymbol (Object name)
  {
    setProperty(nameKey, name);
  }

  public final void setName (String name)
  {
    setProperty(nameKey, name);
  }

  public Object getProperty(Object key, Object defaultValue)
  {
    if (properties != null)
      {
	for (int i = properties.length;  (i -= 2) >= 0; )
	  {
	    if (properties[i] == key)
	      return properties[i + 1];
	  }
      }
    return defaultValue;
  }

  public synchronized void setProperty(Object key, Object value)
  {
    properties = PropertySet.setProperty(properties, key, value);
  }

  /** Given a property list, update it.
   * @param properties the input property list
   * @param key
   * @param value associate this with key in result
   * @return updated property list (maybe the same as the input)
   */
  public static Object[] setProperty(Object[] properties,
				     Object key, Object value)
  {
    int avail;
    Object[] props = properties;
    if (props == null)
      {
	properties = props = new Object[10];
	avail = 0;
      }
    else
      {
	avail = -1;
	for (int i = props.length;  (i -= 2) >= 0; )
	  {
	    Object k = props[i];
	    if (k == key)
	      {
		Object old = props[i + 1];
		props[i + 1] = value;
		return properties;
	      }
	    else if (k == null)
	      avail = i;
	  }
	if (avail < 0)
	  {
	    avail = props.length;
	    properties = new Object[2 * avail];
	    System.arraycopy(props, 0, properties, 0, avail);
	    props = properties;
	  }
      }
    props[avail] = key;
    props[avail+1] = value;
    return properties;
  }

  public Object removeProperty(Object key)
  {
    Object[] props = properties;
    if (props == null)
      return null;
    for (int i = props.length;  (i -= 2) >= 0; )
      {
	Object k = props[i];
	if (k == key)
	  {
	    Object old = props[i + 1];
	    props[i] = null;
	    props[i + 1] = null;
	    return old;
	  }
      }
    return null;
  }
}
