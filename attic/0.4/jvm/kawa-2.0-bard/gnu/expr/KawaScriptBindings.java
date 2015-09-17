package gnu.expr;
import javax.script.*;
import java.util.*;
import gnu.mapping.*;

/** An implementation of javax.script.Bindings that uses a SimpleEnvironment.
 */

public class KawaScriptBindings
  extends AbstractMap<String,Object>
  implements Bindings
{
  final SimpleEnvironment environment;

  public KawaScriptBindings (SimpleEnvironment environment)
  {
    this.environment = environment;
  }

  Symbol asSymbol (String name)
  {
    if (name.length() > 0 && name.charAt(0) == '{')
      return Symbol.parse(name);
    // FIXME should handle prefix:name here.
    return environment.getSymbol(name);
  }

  public Object get(Object key)
  {
    return environment.get(key);
  }

  public Object put (String key, Object value)
  {
    return environment.put(key, value);
  }

  class Entry implements Map.Entry<String,Object>
  {
    public Entry(Location nloc) { this.nloc = nloc; }
    Location nloc;
    public String getKey () { return nloc.getKeySymbol().toString(); }
    public Object getValue() { return nloc.getValue(); }
    public Object setValue(Object v) { return nloc.setValue(v); }
    public int hashCode() { return nloc.hashCode(); }
    public boolean equals(Object o)
    {
      if (! (o instanceof Entry))
        return false;
      Entry e2 = (Entry) o;
      return ((getKey()==null ?
               e2.getKey()==null : getKey().equals(e2.getKey())) &&
              (getValue()==null ?
               e2.getValue()==null : getValue().equals(e2.getValue())));
    }
  }

  class EntrySet extends AbstractSet<Map.Entry<String,Object>>
  {
    public Iterator<Map.Entry<String,Object>> iterator()
    {
      return new Iterator<Map.Entry<String,Object>>()
        {
          LocationEnumeration locEnum = environment.enumerateLocations();
          public boolean hasNext() { return locEnum.hasNext(); }
          public Map.Entry<String,Object> next()
            { return new Entry(locEnum.next()); }
          public void remove() { locEnum.remove(); }
        };
    }
    public int size() { return environment.size(); }
  }

  public Set<Map.Entry<String,Object>> entrySet()
  {
    return new EntrySet();
  }
}
