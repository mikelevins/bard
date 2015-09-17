package gnu.mapping;

/** A property whose value can be found lazily.
 * The property is initialized with a specifier string, which must have the
 * form of either:
 * <ol>
 * <li> {@code "ClassName:fieldName"}: In this case {@code "fieldName"} must
 * be the name of a static field in {@code "ClassName"}, of type {@code T}.
 * <li> {@code "*ClassName:methodName"}: In this case {@code "methodName"}
 * must be the name of a static method that takes one parameter
 * (the {@code PropertySet}), and returns an object of type {@code T}.
 * </ol>
 */
public class LazyPropertyKey<T> extends PropertyKey<T>
{
  public LazyPropertyKey(String name)
  {
    super(name);
  }

  @SuppressWarnings("unchecked")
  @Override
  public T get(PropertySet container, T defaultValue)
  {
    Object raw = container.getProperty((Object) this, (Object) defaultValue);
    if (raw instanceof String)
      {
        String str = (String) raw;
        boolean factory = false;
        int cstart = str.charAt(0) == '*' ? 1 : 0;
        int colon = str.indexOf(':');
        if (colon <= cstart || colon >= str.length() - 1)
          throw new RuntimeException("lazy property "+this+" must have the form \"ClassName:fieldName\" or \"ClassName:staticMethodName\"");
        java.lang.reflect.Method method = null;
        String cname = str.substring(cstart, colon);
        String mname = str.substring(colon+1);
        T result;
        try
          {
            /* #ifdef JAVA2 */
            Class clas = Class.forName(cname, true,
                                 container.getClass().getClassLoader());
            /* #else */
            // Class clas = Class.forName(cname);
            /* #endif */
            if (cstart == 0)
              {
                result = (T) clas.getField(mname).get(null);
              }
            else
              {
                result = (T) clas.getDeclaredMethod(mname, new Class[] { Object.class })
                  .invoke(null, container);
              }
          }
        catch (Exception ex)
          {
            throw new RuntimeException("lazy property "+this+" has specifier \""+str+"\" but there is no such "+(cstart==0?"field":"method"), ex);
          }
        container.setProperty(this, result);
        return result;
      }
    return (T) raw;
  }

  public void set (PropertySet container, String specifier)
  {
    container.setProperty(this, specifier);
  }
}
