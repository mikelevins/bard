package gnu.expr;
import gnu.mapping.*;

/** An Environment containing the default bindings for the current Language.
 * This is a singleton class.
 * All <code>lookup</code> operations are indirected to the
 * current <code>Language</code>. */

public class BuiltinEnvironment extends Environment
{
  static final BuiltinEnvironment instance = new BuiltinEnvironment();
  static { instance.setName("language-builtins"); }
  private BuiltinEnvironment() { }

  public static BuiltinEnvironment getInstance() { return instance; }

  public Environment getLangEnvironment ()
  {
    Language lang = Language.getDefaultLanguage();
    return lang == null ? null : lang.getLangEnvironment();
  }

  public NamedLocation lookup (Symbol name, Object property, int hash)
  {
    Language lang = Language.getDefaultLanguage();
    return lang == null ? null : lang.lookupBuiltin(name, property, hash);
  }

  public NamedLocation getLocation (Symbol key, Object property,
                                    int hash, boolean create)
  {
    throw new RuntimeException();
  }

  public void define (Symbol key, Object property, Object newValue)
  {
    throw new RuntimeException();
  }

  public LocationEnumeration enumerateLocations ()
  {
    return getLangEnvironment().enumerateLocations();
  }

  public LocationEnumeration enumerateAllLocations ()
  {
    return getLangEnvironment().enumerateAllLocations();
  }

  protected boolean hasMoreElements (LocationEnumeration it)
  {
    throw new RuntimeException();
  }

  public NamedLocation addLocation (Symbol name, Object prop, Location loc)
  {
    throw new RuntimeException();
  }
}
