package gnu.expr;
import javax.script.*;
import java.util.*;
import gnu.mapping.*;
import gnu.kawa.util.WeakIdentityHashMap;

/** Abstract implementation of ScriptEngineFactory for any Kawa Language. */

public abstract class AbstractScriptEngineFactory implements ScriptEngineFactory
{
  Language language;
  protected List<String> names;
  private static List<String> noMimeTypes;

    InheritingEnvironment  savedEnvironment;
    Bindings saveEngineBindings;
    Bindings saveGlobalBindings;

    Environment getEnvironment(ScriptContext ctx) {
        Bindings ebindings = ctx.getBindings(ScriptContext.ENGINE_SCOPE);
        Bindings gbindings = ctx.getBindings(ScriptContext.GLOBAL_SCOPE);
        if (ebindings == saveEngineBindings && gbindings == saveGlobalBindings)
            return savedEnvironment;
        InheritingEnvironment env
            = new BindingsEnvironment("environment-"+(++Language.envCounter),
                                      ebindings, gbindings,
                                      language.getLangEnvironment());
        savedEnvironment = env;
        saveEngineBindings = ebindings;
        saveGlobalBindings = gbindings;
        return env;
    }

  protected AbstractScriptEngineFactory(Language language)
  {
    this.language = language;
  }

  public String getEngineName ()
  {
    return "Kawa-"+language.getName();
  }

  public String getEngineVersion ()
  {
    return kawa.Version.getVersion();
  }

  public String getLanguageVersion ()
  {
    return kawa.Version.getVersion();
  }

  public String getLanguageName ()
  {
    return language.getName();
  }

    public List<String> getExtensions() {
        return language.getExtensions();
    }

  public List<String> getMimeTypes ()
  {
    if (noMimeTypes == null)
      noMimeTypes = Collections.unmodifiableList(new ArrayList<String>(0));
    return noMimeTypes;
  }

  public List<String> getNames()
  {
    if (names == null)
      {
        ArrayList<String> n = new ArrayList<String>(3);
        getNames(n);
        names = Collections.unmodifiableList(n);
      }
    return names;
  }

  protected void getNames (List<String> names)
  {
    names.add(language.getName());
  }

  public String getMethodCallSyntax(String obj, String m, String... args)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getMethodCalSyntax not supported");
  }

  public String getOutputStatement(String toDisplay)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getOutputStatement not supported");
  }

  public String getProgram(String... statements)
  {
    throw new UnsupportedOperationException(getClass().getName()+".getProgram not supported");
  }

  public javax.script.ScriptEngine getScriptEngine()
  {
    return new KawaScriptEngine(this);
  }

  public String getParameter(String key)
  {
    if (key.equals(ScriptEngine.ENGINE))
      return getEngineName();
    if (key.equals(ScriptEngine.ENGINE_VERSION))
      return getEngineVersion();
    if (key.equals(ScriptEngine.NAME))
      return getEngineName();
    if (key.equals(ScriptEngine.LANGUAGE))
      return getLanguageName();
    if (key.equals(ScriptEngine.LANGUAGE_VERSION))
      return getLanguageVersion();
    if (key.equals("THREADING"))
      return "MULTITHREADED";
    return null;
  }

    /** A Location object that proxies lookup in global and engine Bindings.
     */
    public static class BindingsLocation extends NamedLocation<Object> {
        final BindingsEnvironment env;
        final String sname;
        public BindingsLocation(SimpleSymbol name, BindingsEnvironment env) {
            super(name, null);
            this.env = env;
            this.sname = name.getName();
            this.setAlias(null);
        }
        public Object get(Object defaultValue) {
            Object r = env.ebindings.get(sname);
            if (r != null || env.ebindings.containsKey(sname))
                return r;
            r = env.gbindings.get(sname);
            if (r != null || env.gbindings.containsKey(sname))
                return r;
            return defaultValue;
        }
        public Object get() {
            Object unbound = Location.UNBOUND;
            Object v = get(unbound);
            if (v == unbound)
                throw new UnboundLocationException();
            return v;
        }

        public boolean isBound() {
            Object unbound = get(Location.UNBOUND);
            return get(unbound) == unbound;
        }

        public void set(Object newValue) {
            env.ebindings.put(sname, newValue);
        }

        public Environment getEnvironment () {
            return env;
        }
    }

    /** A Environment that proxies lookup in global and engine Bindings.
     */
    public static class BindingsEnvironment extends InheritingEnvironment {
        Bindings ebindings; // Engine Bindings
        Bindings gbindings; // Global Bindings
        public BindingsEnvironment(String name, Bindings ebindings, Bindings gbindings, Environment builtin) {
            super(name, builtin);
            this.ebindings = ebindings;
            this.gbindings = gbindings;
        }

        protected NamedLocation newLocation(Symbol name, Object property) {
            if (property == null && name instanceof SimpleSymbol)
                return new BindingsLocation((SimpleSymbol) name, this);
            else
                return super.newLocation(name, property);
        }

        public NamedLocation lookupDirect (Symbol name, Object property, int hash) {
            NamedLocation loc = super.lookupDirect(name, property, hash);
            if (loc != null && loc.isBound())
                return loc;
            if (ebindings instanceof KawaScriptBindings) {
                loc = ((KawaScriptBindings) ebindings).environment.lookup(name, property, hash);
                if (loc != null && loc.isBound())
                    return loc;
            } else if (property == null && name instanceof SimpleSymbol) {
                String sname = name.getName();
                if (ebindings.containsKey(sname))
                    return addUnboundLocation(name, property, hash);
            }
            if (gbindings instanceof KawaScriptBindings) {
                loc = ((KawaScriptBindings) gbindings).environment.lookup(name, property, hash);
                if (loc != null && loc.isBound())
                    return loc;
            } else if (property == null && name instanceof SimpleSymbol) {
                String sname = name.getName();
                if (gbindings.containsKey(sname))
                    return addUnboundLocation(name, property, hash);
            }
            return loc;
        }

        public void define (Symbol key, Object property, Object newValue) {
            put(key, property, newValue);
        }
        public void fixEntries() {
            for (String key : ebindings.keySet()) {
                lookup(Symbol.valueOf(key));
            }
            for (String key : gbindings.keySet()) {
                lookup(Symbol.valueOf(key));
            }
        }
        public LocationEnumeration enumerateLocations() {
            fixEntries();
            return super.enumerateLocations();
        }
        public LocationEnumeration enumerateAllLocations() {
            fixEntries();
            return super.enumerateAllLocations();
        }
    }

}
