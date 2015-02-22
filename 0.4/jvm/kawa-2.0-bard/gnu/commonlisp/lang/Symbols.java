package gnu.commonlisp.lang;
import gnu.mapping.*;

/** Support for Lisp Symbols.
 * The special symbol `nil' is actually the value gnu.lists.LList.Empty. */

public class Symbols
{
  private Symbols ()
  {
  }

  /**
   * Predicate to check whether a given object is a CL symbol.
   * 
   * Used for {@code SYMBOLP}.
   */
  public static boolean isSymbol(Object val)
  {
    return val == Lisp2.FALSE || val instanceof Symbol;
  }

  public static boolean isBound(Object sym)
  {
    if (sym == Lisp2.FALSE)
      return true;
    Environment env = Environment.getCurrent();
    Symbol symbol;
    if (sym instanceof Symbol)
      symbol = (Symbol) sym;
    else
      symbol = env.defaultNamespace().lookup((String) sym);
    return symbol != null && env.isBound(symbol);
  }
  
  public static Symbol getSymbol(Environment env, Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : env.defaultNamespace().getSymbol((String) sym);
  }

  public static Symbol getSymbol(Object sym)
  {
    if (sym == Lisp2.FALSE)
      sym = "nil";
    return sym instanceof Symbol ? (Symbol) sym
      : Namespace.getDefaultSymbol((String) sym); // FIXME
  }

  public static Object getPrintName(Object sym)
  {
    return sym == Lisp2.FALSE ? "nil"
      : Lisp2.getString(((Symbol) sym).getName());
  }

  /**
   * Get the function binding for a given symbol in the current environment.
   * 
   * Will throw an {@link UnboundLocationException} if no such symbol exists
   * in the current environment.
   */
  public static Object getFunctionBinding (Object symbol)
  {
    return Environment.getCurrent().getFunction(getSymbol(symbol));
  }

  /**
   * Get the function binding for a given symbol in a given environment.
   * 
   * Will throw an {@link UnboundLocationException} if no such symbol exists
   * in the current environment.
   */
  public static Object getFunctionBinding (Environment environ, Object symbol)
  {
    return environ.getFunction(getSymbol(symbol));
  }

  public static void setFunctionBinding (Environment environ,
					 Object symbol, Object newValue)
  {
    environ.put(getSymbol(symbol), EnvironmentKey.FUNCTION, newValue);
  }

}
