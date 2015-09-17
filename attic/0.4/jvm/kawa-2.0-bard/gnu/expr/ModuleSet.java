package gnu.expr;

/** The compiler generates extensions of this class.
 * For each compiler-generated {@code PACKAGE} the
 * compiler also generates a class {@code PACKAGE.$ModulesMap$}
 * that extends {@code ModuleSet}.
 * This is used to register the modules in a given package
 * with the active {@link ModuleManager}.  This is needed for XQuery where
 * there may be multiple modules with the same namespace URI.  To import
 * all the modules in a given namespace, the compiler maps the
 * namespace to a package name, makes sure the package's {@code ModuleSet}
 * is loaded, and then selects those modules whose namespace URI matches.
 */

public abstract class ModuleSet
{
  public static final String MODULES_MAP = "$ModulesMap$";

  /** Next ModuleSet in list headed by {@link ModuleManager#packageInfoChain}. */
  ModuleSet next;

  /** The compiler generates implementations of this method.
   * Normally, the implementation calls {@link ModuleManager#register}
   * once for each each module in the current package.
   */
  public abstract void register (ModuleManager manager);
}
