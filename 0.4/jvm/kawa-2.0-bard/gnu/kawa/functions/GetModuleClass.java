package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.io.Path;
import gnu.kawa.io.URLPath;
import java.net.URL;
import gnu.text.ResourceStreamHandler;

/** Special procedure to get the Class of the current module.
 * Since "current module" is defined by lexical scope,
 * this isn't a first-class procedure - it has to be inlined.
 */

public class GetModuleClass extends ProcedureN
  implements Inlineable
{
  /** A function that returns the class of the current module. */
  public static final GetModuleClass getModuleClass
    = new GetModuleClass();
  /** A function that returns the URLPath of the current module.
   * This is {@code "class-resource:"} URL (see {@link gnu.text.ResourceStreamHandler}),
   * though in immediate mode it gets optimized to a {@code "file:"} URL.
   */
  public static final GetModuleClass getModuleUri
    = new GetModuleClass();
  /** A dummy 1-argument functions used in implementing getModuleUri.
   * It takes one argument - a reference to a declaration containing
   * the intended result, with a value to initialize it.
   * The compile method sets up the needed static initialization.
   */
  public static final GetModuleClass getModuleUriDummy
    = new GetModuleClass();

  static final ClassType typeURLPath = ClassType.make("gnu.kawa.io.URLPath");
    static final Method maker =
        ClassType.make("gnu.kawa.functions.GetModuleClass")
        .getDeclaredMethod("classResourcePath", 1);

  public Object applyN (Object[] args)
  {
    throw new Error("get-module-class must be inlined");
  }

  public int numArgs()
  {
    return this == getModuleUriDummy ? 0x1001 : 0;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (this == getModuleUriDummy)
      {
        ReferenceExp ref = (ReferenceExp) exp.getArgs()[0];
        ref.compile(comp, target);
        Declaration decl = ref.getBinding();
        Expression init = decl.getValue();
        if (init != null)
          {
            BindingInitializer.create(decl, init, comp);
            decl.noteValueUnknown();
          }
      }
    else
      {
        comp.loadClassRef(comp.mainClass);
        if (this == getModuleUri)
          comp.getCode().emitInvoke(maker);
        target.compileFromStack(comp, exp.getType());
      }
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return this == getModuleClass ? Type.javalangClassType : typeURLPath;
  }

  private static Symbol CLASS_RESOURCE_NAME =
    Namespace.getDefaultSymbol("$class_resource_URL$");

  /** Return an expression that evaluates to a module-relative URL.
   * This has the Kawa-specific URL scheme "class-resource:" and an
   * associated ClassLoader (using a WeakHashMap).  It's used to reference
   * resources located using the compiled class's ClassLoader. */
  public static Expression getModuleClassURI (Compilation comp)
  {
    Declaration decl = comp.mainLambda.lookup(CLASS_RESOURCE_NAME);
    if (decl == null)
      {
        decl = new Declaration(CLASS_RESOURCE_NAME, typeURLPath);
        decl.setFlag(Declaration.IS_CONSTANT|Declaration.STATIC_SPECIFIED|Declaration.EARLY_INIT);
        Expression value;
        if (comp.immediate)
          {
            Path path = comp.getMinfo().getSourceAbsPath();
            if (path == null)
              path = Path.currentPath();
            if (! (path instanceof URLPath))
              path = URLPath.valueOf(path.toURL());
            value = QuoteExp.getInstance(path);
          }
        else
          {
            Expression clas
              = new ApplyExp(gnu.kawa.functions.GetModuleClass.getModuleClass,
                             Expression.noExpressions);
            value = new ApplyExp(maker, clas);
          }
        decl.setValue(value);
        comp.mainLambda.add(null, decl);
      }
    ReferenceExp ref = new ReferenceExp(decl);
    if (comp.immediate)
      return ref;
    else
      return new ApplyExp(getModuleUriDummy, ref);
  }

    public static URLPath classResourcePath(Class clas) {
        URL url;
        try {
            try {
                // This throws a SecurityException in the applet case.
                url = ResourceStreamHandler.makeURL(clas);
            } catch (SecurityException ex) {
                // The following assumes we have an actual .class file
                // (possibly inside a .jar) available in the classpath.
                // That would be the case in a normal Java environment,
                // though not (for example) on Android.
                String classFileName = clas.getName().replace('.', '/')+".class";
                url = clas.getClassLoader().getResource(classFileName);
            }
        } catch (Exception ex) {
            throw WrappedException.wrapIfNeeded(ex);
        }
        return URLPath.valueOf(url);
    }
}
