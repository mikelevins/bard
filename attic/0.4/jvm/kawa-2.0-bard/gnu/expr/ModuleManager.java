package gnu.expr;
import java.net.*;
import gnu.kawa.io.Path;
import gnu.kawa.io.URLPath;
import gnu.mapping.WrappedException;
import gnu.bytecode.ClassType;

/** A database of known modules as represented by {@link ModuleInfo}.
 * Currently there is only a single global instance of {@code ModuleManager};
 * in the future each different "application" may have their own.
 */

public class ModuleManager
{
  private String compilationDirectory = "";
  public void setCompilationDirectory (String path)
  {
    if (path == null)
      path = "";
    int plen = path.length();
    if (plen > 0)
      {
        char sep = java.io.File.separatorChar; // Or '/' if path is a URL??
        if (path.charAt(plen - 1) != sep)
          path = path + sep;
      }
    compilationDirectory = path;
  }
  public String getCompilationDirectory () { return compilationDirectory; }

  static ModuleManager instance = new ModuleManager();

  /** For now assumes a single global ModuleManager.
   * Later, might have multiple managers. */
  public static ModuleManager getInstance() { return instance; }

    int interactiveCounter;

    int evalCounter;

    public static final String interactiveClassPrefix = "atInteractiveLevel$";
    public String evalClassPrefix = "atEvalLevel$";

    /** Used to generate unique class names for interactive REPLs and loads.
     * This is incremented from Shell.run.
     * Unique class names are essential for {@link Compilation#usedClass}.
     * They're also desirable for debugging.
     */
    public synchronized String getNewInteractiveName() {
        return interactiveClassPrefix + (++interactiveCounter);
    }

    /** Used to generate unique class names for other evals.
     * Equivalent in functionality to getNewInteractiveName, but with
     * a different prefix, for better user-friendliness.
     */
    public synchronized String getNewEvalName() {
        return evalClassPrefix + (++evalCounter);
    }

  public static final long LAST_MODIFIED_CACHE_TIME = 1000;
  /** Number of milliseconds before we re-check file's modified time. */
  public long lastModifiedCacheTime = LAST_MODIFIED_CACHE_TIME;

  /** List of all modules managed by this ModuleManager. */
  ModuleInfo[] modules;
  int numModules;

  public synchronized ModuleInfo getModule (int index)
  {
    return index >= numModules ? null : modules[index];
  }

  public synchronized ModuleInfo find (Compilation comp)
  {
    ModuleExp mexp = comp.getModule();
    ClassType ctype = mexp.classFor(comp);
    String fileName = mexp.getFileName();
    Path sourceAbsPath = ModuleInfo.absPath(fileName);
    ModuleInfo info = findWithSourcePath(sourceAbsPath, fileName);
    info.setClassName(ctype.getName());
    info.setCompilation(comp);
    return info;
  }

  private synchronized void add (ModuleInfo info)
  {
    if (modules == null)
      modules = new ModuleInfo[10];
    else if (numModules == modules.length)
      {
        ModuleInfo[] tmp = new ModuleInfo[2 * numModules];
        System.arraycopy(modules, 0, tmp, 0, numModules);
        modules = tmp;
      }
    modules[numModules++] = info;
  }

    public ModuleInfo createWithClassName(String className) {
        ModuleInfo info = new ModuleInfo();
        info.setClassName(className);
        add(info);
        return info;
    }

  public synchronized ModuleInfo searchWithClassName (String className)
  {
    for (int i = numModules;  --i >= 0; )
      {
        ModuleInfo info = modules[i];
        if (className.equals(info.getClassName()))
          return info;
      }
    return null;
  }

  public static synchronized ModuleInfo findWithClass (Class clas)
  {
    ModuleInfo info = ModuleInfo.mapClassToInfo.get(clas);
    if (info == null)
      {
        info = new ModuleInfo();
        info.setModuleClass(clas);
      }
    return info;
  }

  public ModuleInfo findWithClassName (String className) 
  {
    ModuleInfo info = searchWithClassName(className);
    if (info != null)
      return info;
    try
      {
        return findWithClass(ClassType.getContextClass(className));
      }
    catch (Exception ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }

  private synchronized ModuleInfo searchWithAbsSourcePath (String sourcePath)
  {
    for (int i = numModules;  --i >= 0; )
      {
        ModuleInfo info = modules[i];
        if (sourcePath.equals(info.getSourceAbsPathname()))
          return info;
      }
    return null;
  }

  public synchronized ModuleInfo
  findWithSourcePath (Path sourceAbsPath, String sourcePath)
  {
    String sourceAbsPathname = sourceAbsPath.toString();
    ModuleInfo info = searchWithAbsSourcePath(sourceAbsPathname);
    if (info == null)
      {
        info = new ModuleInfo();
        info.sourcePath = sourcePath;
        info.sourceAbsPath = sourceAbsPath;
        info.sourceAbsPathname = sourceAbsPathname;
        add(info);
      }
    return info;
  }

  public synchronized ModuleInfo findWithSourcePath (String sourcePath)
  {
    return findWithSourcePath(ModuleInfo.absPath(sourcePath), sourcePath);
  }

  public synchronized ModuleInfo findWithURL (URL url)
  {
    Path sourceAbsPath = URLPath.valueOf(url);
    String sourcePath = url.toExternalForm();
    return findWithSourcePath(sourceAbsPath, sourcePath);
  }

  /** Called by compiler-generated code.
   * The compiler generates in each package a class that extends
   * {@link ModuleSet}, and that contains a
   * {@link ModuleSet#register(ModuleManager)} method that calls
   * back to this method.  This method then registers the specified module.
   */
  public synchronized void register (String moduleClass, String moduleSource, String moduleUri)
  {
    // Unclear what is the right thing to do if we have an existing module
    // with the same source or class name.  One case is when we're explicitly
    // compiling a source file and (in XQuery) importing (other) modules in the
    // same namespace.  In that case the file we're compiling should take
    // precedence over old data in the packages's existing $ModulesMap$ class.
    if (searchWithClassName(moduleClass) != null)
      return;
    Path sourcePath = Path.valueOf(moduleSource);
    Path sourceAbsPath = sourcePath.getCanonical();
    String sourceAbsPathname = sourceAbsPath.toString();
    if (searchWithAbsSourcePath(sourceAbsPathname) != null)
      return;
    ModuleInfo info = new ModuleInfo();
    if (sourcePath.isAbsolute())
      {
        info.sourceAbsPath = sourcePath;
        info.sourceAbsPathname = sourceAbsPathname;
      }
    else
      {
        // Resolve moduleSource against moduleClass path.
        try
          {
            // See comment in loadPackageInfo.
            Class setClass = this.packageInfoChain.getClass();
            String setClassName = setClass.getName().replace('.', '/')+".class";
            java.net.URL setClassURL
              = setClass.getClassLoader().getResource(setClassName);
            sourceAbsPath = URLPath.valueOf(setClassURL).resolve(moduleSource);
            info.sourceAbsPath = sourceAbsPath;
            info.sourceAbsPathname = sourceAbsPath.toString();
          }
        catch (Exception ex)
          {
            return;
          }
      }
    info.setClassName(moduleClass);
    info.sourcePath = moduleSource;
    info.uri = moduleUri;
    add(info);
  }

  /** List of {@link ModuleSet}s registered with this {@code ModuleManager}. */
  ModuleSet packageInfoChain;

  /** Search for and if needed load the {@link ModuleSet} for a package.
   */
  public synchronized void loadPackageInfo (String packageName)
    throws ClassNotFoundException, InstantiationException, IllegalAccessException
  {
    String moduleSetClassName = packageName + "." + ModuleSet.MODULES_MAP;

    for (ModuleSet set = packageInfoChain;  set != null;  set = set.next)
      {
        String setName = set.getClass().getName();
        if (setName.equals(moduleSetClassName))
          continue;
      }
    Class setClass = Class.forName(moduleSetClassName);
    ModuleSet instance = (ModuleSet) setClass.newInstance();

    instance.next = this.packageInfoChain;
    // The ModuleInfo.register method depends on packageInfoChain being
    // the current ModuleSet.  Bit of a kludge.
    this.packageInfoChain = instance;
    instance.register(this);
  }

  /** Reset the set of known modules. */
  public synchronized void clear ()
  {
    // Clear modules and packageIndoChain lists.
    // We also clean the 'next' fields, to avoid leaks if
    // somethings happens to be pointing at a ModuleInto or ModuleSet.
    // This may be overkill.
    ModuleSet set = packageInfoChain;
    while (set != null)
      {
        ModuleSet next = set.next;
        set.next = null;
        set = next;
      }
    packageInfoChain = null;

    modules = null;
    numModules = 0;
  }
}
