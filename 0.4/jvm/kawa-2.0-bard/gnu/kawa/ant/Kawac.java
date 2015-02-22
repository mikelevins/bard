// An Ant Task to invoke the kawa compiler.
// Jamison Hope
package gnu.kawa.ant;

import java.io.File;
import java.io.IOException;
import java.util.Vector;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.MagicNames;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Execute;
import org.apache.tools.ant.taskdefs.LogStreamHandler;
import org.apache.tools.ant.taskdefs.MatchingTask;
import org.apache.tools.ant.taskdefs.condition.Os;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.DataType;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PatternSet;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.selectors.AndSelector;
import org.apache.tools.ant.types.selectors.ContainsRegexpSelector;
import org.apache.tools.ant.types.selectors.ContainsSelector;
import org.apache.tools.ant.types.selectors.DateSelector;
import org.apache.tools.ant.types.selectors.DependSelector;
import org.apache.tools.ant.types.selectors.DepthSelector;
import org.apache.tools.ant.types.selectors.ExtendSelector;
import org.apache.tools.ant.types.selectors.FilenameSelector;
import org.apache.tools.ant.types.selectors.FileSelector;
import org.apache.tools.ant.types.selectors.MajoritySelector;
import org.apache.tools.ant.types.selectors.NoneSelector;
import org.apache.tools.ant.types.selectors.NotSelector;
import org.apache.tools.ant.types.selectors.OrSelector;
import org.apache.tools.ant.types.selectors.PresentSelector;
import org.apache.tools.ant.types.selectors.SelectSelector;
import org.apache.tools.ant.types.selectors.SizeSelector;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.GlobPatternMapper;
import org.apache.tools.ant.util.JavaEnvUtils;
import org.apache.tools.ant.util.SourceFileScanner;
import org.apache.tools.ant.util.StringUtils;

/**
 * Compiles Kawa source files. This task can take the following
 * arguments:
 * <ul>
 * <li>srcdir
 * <li>destdir
 * <li>classpath
 * <li>target
 * <li>failonerror
 * <li>prefix
 * <li>main
 * <li>fulltailcalls
 * <li>modulestatic
 * <li>warnundefinedvariable
 * <li>language
 * </ul>
 * If <b>srcdir</b> is set, then that directory is searched for source
 * files to compile (subject to any nested
 * includes/excludes). Otherwise, files included in nested filesets
 * and filelists will be compiled and written into <b>destdir</b>.
 * <p>
 * When this task executes, it will compile any listed source file
 * which is younger than its corresponding class file.
 * <p>
 * Superficially based upon the standard Ant Javac Task, but with
 * FileSet and FileList support as well.
 *
 * @author Jamison Hope
 */
public class Kawac extends MatchingTask {

  private static final String FAIL_MSG
    = "Compile failed; see the compiler error output for details.";

  // generic compiler stuff
  private Path src;                  // location of source files
  private File destDir;              // destination for class files
  private Path compileClasspath;     // classpath
  private boolean listFiles = false; // list the files being compiled
  private boolean failOnError = true; // does what it says
  private String target;              // target VM ("1.5", "1.6", etc)
  private boolean includeDestClasses = true; // include destDir in classpath

  // properties for success or failure
  private String updatedProperty; // property to be set on successful compile
  private String errorProperty;   // property to be set on error

  // properties corresponding to various kawa.repl flags
  private String prefix;                 // Kawa -P argument
  private boolean main = false;          // Kawa --main argument
  private boolean applet = false;        // Kawa --applet argument
  private boolean servlet = false;       // Kawa --servlet argument
  private boolean fullTailCalls = false; // Kawa --full-tailcalls argument
  private String moduleStatic; // Kawa --module-static, --module-static-run
  private boolean warnUndefinedVariable = false; // Kawa --warn-undefined-variable
  private boolean warnAsError = false;           // Kawa --warn-as-error
  private String language;                       // Kawa language (--clisp, --scheme, etc)

  private File[] compileList = new File[0]; // the Files to compile
  private boolean taskSuccess = true;       // will be set false on error

  private boolean usedMatchingTask = false; // a la org.apache.tools.ant.taskdefs.Delete
  private Vector<DataType> filesets = new Vector<DataType>();

  private Vector<Commandline.Argument> otherArgs =
    new Vector<Commandline.Argument>();

  /**
   * Kawac task for compilation of Kawa files.
   */
  public Kawac() {
  }

  /**
   * Set the source directories to find the source Kawa files.
   * @param srcDir the source directories as a path
   */
  public void setSrcdir(Path srcDir) {
    usedMatchingTask = true;
    if (src == null)
      src = srcDir;
    else
      src.append(srcDir);
  }

  /**
   * Gets the source dirs to find the source Kawa files.
   * @return the source directories as a path
   */
  public Path getSrcdir() {
    return src;
  }

  /**
   * Set the destination directory into which the Kawa source files
   * should be compiled.
   * @param destDir the destination directory
   */
  public void setDestdir(File destDir) {
    this.destDir = destDir;
  }

  /**
   * Gets the destination directory into which the Kawa source files
   * should be compiled.
   * @return the destination directory
   */
  public File getDestdir() {
    return destDir;
  }

  /**
   * Set the classpath to be used for this compilation.
   * @param classpath an Ant Path object containing the compilation
   * classpath.
   */
  public void setClasspath(Path classpath) {
    if (compileClasspath == null)
      compileClasspath = classpath;
    else
      compileClasspath.append(classpath);
  }

  /**
   * Gets the classpath to be used for this compilation.
   * @return the class path
   */
  public Path getClasspath() {
    return compileClasspath;
  }

  /**
   * Adds a path to the classpath.
   * @return a class path to be configured
   */
  public Path createClasspath() {
    if (compileClasspath == null)
      compileClasspath = new Path(getProject());
    return compileClasspath.createPath();
  }

  /**
   * Adds a reference to a classpath defined elsewhere.
   * @param r a reference to a classpath
   */
  public void setClasspathRef(Reference r) {
    createClasspath().setRefid(r);
  }

  /**
   * If true, list the source files being handed off to the compiler.
   * @param list if true list the source files
   */
  public void setListfiles(boolean list) {
    listFiles = list;
  }

  /**
   * Get the listfiles flag.
   * @return the listfiles flag
   */
  public boolean getListfiles() {
    return listFiles;
  }

  /**
   * Indicates whether the build will continue even if there are
   * compilation errors; defaults to true.
   * @param fail if true halt the build on failure
   */
  public void setFailonerror(boolean fail) {
    failOnError = fail;
  }

  /**
   * Gets the failonerror flag.
   * @return the failonerror flag
   */
  public boolean getFailonerror() {
    return failOnError;
  }

  /**
   * Sets the target VM that the classes will be compiled for. Valid
   * values are "7", "6", "1.6", "5", "1.5", "1.4", "1.3", "1.2", and
   * "1.1".
   * @param target the target VM
   */
  public void setTarget(String target) {
    this.target = target;
  }

  /**
   * Gets the target VM that the classes will be compiled for.
   * @return the target VM
   */
  public String getTarget() {
    return target != null
      ? target : getProject().getProperty(MagicNames.BUILD_JAVAC_TARGET);
  }

  /**
   * This property controls whether to include the destination classes
   * directory in the classpath given to the compiler. The default
   * value is true.
   * @param includeDestClasses the value to use
   */
  public void setIncludeDestClasses(boolean includeDestClasses) {
    this.includeDestClasses = includeDestClasses;
  }

  /**
   * Get the value of the includeDestClasses property.
   * @return the value
   */
  public boolean isIncludeDestClasses() {
    return includeDestClasses;
  }

  /**
   * The property to set on compilation success. This property will
   * not be set if the compilation fails, or if there are no files to
   * compile.
   * @param updatedProperty the property name to use
   */
  public void setUpdatedProperty(String updatedProperty) {
    this.updatedProperty = updatedProperty;
  }

  /**
   * The property to set on compilation failure. This property will be
   * set if the compilation fails.
   * @param errorProperty the property name to use
   */
  public void setErrorProperty(String errorProperty) {
    this.errorProperty = errorProperty;
  }

  /**
   * Sets the prefix.
   * @param prefix the prefix to use
   */
  public void setPrefix(String prefix) {
    this.prefix = prefix;
  }

  /**
   * Gets the prefix
   * @return the prefix
   */
  public String getPrefix() {
    return prefix;
  }

  /**
   * If true, then Kawa will create a static main method.
   * @param main true to pass --main to Kawa
   */
  public void setMain(boolean main) {
    this.main = main;
  }

  /**
   * Gets the value of the "main" property.
   * @return the value
   */
  public boolean getMain() {
    return main;
  }

  /**
   * If true, then Kawa will generate an applet.
   * @param applet true to pass --applet to Kawa
   */
  public void setApplet(boolean applet) {
    this.applet = applet;
  }

  /**
   * Gets the value of the "applet" property.
   * @return the value
   */
  public boolean getApplet() {
    return applet;
  }

  /**
   * If true, then Kawa will generate a servlet.
   * @param servlet true to pass --applet to Kawa
   */
  public void setServlet(boolean servlet) {
    this.servlet = servlet;
  }

  /**
   * Gets the value of the "servlet" property.
   * @return the value
   */
  public boolean getServlet() {
    return servlet;
  }

  /**
   * If true, Kawa will use full tailcalls.
   * @param tailcalls true to use full-tailcalls
   */
  public void setFulltailcalls(boolean tailcalls) {
    fullTailCalls = tailcalls;
  }

  /**
   * Gets the value of the fulltailcalls property.
   * @return the value
   */
  public boolean getFulltailcalls() {
    return fullTailCalls;
  }

  /**
   * Passes the --module-static or --module-static-run flag.
   * @param moduleStatic the flag
   */
  public void setModulestatic(String moduleStatic) {
    this.moduleStatic = moduleStatic;
  }

  /**
   * Gets the value of the modulestatic property.
   * @return the value
   */
  public String getModulestatic() {
    return moduleStatic;
  }

  /**
   * Passes the --warn-undefined-variable flag if true.
   * @param undefined true for --warn-undefined-variable
   */
  public void setWarnundefinedvariable(boolean undefined) {
    warnUndefinedVariable = undefined;
  }

  /**
   * Gets the value of the warnundefinedvariable property.
   * @return the value
   */
  public boolean getWarnundefinedvariable() {
    return warnUndefinedVariable;
  }

  /**
   * Passes the --warn-as-error flag if true.
   * @param warnaserror true for --warn-as-error
   */
  public void setWarnaserror(boolean warnaserror) {
    warnAsError = warnaserror;
  }

  /**
   * Gets the value of the warnaserror property.
   * @return the value
   */
  public boolean getWarnaserror() {
    return warnAsError;
  }

  /**
   * Sets the language to use: "scheme", "commonlisp", or "elisp".
   * @param lang the language to use
   */
  public void setLanguage(String lang) {
    language = lang;
  }

  /**
   * Gets the language.
   * @return the value
   */
  public String getLanguage() {
    return language;
  }

  /**
   * Adds a set of files to be compiled.
   * @param set the set of files to be compiled
   */
  public void addFileset(FileSet set) {
    filesets.addElement(set);
  }

  /**
   * Adds a list of files to be compiled.
   * @param list the list of files to be compiled
   */
  public void addFilelist(FileList list) {
    filesets.addElement(list);
  }

  /**
   * Adds a nested {@code <arg>}.
   * @param arg the argument to add to the kawa command line
   */
  public void addArg(Commandline.Argument arg) {
    otherArgs.add(arg);
  }

  //////////////////////////////////
  ///// MatchingTask overrides /////
  //////////////////////////////////

  @Override public PatternSet.NameEntry createInclude() {
    usedMatchingTask = true;
    return super.createInclude();
  }

  @Override public PatternSet.NameEntry createIncludesFile() {
    usedMatchingTask = true;
    return super.createIncludesFile();
  }

  @Override public PatternSet.NameEntry createExclude() {
    usedMatchingTask = true;
    return super.createExclude();
  }

  @Override public PatternSet.NameEntry createExcludesFile() {
    usedMatchingTask = true;
    return super.createExcludesFile();
  }

  @Override public PatternSet createPatternSet() {
    usedMatchingTask = true;
    return super.createPatternSet();
  }

  @Override public void setIncludes(String includes) {
    usedMatchingTask = true;
    super.setIncludes(includes);
  }

  @Override public void setExcludes(String excludes) {
    usedMatchingTask = true;
    super.setExcludes(excludes);
  }

  @Override public void setDefaultexcludes(boolean useDefaultExcludes) {
    usedMatchingTask = true;
    super.setDefaultexcludes(useDefaultExcludes);
  }

  @Override public void setIncludesfile(File includesfile) {
    usedMatchingTask = true;
    super.setIncludesfile(includesfile);
  }

  @Override public void setExcludesfile(File excludesfile) {
    usedMatchingTask = true;
    super.setExcludesfile(excludesfile);
  }

  @Override public void setCaseSensitive(boolean isCaseSensitive) {
    usedMatchingTask = true;
    super.setCaseSensitive(isCaseSensitive);
  }

  @Override public void setFollowSymlinks(boolean followSymlinks) {
    usedMatchingTask = true;
    super.setFollowSymlinks(followSymlinks);
  }

  @Override public void addSelector(SelectSelector selector) {
    usedMatchingTask = true;
    super.addSelector(selector);
  }

  @Override public void addAnd(AndSelector selector) {
    usedMatchingTask = true;
    super.addAnd(selector);
  }

  @Override public void addOr(OrSelector selector) {
    usedMatchingTask = true;
    super.addOr(selector);
  }

  @Override public void addNot(NotSelector selector) {
    usedMatchingTask = true;
    super.addNot(selector);
  }

  @Override public void addNone(NoneSelector selector) {
    usedMatchingTask = true;
    super.addNone(selector);
  }

  @Override public void addMajority(MajoritySelector selector) {
    usedMatchingTask = true;
    super.addMajority(selector);
  }

  @Override public void addDate(DateSelector selector) {
    usedMatchingTask = true;
    super.addDate(selector);
  }

  @Override public void addSize(SizeSelector selector) {
    usedMatchingTask = true;
    super.addSize(selector);
  }

  @Override public void addFilename(FilenameSelector selector) {
    usedMatchingTask = true;
    super.addFilename(selector);
  }

  @Override public void addCustom(ExtendSelector selector) {
    usedMatchingTask = true;
    super.addCustom(selector);
  }

  @Override public void addContains(ContainsSelector selector) {
    usedMatchingTask = true;
    super.addContains(selector);
  }

  @Override public void addPresent(PresentSelector selector) {
    usedMatchingTask = true;
    super.addPresent(selector);
  }

  @Override public void addDepth(DepthSelector selector) {
    usedMatchingTask = true;
    super.addDepth(selector);
  }

  @Override public void addDepend(DependSelector selector) {
    usedMatchingTask = true;
    super.addDepend(selector);
  }

  @Override public void addContainsRegexp(ContainsRegexpSelector selector) {
    usedMatchingTask = true;
    super.addContainsRegexp(selector);
  }

  @Override public void addModified(ModifiedSelector selector) {
    usedMatchingTask = true;
    super.addModified(selector);
  }

  @Override public void add(FileSelector selector) {
    usedMatchingTask = true;
    super.add(selector);
  }

  /**
   * Get the result of the kawac task (success or failure).
   * @return true if compilation succeeded, or was not necessary,
   * false if the compilation failed.
   */
  public boolean getTaskSuccess() {
    return taskSuccess;
  }

  /**
   * Executes the task.
   * @exception BuildException if an error occurs
   */
  public void execute() throws BuildException {
    checkParameters();
    resetFileLists();

    // scan source directories and dest directory to build up compile
    // lists
    FileNameMapper mapper = getMapper();

    if (usedMatchingTask) {     // scan srcdir
      String[] list = src.list();

      for (int i = 0; i < list.length; i++) {
        File srcDir = getProject().resolveFile(list[i]);
        if (!srcDir.exists()) {
          throw new BuildException("srcdir \"" + srcDir.getPath() +
                                   "\" does not exist!", getLocation());
        }

        DirectoryScanner ds = getDirectoryScanner(srcDir);
        String[] files = ds.getIncludedFiles();
        scanDir(srcDir, destDir != null ? destDir : srcDir, files,
                mapper);
      }
    }
    // now scan each fileset and filelist
    for (DataType dt : filesets) {
      if (dt instanceof FileSet) {
        FileSet set = (FileSet) dt;
        DirectoryScanner ds = set.getDirectoryScanner(getProject());
        String[] files = ds.getIncludedFiles();
        scanDir(set.getDir(), destDir, files, mapper);
      } else if (dt instanceof FileList) {
        FileList list = (FileList) dt;
        scanDir(list.getDir(getProject()), destDir,
                list.getFiles(getProject()), mapper);
      }
    }

    compile();
    if (updatedProperty != null
        && taskSuccess
        && compileList.length != 0) {
      getProject().setNewProperty(updatedProperty, "true");
    }
  }

  /**
   * Clear the list of files to be compiled.
   */
  private void resetFileLists() {
    compileList = new File[0];
  }

  /**
   * Scans the directory looking for source files to be compiled.
   * The results are returned in the class variable compileList. Uses
   * the provided mapper to compare source and class file names.
   * @param srcDir  the source directory
   * @param destDir the destination directory
   * @param files   an array of filenames
   * @param mapper  a FileNameMapper
   */
  private void scanDir(File srcDir, File destDir, String[] files,
                       FileNameMapper mapper) {

    if ((prefix != null) &&
        (prefix.length() > 0)) {
      String prefixPath = prefix.replace('.', File.separatorChar);
      String prefixPathSlash = prefixPath;
      if (prefixPath.endsWith(File.separator)) {
        prefixPath = prefixPath.substring(0, prefixPath.length()-1);
      } else {
        prefixPathSlash += File.separator;
      }
      String srcStr = srcDir.getPath();
      if (srcStr.endsWith(prefixPath) ||
          srcStr.endsWith(prefixPathSlash)) {
        // the listed file names do not include the prefix
        String destStr = destDir.getPath();
        if (!(destStr.endsWith(prefixPath) ||
              destStr.endsWith(prefixPathSlash))) {
          // The destination path does not include the prefix, so the
          // glob will look in the wrong place if we use the original
          // destDir.
          destDir = new File(destDir, prefixPath);
        }
      }
    }

    SourceFileScanner sfs = new SourceFileScanner(this);
    File[] newFiles = sfs.restrictAsFiles(files, srcDir, destDir, mapper);

    if (newFiles.length > 0) {
      File[] newCompileList
        = new File[compileList.length + newFiles.length];
      System.arraycopy(compileList, 0, newCompileList, 0,
                       compileList.length);
      System.arraycopy(newFiles, 0, newCompileList,
                       compileList.length, newFiles.length);
      compileList = newCompileList;
    }
  }

  /**
   * Returns a file name mapper which maps source files to class
   * files. This needs to stay in sync with Languages and file name
   * extensions supported by Kawa.
   */
  private FileNameMapper getMapper() {
    if (language == null) {
      // unspecified, use all
      CompositeMapper mapper = new CompositeMapper();
      mapper.add(getSchemeMapper());
      mapper.add(getKrlMapper());
      mapper.add(getBrlMapper());
      mapper.add(getEmacsLispMapper());
      mapper.add(getXQueryMapper());
      mapper.add(getQ2Mapper());
      mapper.add(getXsltMapper());
      mapper.add(getCommonLispMapper());
      return mapper;
    } else if (languageMatches("scheme", ".scm", ".sc")) { // Scheme
      return getSchemeMapper();
    } else if (languageMatches("krl", ".krl")) {
      return getKrlMapper();
    } else if (languageMatches("brl", ".brl")) {
      return getBrlMapper();
    } else if (languageMatches("emacs", "elisp", "emacs-lisp", ".el")) {
      return getEmacsLispMapper();
    } else if (languageMatches("xquery", ".xquery", ".xq", ".xql")) {
      return getXQueryMapper();
    } else if (languageMatches("q2", ".q2")) {
      return getQ2Mapper();
    } else if (languageMatches("xslt", "xsl", ".xsl")) {
      return getXsltMapper();
    } else if (languageMatches("commonlisp", "common-lisp", "clisp",
                               "lisp", ".lisp", ".lsp", ".cl")) {
      return getCommonLispMapper();
    } else {
      return null;
    }
  }

  /**
   * Compares the language property to each of the given strings, and
   * returns true if there is a match.
   */
  private boolean languageMatches(String... possibilities) {
    for (String s : possibilities)
      if (s.equalsIgnoreCase(language))
        return true;
    return false;
  }

  ///////////////////////////
  ///// FileNameMappers /////
  ///////////////////////////

  private FileNameMapper getSchemeMapper() {
    return getMapper(".scm", ".sc");
  }
  private FileNameMapper getKrlMapper() {
    return getMapper(".krl");
  }
  private FileNameMapper getBrlMapper() {
    return getMapper(".brl");
  }
  private FileNameMapper getEmacsLispMapper() {
    return getMapper(".el");
  }
  private FileNameMapper getXQueryMapper() {
    return getMapper(".xquery", ".xq", ".xql");
  }
  private FileNameMapper getQ2Mapper() {
    return getMapper(".q2");
  }
  private FileNameMapper getXsltMapper() {
    return getMapper(".xsl");
  }
  private FileNameMapper getCommonLispMapper() {
    return getMapper(".lisp", ".lsp", ".cl");
  }

  /**
   * Constructs a glob pattern mapper which matches file names ending
   * with {@code ext} to ones ending with ".class".
   * @param ext a file name extension, including the period
   * (e.g. ".scm").
   */
  private FileNameMapper getMapper(String ext) {
    GlobPatternMapper m = new GlobPatternMapper();
    m.setFrom("*" + ext);
    m.setTo("*.class");
    return m;
  }

  /**
   * Constructs a composite mapper which consists of glob pattern
   * mappers for each of the given extensions.
   */
  private FileNameMapper getMapper(String... extensions) {
    CompositeMapper m = new CompositeMapper();
    for (String ext : extensions)
      m.add(getMapper(ext));
    return m;
  }

  /**
   * Gets the list of files to be compiled.
   * @return the list of files as an array
   */
  public File[] getFileList() {
    return compileList;
  }

  /**
   * Check that all required attributes have been set and nothing
   * silly has been entered.
   * @exception BuildException if an error occurs
   */
  private void checkParameters() throws BuildException {
    if (usedMatchingTask && src == null) {
      throw new BuildException("srcdir attribute must be set!",
                               getLocation());
    }
    if (usedMatchingTask && src.size() == 0) {
      throw new BuildException("srcdir attribute must be set!",
                               getLocation());
    }

    if (destDir != null && !destDir.isDirectory()) {
      throw new BuildException("destination directory \"" + destDir +
                               "\" does not exist or is not a directory",
                               getLocation());
    }

    if (filesets.size() > 0 && destDir == null) {
      throw new BuildException("destination directory must be set when"
                               + " compiling filesets!", getLocation());
    }

    if (!usedMatchingTask && filesets.size() == 0) {
      throw new BuildException("either srcdir or a nested fileset/list"
                               + " must be specified", getLocation());
    }
  }

  /**
   * Perform the compilation.
   */
  private void compile() {
    if (compileList.length > 0) {
      log("Compiling " + compileList.length + " source file"
          + (compileList.length == 1 ? "" : "s")
          + (destDir != null ? " to " + destDir : ""));

      if (listFiles) {
        for (int i = 0; i < compileList.length; i++) {
          String filename = compileList[i].getAbsolutePath();
          log(filename);
        }
      }

      // Set up the command line.
      Commandline cmd = new Commandline();
      setupKawaCommandline(cmd);
      int firstFileName = cmd.size();
      logAndAddFilesToCompile(cmd);

      // Do the compilation
      int rc = executeExternalCompile(cmd.getCommandline(),
                                      firstFileName, true);

      if (rc != 0) {            // Failure
        taskSuccess = false;
        if (errorProperty != null) {
          getProject().setNewProperty(errorProperty, "true");
        }
        if (failOnError) {
          throw new BuildException(FAIL_MSG, getLocation());
        } else {
          log(FAIL_MSG, Project.MSG_ERR);
        }
      }
    }
  }

  /**
   * Get the Java executable which will invoke kawa.repl.
   */
  private String getSystemJava() {
    return JavaEnvUtils.getJreExecutable("java");
  }

  /**
   * Build the command line, translating the parameters into flags for
   * java and kawa.repl.
   * @param cmd the Commandline to be executed
   */
  private void setupKawaCommandline(Commandline cmd) {
    cmd.setExecutable(getSystemJava());

    Path classpath = getCompileClasspath();

    cmd.createArgument().setValue("-classpath");
    cmd.createArgument().setPath(classpath);

    cmd.createArgument().setValue("kawa.repl");

    if (target != null) {
      cmd.createArgument().setValue("--target");
      cmd.createArgument().setValue(target);
    }

    if (destDir != null) {
      cmd.createArgument().setValue("-d");
      cmd.createArgument().setFile(destDir);
    }

    if (prefix != null) {
      cmd.createArgument().setValue("-P");
      cmd.createArgument().setValue(prefix);
    }

    if (language != null) {
      cmd.createArgument().setValue("--"+language);
    }

    if (main) {
      cmd.createArgument().setValue("--main");
    }

    if (applet) {
      cmd.createArgument().setValue("--applet");
    }

    if (servlet) {
      cmd.createArgument().setValue("--servlet");
    }

    if (fullTailCalls) {
      cmd.createArgument().setValue("--full-tailcalls");
    // } else {
    //   cmd.createArgument().setValue("--no-full-tailcalls");
    }

    if (moduleStatic != null) {
      if ("yes".equals(moduleStatic) || "on".equals(moduleStatic) ||
          "true".equals(moduleStatic))
        cmd.createArgument().setValue("--module-static");
      else if ("run".equals(moduleStatic))
        cmd.createArgument().setValue("--module-static-run");
      else if("no".equals(moduleStatic) || "off".equals(moduleStatic)
              || "false".equals(moduleStatic))
        cmd.createArgument().setValue("--no-module-static");
    }

    if (warnUndefinedVariable) {
      cmd.createArgument().setValue("--warn-undefined-variable");
    }

    if (warnAsError) {
      cmd.createArgument().setValue("--warn-as-error");
    }

    // Add nested command line args.
    for (Commandline.Argument arg : otherArgs) {
      for (String p : arg.getParts()) {
        cmd.createArgument().setValue(p); // should probably do some validation?
      }
    }

    cmd.createArgument().setValue("-C");
  }

  /**
   * Add the file names to the Commandline. Log them in verbose mode.
   * @param cmd the Commandline to be executed
   */
  private void logAndAddFilesToCompile(Commandline cmd) {
    log("Compilation " + cmd.describeArguments(),
        Project.MSG_VERBOSE);

    StringBuffer niceSourceList = new StringBuffer("File");
    if (compileList.length != 1) {
      niceSourceList.append("s");
    }
    niceSourceList.append(" to be compiled:");
    niceSourceList.append(StringUtils.LINE_SEP);

    for (int i = 0; i < compileList.length; i++) {
      String arg = compileList[i].getAbsolutePath();
      cmd.createArgument().setValue(arg);
      niceSourceList.append("    ");
      niceSourceList.append(arg);
      niceSourceList.append(StringUtils.LINE_SEP);
    }
    log(niceSourceList.toString(), Project.MSG_VERBOSE);
  }

  /**
   * Get the classpath to use, which includes a specified CP along
   * with the destination directory if appropriate.
   * @return the classpath as a Path
   */
  private Path getCompileClasspath() {
    Path classpath = new Path(getProject());
    // add dest dir to classpath so that previously compiled and
    // untouched classes are on classpath
    if (destDir != null && isIncludeDestClasses()) {
      classpath.setLocation(destDir);
    }

    Path cp = compileClasspath;
    if (cp == null) {
      cp = new Path(getProject());
    }
    classpath.addExisting(cp);
    return classpath;
  }

  /**
   * Invoke kawa.repl to really do the compilation.
   */
  private int executeExternalCompile(String[] args, int firstFileName,
                                     boolean quoteFiles) {

    try {
      Execute exe = new Execute(new LogStreamHandler(this,
                                                     Project.MSG_INFO,
                                                     Project.MSG_WARN));
      if (Os.isFamily("openvms")) {
        // Use the VM launcher instead of shell launcher on VMS for
        // java
        exe.setVMLauncher(true);
      }
      exe.setAntRun(getProject());
      exe.setWorkingDirectory(getProject().getBaseDir());
      exe.setCommandline(args);
      exe.execute();
      return exe.getExitValue();
    } catch (IOException e) {
      throw new BuildException("Error running Kawa compiler", e, getLocation());
    }
  }
}
