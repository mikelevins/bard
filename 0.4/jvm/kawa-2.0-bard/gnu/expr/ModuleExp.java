package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.reflect.StaticFieldLocation;
import gnu.kawa.reflect.FieldLocation;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import java.io.*;
import java.net.URL;

/**
 * Class used to implement Scheme top-level environments.
 * @author	Per Bothner
 */

public class ModuleExp extends LambdaExp
		       implements Externalizable
{
  public static final int EXPORT_SPECIFIED = LambdaExp.NEXT_AVAIL_FLAG;
  public static final int STATIC_SPECIFIED = EXPORT_SPECIFIED << 1;
  public static final int NONSTATIC_SPECIFIED = STATIC_SPECIFIED << 1;
  public static final int SUPERTYPE_SPECIFIED = NONSTATIC_SPECIFIED << 1;
  public static final int STATIC_RUN_SPECIFIED = SUPERTYPE_SPECIFIED << 1;
  public static final int LAZY_DECLARATIONS = STATIC_RUN_SPECIFIED << 1;

  /** True if the module is immediately evaluated. */
  public static final int IMMEDIATE = LAZY_DECLARATIONS << 1;

  /** True of a read-eval-print interface where each module is only partial.
   * Conceptually, each statement is a fragment of a imagined super-module
   * for the whole interaction, though currently there is no super-module object.
   * IMMEDIATE is also set in this case.
   */
  public static final int INTERACTIVE = IMMEDIATE << 1;

  /** Using explicit class (e.g. define-simple-class) for module class. */
  public static final int USE_DEFINED_CLASS = INTERACTIVE << 1;

  public static final int HAS_SUB_MODULE = USE_DEFINED_CLASS << 1;

  public ModuleExp ()
  {
  }

    public static ModuleExp valueOf(ClassType type) {
        return ModuleInfo.find(type).getModuleExp();
    }

  /** Used to control which .zip file dumps are generated. */
  public static String dumpZipPrefix;

  static int lastZipCounter;

  /** Compile to a class for immediate evaluation.
   * Return null on error, if so errors go to comp.getMessages().
   */
  public static Class evalToClass (Compilation comp, URL url)
    throws SyntaxException
  {
    ModuleExp mexp = comp.getModule();
    ModuleInfo minfo = mexp.info;
    SourceMessages messages = comp.getMessages();
    try
      {

        minfo.loadByStages(Compilation.COMPILED);

	if (messages.seenErrors())
	  return null;

	ArrayClassLoader loader = comp.loader;
        if (url == null)
          url = Path.currentPath().toURL();
        loader.setResourceContext(url);

	java.util.zip.ZipOutputStream zout = null;
	if (dumpZipPrefix != null)
	  {
	    StringBuffer zipname = new StringBuffer(dumpZipPrefix);
            ModuleManager manager = ModuleManager.getInstance();
            lastZipCounter++;
	    if (manager.interactiveCounter > lastZipCounter)
	      lastZipCounter = manager.interactiveCounter;
            zipname.append(lastZipCounter);
	    zipname.append(".zip");
	    java.io.FileOutputStream zfout
	      = new java.io.FileOutputStream(zipname.toString());
	    zout = new java.util.zip.ZipOutputStream(zfout);
	  }

	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType clas = comp.classes[iClass];
	    String className = clas.getName ();
	    byte[] classBytes = clas.writeToArray ();
	    loader.addClass(className, classBytes);

	    if (zout != null)
	      {
		String clname = className.replace ('.', '/') + ".class";
		java.util.zip.ZipEntry zent
		  = new java.util.zip.ZipEntry (clname);
		zent.setSize(classBytes.length);
		java.util.zip.CRC32 crc = new java.util.zip.CRC32();
		crc.update(classBytes);
		zent.setCrc(crc.getValue());
		zent.setMethod(java.util.zip.ZipEntry.STORED);
		zout.putNextEntry(zent);
		zout.write(classBytes);
	      }
	  }
	if (zout != null)
	  {
	    zout.close ();
	  }

	/* DEBUGGING:
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  ClassTypeWriter.print(comp.classes[iClass], System.out, 0);
	*/

        Class clas = null;
        // Use the "session" ClassLoader, for remembering classes
        // created in one command (Compilation) through further command,
        // while still allowing the classes to be replaced and collected.
        ArrayClassLoader context = loader;
        while (context.getParent() instanceof ArrayClassLoader)
          context = (ArrayClassLoader) context.getParent();
	for (int iClass = 0;  iClass < comp.numClasses;  iClass++)
	  {
	    ClassType ctype = comp.classes[iClass];
            Class cclass = loader.loadClass(ctype.getName());
            ctype.setReflectClass(cclass);
            ctype.setExisting(true);
            if (iClass == 0)
              clas = cclass;
            // Add all classes except the main module class to the "session"
            // ClassLoader.  Don't add the main module class, as it's
            // anonymous.  We might go further and skip other anonymous
            // classes (theough defining which classes are anonymous is tricky).
            else if (context != loader)
              context.addClass(cclass);
          }

        minfo.setModuleClass(clas);
        comp.cleanupAfterCompilation();
        int ndeps = minfo.numDependencies;

        for (int idep = 0;  idep < ndeps;  idep++)
          {
            ModuleInfo dep = minfo.dependencies[idep];
            Class dclass = dep.getModuleClassRaw();
            if (dclass == null)
              dclass = evalToClass(dep.getCompilation(), null);
            comp.loader.addClass(dclass);
          }

        return clas;
      }
    catch (java.io.IOException ex)
      {
	throw new WrappedException("I/O error in lambda eval", ex);
      }
    catch (ClassNotFoundException ex)
      {
	throw new WrappedException("class not found in lambda eval", ex);
      }
    catch (Exception ex)
      {
        messages.error('f', "internal compile error - caught "+ex, ex);
        throw new SyntaxException(messages);
      }
  }

    /** @deprecated */
    public static boolean compilerAvailable = true;

    /** 1: have compiler; -1: don't have compiler: 0: need to check. */
    private static int haveCompiler;

    public static synchronized boolean compilerAvailable() {
        if (haveCompiler == 0) {
            if (! compilerAvailable)
                haveCompiler = -1;
            else if ("Dalvik".equals(System.getProperty("java.vm.name")))
                haveCompiler = -1;
            else {
                try {
                    // Just in case you somehow managed to get this far
                    // while using kawart - i.e. the Kawa "runtime" jar.
                    Class.forName("gnu.expr.TryExp");
                    haveCompiler = 1;
                } catch (Exception ex) {
                    haveCompiler = -1;
                }
            }
        }
        return haveCompiler >= 0;
    }

  /** Flag to force compilation, even when not required. */
  public static boolean alwaysCompile = compilerAvailable();

  public final static boolean evalModule (Environment env, CallContext ctx,
                                       Compilation comp, URL url,
                                       OutPort msg)
    throws Throwable
  {
    ModuleExp mexp = comp.getModule();
    Language language = comp.getLanguage();
    Object inst = evalModule1(env, comp, url, msg);
    if (inst == null) {
	comp.pop(comp.mainLambda);
	return false;
    }
    evalModule2(env, ctx, language, mexp, inst);
    return true;
  }

  /** Parse and compile a module.
   * @return null on error; otherwise a "cookie" that can be passed
   * to evalModule2 or CompiledModule.
   */
  public final static Object evalModule1 (Environment env,
                                          Compilation comp, URL url,
                                          OutPort msg)
    throws SyntaxException
  {
    ModuleExp mexp = comp.getModule();
    Environment orig_env = Environment.setSaveCurrent(env);
    Compilation orig_comp = Compilation.setSaveCurrent(comp);
    SourceMessages messages = comp.getMessages();
    ClassLoader savedLoader = null;
    Thread thread = null; // Non-null if we need to restore context ClassLoader.
    try
      {
        comp.process(Compilation.RESOLVED);
        comp.getMinfo().loadByStages(Compilation.WALKED);

        if (msg != null ? messages.checkErrors(msg, 20) : messages.seenErrors())
          return null;

	if (! comp.mustCompile)
	  {
	    if (Compilation.debugPrintFinalExpr && msg != null)
	      {
		msg.println ("[Evaluating final module \""+mexp.getName()+"\":");
		mexp.print(msg);
		msg.println(']');
		msg.flush();
	      }
            return Boolean.TRUE;
          }
        else
          {
		Class clas = evalToClass(comp, url);
		if (clas == null)
		  return null;
                try
                  {
                    thread = Thread.currentThread();
                    savedLoader = thread.getContextClassLoader();
                    thread.setContextClassLoader(clas.getClassLoader());
                  }
                catch (Exception ex)
                  {
                    thread = null;
                  }

                mexp.body = null;
                mexp.thisVariable = null;
                if (msg != null ? messages.checkErrors(msg, 20)
                    : messages.seenErrors())
                  return null;
                return clas;
          }
      }
    finally
      {
        Environment.restoreCurrent(orig_env);
        Compilation.restoreCurrent(orig_comp);
        if (thread != null)
          thread.setContextClassLoader(savedLoader);
      }
  }

  public final static void evalModule2 (Environment env, CallContext ctx,
                                        Language language, ModuleExp mexp,
                                        Object inst)
    throws Throwable
  {
    Environment orig_env = Environment.setSaveCurrent(env);
    ClassLoader savedLoader = null;
    Thread thread = null; // Non-null if we need to restore context ClassLoader.
    try
      {
	if (inst == Boolean.TRUE)
	  { // optimization - don't generate unneeded Class.
	    mexp.body.apply(ctx);
	  }
	else
	  {
            if (inst instanceof Class)
              inst = ModuleContext.getContext().findInstance((Class) inst);

            if (inst instanceof Runnable)
              {
                if (inst instanceof ModuleBody)
                  {
                    ModuleBody mb = (ModuleBody) inst;
                    if (! mb.runDone)
                      {
                        mb.runDone = true;
                        mb.run(ctx);
                      }
                  }
                else
                  ((Runnable) inst).run();
              }

            if (mexp == null)
              gnu.kawa.reflect.ClassMemberLocation.defineAll(inst, language, env);
            else
              {
                // Import declarations defined in module into the Environment.
		for (Declaration decl = mexp.firstDecl();
		     decl != null;  decl = decl.nextDecl())
		  {
		    Object dname = decl.getSymbol();
		    if (decl.isPrivate() || dname == null)
		      continue;
		    Field fld = decl.field;
		    Symbol sym = dname instanceof Symbol ? (Symbol) dname
		      : Symbol.make("", dname.toString().intern());
		    Object property = language.getEnvPropertyFor(decl);
                    Expression dvalue = decl.getValue();
                    // It would be cleaner to not bind these values in
                    // the environment, and just require lexical lookup.
                    // However, various parts of the code makes use of
                    // the environment.
		    if ((decl.field.getModifiers() & Access.FINAL) != 0)
		      {
			Object value;
			if (dvalue instanceof QuoteExp
			    && dvalue != QuoteExp.undefined_exp)
			  value = ((QuoteExp) dvalue).getValue();
			else
			  {
			    value = decl.field.getReflectField().get(null);
                            if (! decl.isIndirectBinding())
                              decl.setValue(QuoteExp.getInstance(value));
                            else if (! decl.isAlias() || ! (dvalue instanceof ReferenceExp))
                              decl.noteValueUnknown();
                          }
			if (decl.isIndirectBinding())
                          env.addLocation(sym, property, (Location) value);
                        else
                          env.define(sym, property, value);
		      }
		    else
		      {
                        StaticFieldLocation loc
                          = new StaticFieldLocation(fld.getDeclaringClass(),
                                                    fld.getName());
			loc.setDeclaration(decl);
			env.addLocation(sym, property, loc);
			decl.noteValueUnknown();
		      }
		  }
	      }
            /*
	    catch (IllegalAccessException ex)
	      {
		throw new RuntimeException("class illegal access: in lambda eval");
	      }
            */
	  }
	ctx.runUntilDone();
      }
    finally
      {
        Environment.restoreCurrent(orig_env);
        if (thread != null)
          thread.setContextClassLoader(savedLoader);
      }
  }

  ModuleInfo info;

  public String getNamespaceUri () { return info.uri; }

    public final ClassType getSuperType() {
        return compiledType.getSuperclass();
    }
    public final void setSuperType(ClassType s) {
        compiledType.setSuper(s);
    }
    public final ClassType[] getInterfaces() {
        return compiledType.getInterfaces();
    }
    public final void setInterfaces(ClassType[] s) {
        compiledType.setInterfaces(s);
    }

  public final boolean isStatic ()
  {
    // In immediate mode there is no point in a non-static module:
    // a static module is simpler and more efficient.
    return (getFlag(STATIC_SPECIFIED)
	    || ((gnu.expr.Compilation.moduleStatic >= Compilation.MODULE_STATIC_DEFAULT
                 || getFlag(IMMEDIATE))
		&& ! getFlag(SUPERTYPE_SPECIFIED)
		&& ! getFlag(NONSTATIC_SPECIFIED)));
  }

  /** True if module body (i.e. run) is called by class initializer. */
  public boolean staticInitRun ()
  {
    return (isStatic()
            && (getFlag(STATIC_RUN_SPECIFIED)
                || Compilation.moduleStatic == Compilation.MODULE_STATIC_RUN));
  }

  public void allocChildClasses (Compilation comp)
  {
    declareClosureEnv();
    if (! comp.usingCPStyle())
      return;
    allocFrame(comp);
  }

  void allocFields (Compilation comp)
  {
    // We want the create the loc$XXX Symbol fields for unknowns first,
    // because it is possible some later Declaration's initializer may depend
    // on it.  Normally this is not an issue, as initializer are usually
    // run as part of the "body" of the module, which is executed later.
    // However, constant initializers are an exception - they are
    // executed at init time.
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if ((decl.isSimple() && ! decl.isPublic()) || decl.field != null)
	  continue;
	if (decl.getFlag(Declaration.IS_UNKNOWN)
            // We might have an unrefered unknown if the reference gets
            // optimized away. For example references to <CLASSNAME>.
            && decl.getFlag(Declaration.CAN_READ|Declaration.CAN_CALL))
	  decl.makeField(comp, null);
      }
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (decl.field != null)
	  continue;
	Expression value = decl.getValue();
        if (((decl.isSimple() && decl.isModuleLocal()))
            // Kludge - needed for macros - see Savannah bug #13601.
            && ! decl.isNamespaceDecl()
            && ! (decl.getFlag(Declaration.IS_CONSTANT)
                  && decl.getFlag(Declaration.CAN_READ|Declaration.CAN_CALL)))
	  continue;
	if (decl.getFlag(Declaration.IS_UNKNOWN))
	  continue;
	if (value instanceof LambdaExp
            && ! (value instanceof ModuleExp // from a module-name command.
                  || value instanceof ClassExp))
	  {
	    ((LambdaExp) value).allocFieldFor(comp);
	  }
	else
	  {
	    decl.makeField(comp,
                           decl.shouldEarlyInit() || decl.isAlias() ? value
                           : null);
	  }
      }
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitModuleExp(this, d);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Module/", ")", 2);
    Object sym = getSymbol();
    if (sym != null)
      {
	out.print(sym);
	out.print('/');
      }
    out.print(id);
    out.print('/');
    out.writeSpaceFill();
    out.startLogicalBlock("(", false, ")");
    Declaration decl = firstDecl();
    if (decl != null)
      {
	out.print("Declarations:");
	for (; decl != null;  decl = decl.nextDecl())
	  {
	    out.writeSpaceFill();
	    decl.printInfo(out);
	  }
      }
    out.endLogicalBlock(")");
    out.writeSpaceLinear();
    if (body == null)
      out.print("<null body>");
    else
      body.print (out);
    out.endLogicalBlock(")");
  }

  public Declaration firstDecl ()
  {
    synchronized (this)
      {
	if (getFlag(LAZY_DECLARATIONS))
          info.setupModuleExp();
      }
    return decls;
  }

  /** Return the class for this module.
   * If not set yet, sets it now, based on the source file name.
   */
  public ClassType classFor (Compilation comp)
  {
    if (compiledType != null && compiledType != Compilation.typeProcedure)
      return (ClassType) compiledType;
    String mname = getName();
    String className = getFileName();
    Path path = null;
    if (comp.getModule() == this && info != null
        && info.className != null)
      // If explicitly set, perhaps using command-line flags.
      className = info.className;
    else
      {
        if (mname != null)
          className = mname;
        else if (className == null)
          className = "$unnamed_input_file$";
        else if (className.equals("-") || className.equals("/dev/stdin"))
          className = "$stdin$";
        else
          {
            path = Path.valueOf(className);
            className = path.getLast();
            int dotIndex = className.lastIndexOf('.');
            if (dotIndex > 0)
              className = className.substring (0, dotIndex);
          }
        className = Compilation.mangleNameIfNeeded(className);

        Path parentPath;
        String parent;
        if (comp.classPrefix.length() == 0
            && path != null
            && ! path.isAbsolute()
            && (parentPath = path.getParent()) != null
            && (parent = parentPath.toString()).length() > 0 // Probably redundant.
            && parent.indexOf("..") < 0)
          {
            parent = parent.replace(System.getProperty("file.separator"), "/");
            if (parent.startsWith("./"))
              parent = parent.substring(2);
            className = parent.equals(".") ? className
              : Compilation.mangleURI(parent) + "." + className;
          }
        else
          className = comp.classPrefix + className;
      }
    if (mname == null)
      setName(className);
    ClassType clas = new ClassType(className);
    setType(clas);
    if (comp.mainLambda == this)
      {
        if (comp.mainClass == null)
          comp.mainClass = clas;
        else if (! className.equals(comp.mainClass.getName()))
          comp.error('e', "inconsistent main class name: "+className
                     +" - old name: "+comp.mainClass.getName());
      }
    return clas;
  }

  void makeDeclInModule2 (Declaration fdecl)
  {
    Object fvalue = fdecl.getConstantValue();
    if (fvalue instanceof FieldLocation)
      {
	FieldLocation floc = (FieldLocation) fvalue;
        Declaration vdecl = floc.getDeclaration();
        ReferenceExp fref = new ReferenceExp(vdecl);
        fdecl.setAlias(true);
        fref.setDontDereference(true);
        fdecl.setValue(fref);
        if (vdecl.isProcedureDecl())
          fdecl.setProcedureDecl(true);
        if (vdecl.getFlag(Declaration.IS_SYNTAX))
          fdecl.setSyntax();
        if (! fdecl.getFlag(Declaration.STATIC_SPECIFIED))
          {
            ClassType vtype = floc.getDeclaringClass();
            String vname = vtype.getName();
            for (Declaration xdecl = firstDecl();
                 xdecl != null;  xdecl = xdecl.nextDecl())
              {
                if (vname.equals(xdecl.getType().getName())
                    && xdecl.getFlag(Declaration.MODULE_REFERENCE))
                  {
                    fref.setContextDecl(xdecl);
                    break;
                  }
              }
          }
      }
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    String name = null;
    if (compiledType != null && compiledType != Compilation.typeProcedure
	&& ! compiledType.isExisting())
      // The class is (presumably) one we're currently generating.
      // At run-time it may be loaded by a non-system ClassLoader.
      // Thus compiling the class literal needs to use loadClassRef.
      out.writeObject(compiledType);
    else
      {
	if (name == null)
	  name = getName();
	if (name == null)
	  name = getFileName();
	out.writeObject(name);
      }
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    Object name = in.readObject();
    if (name instanceof ClassType)
      {
	compiledType = (ClassType) name;
	setName(compiledType.getName());
      }
    else
      setName((String) name);
    flags |= LAZY_DECLARATIONS;
  }
}
