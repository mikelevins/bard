// Copyright (c) 2006, 2008 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.io.Path;
import java.util.*;
import gnu.kawa.util.AbstractWeakHashTable;

public class ModuleInfo {

    /** Name of class that implements module.
     * Must be non-null unless we're currently compiling the module,
     * in which case sourcePath and comp must both be non-null.
     */
    protected String className;

    Class moduleClass;

    static ClassToInfoMap mapClassToInfo = new ClassToInfoMap();

    /** The namespace URI associated with this module, or {@code null}.
     * This is null for Scheme modules, but non-null for XQuery modules.
     */
    public String getNamespaceUri() { return uri; }
    public void setNamespaceUri(String uri) { this.uri = uri; }
    String uri;

    ModuleExp exp;
    private Compilation comp;

    public Compilation getCompilation() { return comp; }

    public void setCompilation(Compilation comp) {
        this.comp = comp;
        if (comp == null)
            return;
        ModuleExp mod = comp.mainLambda;
        mod.info = this;
        this.exp = mod;
        String fileName;
        if (mod != null && (fileName = mod.getFileName()) != null) {
            this.sourcePath = fileName;
            Path abs = absPath(fileName);
            this.sourceAbsPath = abs;
        }
    }

    public void cleanupAfterCompilation() {
        if (comp != null)
            comp.cleanupAfterCompilation();
    }

    public static Path absPath(String path) {
        return Path.valueOf(path).getCanonical();
    }
  
    ModuleInfo[] dependencies;
    int numDependencies;

    /** Location of source for module, if known.
     * This is an absolute URI, absolute filename,
     * or filename relative to current working directory.
     * Null if source not known; in that case className must be non-null.
     * Avoid using, since "relative to current working directory"
     * is unreliable if the working directory can change.
     */
    public String sourcePath;
    Path sourceAbsPath;
    String sourceAbsPathname;

    public long lastCheckedTime;
    /** Last time the source file was modified.
     * At least the last time we checked ...
     */
    public long lastModifiedTime;

    public Path getSourceAbsPath() {
        return sourceAbsPath;
    }

    public void setSourceAbsPath(Path path) {
        sourceAbsPath = path;
        sourceAbsPathname = null;
    }

    public String getSourceAbsPathname() {
        String str = sourceAbsPathname;
        if (str == null && sourceAbsPath != null) {
            str = sourceAbsPath.toString();
            sourceAbsPathname = str;
        }
        return str;
    }

    public synchronized void addDependency(ModuleInfo dep) {
        if (dependencies == null)
            dependencies = new ModuleInfo[8];
        else if (numDependencies == dependencies.length) {
            ModuleInfo[] deps = new ModuleInfo[2 * numDependencies];
            System.arraycopy(dependencies, 0, deps, 0, numDependencies);
            dependencies = deps;
        }
        dependencies[numDependencies++] = dep;
    }

    public synchronized ClassType getClassType() {
        if (moduleClass != null)
            return (ClassType) Type.make(moduleClass);
        if (comp != null && comp.mainClass != null)
            return comp.mainClass;
        return ClassType.make(className);
    }

    public synchronized String getClassName() {
        if (className == null) {
            if (moduleClass != null)
                className = moduleClass.getName();
            else if (comp != null && comp.mainClass != null)
                className = comp.mainClass.getName();
        }
        return className;
    }

    public void setClassName(String name) {
        className = name;
    }

    public ModuleExp getModuleExpRaw() { return exp; }

    public synchronized ModuleExp getModuleExp() {
        ModuleExp m = exp;
        if (m == null) {
            ClassType ctype = ClassType.make(className);
            m = new ModuleExp();
            m.compiledType = ctype;
            m.setName(ctype.getName());
            m.flags |= ModuleExp.LAZY_DECLARATIONS;
            m.info = this;
            exp = m;
        }
        return m;
    }

    /** If module has LAZY_DECLARATIONS, fix that. */
    public synchronized ModuleExp setupModuleExp() {
        ModuleExp mod = getModuleExp();
        if ((mod.flags & ModuleExp.LAZY_DECLARATIONS) == 0) {
            if (moduleClass != null) {
                for (Declaration decl = mod.firstDecl();
                     decl != null;  decl = decl.nextDecl()) {
                    Field fld = decl.field;
                    if (fld == null
                        || decl.isIndirectBinding()
                        || (fld.getFlags() & Access.STATIC) == 0
                        || decl.getValueRaw() instanceof QuoteExp)
                        continue;
                    try {
                        Object fvalue = moduleClass
                            .getField(fld.getName()).get(null);
                        decl.setValue(new QuoteExp(fvalue));
                    } catch (Exception ex) {
                        continue;
                    }
                }
            }
            return mod;
        }
        mod.setFlag(false, ModuleExp.LAZY_DECLARATIONS);
        ClassType type;
        Class rclass;
        if (moduleClass != null) {
            rclass = moduleClass;
            type = (ClassType) Type.make(rclass);
        } else {
            type = ClassType.make(className);
            rclass = type.getReflectClass();
        }
        Object instance = null;

        Language language = Language.getDefaultLanguage();
        for (Field fld = type.getFields();
             fld != null;  fld = fld.getNext()) {
            int flags = fld.getFlags();
            if ((flags & Access.PUBLIC) == 0)
                continue;
            try {
                Type ftype = fld.getType();
                Declaration fdecl;
                if ((flags & Access.FINAL) != 0
                    && (! ftype.isSubtype(Compilation.typeLocation)
                        || ftype.isSubtype(Compilation.typeFieldLocation))) {
                    if ((flags & Access.STATIC) == 0 && instance == null)
                        instance = getInstance();
                    Object fvalue = rclass.getField(fld.getName()).get(instance);
                    fdecl = language.declFromField(mod, fvalue, fld);
                    fdecl.noteValue(new QuoteExp(fvalue));
                } else {
                    // FIXME - Only used to get name - better to use an annotation.
                    Object fvalue = (flags & Access.STATIC) == 0 ? null
                        : rclass.getField(fld.getName()).get(null);
                    fdecl = language.declFromField(mod, fvalue, fld);
                    fdecl.noteValueUnknown();
                }
            } catch (Exception ex) {
                throw new WrappedException(ex);
            }
        }

        for (Declaration fdecl = mod.firstDecl();
             fdecl != null;  fdecl = fdecl.nextDecl()) {
            mod.makeDeclInModule2(fdecl);
        }
        return mod;
    }

    public synchronized Class getModuleClass() throws ClassNotFoundException {
        Class mclass = moduleClass;
        if (mclass != null)
            return mclass;
        mclass = ClassType.getContextClass(className);
        moduleClass = mclass;
        return mclass;
    }

    public Class getModuleClassRaw() {
        return moduleClass;
    }

    public void setModuleClass(Class clas) {
        moduleClass = clas;
        className = clas.getName();
        mapClassToInfo.put(clas, this);
    }

    public static ModuleInfo findFromInstance(Object instance) {
        return ModuleContext.getContext().findFromInstance(instance);
    }

    public static ModuleInfo find(ClassType type) {
        if (type.isExisting()) {
            try {
                return ModuleManager.findWithClass(type.getReflectClass());
            } catch (Exception ex) {
            }
        }
        return ModuleManager.getInstance().findWithClassName(type.getName());
    }

    public static void register(Object instance) {
        ModuleContext.getContext().setInstance(instance);
    }

    public Object getInstance() {
        return ModuleContext.getContext().findInstance(this);
    }

    public Object getRunInstance() {
        Object inst = getInstance();
        if (inst instanceof Runnable)
            ((Runnable) inst).run();
        return inst;
    }

    public int getState() { return comp == null ? Compilation.CLASS_WRITTEN : comp.getState(); }

    public void loadByStages(int wantedState) {
        int state = getState();
        if (state + 1 >= wantedState)
            return;
        loadByStages(wantedState - 2);
        state = getState();
        if (state >= wantedState) // Most likely? if ERROR_SEEN.
            return;
        comp.setState(state+1);
        int ndeps = numDependencies;
        int depWanted = ((Compilation.writeImplicitClasses
                          && wantedState >= Compilation.CLASS_WRITTEN)
                         ? Compilation.COMPILED
                         : wantedState);
        for (int idep = 0;  idep < ndeps;  idep++) {
            ModuleInfo dep = dependencies[idep];
            dep.loadByStages(depWanted);
        }
        state = getState();
        if (state >= wantedState) // Most likely? if ERROR_SEEN.
            return;
        comp.setState(state & ~1);
        comp.process(wantedState);
    }

    /** Eagerly process the module and dependencies.
     * @return true on success; false if we were unable to because of
     * an error or a cyclic dependency.
     */
    public boolean loadEager(int wantedState) {
        if (comp == null && className != null)
            return false;
        int state = getState();
        if (state >= wantedState)
            return true;
        if ((state & 1) != 0)
            return false;
        comp.setState(state + 1);
        int ndeps = numDependencies;
        for (int idep = 0;  idep < ndeps;  idep++) {
            ModuleInfo dep = dependencies[idep];
            if (! dep.loadEager(wantedState)) {
                if (getState() == state+1)
                    comp.setState(state);
                return false;
            }
        }
        if (getState() == state+1)
            comp.setState(state);
        comp.process(wantedState);
        return getState() == wantedState;
    }

    public void clearClass() {
        moduleClass = null;
        numDependencies = 0;
        dependencies = null;
    }

    /** Check if this module and its dependencies are up-to-dete.
     * Only checks the sourcePath's modification time if it is at least
     * ModifiedCacheTime since last time we checked.
     * As as side-effects update lastModifiedTime and lastCheckedTime.
     */
    public boolean checkCurrent(ModuleManager manager, long now) {
        if (sourceAbsPath == null)
            return true;
        if (lastCheckedTime + manager.lastModifiedCacheTime >= now)
            return moduleClass != null;
        long lastModifiedTime = sourceAbsPath.getLastModified();
        long oldModifiedTime = this.lastModifiedTime;
        this.lastModifiedTime = lastModifiedTime;
        lastCheckedTime = now;
        if (className == null)
            return false;
        if (moduleClass == null) {
            try {
                moduleClass = ClassType.getContextClass(className);
            } catch (ClassNotFoundException ex) {
                return false;
            }
        }
        if (oldModifiedTime == 0 && moduleClass != null) {
            String classFilename = className;
            int dot = classFilename.lastIndexOf('.');
            if (dot >= 0)
                classFilename = classFilename.substring(dot+1);
            classFilename = classFilename + ".class";
            java.net.URL resource = moduleClass.getResource(classFilename);
            if (resource != null) {
                try {
                    oldModifiedTime = resource.openConnection().getLastModified();
                } catch (java.io.IOException ex) {
                    resource = null;
                }
            }
            if (resource == null) {
                // Couldn't open timestamp of the .class file.
                // Assume it is current.
                return true;
            }
        }
        if (lastModifiedTime > oldModifiedTime) {
            moduleClass = null;
            return false;
        }
        for (int i = numDependencies;  --i >= 0; ) {
            ModuleInfo dep = dependencies[i];
            if (dep.comp == null && ! dep.checkCurrent(manager, now)) {
                moduleClass = null;
                return false;
            }
        }
        return true;
    }

    public String toString()
    {
        StringBuffer sbuf = new StringBuffer();
        sbuf.append("ModuleInfo[");
        if (moduleClass != null) {
            sbuf.append("class: ");
            sbuf.append(moduleClass);
        } else if (className != null) {
            sbuf.append("class-name: ");
            sbuf.append(className);
        }
        sbuf.append(']');
        return sbuf.toString();
    }

    static class ClassToInfoMap
        extends AbstractWeakHashTable<Class,ModuleInfo> {

        protected Class getKeyFromValue(ModuleInfo minfo) {
            return minfo.moduleClass;
        }

        protected boolean matches(Class oldValue, Class newValue) {
            return oldValue == newValue;
        }
    }
}
