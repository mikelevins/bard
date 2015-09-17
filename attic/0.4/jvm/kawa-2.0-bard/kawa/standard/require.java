// Copyright (C) 2005, 2006 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.kawa.reflect.*;
import gnu.text.*;
import java.util.*;
import kawa.lang.Translator.FormStack;

public class require extends Syntax
{
    public static final require require = new require();
    static { require.setName("require"); }

    /* NOTE on handling mutually recursive modules:

       How can Kawa compile two or more modules that mutually require
       each other?  Kawa separates the "scan" stage (top-level
       scanning of a module, looking for definitions), and "rewrite"
       (expand macros and resolve names) makes this possible.

       If module A sees a (require <B>), it needs to suspend scanning A,
       and import the definitions exported by B.  If B has not been
       compiled yet, it must parse and scan B.  If while scanning B, it
       sees a (require <A>), it must wait to import the definitions of A
       until we've done scanning B, returned to A, and finished scanning
       A.  At that point we can add to B the definitions exported from
       A.  Thus the (require <A>) in B.has to *lazily* imports A's
       definitions, using some kind of placeholder.

       One complication is knowing whether a (require <B>) refers to a
       source file to be compiled.  It is not enough to check if a class
       B exists, since if we're compiling B we want to use the current
       source B.scm, not an older B.class.  This is complicated by the
       (module-name B) declaration: We don't know whether source file
       B.scm provides the B class until we've parsed B.scm.  A solution
       to this problem is that we first parse all the source files (as
       listed on the command line),
       yielding their S-expression form.  We then check for module-name
       forms.  However, the possibility of macros does complicate this:
       There could be a macro that re-defines module-name, and there
       could be a macro that expands to module-name.  Also, we could
       have commands that change the reader or read-table.  Arguably worrying
       about these possibilities may be overkill.  However, it can be
       handled thus: Parse each source file to S-expressions.  Scan each
       source file's S-expression until the first require (if any).
       Then go back to each source file, process the require, and scan
       the rest of the file.  If we see a require for one of the source
       files later in the compilation list, skip it until the end.  At
       the end process any deferred require's.  Finally, do the
       "rewrite" step and the rest of compilation.
    */
    static java.util.Hashtable featureMap = new java.util.Hashtable();

    static void map(String featureName, String className) {
        featureMap.put(featureName, className);
    }

    private static final String SLIB_PREFIX = "gnu.kawa.slib.";

    static {
        map("generic-write", SLIB_PREFIX + "genwrite");
        map("pretty-print", SLIB_PREFIX + "pp");
        map("pprint-file", SLIB_PREFIX + "ppfile");
        map("printf", SLIB_PREFIX + "printf");
        map("xml", SLIB_PREFIX + "XML");
        map("readtable", SLIB_PREFIX + "readtable");
        map("srfi-10", SLIB_PREFIX + "readtable");
        map("http", "gnu.kawa.servlet.HTTP");
        map("servlets", "gnu.kawa.servlet.servlets");
        map("srfi-1", SLIB_PREFIX + "srfi1");
        map("list-lib", SLIB_PREFIX + "srfi1");
        map("srfi-2", SLIB_PREFIX + "srfi2");
        map("and-let*", SLIB_PREFIX + "srfi2");
        map("srfi-13", SLIB_PREFIX + "srfi13");
        map("srfi-14", SLIB_PREFIX + "srfi14");
        map("string-lib", SLIB_PREFIX + "srfi13");
        map("srfi-34", SLIB_PREFIX + "srfi34");
        map("srfi-35", SLIB_PREFIX + "conditions");
        map("condition", SLIB_PREFIX + "conditions");
        map("conditions", SLIB_PREFIX + "conditions");
        map("srfi-37", SLIB_PREFIX + "srfi37");
        map("args-fold", SLIB_PREFIX + "srfi37");
        map("srfi-41", SLIB_PREFIX + "Streams");
        map("srfi-41-streams", SLIB_PREFIX + "Streams");
        map("srfi-41-streams-type", SLIB_PREFIX + "StreamsType");
        map("srfi-41-streams-primitive", SLIB_PREFIX + "StreamsPrimitive");
        map("srfi-41-streams-derived", SLIB_PREFIX + "StreamsDerived");
        map("srfi-60", SLIB_PREFIX + "srfi60");
        map("srfi-64", SLIB_PREFIX + "testing");
        map("testing", SLIB_PREFIX + "testing");
        map("srfi-69", SLIB_PREFIX + "srfi69");
        map("hash-table", SLIB_PREFIX + "srfi69");
        map("basic-hash-tables", SLIB_PREFIX + "srfi69");
        map("srfi-95", "kawa.lib.srfi95");
        map("sorting-and-merging", "kawa.lib.srfi95");
        map("srfi-101", SLIB_PREFIX + "ralists");
        map("random-access-lists", SLIB_PREFIX + "ralists");
        map("ra-lists", SLIB_PREFIX + "ralists");
        map("regex", "kawa.lib.kawa.regex");
        map("pregexp", SLIB_PREFIX + "pregexp");
        map("gui", SLIB_PREFIX + "gui");
        map("swing-gui", SLIB_PREFIX + "swing");
        map("android-defs", "gnu.kawa.android.defs");
        map("javafx-defs", "gnu.kawa.javafx.defs");
        map("syntax-utils", SLIB_PREFIX + "syntaxutils");
        map("quaternions", "kawa.lib.kawa.quaternions");
    }

    public static String mapFeature(String featureName) {
        return (String) featureMap.get(featureName);
    }

    public static Object find(String typeName) {
        return ModuleManager.getInstance()
            .findWithClassName(typeName).getInstance();
    }

    @Override
    public boolean scanForDefinitions (Pair st, ScopeExp defs, Translator tr) {
        if (tr.getState() == Compilation.PROLOG_PARSING) {
            tr.setState(Compilation.PROLOG_PARSED);
            tr.pendingForm = st;
            // FIXME - we want to call 'run' here anyway, rather than have
            // it be emitted at the end of the 'body'.
            return true;
        }
        Pair args = (Pair) st.getCdr();
        Object name = args.getCar();
        Type type = null;
        Pair p;
        if (name instanceof Pair
            && tr.matches((p = (Pair) name).getCar(), Scheme.quote_str)) {
            Object fname = p.getCdr();
            if (! (fname instanceof Pair)
                || (p = (Pair) fname).getCdr() != LList.Empty
                || ! (p.getCar() instanceof Symbol)) {
                tr.error('e', "invalid quoted symbol for 'require'");
                return false;
            }
            fname = mapFeature(p.getCar().toString());
            if (fname == null) {
                tr.error('e', "unknown feature name '"+p.getCar()+"' for 'require'");
                return false;
            }
            type = ClassType.make((String) fname);
        }
        else if (name instanceof CharSequence) {
            String sourceName = name.toString();
            ModuleInfo info = lookupModuleFromSourcePath(sourceName, defs);
            if (info == null) {
                tr.error('e', "malformed URL: "+sourceName);
                return false;
            }
            return importDefinitions(null, info, null, tr.formStack, defs, tr);
        } else if (name instanceof Symbol && ! tr.selfEvaluatingSymbol(name)) {
            String requestedClass = name.toString();
            int nlen = requestedClass.length();
            if (nlen > 2 && requestedClass.charAt(0) == '<'
                && requestedClass.charAt(nlen-1) == '>')
                requestedClass = requestedClass.substring(1, nlen-1);
            String implicitSource = requestedClass.replace('.', '/');
            String explicitSource = null;
            if (args.getCdr() instanceof Pair) {
                Object sname = ((Pair) args.getCdr()).getCar();
                if (sname instanceof CharSequence) {
                    explicitSource = sname.toString();
                } // else ERROR
            }
            ImportFromLibrary.handleImport(implicitSource, explicitSource, requestedClass, defs, tr, null);
            return true;

        }
        if (! (type instanceof ClassType)) {
            if (type != null)
                tr.error('e', "specifier for 'require' is not a classname");
            else if (name instanceof SimpleSymbol)
                tr.error('e', "class '"+name+"' for 'require' not found");
            else
                tr.error('e', "invalid specifier for 'require'");
            return false;
        }
        ModuleInfo minfo;
        try {
            minfo = ModuleInfo.find((ClassType) type);
        } catch (Exception ex) {
            tr.error('e', "unknown class "+type.getName());
            return false;
        }
        importDefinitions(null, minfo, null,
                          tr.formStack, defs, tr);
        return true;
    }

    public static ModuleInfo lookupModuleFromSourcePath (String sourceName, ScopeExp defs) {
        ModuleManager manager = ModuleManager.getInstance();
        String baseName = defs.getFileName();
        if (baseName != null && baseName != InPort.systemInFilename)
            sourceName = Path.valueOf(baseName).resolve(sourceName).toString();
        return manager.findWithSourcePath(sourceName);
    }

    /** Import a module with a known source path.
     * @param className Optional fully-qualified name of module's class,
     *   or null if unknown.
     */
    public static boolean
    importDefinitions(String className, ModuleInfo info,
                      DeclSetMapper mapper, FormStack forms, 
                      ScopeExp defs, Compilation tr) {
        ModuleManager manager = ModuleManager.getInstance();
        long now;
        if ((info.getState() & 1) == 0
            && info.getCompilation() == null
            && ! info.checkCurrent(manager, (now = System.currentTimeMillis()))) {
            SourceMessages messages = tr.getMessages();
            Language language = Language.getDefaultLanguage();
            Compilation comp;
            try {
                InPort fstream = InPort.openFile(info.getSourceAbsPath());
                info.clearClass();
                int options = Language.PARSE_PROLOG;
                if (tr.immediate)
                    options |= Language.PARSE_IMMEDIATE;
                comp = language.parse(fstream, messages, options, info);
            } catch (java.io.FileNotFoundException ex) {
                tr.error('e', "not found: "+ex.getMessage());
                return false;
            } catch (java.io.IOException ex) {
                tr.error('e', "caught "+ex);
                return false;
            } catch (SyntaxException ex) {
                if (ex.getMessages() != messages)
                    throw new RuntimeException ("confussing syntax error: "+ex);
                // otherwise ignore it - it's already been recorded in messages.
                return false;
            }
            String compiledClassName = comp.getModule().classFor(comp).getName();
 
            if (className != null) {
                Map<String,ModuleInfo> subModuleMap = comp.subModuleMap;
                ModuleInfo modinfo;
                if (subModuleMap != null) {
                    modinfo = subModuleMap.get(className);
                } else
                    modinfo = null;
                if (modinfo == null) {
                    String[] classPrefixPath
                        = ImportFromLibrary.classPrefixPath;
                    int classPrefixPathLength = classPrefixPath.length;
                    for (int i = 0;  i < classPrefixPathLength;  i++) {
                        String tname = classPrefixPath[i] + className;
                        if (tname.equals(compiledClassName)) {
                            modinfo = info;
                            break;
                        }
                    }
                }
                if (modinfo == null)
                    tr.error('e', ("file '"+info.getSourceAbsPath()
                                   +"' does not declare library '"
                                   +className+"'"));
                else
                    info = modinfo;
            }
        }

        ModuleInfo curinfo = tr.getMinfo();
        if (curinfo != null && tr.getState() < Compilation.BODY_PARSED) {
            curinfo.addDependency(info);

            if (! info.loadEager(Compilation.COMPILED)
                && info.getState() < Compilation.RESOLVED) {
                // Oops.  We found a cycle.
                tr.pushPendingImport(info, defs, forms);
                return true;
            }
        }

        ClassType type = info.getClassType();
        String tname = type.getName();
        boolean sharedModule = tr.sharedModuleDefs();
        boolean isRunnable = (info.getState() < Compilation.RESOLVED
                              ? info.getCompilation().makeRunnable()
                              : type.isSubtype(Compilation.typeRunnable));
        Declaration decl = null;
        ClassType thisType = ClassType.make("kawa.standard.require");
        Expression[] args = { new QuoteExp(tname) };
        Expression dofind = Invoke.makeInvokeStatic(thisType, "find", args);
        Field instanceField = null;
        Language language = tr.getLanguage();
        dofind.setLine(tr);

        ModuleExp mod = info.setupModuleExp();

        Map<Symbol,Declaration> dmap
            = new LinkedHashMap<Symbol,Declaration>();
        Map<String,Declaration> moduleReferences = null;
       
        for (Declaration fdecl = mod.firstDecl();
             fdecl != null;  fdecl = fdecl.nextDecl()) {
            if (fdecl.isPrivate())
                continue;

            if (fdecl.field != null) {
                String fname = fdecl.field.getName();
                if (fname.equals("$instance"))
                {
                    instanceField = fdecl.field;
                    continue;
                }
            }

            if (fdecl.field != null
                && fdecl.field.getName().endsWith("$instance")) {
                if (moduleReferences == null)
                    moduleReferences = new HashMap<String,Declaration>();
                moduleReferences.put(fdecl.field.getName(), fdecl);
            } else
                dmap.put((Symbol) fdecl.getSymbol(), fdecl);
        }

        if (mapper != null)
            dmap = mapper.map(dmap, tr);

        for (Map.Entry<Symbol,Declaration> entry : dmap.entrySet()) {
            Symbol aname = entry.getKey();
            Declaration fdecl = entry.getValue();

            // We create an alias in the current context that points
            // a dummy declaration in the exported module.  Normally,
            // followAliases will skip the alias, so we use the latter.
            // But if the binding is re-exported (or EXTERNAL_ACCESS
            // gets set), then we need a separate declaration.
            // (If EXTERNAL_ACCESS, the field gets PRIVATE_PREFIX.)

            Declaration adecl;
            Declaration old = defs.lookup(aname, language, language.getNamespaceOf(fdecl));
            if (old != null
                     && ! old.getFlag(Declaration.NOT_DEFINING)
                     && (Declaration.followAliases(old)
                         == Declaration.followAliases(fdecl)))
                continue;

            if (decl == null && ! fdecl.getFlag(Declaration.STATIC_SPECIFIED)) {
                String iname = tname.replace('.', '$') + "$instance";
                decl = new Declaration(SimpleSymbol.valueOf(iname), type);
                decl.setPrivate(true);
                decl.setFlag(Declaration.IS_CONSTANT
                             |Declaration.MODULE_REFERENCE);
                defs.addDeclaration(decl);

                decl.noteValue(dofind);
                SetExp sexp = new SetExp(decl, dofind);
                sexp.setLine(tr);
                sexp.setDefining(true);
                forms.push(sexp);
                decl.setFlag(Declaration.EARLY_INIT);
                // If Runnable, we need to set decl value in initializer,
                // and later 'run' it, so it needs to be stored in a field.
                if (isRunnable)
                    decl.setSimple(false);

                decl.setFlag(Declaration.TYPE_SPECIFIED);
            }

            if (old != null
                && (old.getFlag(Declaration.NOT_DEFINING | Declaration.IS_UNKNOWN))) {
                old.setFlag(false, Declaration.NOT_DEFINING|Declaration.IS_UNKNOWN);
                adecl = old;
            } else {
                adecl = defs.addDeclaration(aname);
                if (old != null)
                    ScopeExp.duplicateDeclarationError(old, adecl, tr);
            }

            adecl.setAlias(true);
            adecl.setIndirectBinding(true);

            ReferenceExp fref = new ReferenceExp(fdecl);
            fref.setContextDecl(decl);
            fref.setDontDereference(true);
            if (! sharedModule)
                adecl.setPrivate(true);
            linkDecls(adecl, fdecl, fref, forms, tr);

            Expression fval = fdecl.getValue();
            if (fdecl.isIndirectBinding() && fval instanceof ReferenceExp) {
                ReferenceExp aref = (ReferenceExp) adecl.getValue();
                Declaration xdecl = ((ReferenceExp) fval).getBinding();
                aref.setBinding(xdecl);
                // xdecl can be null on an error.
                if (xdecl != null && xdecl.needsContext()) {
                    String iname
                        = (xdecl.field.getDeclaringClass().getName().replace('.', '$')
                           + "$instance");
                    Declaration cdecl = moduleReferences == null ? null
                        : moduleReferences.get(iname);
                    if (cdecl != null) {
                        if (cdecl.context != defs) {
                        Declaration acdecl = defs.addDeclaration(SimpleSymbol.valueOf(iname));
                        moduleReferences.put(iname, acdecl);
                        acdecl.setFlag(Declaration.IS_CONSTANT
                                      |Declaration.TYPE_SPECIFIED
                                      |Declaration.MODULE_REFERENCE);
                        acdecl.setType(cdecl.getType());
                        ReferenceExp cref = new ReferenceExp(cdecl);
                        cref.setContextDecl(decl);
                        linkDecls(acdecl, cdecl, cref, forms, tr);
                        cdecl = acdecl;
                    }
                    cdecl.setFlag(Declaration.EXPORT_SPECIFIED);
                    aref.setContextDecl(cdecl);
                    }
                }
            }
        }

        if (isRunnable) {
            Method run = Compilation.typeRunnable.getDeclaredMethod("run", 0);
            if (decl != null) // Need to make sure 'run' is invoked.
                dofind = new ReferenceExp(decl);
            else {
                if (instanceField != null)
                { //Optimization
                    args = new Expression[]
                        { new QuoteExp(type), new QuoteExp("$instance") };
                    dofind = new ApplyExp(SlotGet.staticField, args);
                }
            }
            dofind = new ApplyExp(run, new Expression[] { dofind });
            dofind.setLine(tr);
            forms.push(dofind);
        }
        return true;
    }

    static void linkDecls(Declaration adecl, Declaration fdecl,
                          ReferenceExp fref, FormStack forms, Compilation tr) {
        adecl.setLocation(tr);
        // Imported variables should be read-only.
        adecl.setFlag(Declaration.IS_CONSTANT);
        if (fdecl.getFlag(Declaration.IS_SYNTAX))
            adecl.setFlag(Declaration.IS_SYNTAX);
        if (fdecl.isProcedureDecl())
            adecl.setProcedureDecl(true);
        if (fdecl.getFlag(Declaration.STATIC_SPECIFIED))
            adecl.setFlag(Declaration.STATIC_SPECIFIED);

        SetExp sexp = new SetExp(adecl, fref);
        adecl.setFlag(Declaration.EARLY_INIT);
        sexp.setDefining(true);
        forms.push(sexp);
        adecl.noteValue(fref);
        adecl.setFlag(Declaration.IS_IMPORTED);
        tr.push(adecl);  // Add to translation env.
    }

    public Expression rewriteForm(Pair form, Translator tr) {
        return null;
    }

    public static interface DeclSetMapper {
        public Map<Symbol, Declaration> map(Map<Symbol, Declaration> decls, Compilation comp);
    }
}
