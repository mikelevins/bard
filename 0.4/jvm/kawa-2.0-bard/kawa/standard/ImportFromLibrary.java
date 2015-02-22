// Copyright (C) 2009 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.Path;
import gnu.mapping.*;
import java.io.File;
import gnu.bytecode.ObjectType;
import java.util.*;
import kawa.lang.Translator.FormStack;

/** Implement R6RS import form.
 * This actually only implements simplified import;
 * we assumes it has been simplified by import macro defined in syntax.scm.
 */

public class ImportFromLibrary extends Syntax
{
    public static final ImportFromLibrary instance = new ImportFromLibrary();

    public static String[] classPrefixPath = { "", "kawa.lib." };

    private static final String BUILTIN = "<builtin>";
    private static final String MISSING = null;

    static final String[][] SRFI97Map = {
        { "1", "lists", "gnu.kawa.slib.srfi1" },
        { "2", "and-let*", "gnu.kawa.slib.srfi2" },
        { "5", "let", MISSING },
        { "6", "basic-string-ports", BUILTIN },
        { "8", "receive", BUILTIN },
        { "9", "records", BUILTIN },
        { "11", "let-values", BUILTIN },
        { "13", "strings", "gnu.kawa.slib.srfi13" },
        { "14", "char-sets", "gnu.kawa.slib.srfi14" },
        { "16", "case-lambda", BUILTIN },
        { "17", "generalized-set!", BUILTIN },
        { "18", "multithreading", MISSING },
        { "19", "time", MISSING },
        { "21", "real-time-multithreading", MISSING },
        { "23", "error", BUILTIN },
        { "25", "multi-dimensional-arrays", BUILTIN },
        { "26", "cut", "gnu.kawa.slib.cut" },
        { "27", "random-bits", MISSING },
        { "28", "basic-format-strings", BUILTIN },
        { "29", "localization", MISSING },
        { "31", "rec", MISSING },
        { "38", "with-shared-structure", MISSING },
        { "39", "parameters", BUILTIN },
        // Note the default for (srfi :41) should be "streams".  We put that last,
        // since the lable is searched from high index to low index.
        { "41", "streams.primitive", "gnu.kawa.slib.StreamsPrimitive" },
        { "41", "streams.derived", "gnu.kawa.slib.StreamsDerived" },
        { "41", "streams", "gnu.kawa.slib.Streams" },
        { "42", "eager-comprehensions", MISSING },
        { "43", "vectors", MISSING },
        { "44", "collections", MISSING },
        { "45", "lazy", MISSING },
        { "46", "syntax-rules", MISSING },
        { "47", "arrays", MISSING },
        { "48", "intermediate-format-strings", MISSING },
        { "51", "rest-values", MISSING },
        { "54", "cat", MISSING },
        { "57", "records", MISSING },
        { "59", "vicinities", MISSING },
        { "60", "integer-bits", "gnu.kawa.slib.srfi60" },
        { "61", "cond", MISSING },
        { "63", "arrays", MISSING },
        { "64", "testing", "gnu.kawa.slib.testing" },
        { "66", "octet-vectors", MISSING },
        { "67", "compare-procedures", MISSING },
        { "69", "basic-hash-tables", "gnu.kawa.slib.srfi69" },
        { "71", "let", MISSING },
        { "74", "blobs", MISSING },
        { "78", "lightweight-testing", MISSING },
        { "86", "mu-and-nu", MISSING },
        { "87", "case", BUILTIN },
        { "95", "sorting-and-merging", "kawa.lib.srfi95" },
        { "98", "os-environment-variables", BUILTIN },
        { "101", "random-access-lists", "gnu.kawa.slib.ralists" }
    };

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        Object obj = st.getCdr();
        while (obj instanceof Pair) {
            Pair pair = (Pair) obj;
            Object save1 = tr.pushPositionOf(pair);
            scanImportSet(pair.getCar(), defs, tr, null);
            tr.popPositionOf(save1);
            obj = pair.getCdr();
        }
        if (obj != LList.Empty) tr.error('e', "improper list");
    }

    public static String checkSrfi(String lname, Translator tr) {
        if (lname.startsWith("srfi.")) {
            String demangled = Compilation.demangleName(lname.substring(5));
            int dot = demangled.indexOf('.');
            String srfiName;
            StringBuilder badNameBuffer = null;

            if (dot < 0) {
                srfiName = null;
                dot = demangled.length();
            } else
                srfiName = demangled.substring(dot+1);
            String srfiNumber = null;
            if (dot > 0) {
                int numStart = demangled.charAt(0) == ':' ? 1 : 0;
                for (int i = numStart;  ;  i++) {
                    if (i == dot) {
                        srfiNumber = demangled.substring(numStart, dot);
                        break;
                    }
                    if (Character.digit(demangled.charAt(i), 10) < 0)
                        break;
                }
            }
            if (srfiNumber == null) {
                tr.error('e', "SRFI library reference must have the form: (srfi NNN [name]) or (srfi :NNN [name])");
                return lname;
            }
            int srfiIndex = SRFI97Map.length;
            for (;;) {
                if (--srfiIndex < 0) {
                    break;
                }
                if (!SRFI97Map[srfiIndex][0].equals(srfiNumber))
                    continue;
                String srfiNameExpected = SRFI97Map[srfiIndex][1];
                String srfiClass = SRFI97Map[srfiIndex][2];

                if (srfiName == null || srfiName.equals(srfiNameExpected))
                    return srfiClass != MISSING ? srfiClass : lname;

                if (badNameBuffer == null) {
                    badNameBuffer = new StringBuilder("the name of SRFI ");
                    badNameBuffer.append(srfiNumber);
                    badNameBuffer.append(" should be '");
                }
                else
                    badNameBuffer.append(" or '");
                badNameBuffer.append(srfiNameExpected);
                badNameBuffer.append('\'');
            }
            if (badNameBuffer != null) {
                tr.error('e', badNameBuffer.toString());
                return BUILTIN;
            }
        }
        return lname;
    }

    void scanImportSet(Object imports, ScopeExp defs, Translator tr, require.DeclSetMapper mapper) {
        int specLength = Translator.listLength(imports);
        if (specLength <= 0) {
            Object save1 = tr.pushPositionOf(imports);
            tr.error('e', "import specifier is not a proper list");
            tr.popPositionOf(save1);
            return;
        }
        Pair pimport = (Pair) imports;
        Object first = pimport.getCar();
        Object rest = pimport.getCdr();
        Pair cdrPair = specLength >= 2 ? (Pair) rest : null;
        char kind = '\0';
        if (first == onlySymbol)
            kind = 'O';
        else if (first == exceptSymbol)
            kind = 'E';
        else if (first == renameSymbol)
            kind = 'R';
        else if (first == prefixSymbol)
            kind = 'P';
        else if (first == librarySymbol && specLength == 2
                 && cdrPair.getCar() instanceof Pair)
            pimport = (Pair) cdrPair.getCar();
        if (specLength >= 2 && kind != '\0'
            // A keyword such as 'only must be followed by an <import set>.
            && cdrPair.getCar() instanceof LList) {
            ImportSetMapper nmapper
                = new ImportSetMapper(kind, cdrPair.getCdr(), specLength-2);
            nmapper.chain = mapper;
            scanImportSet(cdrPair.getCar(), defs, tr, nmapper);
            return;
        }

        String explicitSource = null;
        Object versionSpec = null;
        StringBuilder cbuf = new StringBuilder(); // for class name
        StringBuilder sbuf = new StringBuilder(); // for source file name
        Object libref = pimport;
        while (libref instanceof Pair) {
            Pair pair = (Pair) libref;
            Object car = pair.getCar();
            Object cdr = pair.getCdr();
            if (car instanceof Pair) {
                if (versionSpec != null) {
                    tr.error('e', "duplicate version reference - was "+versionSpec);
                }
                versionSpec = car;
            } else if (car instanceof String) {
                if (cdr instanceof Pair)
                    tr.error('e', "source specifier must be last element in library reference");
                explicitSource = (String) car;
            } else {
                if (cbuf.length() > 0)
                    cbuf.append('.');
                if (sbuf.length() > 0)
                    sbuf.append('/');
                String part = car.toString();
                cbuf.append(Compilation.mangleNameIfNeeded(part));
                sbuf.append(part);
            }
            libref = cdr;
        }
        handleImport(sbuf.toString(), explicitSource,
                     cbuf.toString(),
                     defs, tr, mapper);
    }

    /** Do the actual work of importing a module.
     * @param implicitSource Source name inferred from library name,
     *   with '/' as separator. Does not include a file extension.
     * @param explicitSource If non-null, an exlicitly specified
     *   source file name.
     */
    public static void handleImport(String implicitSource, String explicitSource, String requestedClass, ScopeExp defs, Translator tr, require.DeclSetMapper mapper) {

        ModuleManager mmanager = ModuleManager.getInstance();
        ModuleInfo minfo = null;
        String lname = checkSrfi(requestedClass, tr);
        if (lname == BUILTIN)
            return; // nothing to do
        boolean foundSrfi = lname != requestedClass;

        int classPrefixPathLength = classPrefixPath.length;
        Class existingClass = null;
        for (int i = 0;  i < classPrefixPathLength;  i++) {
            String tname = classPrefixPath[i] + lname;
            minfo = mmanager.searchWithClassName(tname);
            if (minfo != null)
                break;
            try {
                existingClass = ObjectType.getContextClass(tname);
                break;
            } catch (Exception ex) {
            }
        }

        String currentFileName = tr.getFileName();
        Path currentSource = currentFileName == null ? null
            : Path.valueOf(currentFileName).getAbsolute();
        String currentExtension = currentSource == null ? null
            : currentSource.getExtension();
        if (currentExtension == null) {
            List<String> langExtensions = tr.getLanguage().getExtensions();
            if (! langExtensions.isEmpty())
                currentExtension = langExtensions.get(0);
        }

        boolean hasDot;
        boolean isAbsolute;
        if (explicitSource != null) {
            hasDot = explicitSource.indexOf("./") >= 0;
            isAbsolute = Path.valueOf(explicitSource).isAbsolute();
        } else {
            hasDot = false;
            isAbsolute = false;
        }

        ModuleInfo curinfo = tr.getMinfo();
        String currentClassName = curinfo.getClassName();

        // Is the current module a file - as opposed to (say) a tty?
        boolean currentIsFile = currentSource instanceof FilePath
            && ((FilePath) currentSource).toFileRaw().isFile();
        Path currentRoot = currentIsFile ? currentSource.getDirectory()
            : Path.currentPath();
        if (currentIsFile
            && ! (explicitSource != null && (hasDot || isAbsolute))) {
            int currentDots = 0;
            String prefix = currentClassName != null ? currentClassName
                : tr.classPrefix != null ? tr.classPrefix : "";
            for (int i = prefix.length(); --i >= 0; )
                if (prefix.charAt(i) == '.')
                    currentDots++;
            if (currentDots > 0) {
                StringBuilder ups = new StringBuilder("..");
                for (int i = currentDots; -- i > 0; )
                    ups.append("/..");
                currentRoot = currentRoot.resolve(ups.toString());
            }
        }

        List<CharSequence> srcSearchPath;
        boolean skipSourceSearch = minfo != null && explicitSource == null;
        if (isAbsolute || hasDot || skipSourceSearch) {
            srcSearchPath = new ArrayList<CharSequence>();
            if (! skipSourceSearch)
                srcSearchPath.add(currentRoot.toString());
        }
        else
            srcSearchPath = getImportSearchPath();
        String pathStr = null;
        for (CharSequence searchElement : srcSearchPath) {
            if (isAbsolute)
                pathStr = explicitSource;
            else {
                String pathElement = searchElement.toString();
                int selectorEnd;
                int star;
                int prefixLength = 0;
                StringBuilder pbuf = new StringBuilder();
                if (pathElement.length() >= 3
                    && pathElement.charAt(0) == '<'
                    && (selectorEnd = pathElement.indexOf('>')+1) > 0) {
                    StringBuilder prefixBuf = new StringBuilder();
                    boolean slashNeeded = false;
                    for (int i = 1; i < selectorEnd-1; i++) {
                        char ch = pathElement.charAt(i);
                        if (ch == ' ') {
                            if (prefixBuf.length() > 0)
                                slashNeeded = true;
                        } else {
                            if (slashNeeded)
                                prefixBuf.append('/');
                            prefixBuf.append(ch);
                            prefixLength += slashNeeded ? 2 : 1;
                            slashNeeded = false;
                        }
                    }
                    if (! implicitSource.startsWith(prefixBuf.toString()))
                        continue;
                    if (implicitSource.length() != prefixLength) {
                        if (implicitSource.charAt(prefixLength) != '/')
                            continue;
                        prefixLength++;
                    }
                    star = pathElement.indexOf('*', selectorEnd);
                    if (star < 0) {
                         pathElement = pathElement.substring(selectorEnd);
                    }
                } else { // No "<...>..." selector
                    star = pathElement.indexOf('*');
                    selectorEnd = 0;
                    if (foundSrfi && explicitSource == null)
                        continue;
                }

                if (star >= 0) {
                    pbuf.append(pathElement.substring(selectorEnd, star));
                    pbuf.append(implicitSource.substring(prefixLength));
                    pbuf.append(pathElement.substring(star+1));
                } else {
                    pbuf.append(pathElement);
                    pbuf.append('/');
                    if (explicitSource != null)
                        pbuf.append(explicitSource);
                    else {
                        pbuf.append(implicitSource);
                        if (currentExtension != null) {
                            pbuf.append('.');
                            pbuf.append(currentExtension);
                        }
                    }
                }
                pathStr = pbuf.toString();
            }
            Path path = currentRoot.resolve(pathStr);
            // Might be more efficient to first check the ModuleManager,
            // before asking the file-system.  FIXME
            long lastModifiedTime = path.getLastModified();
            if (lastModifiedTime != 0) {
                if (minfo != null) {
                    String pstring = path.getCanonical().toString();
                    Path infoPath = minfo.getSourceAbsPath();
                    if (infoPath == null
                        || ! (pstring.equals(infoPath.toString()))) {
                        tr.error('w', "ignoring source file at "+pstring
                                 +" - instead using class "+minfo.getClassName()
                                 +(infoPath==null?""
                                   :(" from "+infoPath.toString())));
                    }
                } else
                    minfo = mmanager.findWithSourcePath(path, pathStr);
                // Should save lastModifiedTime in minfo FIXME
                if (foundSrfi)
                    lname = requestedClass;
                break;
            }
        }

        if (existingClass != null) {
            if (minfo == null)
                minfo = mmanager.findWithClass(existingClass);
            else
                minfo.setModuleClass(existingClass);
        }
        if (minfo == null)
            tr.error('e', "unknown library ("+implicitSource.replace('/', ' ')+")");
        else
            require.importDefinitions(lname, minfo, mapper,
                                      tr.formStack, defs, tr);
    }

    public Expression rewriteForm(Pair form, Translator tr) {
        return null;
    }

    static class ImportSetMapper implements require.DeclSetMapper {
        char kind;
        Object list;
        int listLength;
        require.DeclSetMapper chain;

        public ImportSetMapper(char kind, Object list, int listLength) {
            this.kind = kind;
            this.list = list;
            this.listLength = listLength;
        }

        public Map<Symbol, Declaration> map(Map<Symbol, Declaration> decls, Compilation comp) {
            Translator tr = (Translator) comp;
            Object lst = this.list;
            Map<Symbol,Declaration> nmap = decls;

            switch (kind) {
            case 'E': // 'except; list has the form (name ...)
            case 'O': // 'only; list has the form (name ...)
                if (kind == 'O')
                    nmap = new LinkedHashMap<Symbol,Declaration>();
                while (lst instanceof Pair) {
                    Pair pair = (Pair) lst;
                    Object save1 = tr.pushPositionOf(pair);
                    Object name = Translator.stripSyntax(pair.getCar());
                    if (name instanceof Symbol) {
                        Symbol sym = (Symbol) name;
                        Declaration old = decls.get(sym);
                        if (old == null)
                            tr.error('e', "unknown symbol in import set: "+sym);
                        else if (kind == 'E')
                            nmap.remove(sym);
                        else
                            nmap.put(sym, old);
                    }
                    else
                        tr.error('e', "non-symbol in name list");
                    tr.popPositionOf(save1);
                    lst = pair.getCdr();
                }
                break;

            case 'R': // 'rename; list has the form: ((oldname newname) ...)
                Symbol[] pendingSymbols = new Symbol[listLength];
                Declaration[] pendingDecls = new Declaration[listLength];
                int npending = 0;
                while (lst instanceof Pair) {
                    Pair pair = (Pair) lst;
                    Object save1 = tr.pushPositionOf(pair);
                    Object entry = pair.getCar();
                    int entryLen = Translator.listLength(entry);
                    if (entryLen == 2) {
                        Pair p1 = (Pair) entry;
                        Object oldname = p1.getCar();
                        Object newname = ((Pair) p1.getCdr()).getCar();
                        if (oldname instanceof Symbol
                            && newname instanceof Symbol) {
                            Symbol oldSymbol = (Symbol) oldname;
                            Symbol newSymbol = (Symbol) newname;
                            Declaration oldDecl = decls.remove(oldSymbol);
                            if (oldDecl == null)
                                tr.error('e', "missing binding "+oldSymbol);
                            else {
                                pendingSymbols[npending] = newSymbol;
                                pendingDecls[npending] = oldDecl;
                                npending++;
                            }
                        }
                        else
                            entryLen = -1;
                    }
                    if (entryLen != 2)
                        tr.error('e', "entry is not a pair of names");
                    tr.popPositionOf(save1);
                    lst = pair.getCdr();
                }
                for (int i = 0;  i < npending;  i++) {
                    Symbol newSymbol = pendingSymbols[i];
                    Declaration decl = pendingDecls[i];
                    if (decls.put(newSymbol, decl) != null)
                        tr.error('e', "duplicate binding for "+newSymbol);
                }
                break;

            case 'P':  // 'prefix; list has the form: (name-prefix)
                nmap = new LinkedHashMap<Symbol,Declaration>();
                if (listLength != 1
                    || ! (((Pair) list).getCar() instanceof SimpleSymbol))
                    tr.error('e', "bad syntax for prefix import specifier");
                else {
                    String prefix
                        = ((SimpleSymbol) ((Pair) list).getCar()).getName();
                    for (Map.Entry<Symbol,Declaration> entry : decls.entrySet()) {
                        Symbol aname = entry.getKey();
                        Declaration decl = entry.getValue();
                        Symbol nname = Symbol.valueOf(prefix+aname);
                        nmap.put(nname, decl);
                    }
                }
                break;
            }
            if (chain != null)
                nmap = chain.map(nmap, tr);
            return nmap;
        }
    }

    /** Check if library (in r7rs import syntax) exists.
     * @return if library exists: class name of (existing) library class,
     * or the special BUILTIN value; otherwise null.
     */
    public String libraryExists(Object list, Translator tr) {
        String lname = module_name.listToModuleName(list, tr);
        lname = checkSrfi(lname, tr);
        if (lname == BUILTIN)
            return lname;
        int classPrefixPathLength = classPrefixPath.length;
        for (int i = 0;  i < classPrefixPathLength;  i++) {
            String className = classPrefixPath[i] + lname;
            try {
                ObjectType.getContextClass(className);
                return className;
            } catch (Exception ex) {
                continue;
            }
        }
        return null;
    }

    public static final ThreadLocal<List<CharSequence>> searchPath
        = new InheritableThreadLocal<List<CharSequence>>();

    public static List<CharSequence> getImportSearchPath() {
        return Include.getSearchPath(searchPath, "kawa.import.path", ".");
    }

    public static final SimpleSymbol exceptSymbol = Symbol.valueOf("except");
    public static final SimpleSymbol librarySymbol = Symbol.valueOf("library");
    public static final SimpleSymbol onlySymbol = Symbol.valueOf("only");
    public static final SimpleSymbol prefixSymbol = Symbol.valueOf("prefix");
    public static final SimpleSymbol renameSymbol = Symbol.valueOf("rename");
}
