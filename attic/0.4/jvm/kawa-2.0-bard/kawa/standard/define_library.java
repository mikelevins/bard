package kawa.standard;

import gnu.bytecode.ClassType;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.SimpleSymbol;
import gnu.mapping.Symbol;
import gnu.text.SourceMessages;
import kawa.lang.*;
import java.util.LinkedHashMap;
import java.util.Map;

/** Implements the R7RS 'define-library' form. */

public class define_library extends Syntax {

    public static final define_library define_library = new define_library();
    public static final define_library define_library_scan = new define_library();
    static { define_library.setName("define-library"); }

    @Override
    public void scanForm(Pair st, ScopeExp defs, Translator tr) {
        if (this != define_library_scan)
            createModulePass(st, defs, tr);
        else {
            tr.push(defs);
            scanModulePass(st.getCdr(), defs, tr);
        }
    }
    public void createModulePass(Pair st, ScopeExp defs, Translator tr) {
        Object cdr1 = st.getCdr();
        if (! (cdr1 instanceof Pair)) {
            tr.error('e', "missing library name");
            return;
        }
        if (! (defs instanceof ModuleExp)) {
            tr.error('e', "define-library must be a top level");
            return;
        }
        defs.setFlag(ModuleExp.HAS_SUB_MODULE);
        Pair pair2 = (Pair) cdr1;
        Object car2 = pair2.getCar();
        if (LList.listLength(car2, false) <= 0) {
            tr.error('e', "invalid list in library name");
            return;
        }
        Language language = tr.getLanguage();
        ModuleExp module = new ModuleExp();
        module.setFile(defs.getFileName());
        NameLookup lexical = new NameLookup(language);
        SourceMessages messages = tr.getMessages();
        // It might be cleaner to re-use the incoming Compilation.
        // However Compilation does not currently support multiple ModuleExps.
        SchemeCompilation mcomp = new SchemeCompilation(language, messages, lexical, gnu.mapping.Environment.make());
        mcomp.setPedantic(tr.isPedantic());
        mcomp.explicit = tr.explicit;
        mcomp.immediate = tr.immediate;

        String name = module_name.listToModuleName(car2, tr);
        String className = name;
        int index = name.lastIndexOf('.');
        if (index >= 0)
            mcomp.classPrefix = name.substring(0, index+1);
        else
            className = tr.classPrefix + Compilation.mangleName(name);
        ClassType moduleClass = new gnu.bytecode.ClassType(className);
        mcomp.mainLambda = module;
        module.setType(tr.mainClass);
        module.setName(name);
        ModuleInfo curinfo = tr.getMinfo();
        ModuleManager manager = ModuleManager.getInstance();
        ModuleInfo info = manager.createWithClassName(className);
        info.setCompilation(mcomp);
        curinfo.addDependency(info);

        // It might be desirable to process 'export' forms at this point,
        // though so far I don't have a test case where it matters (even if
        // a cycle is involved).  Note to process 'export' early, we first
        // have to expand 'include-library-declarations' and 'cond-expand'.

        mcomp.setState(Compilation.PROLOG_PARSED);
        mcomp.pendingForm = tr.makePair(pair2,
                                        define_library_scan, pair2.getCdr());
        SchemeCompilation curcomp = (SchemeCompilation) tr;
        Map<String,ModuleInfo> subModuleMap = curcomp.subModuleMap;
        if (subModuleMap == null) {
            subModuleMap = new LinkedHashMap<String,ModuleInfo>();
            curcomp.subModuleMap = subModuleMap;
        }
        ModuleInfo oldinfo = subModuleMap.get(name);
        if (oldinfo != null)
            tr.error('e', "duplicate library name "+name);
        subModuleMap.put(name, info);
    }

    void scanModulePass(Object form, ScopeExp defs, Translator tr) {
        while (form instanceof Pair) {
            Pair pform = (Pair) form;
            Object save1 = tr.pushPositionOf(form);
            Object clause = pform.getCar();
            if (clause instanceof Pair) {
                Pair pclause = (Pair) clause;
                Object clauseHead = pclause.getCar();
                Syntax syntax = null;
                if (clauseHead == beginSymbol)
                    syntax = begin.begin;
                else if (clauseHead == exportSymbol)
                    syntax = export.export;
                else if (clauseHead == includeSymbol)
                    syntax = Include.include;
                else if (clauseHead == includeCiSymbol)
                    syntax = Include.includeCi;
                else if (clauseHead == importSymbol)
                    syntax = ImportFromLibrary.instance;
                if (clauseHead == includeLibraryDeclarationsSymbol) {
                    Object forms = Include.includeRelative
                        .process(pclause.getCdr(), tr, null, false);
                    scanModulePass(forms, defs, tr);
                } else if (clauseHead == condExpandSymbol) {
                    Object forms = IfFeature.condExpand
                        .evaluate(pclause.getCdr(), tr);
                    scanModulePass(forms, defs, tr);
                } else if (syntax != null) {
                    syntax.scanForm(pclause, defs, tr);
                }
                else {
                    if (tr.isPedantic())
                        tr.error('e', ("unknown define-library keyword: "
                                       +clauseHead));
                    tr.scanForm(clause, defs);
                }
            } else {
                tr.error('e', "define-library clause is not a list");
            }
            form = pform.getCdr();
            tr.popPositionOf(save1);
        }
        tr.errorIfNonEmpty(form);
    }

    public static final SimpleSymbol beginSymbol = Symbol.valueOf("begin");
    public static final SimpleSymbol condExpandSymbol = Symbol.valueOf("cond-expand");
    public static final SimpleSymbol exportSymbol = Symbol.valueOf("export");
    public static final SimpleSymbol importSymbol = Symbol.valueOf("import");
    public static final SimpleSymbol includeSymbol = Symbol.valueOf("include");
    public static final SimpleSymbol includeCiSymbol = Symbol.valueOf("include-ci");
    public static final SimpleSymbol includeLibraryDeclarationsSymbol = Symbol.valueOf("include-library-declarations");
}
