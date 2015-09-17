package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.Symbol;

public class export extends Syntax {
    public static final export module_export = new export();
    static { module_export.setName("module-export"); }

    public static final export export = new export();
    static { export.setName("export"); }

    @Override
    public boolean scanForDefinitions (Pair st, ScopeExp defs, Translator tr) {
        Object list = st.getCdr();
        Object savePos = tr.pushPositionOf(st);
        try {
            if (! (defs instanceof ModuleExp)) {
                tr.error('e', "\'" + getName() + "\' not at module level");
                return false;
            }
            ModuleExp mexp = (ModuleExp) defs;
            if (mexp.getFlag(ModuleExp.HAS_SUB_MODULE)) {
                tr.error('e', "'export' used follow explicit modules");
                return false;
            }
            mexp.setFlag(ModuleExp.EXPORT_SPECIFIED);
            SyntaxForm restSyntax = null;
            while (list != LList.Empty) {
                tr.pushPositionOf(list);
                while (list instanceof SyntaxForm) {
                    restSyntax = (SyntaxForm) list;
                    list = restSyntax.getDatum();
                }
                SyntaxForm nameSyntax = restSyntax;
                if (list instanceof Pair) {
                    st = (Pair) list;
                    Object symbol = st.getCar();
                    while (symbol instanceof SyntaxForm) {
                        nameSyntax = (SyntaxForm) symbol;
                        symbol = nameSyntax.getDatum();
                    }
                    if (symbol instanceof String) {
                        String str = (String) symbol;
                        if (str.startsWith("namespace:")) {
                            tr.error('w', "'namespace:' prefix ignored");
                            symbol = str.substring(10).intern();
                        }
                    }
                    symbol = tr.namespaceResolve(symbol);
                    if (symbol instanceof Pair) {
                        // Match (rename name1 name2)
                        Pair psym = (Pair) symbol;
                        Object symcdr, symcddr; Pair psymcdr;
                        if (tr.matches(psym.getCar(), "rename")
                            && (symcdr = psym.getCdr()) instanceof Pair
                            && ((symcddr = (psymcdr = (Pair) symcdr)
                                 .getCdr()) instanceof Pair)) {
                            Pair psymcddr = (Pair) symcddr;
                            Object symcdddr = psymcddr.getCdr();
                            Object name1 = tr.namespaceResolve(psymcdr.getCar());
                            Object name2 = tr.namespaceResolve(psymcddr.getCar());
                            if (symcdddr == LList.Empty
                                && name1 instanceof Symbol
                                && name2 instanceof Symbol) {
                                Declaration decl1 = defs.getNoDefine(name1);
                                if (decl1.getFlag(Declaration.NOT_DEFINING))
                                    Translator.setLine(decl1, st);
                                decl1.setFlag(Declaration.EXTERNAL_ACCESS);
                                // Name of decl2 will be set to name2 later
                                // using Declaration#patchSymbolFromSet.
                                Declaration decl2 = tr.define(null, nameSyntax, defs);
                                decl2.setIndirectBinding(true);
                                decl2.setAlias(true);
                                decl2.setFlag(Declaration.EXPORT_SPECIFIED
                                              | Declaration.EARLY_INIT);
                                ReferenceExp ref1 = new ReferenceExp(decl1);
                                ref1.setDontDereference(true);
                                SetExp sexp = new SetExp(name2, ref1);
                                sexp.setBinding(decl2);
                                tr.setLineOf(sexp);
                                decl2.noteValueFromSet(sexp);
                                sexp.setDefining(true);
                                list = st.getCdr();
                                tr.pushForm(sexp);
                                continue;
                            }
                        }
                    }
                    if (symbol instanceof String
                        || symbol instanceof Symbol) {
                        if (nameSyntax != null) {
                            // Difficult to implement correctly.  And
                            // probably not much point in doing so.  FIXME.
                        }
                        Declaration decl = defs.getNoDefine(symbol);
                        if (decl.getFlag(Declaration.NOT_DEFINING))
                            Translator.setLine(decl, st);
                        decl.setFlag(Declaration.EXPORT_SPECIFIED);
                        list = st.getCdr();
                        continue;
                    }
                }
                tr.error('e', "invalid syntax in '" + getName() + '\'');
                return false;
            }
            return true;
        } finally {
            tr.popPositionOf(savePos);
        }
    }

    public Expression rewriteForm (Pair form, Translator tr) {
        return null;
    }
}
