package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;

public class module_extends extends Syntax {
    public static final module_extends module_extends = new module_extends();
    static { module_extends.setName("module-extends"); }

    public void scanForm (Pair form, ScopeExp defs, Translator tr) {
        tr.getModule().setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
        super.scanForm(form, defs, tr);
    }

    public Expression rewriteForm (Pair form, Translator tr) {
        Type base = tr.exp2Type((Pair) form.getCdr());
        ModuleExp module = tr.getModule();
        module.setSuperType((ClassType) base);
        return QuoteExp.voidExp;
    }
}
