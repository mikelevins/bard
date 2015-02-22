package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.Symbol;

public class module_name extends Syntax {
    public static final module_name module_name = new module_name();
    static { module_name.setName("module-name"); }

    public void scanForm (Pair form, ScopeExp defs, Translator tr)  {
        Object form_cdr = form.getCdr();
        SyntaxForm nameSyntax = null;
        while (form_cdr instanceof SyntaxForm) {
            nameSyntax = (SyntaxForm) form_cdr;
            form_cdr = nameSyntax.getDatum();
        }
        Object arg = form_cdr instanceof Pair ? ((Pair) form_cdr).getCar() : null;
        while (arg instanceof SyntaxForm) {
            nameSyntax = (SyntaxForm) arg;
            arg = nameSyntax.getDatum();
        }
        String name = null;
        Pair p;
        String err = null;
        if (arg instanceof Pair && (p = (Pair) arg).getCar() == "quote") {
            arg = p.getCdr();
            if (! (arg instanceof Pair)
                || (p = (Pair) arg).getCdr() != LList.Empty
                || ! (p.getCar() instanceof String))
                err = "invalid quoted symbol for 'module-name'";
            else
                name = (String) p.getCar();
        } else if (arg instanceof Pair) {
            name = listToModuleName(arg, tr);
        } else if (arg instanceof FString || arg instanceof String)
            name = arg.toString();
        else if (arg instanceof Symbol) {
            name = arg.toString();
            int len = name.length();
            if (len > 2
                && name.charAt(0) == '<'
                && name.charAt(len-1) == '>') {
                name = name.substring(1, len-1);
            }
        } else
            err = "un-implemented expression in module-name";
        if (err != null)
            tr.pushForm(tr.syntaxError(err));
        else {
            int index = name.lastIndexOf('.');
            String className = name;
            if (index >= 0)
                tr.classPrefix = name.substring(0, index+1);
            else
                className = tr.classPrefix + Compilation.mangleName(name);
            ModuleExp module = tr.getModule();
            if (tr.mainClass == null)
                tr.mainClass = new gnu.bytecode.ClassType(className);
            else {
                String oldName = tr.mainClass.getName();
                if (oldName == null)
                    tr.mainClass.setName(className);
                else if (! oldName.equals(className))
                    tr.syntaxError("inconsistent module-name - old name: "+oldName);
            }
            module.setType(tr.mainClass);
            module.setName(name);

            tr.mustCompileHere();
        }
    }

    public static String listToModuleName(Object list, Translator tr) {
        StringBuilder sbuf = new StringBuilder(Compilation.classPrefixDefault);
        boolean first = true;
        for (;;) {
            Pair parg = (Pair) list;
            if (! first)
                sbuf.append('.');
            first = false;
            Object car = parg.getCar();
            if (car != null)
                sbuf.append(Compilation.mangleNameIfNeeded(car.toString()));
            list = parg.getCdr();
            if (list == LList.Empty)
                break;
            if (car == null || ! (list instanceof Pair)) {
                tr.error('e', "invalid list in module name");
                break;
            }
        }
        return sbuf.toString();
    }
}
