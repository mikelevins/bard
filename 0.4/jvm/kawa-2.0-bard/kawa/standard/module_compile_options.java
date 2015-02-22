package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;

public class module_compile_options extends Syntax
{
  public static final module_compile_options module_compile_options
    = new module_compile_options();
  static { module_compile_options.setName("module-compile-options"); }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    Object rest = with_compile_options.getOptions(st.getCdr(), null, this, tr);
    if (rest != LList.Empty)
      tr.error('e', getName() + " key must be a keyword");
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
