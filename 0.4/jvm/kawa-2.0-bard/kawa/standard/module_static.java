package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.mapping.Symbol;
import gnu.mapping.SimpleSymbol;

public class module_static extends Syntax
{
  public static final module_static module_static = new module_static();
  static { module_static.setName("module-static"); }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    Object list = st.getCdr();
    if (! (defs instanceof ModuleExp))
      {
	tr.error('e', "\'" + getName() + "\' not at module level");
	return true;
      }
    ModuleExp mexp = (ModuleExp) defs;
    if (list instanceof Pair
	&& (st = (Pair) list).getCdr() == LList.Empty
	&& st.getCar() instanceof Boolean)
      {
	if (st.getCar() == Boolean.FALSE)
	  mexp.setFlag(ModuleExp.NONSTATIC_SPECIFIED);
	else
	  mexp.setFlag(ModuleExp.STATIC_SPECIFIED);
      }
    else if (list instanceof Pair
             && (st = (Pair) list).getCdr() == LList.Empty
             && st.getCar() instanceof Pair
             && tr.matches((st = (Pair) st.getCar()).getCar(), Scheme.quote_str))
      {
        Object cdr = st.getCdr();
        if (cdr != LList.Empty
            && (st = (Pair) cdr).getCar() instanceof SimpleSymbol
            && st.getCar().toString() == "init-run")
          {
            // (module-static 'init-run) implies (module-static #t)
            mexp.setFlag(ModuleExp.STATIC_SPECIFIED);
            mexp.setFlag(ModuleExp.STATIC_RUN_SPECIFIED);
          }
          else
          {
            tr.error('e', "invalid quoted symbol for '" + getName() + '\'');
            return false;
          }
      }
    else
      {
	mexp.setFlag(ModuleExp.NONSTATIC_SPECIFIED);


	while (list != LList.Empty)
	  {
	    if (! (list instanceof Pair)
		|| ! ((st = (Pair) list).getCar() instanceof Symbol))
	      {
		tr.error('e', "invalid syntax in '" + getName() + '\'');
		return false;
	      }
	    Symbol symbol = (Symbol) st.getCar();
	    Declaration decl = defs.getNoDefine(symbol);
	    if (decl.getFlag(Declaration.NOT_DEFINING))
	      Translator.setLine(decl, st);
	    decl.setFlag(Declaration.STATIC_SPECIFIED);
	    list = st.getCdr();
	  }
      }
    if (mexp.getFlag(ModuleExp.NONSTATIC_SPECIFIED)
        && mexp.getFlag(ModuleExp.STATIC_SPECIFIED))
      tr.error('e', "inconsistent module-static specifiers");
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
