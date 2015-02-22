package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;

/** "define-variable" is like define, but ignored if variable already bound. */

public class define_variable extends Syntax
{
  public static final define_variable define_variable = new define_variable();
  static { define_variable.setName("define-variable"); }

  @Override
  public boolean scanForDefinitions (Pair st, ScopeExp defs, Translator tr)
  {
    if (! (st.getCdr() instanceof Pair))
      return super.scanForDefinitions(st, defs, tr);
    Pair p = (Pair) st.getCdr();
    Object sym = p.getCar();
    if (sym instanceof String || sym instanceof Symbol)
      {
	Declaration decl = defs.lookup(sym);
	if (decl != null)
	  tr.error('e', "duplicate declaration for '"+sym+"'");
	decl = defs.addDeclaration(sym);
	tr.push(decl);
	decl.setSimple(false);
	decl.setPrivate(true);
	decl.setFlag(Declaration.IS_DYNAMIC|Declaration.IS_SINGLE_VALUE);
	decl.setCanRead(true);
	decl.setCanWrite(true);
	decl.setIndirectBinding(true);
	p = Translator.makePair(p, decl, p.getCdr());
	st = Translator.makePair(st, this, p);
      }
    tr.pushForm(st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	obj = p1.getCar();
	if (obj instanceof String || obj instanceof Symbol)
	  return tr.syntaxError(getName() + " is only allowed in a <body>");
	if (obj instanceof Declaration)
	  {
	    decl = (Declaration) p1.getCar();
	    obj = p1.getCdr();
	    if (obj instanceof Pair
		&& (p1 = (Pair) obj).getCdr() == LList.Empty)
	      value = tr.rewrite (p1.getCar());
	    else if (obj != LList.Empty)
	      decl = null;
	  }
      }
    if (decl == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    if (value == null)
      return QuoteExp.voidExp;
    SetExp sexp = new SetExp (decl, value);
    sexp.setDefining (true);
    sexp.setSetIfUnbound(true);
    
    if (decl != null)
      {
	sexp.setBinding(decl);
	if (decl.context instanceof ModuleExp
	    && decl.getCanWrite())
	  value = null;
	decl.noteValue(value);
      }
    return sexp;
  }
}
