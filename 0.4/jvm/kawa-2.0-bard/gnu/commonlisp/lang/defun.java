package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

/**
 * The Syntax transformer that re-writes the `defun' ELisp builtin.
 * @author	Per Bothner
 */

public class defun extends Syntax
{
  kawa.lang.Lambda lambdaSyntax;

  public defun (kawa.lang.Lambda lambdaSyntax)
  {
    this.lambdaSyntax = lambdaSyntax;
  }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    Pair p;
    if (! (st.getCdr() instanceof Pair)
	|| ! (((p = (Pair) st.getCdr()).getCar() instanceof String)
	      || p.getCar() instanceof Symbol))
      return super.scanForDefinitions(st, defs, tr);
    Object sym = p.getCar();
    Declaration decl = defs.lookup(sym, tr.getLanguage(),
                                   Language.FUNCTION_NAMESPACE);
    if (decl == null)
      {
	decl = new Declaration(sym);
	decl.setProcedureDecl(true);
	defs.addDeclaration(decl);
        tr.push(decl);
      }
    else
      tr.error('w', "duplicate declaration for `"+sym+"'");

    if (defs instanceof ModuleExp)
      decl.setCanRead(true);
    st = Translator.makePair(st, this, Translator.makePair(p, decl, p.getCdr()));
    tr.pushForm(st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    Object name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
        Object p1_car = p1.getCar();

	if (p1_car instanceof Symbol || p1_car instanceof String)
	  {
	    name = p1_car.toString();
	  }
	else if (p1_car instanceof Declaration)
	  {
	    decl = (Declaration) p1_car;
	    name = decl.getSymbol();
	  }
	if (name != null && p1.getCdr() instanceof Pair)
	  {
	    Pair p2 = (Pair) p1.getCdr();
	    LambdaExp lexp = new LambdaExp();
	    lambdaSyntax.rewrite(lexp, p2.getCar(), p2.getCdr(), tr, null);
	    lexp.setSymbol(name);
	    if (p2 instanceof PairWithPosition)
              lexp.setLocation((PairWithPosition) p2);
	    value = lexp;
	    SetExp sexp = new SetExp (name, value);
	    sexp.setDefining(true);
	    sexp.setFuncDef(true);
	    if (decl != null)
	      {
		sexp.setBinding(decl);
		if (decl.context instanceof ModuleExp && decl.getCanWrite())
		  value = null;
		decl.noteValue(value);
	      }
	    return sexp;
	  }
      }
    return tr.syntaxError ("invalid syntax for "+getName());
  }
}
