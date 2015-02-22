package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;
import java.util.Vector;

/**
 * The Syntax transformer that re-writes the `setq' builtin.
 * @author	Per Bothner
 */

public class setq extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    Vector results = null;
    while (obj != LList.Empty)
      {
	if (! (obj instanceof Pair))
	  return tr.syntaxError("invalid syntax for setq");
	Pair pair = (Pair) obj;
	Object sym = pair.getCar();
	Object name;
	if (sym instanceof Symbol || sym instanceof String)
	  name = sym;
	else if (sym == CommonLisp.FALSE)
	  name = "nil";
	else
	  return tr.syntaxError("invalid variable name in setq");
	obj = pair.getCdr();
	if (! (obj instanceof Pair))
	  return tr.syntaxError("wrong number of arguments for setq");
	pair = (Pair) obj;
	Expression value = tr.rewrite(pair.getCar());
	obj = pair.getCdr();
	SetExp sexp = new SetExp(name, value);
	if (obj == LList.Empty)
	  {
	    sexp.setHasValue(true);
	    if (results == null)
	      return sexp;
	  }
	if (results == null)
	  results = new Vector(10);
	results.addElement(sexp);
      }
    if (results == null)
      return CommonLisp.nilExpr;
    Expression[] stmts = new Expression[results.size()];
    results.copyInto(stmts);
    return new BeginExp(stmts);
  }
}
