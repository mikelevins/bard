package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

public class function extends Syntax
{
  Syntax lambda;

  public function(Syntax lambda)
  {
    this.lambda = lambda;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    if (obj instanceof Pair)
      {
	Pair pair = (Pair) obj;
	if (pair.getCdr() != LList.Empty)
	  return tr.syntaxError("too many forms after 'function'");
	Object name = pair.getCar();
	if (name instanceof String || name instanceof Symbol)
	  {
            Declaration decl = tr.lookup(name, Language.FUNCTION_NAMESPACE);
	    ReferenceExp rexp = new ReferenceExp(name, decl);
	    rexp.setProcedureName(true);
	    rexp.setFlag(ReferenceExp.PREFER_BINDING2);
	    return rexp;
	  }
	if (name instanceof Pair)
	  {
	    pair = (Pair) name;
	    name = pair.getCar();
	    if (name instanceof String ? "lambda".equals(name)
		: (name instanceof Symbol
		   && "lambda".equals(((Symbol) name).getName())))
	      return lambda.rewriteForm(pair, tr);
	  }
      }
    return tr.syntaxError("function must be followed by name or lambda expression");
  }
}
