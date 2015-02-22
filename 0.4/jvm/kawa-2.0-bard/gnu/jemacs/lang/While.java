package gnu.jemacs.lang;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;

/** Implement the ELisp `while' syntax form. */

public class While extends Syntax
{
  static kawa.standard.begin begin = new kawa.standard.begin();

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    if (! (obj instanceof Pair))
      return tr.syntaxError("missing arguments for while");
    tr.mustCompileHere();
    form = (Pair) obj;
    return Expression.makeWhile(form.getCar(), new Pair (begin, form.getCdr()), tr);
  }
}
