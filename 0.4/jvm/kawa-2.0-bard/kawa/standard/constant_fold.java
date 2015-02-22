package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Implements the "constant-fold" transformer. */

public class constant_fold extends Syntax
{
  public static final constant_fold constant_fold = new constant_fold();
  static { constant_fold.setName("constant-fold"); }

  static Object checkConstant(Expression exp, Translator tr)
  {
    if (exp instanceof QuoteExp)
      return ((QuoteExp) exp).getValue();
    if (exp instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) exp;
        Declaration decl = rexp.getBinding();
	if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
	  return Environment.user().get(rexp.getName(), null);
        else
          return Declaration.followAliases(decl).getConstantValue();
      }
    return null;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    Expression exp = tr.rewrite(obj);
    if (! (exp instanceof ApplyExp))
      return exp;
    ApplyExp aexp = (ApplyExp) exp;
    Object func = checkConstant(aexp.getFunction(), tr);
    if (! (func instanceof Procedure))
      return exp;

    // Not quite the same - checkConstant also looks up name in Environment,
    // which seems a bit too dangerous for inlineIfConstant.  FIXME.
    // return aexp.inlineIfConstant((Procedure) func, tr.getMessages());

    Expression[] args = aexp.getArgs();
    int i = args.length;
    Object[] vals = new Object[i];
    while (--i >= 0)
      {
	Object val = checkConstant(args[i], tr);
	if (val == null)
	  return exp;
	vals[i] = val;
      }
    try
      {
	return new QuoteExp(((Procedure) func).applyN(vals));
      }
    catch (Error ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
	exp = tr.syntaxError("caught exception in constant-fold:");
	tr.syntaxError(ex.toString());
	return exp;
      }
  }
}
