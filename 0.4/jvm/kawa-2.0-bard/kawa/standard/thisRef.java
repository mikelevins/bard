package kawa.standard;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;
import gnu.bytecode.Type;

public class thisRef extends Syntax
{
  public static final thisRef thisSyntax = new thisRef();
  static { thisSyntax.setName("this"); }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (form.getCdr() == LList.Empty)
      {
        LambdaExp method = tr.curMethodLambda;
        Declaration firstParam = method == null ? null : method.firstDecl();
        if (firstParam == null || ! firstParam.isThisParameter())
          {
            firstParam = null;
            if (method == null || method.nameDecl == null)
              tr.error('e', "use of 'this' not in a named method");
            else if (method.nameDecl.isStatic())
              tr.error('e', "use of 'this' in a static method");
            else
              {
                firstParam = new Declaration(ThisExp.THIS_NAME);
                method.add(null, firstParam);
                method.nameDecl.setFlag(Declaration.NONSTATIC_SPECIFIED);
              }
          }
        return new ThisExp(firstParam);
      }
    else
      return tr.syntaxError("this with parameter not implemented");
  }
}
