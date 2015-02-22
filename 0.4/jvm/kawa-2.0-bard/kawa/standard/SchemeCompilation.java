package kawa.standard;
import gnu.expr.*;
import gnu.kawa.functions.Convert;
import gnu.kawa.functions.Curry1;
import gnu.kawa.functions.CurryExp;
import gnu.mapping.*;
import gnu.text.*;
import kawa.lang.*;

public class SchemeCompilation extends Translator
{
    public SchemeCompilation(Language language, SourceMessages messages,
                             NameLookup lexical, Environment env) {
        super(language, messages, lexical, env);
    }

    public SchemeCompilation(Language language, SourceMessages messages,
                             NameLookup lexical) {
        super(language, messages, lexical);
    }

  public static final Declaration applyFieldDecl =
    Declaration.getDeclarationFromStatic("kawa.standard.Scheme", "applyToArgs");

    @Override
    public Expression applyFunction(Expression func) {
        return new ReferenceExp(applyFieldDecl);
    }

  @Override
  public boolean isApplyFunction (Expression exp)
  {
    return isSimpleApplyFunction(exp);
  }

  @Override
  public boolean isSimpleApplyFunction (Expression exp)
  {
    return exp instanceof ReferenceExp
      && ((ReferenceExp) exp).getBinding() == applyFieldDecl;
  }

  /** Should the values of body/block be appended as multiple values?
   * Otherwise, just return the result of the final expression.
   */
  @Override
  public boolean appendBodyValues ()
  {
    return ((Scheme) getLanguage()).appendBodyValues();
  }

  public static final kawa.repl repl;

  public static final Lambda lambda = new kawa.lang.Lambda();

  static
  {
    repl = new kawa.repl(Scheme.instance);
    lambda.setKeywords(Special.optional, Special.rest, Special.key); 
  }

    /** If a symbol is lexically unbound, look for a default binding.
     * Recognizes {@code typename?} as a type predicate,
     * {@code ->typename} as a conversion function,
     * plus whatever the overridden method handles.
     * @return null if no binding, otherwise an Expression.
     */
    @Override
    public Expression checkDefaultBinding(Symbol symbol, Translator tr) {
        Namespace namespace = symbol.getNamespace();
        String local = symbol.getLocalPart();

        String name = symbol.toString();
        int len = name.length();
        if (len == 0)
            return null;
        int llen = local.length();
        // Map 'TYPE?' to '(lambda (obj) (instance? obj TYPE))'.
        if (len > 1 && llen > 1 && name.charAt(len-1) == '?') {
            String tlocal = local.substring(0, llen-1).intern();
            Symbol tsymbol = namespace.getSymbol(tlocal);
            Expression texp = tr.rewrite(tsymbol, false);
            if (texp instanceof ReferenceExp) {
                Declaration decl = ((ReferenceExp) texp).getBinding();
                if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
                  texp = null;
            }
            else if (! (texp instanceof QuoteExp))
                texp = null;
            if (texp != null) {
                LambdaExp lexp = new LambdaExp(1);
                lexp.setSymbol(symbol);
                Declaration param = lexp.addDeclaration((Object) null);
                param.noteValueUnknown();
                lexp.body = new ApplyExp(Scheme.instanceOf,
                                         new ReferenceExp(param), texp);
                return lexp;
            }
        }
        // Map '->TYPE' to '(lambda (obj) (as TYPE obj))'.
        if (len > 2 && llen > 2
            && name.charAt(0) == '-' && name.charAt(1) == '>') {
            String tlocal = local.substring(2).intern();
            Symbol tsymbol = namespace.getSymbol(tlocal);

            Expression texp = tr.rewrite(tsymbol, false);
            if (texp instanceof ReferenceExp) {
                Declaration decl = ((ReferenceExp) texp).getBinding();
                if (decl == null || decl.getFlag(Declaration.IS_UNKNOWN))
                  texp = null;
            }
            else if (! (texp instanceof QuoteExp))
                texp = null;
            if (texp != null)
                return new CurryExp(Curry1.makeConverter, Convert.cast, texp);
        }
        return super.checkDefaultBinding(symbol, tr);
    }
}
