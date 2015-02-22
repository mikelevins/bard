package kawa.standard;
import kawa.lang.*;
import gnu.bytecode.*;
import gnu.lists.*;
import gnu.expr.*;

public class syntax extends kawa.lang.Quote
{
  public static final syntax syntax = new syntax("syntax", false);
  public static final syntax quasiSyntax = new syntax("quasisyntax", true);

  public syntax (String name, boolean isQuasi)
  {
    super(name, isQuasi);
  }

    protected boolean matchesUnquote(Pair pair, SyntaxForm syntax,
                                     Translator tr) {
        Object form = pair.getCar();
        if (tr.matches(form, syntax, "unsyntax"))
            return true;
        if (tr.matches(form, syntax, "unquote")) {
            tr.error('w',
                "unquote inside quasisyntax is deprecated - use unsyntax or #,",
                pair instanceof PairWithPosition ? (PairWithPosition)pair : tr);
            return true;
        }
        return false;
    }

    protected boolean matchesUnquoteSplicing(Pair pair, SyntaxForm syntax,
                                     Translator tr) {
        Object form = pair.getCar();
        if (tr.matches(form, syntax, "unsyntax-splicing"))
            return true;
        if (tr.matches(form, syntax, "unquote-splicing")) {
            tr.error('w',
                "unquote-splicing inside quasisyntax is deprecated - use unsyntax-splicing or #@,",
                pair instanceof PairWithPosition ? (PairWithPosition)pair : tr);
            return true;
        }
        return false;
    }

    protected boolean matchesQuasiQuote(Object form, SyntaxForm syntax,
                                     Translator tr) {
        return tr.matches(form, syntax, "quasisyntax");
    }

  protected boolean expandColonForms ()
  {
    return false;
  }

  static final ClassType typeTemplateScope =
    ClassType.make("kawa.lang.TemplateScope");
  static final Method makeTemplateScopeMethod =
    typeTemplateScope.getDeclaredMethod("make", 0);

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (! (form.getCdr() instanceof Pair)
	|| (form = (Pair) (form.getCdr())).getCdr() != LList.Empty)
      return tr.syntaxError("syntax forms requires a single form");
    Declaration saveTemplateScopeDecl = tr.templateScopeDecl;
    if (saveTemplateScopeDecl == null)
      {
        tr.letStart();
        Expression init = 
          new ApplyExp(makeTemplateScopeMethod,
                       Expression.noExpressions);
        Declaration templateScopeDecl = tr.letVariable(null, typeTemplateScope, init);
        templateScopeDecl.setCanRead();
        tr.templateScopeDecl = templateScopeDecl;
        tr.letEnter();
      }

    try
      {
        Expression body = coerceExpression(expand(form.getCar(),
                                                  isQuasi ? 1 : Quote.QUOTE_DEPTH, tr),
                                           tr);
        return saveTemplateScopeDecl == null ? tr.letDone(body) : body;
      }
    finally
      {
        tr.templateScopeDecl = saveTemplateScopeDecl;
      }
  }

  protected Expression leaf (Object val, Translator tr)
  {
    return makeSyntax(val, tr);
  }

  static Expression makeSyntax (Object form, Translator tr)
  {
    SyntaxTemplate template = new SyntaxTemplate(form, null,
                                                 SyntaxRule.dots3Symbol, tr);
    Expression matchArray = QuoteExp.nullExp;
    PatternScope patternScope = tr.patternScope;
    if (patternScope != null && patternScope.matchArray != null)
      matchArray = new ReferenceExp(patternScope.matchArray);
    Expression[] args = { new QuoteExp(template), matchArray, new ReferenceExp(tr.templateScopeDecl) };
    return new ApplyExp(ClassType.make("kawa.lang.SyntaxTemplate")
			.getDeclaredMethod("execute",
                                           new Type[]{null, typeTemplateScope}),
			args);
  }
}
