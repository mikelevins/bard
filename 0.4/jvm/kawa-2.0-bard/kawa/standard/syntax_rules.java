package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.Symbol;
import gnu.expr.*;

/** Implement the standard Scheme "syntax-rules" form. */

public class syntax_rules extends Syntax
{
  public static final syntax_rules syntax_rules = new syntax_rules();
  static { syntax_rules.setName("syntax-rules"); }

    public Expression rewriteForm(Pair form, Translator tr) {
        Pair pair1 = (Pair) form.getCdr();

        Object car1 = pair1.getCar();
        Object cdr1 = pair1.getCdr();
        Object ellipsis = SyntaxRule.dots3Symbol;
        if (Translator.stripSyntax(car1) instanceof Symbol
                && cdr1 instanceof Pair) {
            Pair pair2 = (Pair) cdr1;
            Object car2 = pair2.getCar();
            Object cdr2 = pair2.getCdr();
            if (car2 instanceof LList) {
                ellipsis = car1;
                car1 = car2;
                cdr1 = cdr2;
            }
            // otherwise error caught by getLiteralsList
        }
        
        Object[] literal_identifiers
            = SyntaxPattern.getLiteralsList(car1, null, tr);
        SyntaxRules rules = new SyntaxRules(ellipsis, literal_identifiers,
                                            cdr1, tr);
        return new QuoteExp(rules);
    }
}
