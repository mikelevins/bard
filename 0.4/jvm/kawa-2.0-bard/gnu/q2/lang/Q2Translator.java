package gnu.q2.lang;
import kawa.standard.SchemeCompilation;
import gnu.expr.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.mapping.Symbol;
import kawa.lang.*;
import java.util.Stack;

public class Q2Translator extends SchemeCompilation
{
  public Q2Translator (Language language, SourceMessages messages, NameLookup lexical)
  {
    super(language, messages, lexical);
  }

  /** Split list according to operator-precedence priorities.
   */
  public static Object partition (Object p, Translator tr) 
  {
    // A stack of: Fence, (arg-list, Pair<Operator>, Operator)*
    // The "value" of each Pair<Operator> is the same as the following Operator.
    // The invariant is that for each i, where i is 0, 3, 7, ..., we have:
    // ((Operator)st.get(i)).rprio < ((Operator)st.get(i+3)).lprio
    Stack st = new Stack();
    st.add(Operator.FENCE);
    Object larg = p;
    Pair prev = null;

    for (;;)
      {
        if (p instanceof SyntaxForm)
          ; // FIXME
        Operator op = null;
        Pair pp;
        if (p == LList.Empty)
          {
            op = Operator.FENCE;
            pp = null;
          }
        else if (! (p instanceof Pair))
          {
            tr.error('e', "unexpected non-list");
            break;
          }
        else
          {
            pp = (Pair) p;
            Object obj = pp.getCar();
            if (obj instanceof Symbol && ! Q2.instance.selfEvaluatingSymbol(obj))
              {
                Expression func = tr.rewrite(obj, true);
                Declaration decl;
                Object value;
                if (func instanceof ReferenceExp
                    && (decl = ((ReferenceExp) func).getBinding()) != null
                    && (value = decl.getConstantValue()) instanceof Operator)
                  {
                    op = (Operator) value;
                  }
              }
          }
        if (op != null)
          {
            if (prev == null)
              larg = LList.Empty;
            else
              prev.setCdrBackdoor(LList.Empty);
            int stsz = st.size();
            Operator topop = (Operator) st.get(stsz-1);
            while (op.lprio <= topop.rprio)
              {
                larg = topop.combine((LList) st.get(stsz-3), larg,
                                     (PairWithPosition) st.get(stsz-2));
                stsz -= 3;
                st.setSize(stsz);
                topop = (Operator) st.get(stsz-1);
              }
            if (pp == null)
              break;
            st.add(larg);
            st.add(pp);
            st.add(op);
            larg = pp.getCdr();
            prev = null;
          }
        else
          prev = pp;
        p = pp.getCdr();
      }
    return larg;
  }

  public void scanForm (Object st, ScopeExp defs)
  {
    if (st instanceof LList)
      st = partition(st, this);
    if (st != LList.Empty)
      super.scanForm(st, defs);
  }

  public Expression rewrite (Object exp, boolean function)
  {
    if (exp == LList.Empty)
      return QuoteExp.voidExp;
    return super.rewrite(exp, function);
  }

  public Expression rewrite_pair (Pair p, boolean function)
  {
    Object partitioned = partition(p, this);
    if (partitioned instanceof Pair)
      return super.rewrite_pair((Pair) partitioned, function);
    else
      return rewrite(partitioned, function);
  }

    @Override
    public Expression applyFunction(Expression func) {
        return new QuoteExp(Q2Apply.q2Apply);
    }

  public boolean isApplyFunction (Expression exp)
  {
    return exp.valueIfConstant() == Q2Apply.q2Apply;
  }
}
