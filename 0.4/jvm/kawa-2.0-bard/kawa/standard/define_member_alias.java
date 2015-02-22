package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.bytecode.*;

public class define_member_alias extends Syntax
{
  public static final define_member_alias define_member_alias
    = new define_member_alias();
  static { define_member_alias.setName("define-member-alias"); }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    Pair p;
    if (! (st.getCdr() instanceof Pair)
        || (tr.currentScope() instanceof ModuleExp)
        || ! ((p = (Pair) st.getCdr()).getCar() instanceof String))
      return super.scanForDefinitions(st, defs, tr);
    Object name = p.getCar();
    Declaration decl = defs.addDeclaration((String) name,
                                           Compilation.typeSymbol);
    decl.setIndirectBinding(true);
    st = Translator.makePair(st, this, Translator.makePair(p, decl, p.getCdr()));
    tr.pushForm(st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    Pair p1;
    if (! (obj instanceof Pair)
        || ! ((p1 = (Pair) obj).getCar() instanceof String
              || p1.getCar() instanceof Declaration))
      return tr.syntaxError("missing name in " + getName());
    if (p1.getCdr() instanceof Pair)
      {
        String name;
        Declaration decl;
        Object p1_car = p1.getCar();
        if (p1_car instanceof Declaration)
          {
            decl = (Declaration) p1_car;
            name = decl.getName();
          }
        else
          {
            name = (String) p1_car;
            decl = null;
          }
        Pair p2 = (Pair) p1.getCdr();
        Expression fname = null;
        Expression arg = tr.rewrite(p2.getCar());
        Object p2_cdr = p2.getCdr();
        if (p2_cdr == LList.Empty)
          fname = new QuoteExp(gnu.expr.Compilation.mangleName(name));
        else if (p2_cdr instanceof Pair)
          {
            Pair p3 = (Pair) p2_cdr;
            if (p3.getCdr() == LList.Empty)
              fname = tr.rewrite(p3.getCar());
          }
        if (fname != null)
          {
            ClassType t
              = ClassType.make("gnu.kawa.reflect.ClassMemberConstraint");
            Expression[] args = new Expression[3]; 
            args[0] = new QuoteExp(name);
            args[1] = arg;
            args[2] = fname;
            return Invoke.makeInvokeStatic(t, "define", args);
          }
      }
    return tr.syntaxError("invalid syntax for " + getName());
  }
}
