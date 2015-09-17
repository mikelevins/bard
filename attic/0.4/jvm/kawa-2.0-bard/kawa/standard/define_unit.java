// Copyright (C) 2000, 2007 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../COPYING.

package kawa.standard;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.math.*;
import kawa.lang.*;

public class define_unit extends Syntax
{
  public static final define_unit define_unit = new define_unit(false);
  static { define_unit.setName("define-unit"); }
  public static final define_unit define_base_unit = new define_unit(true);
  static { define_base_unit.setName("define-base-unit"); }

  /** True if this is define-base-unit, false if define-unit. */
  boolean base;

  public define_unit (boolean base)
  {
    this.base = base;
  }

  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    if (st.getCdr() instanceof Pair)
      {
	Pair p = (Pair) st.getCdr();
	Object q = p.getCar();
	if (q instanceof SimpleSymbol)
	  {
	    String name = q.toString();
            Symbol sym = LispLanguage.unitNamespace.getSymbol(name);
	    Declaration decl = defs.getDefine(sym, 'w', tr);
	    tr.push(decl);
	    Translator.setLine(decl, p);
	    decl.setFlag(Declaration.IS_CONSTANT);
	    if (defs instanceof ModuleExp)
	      decl.setCanRead(true);
	    Unit unit = null;
	    if (base && p.getCdr() == LList.Empty)
	      unit = BaseUnit.make(name, (String) null);
	    else if (p.getCdr() instanceof Pair)
	      {
		Object v = ((Pair) p.getCdr()).getCar();
		if (base &&
                    /* #ifdef use:java.lang.CharSequence */
                    v instanceof CharSequence
                    /* #else */
                    // (v instanceof String || v instanceof CharSeq)
                    /* #endif */
                    )
		  unit = BaseUnit.make(name, v.toString());
		else if (! base && v instanceof Quantity)
		  unit = Unit.make(name, (Quantity) v);
	      }
	    if (unit != null)
	      decl.noteValue(new QuoteExp(unit));
	    p = Translator.makePair(p, decl, p.getCdr());
	    st = Translator.makePair(st, this, p);
	    tr.pushForm(st);
	    return true;
	  }
      }
    tr.error('e', "missing name in define-unit");
    return false;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    Expression value = null;
    Pair p1;

    if (! (obj instanceof Pair)
	|| ! ((p1 = (Pair) obj).getCar() instanceof Declaration))
      return tr.syntaxError ("invalid syntax for "+getName());
    Declaration decl = (Declaration) p1.getCar();
    Symbol symbol = (Symbol) decl.getSymbol();
    String unit = symbol.getLocalPart();
    ClassType unitType = ClassType.make("gnu.math.Unit");
    decl.setType(unitType);
    if ((value = decl.getValue()) instanceof QuoteExp
	&& ((QuoteExp) value).getValue() instanceof Unit)
      ;
    else if (base)
      {
	String dimension = null;
	if (p1.getCdr() != LList.Empty)
	  {
	    dimension = ((Pair) p1.getCdr()).getCar().toString();
	  }
	BaseUnit bunit = BaseUnit.make(unit, dimension);
	value = new QuoteExp(bunit);
      }
    else
      {
	if (! (p1.getCdr() instanceof Pair))
	  return tr.syntaxError("missing value for define-unit");
	Pair p2 = (Pair) p1.getCdr();
	value = tr.rewrite (p2.getCar());
	Object quantity;
	if (value instanceof QuoteExp
	    && (quantity = ((QuoteExp) value).getValue()) instanceof Quantity)
	  {
	    value = new QuoteExp(Unit.make(unit, (Quantity) quantity));
	  }
	else
	  {
	    Expression[] args = new Expression[2];
	    args[0] = new QuoteExp(unit);
	    args[1] = value;
	    value = gnu.kawa.reflect.Invoke.makeInvokeStatic(unitType, "make",
							     args);
	  }
      }
    SetExp sexp = new SetExp(decl, value);
    sexp.setDefining (true);
    decl.noteValue(value);
    return sexp;
  }
}
