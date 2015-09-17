package gnu.jemacs.lang;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.mapping.Symbol;

public class defcustom extends Syntax
{
  @Override
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    if (! (st.getCdr() instanceof Pair))
      return super.scanForDefinitions(st, defs, tr);
    Pair p = (Pair) st.getCdr();
    Object name = p.getCar();
    if (name instanceof String || name instanceof Symbol)
      {
	Declaration decl = defs.lookup(name);
	if (decl == null)
	  {
	    decl = new Declaration(name);
	    defs.addDeclaration(decl);
	  }
	else
	  tr.error('w', "duplicate declaration for `"+name+"'");
	p = Translator.makePair(p, decl, p.getCdr());
	st = Translator.makePair(st, this, p);
        if (defs instanceof ModuleExp)
          {
	    decl.setCanRead(true);
	    decl.setCanWrite(true);
          }
      }
    tr.pushForm(st);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object obj = form.getCdr();
    String name = null;
    Expression value = null;
    Declaration decl = null;

    if (obj instanceof Pair)
      {
	Pair p1 = (Pair) obj;
	if (p1.getCar() instanceof Declaration)
	  {
	    decl = (Declaration) p1.getCar();
	    name = decl.getName();
	    if (p1.getCdr() instanceof Pair)
	      {
		Pair p2 = (Pair) p1.getCdr();
		value = tr.rewrite (p2.getCar());
		if (p2.getCdr() != LList.Empty)
		  {
		    // Handle the defcustom options.  FIXME.
		  }
	      }
	    else if (p1.getCdr() != LList.Empty)
	      name = null;
	  }
      }
    if (name == null)
      return tr.syntaxError ("invalid syntax for "+getName());
    SetExp sexp = new SetExp (name, value);
    sexp.setDefining (true);
    if (decl != null)
      {
	sexp.setBinding(decl);
	if (decl.context instanceof ModuleExp
	    && decl.getCanWrite())
	  value = null;
	decl.noteValue(value);
      }
    return sexp;
  }

}
