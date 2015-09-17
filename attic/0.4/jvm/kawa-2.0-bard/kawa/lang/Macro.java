package kawa.lang;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.lists.*;
import java.io.*;
import gnu.text.Printable;

public class Macro extends Syntax implements Printable, Externalizable
{
  public Object expander;

  Object instance;

  public static final int HYGIENIC = 1;
  /** If this flag is set, then don't expand during the scan-body phase. */
  public static final int SKIP_SCAN_FORM = 2;
  private int flags = HYGIENIC;

  public final void setFlags(int flags) { this.flags = flags; }
  public final boolean isHygienic() { return (flags & HYGIENIC) != 0; }
  public final void setHygienic (boolean hygienic)
  {
    if (hygienic)
      flags |= HYGIENIC;
    else
      flags &= ~HYGIENIC;
  }

  private ScopeExp capturedScope;

  public ScopeExp getCapturedScope ()
  {
    if (capturedScope == null)
      {
        if (instance instanceof ModuleExp) // possibly if immediate.
          capturedScope = (ModuleExp) instance;
        else if (instance != null)
          capturedScope = ModuleInfo.findFromInstance(instance).getModuleExp();
      }
    return capturedScope;
  }

  public void setCapturedScope (ScopeExp scope)
  {
    capturedScope = scope;
  }

  public static Macro make (Declaration decl)
  {
    Macro mac = new Macro(decl.getSymbol());
    decl.setSyntax();
    mac.capturedScope = decl.context;
    return mac;
  }

  public static Macro makeNonHygienic (Object name, Procedure expander)
  {
    Macro mac = new Macro(name, expander);
    mac.setHygienic(false);
    return mac;
  }

  public static Macro makeNonHygienic (Object name, Procedure expander,
				       Object instance)
  {
    Macro mac = new Macro(name, expander);
    mac.setHygienic(false);
    mac.instance = instance;
    return mac;
  }

  public static Macro makeSkipScanForm (Object name, Procedure expander,
				       Object instance)
  {
    Macro mac = new Macro(name, expander);
    mac.flags = HYGIENIC|SKIP_SCAN_FORM;
    mac.instance = instance;
    return mac;
  }

  public static Macro make (Object name, Procedure expander)
  {
    Macro mac = new Macro(name, expander);
    return mac;
  }

  public static Macro make(Object name, Procedure expander, Object instance)
  {
    Macro mac = new Macro(name, expander);
    mac.instance = instance;
    return mac;
  }

  public Macro ()
  {
  }

  /** Copy constructor. */
  public Macro (Macro old)
  {
    name = old.name;
    expander = old.expander;
    flags = old.flags;
  }

  public Macro(Object name, Procedure expander)
  {
    super(name);
    this.expander = expander instanceof Expression ? expander
        : new QuoteExp(expander);
  }

  public Macro(Object name)
  {
    super(name);
  }

    public gnu.expr.Expression rewriteForm(Pair form, Translator tr) {
        return tr.rewrite(expand(form, tr), 'N');
    }

  public String toString()
  {
    return "#<macro "+getName()+'>';
  }

  public void print (Consumer out)
  {
    out.write("#<macro ");
    out.write(getName());
    out.write ('>');
  }

  public Object expand (Object form, Translator tr)
  {
    Object savedMacroMark = tr.currentMacroMark;
    tr.currentMacroMark = new Object();
    try
      {
	Procedure pr;
	Object exp = expander;
	if (exp instanceof Procedure && ! (exp instanceof Expression))
	  pr = (Procedure) exp;
	else
	  {
	    if (! (exp instanceof Expression))
	      {
		Macro savedMacro = tr.currentMacroDefinition;
		tr.currentMacroDefinition = this;
		try
		  {
		    exp = tr.rewrite(exp);
		    expander = exp;
		  }
		finally
		  {
		    tr.currentMacroDefinition = savedMacro;
		  }
	      }
            /* DEBUGGING:
            if (exp instanceof LambdaExp)
              {
                System.err.println("expand "+this+" expander:"+exp);
                System.err.flush();
                gnu.kawa.io.OutPort dout = gnu.kawa.io.OutPort.errDefault();
		dout.flush();
                ((Expression) exp).print(dout);
                dout.println(']');
		dout.flush();
             }
            */
	    pr = (Procedure)
	      ((Expression) exp).eval(tr.getGlobalEnvironment());
	  }
	Object result;
	if (! isHygienic())
	  {
	    form = Quote.quote(form, tr);
	    int nargs = Translator.listLength(form);
	    if (nargs <= 0)
	      return tr.syntaxError("invalid macro argument list to "+this);
	    Object[] args = new Object[nargs-1];
	    for (int i = 0;  i < nargs;  i++)
	      {
		Pair pair = (Pair) form;
		if (i > 0)
		  args[i-1] = pair.getCar();
		form = pair.getCdr();
	      }
	    result = pr.applyN(args);
	  }
	else
	  result = pr.apply1(form);
	if (form instanceof PairWithPosition && result instanceof Pair
	    && ! (result instanceof PairWithPosition))
	  {
	    Pair p = (Pair) result;
	    result = new PairWithPosition((PairWithPosition) form,
					  p.getCar(), p.getCdr());
	  }
	return result;
      }
    catch (Throwable ex)
      {
        return tr.syntaxError("evaluating syntax transformer '"
                              + getName() + "' threw " + ex);
      }
    finally
      {
        tr.currentMacroMark = savedMacroMark;
      }
  }

  public void scanForm (Pair st, ScopeExp defs, Translator tr)
  {
    if ((flags & SKIP_SCAN_FORM) != 0)
      {
        super.scanForm(st, defs, tr);
        return;
      }
    String save_filename = tr.getFileName();
    int save_line = tr.getLineNumber();
    int save_column = tr.getColumnNumber();
    Syntax saveSyntax = tr.currentSyntax;
    try
      {
	tr.setLine(st);
        tr.currentSyntax = this;
	Object x = expand(st, tr);
	tr.scanForm(x, defs);
      }
    finally
      {
	tr.setLine(save_filename, save_line, save_column);
        tr.currentSyntax = saveSyntax;
      }
  }

  /**
   * @serialData Write the name followed by the expansion procedure,
   *   both using writeObject.
   */
  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(((QuoteExp) expander).getValue());
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    expander = new QuoteExp(in.readObject());
  }
}
