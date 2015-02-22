package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.text.Printable;

/**
 * Abstract class for "syntax" objects.
 * Builtins and macros are instances of this class.
 * @author	Per Bothner
 */

abstract public class Syntax implements Printable, Named
{
  Object name;

  public final String getName()
  {
    return name == null ? null
      : name instanceof Symbol ? ((Symbol) name).getName()
      : name.toString();
  }
  public Object getSymbol() { return name; }

  public void setName (Object name) { this.name = name; }
  public void setName (String name) { this.name = name; }

  public Syntax ()
  {
  }

  public Syntax (Object name)
  {
    setName(name);
  }

  /**
   * Re-write an expression that is an "application" of this Syntax object.
   * @param obj the arguments to this "application" (i.e. the cdr of
   * the macro/builtin invocation)
   * @param tr the Translator that provides context
   * @return the re-written expression
   */

  public Expression rewrite (Object obj, Translator tr)
  {
    throw new InternalError("rewrite method not defined");
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return rewrite(form.getCdr(), tr);
  }

  public void scanForm (Pair st, ScopeExp defs, Translator tr)
  {
    boolean ok = scanForDefinitions(st, defs, tr);
    if (! ok)
      tr.pushForm(new ErrorExp("syntax error expanding "+this));
  }

  /** Check if a statement is a definition, for initial pass.
   * Semi-deprecated - should convert calls to use scanForm.
   * @param st the statement to check
   * @param defs where to add Declarations for found definitions
   * @param tr the compilation state
   * @return true on success
   */
  public boolean scanForDefinitions(Pair st, ScopeExp defs, Translator tr)
  {
    tr.pushForm(st);
    return true;
  }

  public void print (Consumer out)
  {
    out.write("#<syntax ");
    String name = this.getName();
    out.write(name == null ? "<unnamed>" : name);
    out.write('>');
  }
}
