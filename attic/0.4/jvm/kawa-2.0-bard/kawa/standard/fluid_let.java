package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.StaticFieldLocation;

/**
 * The Syntax transformer that re-writes the Scheme "fluid-let" primitive.
 * @author	Per Bothner
 */

public class fluid_let extends Syntax
{
  public static final fluid_let fluid_let = new fluid_let();
  static { fluid_let.setName("fluid-set"); }

  /** True if bindings should be evaluated sequentionally, as in ELisp let*. */
  boolean star;

  /** Value to use if an initial value is not specified.
   * Null means use the existing binding. */
  Expression defaultInit;

  boolean warnIfUndefined;

  public fluid_let(boolean star, boolean warnIfUndefined, Expression defaultInit)
  {
    this.star = star;
    this.defaultInit = defaultInit;
    this.warnIfUndefined = warnIfUndefined;
  }

  public fluid_let()
  {
    this.star = false;
  }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing let arguments");
    Pair pair = (Pair) obj;
    return rewrite(pair.getCar(), pair.getCdr(), tr);
  }

  public Expression rewrite (Object bindings, Object body, Translator tr)
  {
    int decl_count = star ? 1 : LList.length (bindings);
    FluidLetExp let = new FluidLetExp();
    for (int i = 0; i < decl_count; i++)
      {
	Pair bind_pair = (Pair) bindings;
        Object savePos = tr.pushPositionOf(bind_pair);
        try
          {
            Expression value;
            Pair binding;
            Object name = bind_pair.getCar();
            if (name instanceof String || name instanceof Symbol)
              {
                value = defaultInit;
              }
            else if (name instanceof Pair
                     && ((binding = (Pair) name).getCar() instanceof String
                         || binding.getCar() instanceof Symbol 
                         || binding.getCar() instanceof SyntaxForm))
              {
                name = binding.getCar();
                if (name instanceof SyntaxForm)
                  name = ((SyntaxForm) name).getDatum();

                if (binding.getCdr() == LList.Empty)
                  value = defaultInit;
                else if (! (binding.getCdr() instanceof Pair)
                         || (binding = (Pair) binding.getCdr()).getCdr() != LList.Empty)
                  return tr.syntaxError("bad syntax for value of " + name
                                        + " in " + getName());
                else
                  value = tr.rewrite(binding.getCar());
              }
            else
              return tr.syntaxError("invalid " + getName() + " syntax");
            Declaration decl = let.addDeclaration(name);
            Declaration found = tr.lookup(name, -1);
            if (found == null && name instanceof Symbol)
              {
                Location loc = tr.getLanguage().getLangEnvironment()
                  .lookup((Symbol) name, null);
                if (loc != null)
                  loc = loc.getBase();
                if (loc instanceof StaticFieldLocation)
                  found = ((StaticFieldLocation) loc).getDeclaration();
              }
            if (found != null)
              {
                found.maybeIndirectBinding(tr);
                decl.base = found;
                found.setFluid(true);
                found.setCanWrite(true);
              }
            else if (! warnIfUndefined)
              decl.setFlag(Declaration.IS_DYNAMIC);
            decl.setCanWrite(true);
            decl.setFluid(true);
            decl.setIndirectBinding(true);
            if (value == null)
              value = new ReferenceExp(name);
            decl.setInitValue(value);
            decl.noteValueUnknown();
            bindings = bind_pair.getCdr();
          }
        finally
          {
            tr.popPositionOf(savePos);
          }
      }
    tr.push(let);
    let.setBody(star && bindings != LList.Empty ? rewrite (bindings, body, tr)
                : tr.rewrite_body(body));
    tr.pop(let);
    return let;
  }
}
