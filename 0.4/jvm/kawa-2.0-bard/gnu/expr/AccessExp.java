// Copyright (c) 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

/** A common super-type for ReferenceExpa and SetExp.
 * Contains shared information about the variable that is accessed. */

public abstract class AccessExp extends Expression
{
    /** Prefer the function binding rather than the value binding.
     * This applies to languages like Common Lisp (and Java) that
     * have separate namespaces for values/fields and functions/methods.
     */
    public static final int PREFER_BINDING2 = Expression.NEXT_AVAIL_FLAG;
    public static final int NEXT_AVAIL_FLAG = Expression.NEXT_AVAIL_FLAG << 1;

  /** The name of the variable to set - either a String or a Symbol. */
  Object symbol;
  /** If non-null, the local Declaration this refers to. */
  Declaration binding;
  public String string_name () { return symbol.toString(); }

  public final String getName()
  {
    return symbol == null ? null
      : symbol instanceof Symbol ? ((Symbol) symbol).getName()
      : symbol.toString();
  }

    /** Return a simple name, or null if the name has a non-empty namespace. */
    public final String getSimpleName() {
        if (symbol instanceof String)
            return (String) symbol;
        if (symbol instanceof Symbol) {
            Symbol sym = (Symbol) symbol;
            if (sym.hasEmptyNamespace())
                return sym.getLocalName();
            if (sym.hasUnknownNamespace())
                return sym.getPrefix() + ':' + sym.getLocalPart();
        }
        return null;
    }

  public final Object getSymbol() { return symbol; }
  /** If non-null, the local Declaration this refers to. */
  public final Declaration getBinding() { return binding; }

  public final void setBinding(Declaration decl) { binding = decl; }

  /** If binding has a non-static field and no base, use this instead of base.
   *  This is mainly used for aliases of imported module declarations. */
  private Declaration context;
  public final Declaration contextDecl ()
  { return context; }
  public final void setContextDecl(Declaration decl)
  { context = decl; }
}
