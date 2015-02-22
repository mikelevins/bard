// Copyright (c) 2001, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.expr.*;
import gnu.kawa.lispexpr.*;
import gnu.kawa.reflect.FieldLocation;
import gnu.lists.*;
import gnu.mapping.*;

/** Abstract class for Lisp-like languages with separate namespaces. */

public abstract class Lisp2 extends LispLanguage {
    public static final LList FALSE = LList.Empty;
    // FIXME - which namespace?
    public static final Symbol TRUE = Namespace.getDefault().getSymbol("t");
    public static final Expression nilExpr = new QuoteExp(FALSE);

    @Override
    public boolean isTrue(Object value) {
        return value != FALSE;
    }

    @Override
    public Object booleanObject(boolean b) {
        return b ? TRUE : FALSE;
    }

    @Override
    public Object noValue() {
        return FALSE;
    }

    /**
     * Determines if functions in this language have a separate namespace.
     *
     * Different Lisps have different organisations of the symbol namespace.
     * "Lisp1" languages, notably Scheme, have a single namespace where
     * functions and values are not distinguished. "Lisp2" languages,
     * notably CL, separate the two objects into conceptually two namespaces.
     * Kawa doesn't literally use two namespaces, such binding properties are 
     * looked up in an {@link Environment} table.
     * 
     * @return true if this language has a separate function namespace, false
     *         otherwise.
     */
    @Override
    public boolean hasSeparateFunctionNamespace() {
        return true;
    }

    /**
     * Determine whether a symbol evaluates to itself.
     * @param obj The symbol to check
     * @return true if the symbol evaluates to itself, otherwise false.
     */
    @Override
    public boolean selfEvaluatingSymbol (Object obj) {
        return obj instanceof Keyword || obj == TRUE || obj == FALSE;
    }

    /**
     * Given a field, figure out which namespace to put its binding in.
     * Specifically, return the appropriate property key to use when
     * importing the field's binding into an {@link Environment}:
     * This is null for Scheme (as a Lisp1), but may be the FUNCTION property
     * for languages like CL and Emacs Lisp, for example.
     *
     * @param fld The field of some module we're importing.
     * @param value The value of the field (assuming it is constant).
     * @return {@code EnvironmentKey.FUNCTION} if this is a function binding or
     *         {@code null} if this binding has the null property key.
     */
    @Override
    public Object getEnvPropertyFor(java.lang.reflect.Field fld, Object value) {
        if (Compilation.typeProcedure.getReflectClass()
                .isAssignableFrom(fld.getType())
            || value instanceof kawa.lang.Syntax) {
            return EnvironmentKey.FUNCTION;
        }
        return null;
    }

    /**
     * Get the namespace of a declaration.
     *
     * Declaration symbols are either in the function or value namespace.
     *
     * @param decl The declaration to check
     * @return {@code FUNCTION_NAMESPACE} or {@code VALUE_NAMESPACE}.
     */
    @Override
    public int getNamespaceOf(Declaration decl) {
        // This is a kludge because the hygiene renaming in SyntaxRules
        // (which is used for some macros that Lisp uses) doesn't distinguish
        // function and variable position.
        if (decl.isAlias())
            return FUNCTION_NAMESPACE+VALUE_NAMESPACE;
        return decl.getFlag(Declaration.PROCEDURE|Declaration.IS_SYNTAX)
            ? FUNCTION_NAMESPACE
            : VALUE_NAMESPACE;
    }

    /** Get a symbol for a given (interned) Java string. */
    public static Object asSymbol(String name) {
        // FIXME: Symbol comparison instead of string.
        if (name == "nil")
            return FALSE;
        return Environment.getCurrent().getSymbol(name);
    }

  protected Symbol fromLangSymbol (Object obj)
  {
    if (obj == LList.Empty)
      return environ.getSymbol("nil");
    return super.fromLangSymbol(obj);
  }

  /** Get a string for a given Java string. */
  public static Object getString (String name)
  {
    return new FString(name);
  }

  /** Get a string for a given symbol. */
  public static Object getString (Symbol symbol)
  {
    return getString(symbol.getName());
  }

  protected void defun(String name, Object value)
  {
    environ.define(getSymbol(name), EnvironmentKey.FUNCTION, value);
    if (value instanceof Named)
      {
	Named n = (Named) value;
	if (n.getName() == null)
	  n.setName(name);
      }
  }

  protected void defun(Symbol sym, Object value)
  {
    environ.define(sym, EnvironmentKey.FUNCTION, value);
    if (value instanceof Procedure)
      {
	Procedure n = (Procedure) value;
	if (n.getSymbol() == null)
	  n.setSymbol(sym);
      }
  }

  private void defun(Procedure proc)
  {
    defun(proc.getName(), proc);
  }

  protected void importLocation (Location loc)
  {
    Symbol name = ((NamedLocation) loc).getKeySymbol();
    if (environ.isBound(name, EnvironmentKey.FUNCTION))
      return;
    Object val;
    loc = loc.getBase();
    // Disable the following, for now, if using GCJ.  It hangs when using GCJ.
    // The problem appears to be with a _Jv_Field for a static field
    // that is in a BSS segment; the address in the _Jv_Field doesn't
    // get initialized.  FIXME.
    // (We do need to use this for JEmacs.  Sigh.)
    if (loc instanceof FieldLocation
        && ((FieldLocation) loc).isProcedureOrSyntax())
      {
        environ.addLocation(name, EnvironmentKey.FUNCTION, loc);
      }
    else if ((val = loc.get(null)) != null)
      {
        if (val instanceof Procedure || val instanceof kawa.lang.Syntax)
          defun(name, val);
        else if(val instanceof LangObjType)
          defun(name, ((LangObjType) val).getConstructor());
        else
          define(name.getName(), val);
      }
  }

  public ReadTable createReadTable ()
  {
    ReadTable tab = new Lisp2ReadTable();
    tab.initialize(false);
    tab.setInitialColonIsKeyword(true);
    return tab;
  }

   public String getCompilationClass () { return "gnu.commonlisp.lang.Lisp2Compilation"; }
}

class Lisp2ReadTable extends ReadTable
{
  protected Object makeSymbol (String name)
  {
    return Lisp2.asSymbol(name.intern());
  }
}
