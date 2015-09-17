package gnu.expr;
import gnu.bytecode.*;

/**
 * Abstract class for expressions that add local variable bindings.
 * @author	Per Bothner
 */

public abstract class ScopeExp extends Expression
{
  Declaration decls;
  Declaration last;

  private Scope scope;

  public Declaration firstDecl () { return decls; }

  public Scope getVarScope ()
  {
    Scope sc = scope;
    if (sc == null)
      scope = sc = new Scope();
    return sc;
  }

  /** Clear bytecode resources for the ScopeExp.
   * This potentially allows Kawa to generate code for the same (inlined,
   * shared) ScopeExp multiple times - though we're not making use of that yet.
   */
  public void popScope (CodeAttr code)
  {
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      decl.var = null;
    code.popScope();
    scope = null;
  }

  public void add (Declaration decl)
  {
    if (last == null)
      decls = decl;
    else
      last.setNext(decl);
    last = decl;
    decl.context = this;
  }

  /** Add a Declaration at a specified position.
   */
  public void add (Declaration prev, Declaration decl)
  {
    if (prev == null)
      { // Put first
        decl.setNext(decls);
        decls = decl;
      }
    else
      {
        decl.setNext(prev.nextDecl());
        prev.setNext(decl);
      }
    if (last == prev)
      last = decl;
    decl.context = this;
  }

  /** Replace the <code>prev.next</code> by <code>newDecl</code>.
   * If <code>prev==null</code>, replace the first decl. */
  public void replaceFollowing (Declaration prev, Declaration newDecl)
  {
    Declaration oldDecl;
    if (prev == null)
      {
	oldDecl = decls;
	decls = newDecl;
      }
    else
      {
        oldDecl = prev.nextDecl();
        prev.setNext(newDecl);
      }
    newDecl.setNext(oldDecl.nextDecl());
    if (last == oldDecl)
      last = newDecl;
    oldDecl.setNext(null);
    newDecl.context = this;
  }

  public void remove (Declaration decl)
  {
    Declaration prev = null;
    for (Declaration cur = firstDecl(); cur != null; cur = cur.nextDecl())
      {
	if (cur == decl)
	  {
	    remove(prev, decl);
	    return;
	  }
	prev = cur;
      }
  }

  public void remove (Declaration prev, Declaration decl)
  {
    Declaration next = decl.nextDecl();
    if (prev == null)
      decls = next;
    else
      prev.setNext(next);
    if (last == decl)
      last = prev;
    decl.setNext(null);
  }

  public ScopeExp () { }

    /** The statically enclosing binding contour. */
    private ScopeExp outer;

    /** Return the statically enclosing binding contour. */
    public ScopeExp getOuter() { return outer; }

    public void setOuter(ScopeExp outer) { this.outer = outer; }

  public LambdaExp currentLambda ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.getOuter())
      {
	if (exp == null)
	  return null;
	if (exp instanceof LambdaExp)
	  return (LambdaExp) exp;
      }
  }

  /** Return the outermost non-module scope, if there is one. */
  public ScopeExp topLevel ()
  {
    ScopeExp exp = this;
    for (;; )
      {
        ScopeExp outer = exp.getOuter();
        if (outer == null || outer instanceof ModuleExp)
          return exp;
        exp = outer;
      }
  }

  public ModuleExp currentModule ()
  {
    ScopeExp exp = this;
    for (;; exp = exp.getOuter())
      {
	if (exp == null)
	  return null;
	if (exp instanceof ModuleExp)
	  return (ModuleExp) exp;
      }
  }

  /**
   * Find a Declaration by name.
   * @param sym the (interned) name of the Declaration sought
   * @return the matching Declaration, if found;  otherwise null
   */
  public Declaration lookup (Object sym)
  {
    if (sym != null)
      {
        for (Declaration decl = firstDecl();
             decl != null;  decl = decl.nextDecl())
          {
            if (sym.equals(decl.symbol))
              return decl;
          }
      }
    return null;
  }

  public Declaration lookup (Object sym, Language language, int namespace)
  {
    for (Declaration decl = firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
	if (sym.equals(decl.symbol)
	    && language.hasNamespace(decl, namespace))
	  return decl;
      }
    return null;
  }

  /** Lookup a declaration, create a non-defining declaration if needed. */
  public Declaration getNoDefine (Object name)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      {
	decl = addDeclaration(name);
	decl.flags |= Declaration.NOT_DEFINING | Declaration.IS_UNKNOWN;
      }
    return decl;
  }

  /** Add a new Declaration, with a message if there is an existing one. */
  public Declaration getDefine (Object name, char severity, Compilation parser)
  {
    Declaration decl = lookup(name);
    if (decl == null)
      decl = addDeclaration(name);
    else if ((decl.flags & (Declaration.NOT_DEFINING | Declaration.IS_UNKNOWN))
	     != 0)
      decl.flags &= ~ (Declaration.NOT_DEFINING|Declaration.IS_UNKNOWN);
    else
      {
	Declaration newDecl = addDeclaration(name);
        duplicateDeclarationError(decl, newDecl, parser);
        decl = newDecl;
      }
    return decl;
  }

  public static void duplicateDeclarationError (Declaration oldDecl,
                                                Declaration newDecl,
                                                Compilation comp)
  {
    comp.error('e', newDecl, "duplicate declaration of '", "'");
    comp.error('e', oldDecl, "(this is the previous declaration of '", "')");
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   */
  public final Declaration addDeclaration (Object name)
  {
    Declaration decl = new Declaration (name);
    add(decl);
    return decl;
  }

  /**
   * Create a new declaration in the current Scope.
   * @param name name (interned) to give to the new Declaration.
   * @param type type of the new Declaration.
   */
  public final Declaration addDeclaration (Object name, Type type)
  {
    Declaration decl = new Declaration (name, type);
    add(decl);
    return decl;
  }

  /**
   * Add a Declaration to the current Scope.
   */
  public final void addDeclaration (Declaration decl)
  {
    add(decl);  // FIXME just use add
  }

  public int countDecls ()
  {
    int n = 0;
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      n++;
    return n;
  }

  public void clearCallList ()
  {
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      decl.clearCallList();
  }

  public static int nesting (ScopeExp sc)
  {
    int n = 0;
    while (sc != null)
      {
	sc = sc.getOuter();
	n++;
      }
    return n;
  }

  /** True if given scope is nesed in this scope, perhaps indirectly. */
  public boolean nestedIn (ScopeExp outer)
  {
    for (ScopeExp sc = this; sc != null; sc = sc.getOuter())
      {
        if (sc == outer)
          return true;
      }
    return false;
  }
  /** Size of evalFrame to allocate in interpreter. */
  protected int frameSize;

  /** Calculate offset and frameSize needed by interpreter. */
  protected void setIndexes ()
  {
    int i = 0;
    for (Declaration decl = firstDecl(); decl != null; decl = decl.nextDecl())
      {
        decl.evalIndex = i++;
      }
    frameSize = i;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitScopeExp(this, d);
  }

  public String toString() { return getClass().getName()+"#"+id; }

  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  public int id = ++counter;
}
