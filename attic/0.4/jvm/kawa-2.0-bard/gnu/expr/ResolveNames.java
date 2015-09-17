// Copyright (c) 2003, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;

/** This resolves references to lexical Declarations.
 * So far it is only used for XQuery, which overrides it.
 * Lisp-family languages get similar functionality through the 2-phase
 * scan/rewrite framework.
 */

public class ResolveNames extends ExpExpVisitor<Void>
{
  protected NameLookup lookup;

  public ResolveNames ()
  {
  }

  public ResolveNames (Compilation comp)
  {
    setContext(comp);
    lookup = comp.lexical;
  }

  public void resolveModule(ModuleExp exp)
  {
    Compilation saveComp = Compilation.setSaveCurrent(comp);
    try
      {
        push(exp);
        exp.visitChildren(this, null);
      }
    finally
      {
        Compilation.restoreCurrent(saveComp);
        // Note we don't do lookup.pop(exp).  This is so top-level
        // declarations remain for future uses of the same Lexer.
      }
  }

  protected void push (ScopeExp exp)
  {
    lookup.push(exp);
  }

  protected Expression visitScopeExp (ScopeExp exp, Void ignored)
  {
    visitDeclarationTypes(exp);
    push(exp);
    exp.visitChildren(this, ignored);
    lookup.pop(exp);
    return exp;
  }

  protected Expression visitLetExp (LetExp exp, Void ignored)
  {
    visitDeclarationTypes(exp);
    exp.visitInitializers(this, ignored);
    push(exp);
    exp.body = visit(exp.body, ignored);
    lookup.pop(exp);
    return exp;
  }

  public Declaration lookup (Expression exp, Object symbol, boolean function)
  {
    return lookup.lookup(symbol, function);
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Void ignored)
  {
    Declaration decl = exp.getBinding();
    if (decl == null)
      {
	decl = lookup(exp, exp.getSymbol(), exp.isProcedureName());
	if (decl != null)
	  exp.setBinding(decl);
      } 
    return exp;
 }

  protected Expression visitSetExp (SetExp exp, Void ignored)
  {
    if (exp.binding == null)
      {
        Declaration decl = lookup(exp, exp.getSymbol(), exp.isFuncDef());
        if (decl != null)
          decl.setCanWrite(true);
        exp.binding = decl;
      }
    return super.visitSetExp(exp, ignored);
  }
}
