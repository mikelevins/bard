package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;

/** A "catch" clause of a "try-catch" form.
  */

public class CatchClause extends LetExp
{
  CatchClause next;

  public CatchClause ()
  {
  }

    public CatchClause(Declaration decl, Expression body) {
        decl.setInitValue(QuoteExp.undefined_exp);
        add(decl);
        this.body = body;
    }  

    public CatchClause(Object name, Type type, Expression body) {
        this(new Declaration(name, type), body);
    }  

    /** "Convert" a <code>LambdaExp</code> to a <code>CatchClause</code>. */
    public CatchClause (LambdaExp lexp) {
        this();
        Declaration decl = lexp.firstDecl();
        decl.setInitValue(QuoteExp.undefined_exp);
        lexp.remove(null, decl);
        add(decl);
        body = lexp.body;
    }

  public final CatchClause getNext() { return next; }
  public final void setNext (CatchClause next) { this.next = next; }

  public final Expression getBody() { return body; }
  public final void setBody(Expression body) { this.body = body; }

  protected boolean mustCompile () { return false; }

  protected Object evalVariable (Declaration decl, CallContext ctx)
    throws Throwable
  {
    // This is the Throwable caught and set by TryExpr.apply.
    return ctx.value1;
  }

  public void compile (Compilation comp, Target target)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    Declaration catchDecl = firstDecl();
    Variable catchVar = catchDecl.allocateVariable(code);
    code.enterScope(getVarScope());
    code.emitCatchStart(catchVar);
    body.compileWithPosition(comp, target);
    code.emitCatchEnd();
    code.popScope ();
  }

  protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d)
  {
    body = visitor.visitAndUpdate(body, d);
  }

  public void print (OutPort out)
  {
    out.writeSpaceLinear();
    out.startLogicalBlock("(Catch", ")", 2);
    out.writeSpaceFill();
    decls.printInfo(out);
    out.writeSpaceLinear();
    body.print(out);
    out.endLogicalBlock(")");
  }
}
