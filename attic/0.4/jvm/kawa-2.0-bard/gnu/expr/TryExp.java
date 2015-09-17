package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;

/**
  * This class represents try/catch/finally.
  * @author      Per Bothner
  */

public class TryExp extends Expression
{
  Expression try_clause;

  CatchClause catch_clauses;

  Expression finally_clause;
  
  public final CatchClause getCatchClauses () { return catch_clauses; }
  public final Expression getFinallyClause () { return finally_clause; }
  public final void setCatchClauses (CatchClause catch_clauses)
  {
    this.catch_clauses = catch_clauses;
  }

    public void addCatchClause(Declaration decl, Expression body) {
        CatchClause clause = new CatchClause(decl, body);
        CatchClause last = catch_clauses;
        if (last == null)
            catch_clauses = clause;
        else {
            while (last.next != null)
                last = last.next;
            last.next = clause;
        }
    }

  public TryExp (Expression try_clause, Expression finally_clause)
  {
    this.try_clause = try_clause;
    this.finally_clause = finally_clause;
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    try
      {
	try_clause.apply(ctx);
        ctx.runUntilDone();
      }
    catch (Throwable ex)
      {
        for (CatchClause clause = catch_clauses; clause != null;
             clause = clause.next)
          {
            Declaration decl = clause.firstDecl();
            ClassType typeVal = (ClassType) decl.getTypeExp().eval(ctx);
            if (typeVal.isInstance(ex))
              {
                ctx.value1 = ex;
                clause.apply(ctx);
                return;
              }
          }
        throw ex;
      }
    finally
      {
        if (finally_clause != null)
          finally_clause.eval(ctx);
      }
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    boolean has_finally = finally_clause != null;
    Target ttarg;
    if (target instanceof StackTarget
        || target instanceof ConsumerTarget || target instanceof IgnoreTarget
        || (target instanceof ConditionalTarget && finally_clause == null))
      ttarg = target;
    else
      ttarg = Target.pushValue(target.getType());
    code.emitTryStart(has_finally,
                      ttarg instanceof StackTarget ? ttarg.getType() : null);
    try_clause.compileWithPosition(comp, ttarg);
    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.compile(comp, ttarg);
      }

    if (finally_clause != null)
      {
	code.emitFinallyStart();
	finally_clause.compileWithPosition(comp, Target.Ignore);
	code.emitFinallyEnd();
      }
    code.emitTryCatchEnd();
    if (ttarg != target)
      target.compileFromStack(comp, target.getType());
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitTryExp(this, d);
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d)
  {
    try_clause = visitor.visitAndUpdate(try_clause, d);
    CatchClause catch_clause = catch_clauses;
    while (visitor.exitValue == null && catch_clause != null)
      {
	visitor.visit(catch_clause, d); // FIXME update?
	catch_clause = catch_clause.getNext();
      }

    if (visitor.exitValue == null && finally_clause != null)
      finally_clause = visitor.visitAndUpdate(finally_clause, d);
  }

    protected gnu.bytecode.Type calculateType() {
        Type t = try_clause.getType();
        for (CatchClause clause = catch_clauses;
             clause != null; clause = clause.getNext())  {
            t = Language.unionType(t, clause.getType());
        }
        return t;
    }

  public void print (OutPort ps)
  {
    ps.startLogicalBlock("(Try", ")", 2);
    ps.writeSpaceFill();
    try_clause.print(ps);
    CatchClause catch_clause = catch_clauses;
    for (; catch_clause != null;  catch_clause = catch_clause.getNext())
      {
	catch_clause.print(ps);
      }
    if (finally_clause != null)
      {
	ps.writeSpaceLinear();
	ps.print(" finally: ");
	finally_clause.print(ps);
      }
    ps.endLogicalBlock(")");
  }
}
