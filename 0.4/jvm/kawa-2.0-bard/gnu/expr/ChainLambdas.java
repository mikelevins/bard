package gnu.expr;
import gnu.bytecode.Type;
import gnu.kawa.functions.Convert;

/** Sets up the firstChild/nextSibling links of each LambdaExp.
 * Setup 'outer' links of ScopeExp and its sub-classes.
 * Also generates a class name for each ClassExp and registers each class.
 * Also, if lambda is bound to a unique declaration, make that its name.
 *
 * This pass also checks for unreachable code, which happens if a
 * neverReturns expression is followed dynamically by another expression.
 * Doing this check after InlineCalls allows benefiting from data-flow; OTOH
 * checking for unreachable code this late yields less precise type inference,
 * but only when there actually is unreachable code, which is bogus anyway.
 */

public class ChainLambdas extends ExpExpVisitor<ScopeExp> {
    public static void chainLambdas (Expression exp, Compilation comp) {
        ChainLambdas visitor = new ChainLambdas();
        visitor.setContext(comp);
        visitor.visit(exp, null);
    }

    protected void maybeWarnUnreachable(Expression exp) {
        if (! unreachableCodeSeen && comp.warnUnreachable())
            comp.error('w', "unreachable code", exp);
        unreachableCodeSeen = true;
    }

    /** True if we've seen (reported) unreachable code in this procedure. */
    boolean unreachableCodeSeen;

    protected Expression visitBeginExp(BeginExp exp, ScopeExp scope) {
        int neverReturnsIndex = -1;
        int last = exp.length - 1;
        for (int i = 0;  i <= last;  i++) {
            Expression e = visit(exp.exps[i], scope);
            exp.exps[i] = e;
            if (e.neverReturns() && neverReturnsIndex < 0) {
                neverReturnsIndex = i;
                if (i < last)
                    maybeWarnUnreachable(exp.exps[i+1]);
            }
        }
        if (neverReturnsIndex >= 0) {
            exp.type = Type.neverReturnsType;
            exp.length = neverReturnsIndex + 1;
        }
        return exp;
    }

    protected Expression visitApplyExp(ApplyExp exp, ScopeExp scope) {
        Expression f = visit(exp.func, scope);
        Expression[] args = exp.args;
        int nargs = args.length;
        exp.func = f;
        if (f.neverReturns()) {
            maybeWarnUnreachable(nargs > 0 ? args[0] : exp);
            return f;
        }
        for (int i = 0; i < nargs;  i++) {
            Expression e = visit(args[i], scope);
            if (e.neverReturns()
                // It seems best to silently allow converting never-returns
                // to any type.  For example it useful for stub procedures
                // that throw an "unimplemented" exception.
                && ! (f.valueIfConstant() instanceof Convert)) {
                Expression[] xargs = new Expression[i+2];
                xargs[0] = exp.func;
                System.arraycopy(args, 0, xargs, 1, i+1);
                if (i+1 < nargs || ! exp.isAppendValues()) {
                    if (! unreachableCodeSeen && comp.warnUnreachable()) {
                        comp.error('w', "unreachable procedure call", exp);
                        comp.error('i', "this operand never finishes", args[i]);
                    }
                    unreachableCodeSeen = true;
                }
                BeginExp bexp = new BeginExp(xargs);
                bexp.type = Type.neverReturnsType;
                return bexp;
            }
            args[i] = e;
        }
        return exp;
    }

    protected Expression visitSetExp(SetExp sexp, ScopeExp scope) {
        Expression r = super.visitSetExp(sexp, scope);
        if (r == sexp) {
            Expression rval = sexp.getNewValue();
            if (rval.neverReturns()) {
                maybeWarnUnreachable(sexp);
                return rval;
            }
        }
        return r;
    }

    protected Expression visitIfExp(IfExp exp, ScopeExp scope) {
        Expression e = visit(exp.test, scope);
        if (e.neverReturns()) {
            maybeWarnUnreachable(exp.then_clause);
            return e;
        }
        exp.then_clause = visit(exp.then_clause, scope);
        if (exp.else_clause != null) {
            exp.else_clause = visit(exp.else_clause, scope);
            if (exp.then_clause.neverReturns()
                && exp.else_clause.neverReturns())
                exp.type = Type.neverReturnsType;
        }
        return exp;
    }

    protected Expression visitCaseExp(CaseExp exp, ScopeExp scope) {

        Expression e = visit(exp.key, scope);
        if (e.neverReturns()) {
            for (int i = 0; i < exp.clauses.length; i++) {
                maybeWarnUnreachable(exp.clauses[i].exp);
            }
            maybeWarnUnreachable(exp.elseClause.exp);
            return e;
        }

        boolean neverReturns = true;
        for (int i = 0; i < exp.clauses.length; i++) {
            exp.clauses[i].exp = visit(exp.clauses[i].exp, scope);
            if (!exp.clauses[i].exp.neverReturns()) {
                neverReturns = false;
            }
        }
        if (exp.elseClause != null) {
            exp.elseClause.exp = visit(exp.elseClause.exp, scope);
            if (!exp.elseClause.exp.neverReturns())
                neverReturns = false;
        }

        if (neverReturns)
            exp.type = Type.neverReturnsType;

        return exp;
    }

  protected Expression visitScopeExp (ScopeExp exp, ScopeExp scope)
  {
    exp.setOuter(scope); 
    exp.visitChildren(this, exp);
    exp.setIndexes();
    if (exp.mustCompile())
      comp.mustCompileHere();
    return exp;
  }

  protected Expression visitLetExp (LetExp exp, ScopeExp scope)
  {
    exp.setOuter(scope);
    int count = 0;
    for (Declaration decl = exp.firstDecl(); decl != null; decl = decl.nextDecl())
      {
          Expression init = decl.getInitValue();
          Expression e = visit(init, exp);
          count++;
          if (e.neverReturns()) {
              if (! unreachableCodeSeen && comp.warnUnreachable())
                  comp.error('w', "initialization of "+decl.getName()+" never finishes", init);
              unreachableCodeSeen = true;
              Expression[] exps = new Expression[count];
              int i = 0;
              Declaration end = decl.nextDecl();
              for (Declaration d = exp.firstDecl(); d != end; d = d.nextDecl())
                  exps[i++] = d.getInitValue();
              return BeginExp.canonicalize(exps);
          }
          decl.setInitValue(e);
      }
    exp.body = visit(exp.body, exp);
    exp.setIndexes();
    if (exp.mustCompile())
      comp.mustCompileHere();
    return exp;
  }

  protected Expression visitLambdaExp (LambdaExp exp, ScopeExp scope)
  {
    boolean unreachableSaved = unreachableCodeSeen;
    unreachableCodeSeen = false;
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    exp.setOuter(scope);
    exp.firstChild = null;
    exp.visitChildrenOnly(this, exp);
    exp.visitProperties(this, exp);

    // Put list of children in proper order.
    LambdaExp prev = null, child = exp.firstChild;
    while (child != null)
      {
	LambdaExp next = child.nextSibling;
	child.nextSibling = prev;
	prev = child;
	child = next;
      }
    exp.firstChild = prev;

    if (exp.getName() == null && exp.nameDecl != null)
      exp.setName(exp.nameDecl.getName());
    exp.setIndexes();
    if (exp.mustCompile())
      comp.mustCompileHere();
    unreachableCodeSeen = unreachableSaved;
    return exp;
  }

  protected Expression visitClassExp (ClassExp exp, ScopeExp scope)
  {
    LambdaExp parent = currentLambda;
    if (parent != null && ! (parent instanceof ClassExp))
      {
	exp.nextSibling = parent.firstChild;
	parent.firstChild = exp;
      }

    visitScopeExp(exp, scope);

    return exp;
  }
}
