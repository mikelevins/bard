package gnu.expr;
import gnu.bytecode.Type;
import java.util.*;

/** Re-arranges ApplyExp where the function is a LetExp or BeginExp.
 *  Optimizes {@code ((let (...) body) . args)} to (let (...) (body . args)).
 *  Optimizes ((begin ... last) . args) to (begin ... (last . args)).
 *  This helps optimize Scheme "named let" (and some other forms)
 *  by making it more likely the application will be to a known procedure.
 *  Optimizes {@code (if (let (...) body) e1 [e2])}
 *    to {@code (let (...) (if body e1 [e2]))}.
 *  Optimizes {@code (if (begin ... last) e1 [e2])}
 *    to {@code (begin ... (if last e1 [e2]))}.
 *  These optimizations have to be done after Declarations are bound.
 *
 *  Also, for each {@code LambdaExp} set the {@code canFinishCondition} field.
 *  This information is used in the  following {@code InlineCalls} pass,
 *  to determine which calls (and thus other expressions) have the type
 *  {@code neverReturnsType}.  That in turn is used in the later
 *  {@code ChainLambda} pass to warn about unreachable code.
 */

public class PushApply extends ExpVisitor<Expression,Void>
{
  public static void pushApply (Expression exp, Compilation comp)
  {
    PushApply visitor = new PushApply();
    visitor.setContext(comp);
    visitor.visit(exp, null);
  }

  protected Expression update (Expression exp, Expression r)
  {
    return r;
  }

  protected Expression defaultValue(Expression r, Void ignored)
  {
    return r;
  }

    protected Expression visitApplyExp(ApplyExp exp, Void ignored) {
        Expression func = exp.func;
        boolean isApplyFunc = getCompilation().isApplyFunction(func)
            && exp.getArgCount() > 0;
        if (isApplyFunc) {
            func = exp.getArg(0);
        }
        if (func instanceof ReferenceExp) {
            Declaration fdecl = ((ReferenceExp) func).getBinding();
            if (fdecl != null && ! fdecl.hasUnknownValue()) {
                if (! fdecl.inExternalModule(comp))
                    fdecl.addCaller(exp);
                Expression fval = Declaration.followAliases(fdecl).getValue();
                if (fval != null && fval.getClass() == LambdaExp.class
                    && ! canFinishTracker.ignoreThisFork) {
                    noteFinishDependency((LambdaExp) fval,  currentLambda);
                }                                
            }
        }
    if (func instanceof LetExp
        && ! (func instanceof FluidLetExp)) // [APPLY-LET]
      {
	// Optimize ((let (...) body) . args) to (let (...) (body . args))
        // or (APPLY (let (...) body) . args) to (let (...) (APPLY body . args))
	LetExp let = (LetExp) func;
	Expression body = let.body;
	let.body = exp;
        if (isApplyFunc)
          exp.args[0] = body;
        else
          exp.func = body;
	return visit(let, ignored);
      }
    if (func instanceof BeginExp)  // [APPLY-BEGIN]
      {
	// Optimize ((begin ... last) . args) to (begin ... (last . args))
        // or (APPLY (begin ... last) . args) to (begin ... (APPLY last . args))
	BeginExp begin = (BeginExp) func;
	Expression[] stmts = begin.exps;
	int last_index = begin.exps.length - 1;
        if (isApplyFunc)
          exp.args[0] = stmts[last_index];
        else
          exp.func = stmts[last_index];
        stmts[last_index] = exp;
	return visit(begin, ignored);
      }
    exp.visitChildren(this, ignored);
    return exp;
  }

    void noteFinishDependency(LambdaExp callee, LambdaExp caller) {
        if (callee == caller || callee.body.type == Type.neverReturnsType) {
            canFinishTracker.dependencyAddedThisFork = true;
            caller.canFinishCondition = CanFinishMap.CANNOT_FINISH;
        } else if (caller.canFinishCondition != CanFinishMap.CAN_FINISH) {
            CanFinishMap deps = canFinishDeps();
            if (deps != CanFinishMap.CANNOT_FINISH
                    && deps.addDependency(callee))
                canFinishTracker.dependencyAddedThisFork = true;
            if (callee.canFinishListeners == null)
                callee.canFinishListeners = new HashSet<LambdaExp>(); 
            callee.canFinishListeners.add(caller);
        }
    }
    protected Expression visitIfExp(IfExp exp, Void ignored) {
        Expression test = exp.test;
        if (test instanceof LetExp
            && ! (test instanceof FluidLetExp)) { // [IF-LET] 
            // Optimize (if (let (...) body) e1 [e2])
            // to (let (...) (if body e1 [e2]))
            LetExp let = (LetExp) test;
            Expression body = let.body;
            let.body = exp;
            exp.test = body;
            return visit(let, ignored);
        }
        else if (test instanceof BeginExp) { // [IF-BEGIN]
            // Optimize (if (begin ... last) e1 [e2])
            // to (begin ... (if last e1 [e2])).
            BeginExp begin = (BeginExp) test;
            Expression[] stmts = begin.exps;
            int last_index = begin.exps.length - 1;
            exp.test = stmts[last_index];
            stmts[last_index] = exp;
            return visit(begin, ignored);
        }
        else { 
            exp.test = visit(exp.test, ignored);
            forkPush();
            exp.then_clause = visit(exp.then_clause, ignored);
            forkNext();
            if (exp.else_clause != null)
                exp.else_clause = visit(exp.else_clause, ignored);
            forkPop();
            return exp;
        }
    }

    protected Expression visitCaseExp(CaseExp exp, Void ignored) {
        Expression key = exp.key;
        if (key instanceof LetExp && !(key instanceof FluidLetExp)) {
            // [CASE-LET]
            // Optimize (case (let (...) body) clause1 ...)
            // to (let (...) (case body clause1 ...))
            LetExp let = (LetExp) key;
            Expression body = let.body;
            let.body = exp;
            exp.key = body;
            return visit(let, ignored);
        } else if (key instanceof BeginExp) {
            // [CASE-BEGIN]
            // Optimize (case (begin ... last) clause1 ...)
            // to (begin ... (case last clause1 ...)).
            BeginExp begin = (BeginExp) key;
            Expression[] stmts = begin.exps;
            int last_index = begin.exps.length - 1;
            exp.key = stmts[last_index];
            stmts[last_index] = exp;
            return visit(begin, ignored);
        } else {
            exp.key = visit(exp.key, ignored);
            forkPush();
            if (exp.clauses.length > 0) {
                exp.clauses[0].exp = visit(exp.clauses[0].exp, ignored);
                for (int i = 1; i < exp.clauses.length; i++) {
                    forkNext();
                    exp.clauses[i].exp = visit(exp.clauses[i].exp, ignored);
                }
                if (exp.elseClause != null)
                    forkNext();
            }
            if (exp.elseClause != null)
                exp.elseClause.exp = visit(exp.elseClause.exp, ignored);
            forkPop();
            return exp;
        }

    }

    protected Expression visitTryExp (TryExp exp, Void ignored) {
        forkPush();
        exp.try_clause = visit(exp.try_clause, ignored);
        CatchClause catch_clause = exp.catch_clauses;
        while (catch_clause != null) {
            forkNext();
            visit(catch_clause, ignored);
            catch_clause = catch_clause.getNext();
        }
        forkPop();

        if (exp.finally_clause != null)
            exp.finally_clause = visit(exp.finally_clause, ignored);
        return exp;
    }

  protected Expression visitReferenceExp (ReferenceExp exp, Void ignored)
  {
    Declaration decl = exp.getBinding();
    if (decl != null)
      {
        decl.numReferences++;
        // Figure out the innerLambda, which is the LambdaExp (if any)
        // between the declaration and the current context.
        if (decl.context instanceof LetExp)
          {
            LambdaExp innerLambda = getCurrentLambda();
            for (ScopeExp sc = innerLambda; sc != null; sc = sc.getOuter())
              {
                if (sc == decl.context)
                  {
                    // Chain on to innerLambda.siblingReferences list.
                    exp.siblingReferencesNext = innerLambda.siblingReferences;
                    innerLambda.siblingReferences = exp;
                    break;
                  }
                if (sc instanceof LambdaExp)
                  innerLambda = (LambdaExp) sc;
              }
          }
      }
    return super.visitReferenceExp(exp, ignored);
  }

  protected Expression visitClassExp (ClassExp exp, Void ignored)
  {
    // Allocate class fields and methods.  Ideally, setting field and method
    // types should be deferred until InlineClass, when we do type inferencing.
    // But doing it just before InlineCalls works tolerably enough for now.
    exp.declareParts(getCompilation());
    return visitLambdaExp(exp, ignored);
  }

    protected Expression visitLambdaExp (LambdaExp exp, Void ignored) {
        CanFinishTracker oldTracker = canFinishTracker;
        CanFinishTracker newTracker = new CanFinishTracker();
        canFinishTracker = newTracker;
        newTracker.dependenciesAtForkStart = CanFinishMap.CAN_FINISH;
        LambdaExp saveLambda = currentLambda;
        exp.setFlag(true, LambdaExp.IN_EXPWALKER);
        currentLambda = exp;
        try {
            return super.visitLambdaExp(exp, ignored);
        }
        finally {
            exp.setFlag(false, LambdaExp.IN_EXPWALKER);

            if (exp.canFinishCondition == null)
                exp.canFinishCondition = CanFinishMap.CAN_FINISH;
            exp.checkCanFinish();
            currentLambda = saveLambda;
            canFinishTracker = oldTracker;
        }
    }

    class CanFinishTracker {
        CanFinishTracker outer;

        /** Don't need to update canFinishCondition in this fork of an If.
         * The reason is that a prior fork did not add extra dependencies,
         * or that we depend on something known not to return.
         */
        boolean ignoreThisFork;

        boolean dependencyAddedThisFork;
        CanFinishMap dependenciesAtForkStart;
        CanFinishMap dependenciesPreviousForks;
    }

    CanFinishTracker canFinishTracker;

    private static CanFinishMap canFinishDeps(CanFinishTracker outer) {
        if (outer.dependenciesAtForkStart == null)
            outer.dependenciesAtForkStart = canFinishDeps(outer.outer).clone();
        return outer.dependenciesAtForkStart;
    }

    CanFinishMap canFinishDeps() {
        if (currentLambda.canFinishCondition == null)
            currentLambda.canFinishCondition = canFinishDeps(canFinishTracker).clone();
        return currentLambda.canFinishCondition;
    }
    
    public void forkPush() {
        LambdaExp curLambda = getCurrentLambda();
        CanFinishTracker oldTracker = canFinishTracker;
        CanFinishTracker newTracker = new CanFinishTracker();
        newTracker.dependenciesAtForkStart = curLambda.canFinishCondition;
        curLambda.canFinishCondition = null;
        newTracker.ignoreThisFork = false;
        newTracker.dependencyAddedThisFork = false;
        newTracker.outer = oldTracker;
        canFinishTracker = newTracker;
    }

    public void forkNext() {
        LambdaExp curLambda = getCurrentLambda();
        if (! canFinishTracker.dependencyAddedThisFork) {
            canFinishTracker.ignoreThisFork = true;
            canFinishTracker.dependenciesPreviousForks = null;
        } else {
            canFinishTracker.ignoreThisFork = false;
            canFinishTracker.dependencyAddedThisFork = false;
            if (canFinishTracker.dependenciesPreviousForks == null
                || canFinishTracker.dependenciesPreviousForks == CanFinishMap.CANNOT_FINISH)
                canFinishTracker.dependenciesPreviousForks = curLambda.canFinishCondition;
            else if (curLambda.canFinishCondition != CanFinishMap.CANNOT_FINISH) {
                canFinishTracker.dependenciesPreviousForks.addPaths(curLambda.canFinishCondition);
            }
            curLambda.canFinishCondition = null;
        }
    }

    public void forkPop() {
        CanFinishTracker oldTracker = canFinishTracker;
        forkNext();
        LambdaExp curLambda = currentLambda;
        if (canFinishTracker.ignoreThisFork)
            curLambda.canFinishCondition = canFinishTracker.dependenciesAtForkStart;
        else
            curLambda.canFinishCondition = canFinishTracker.dependenciesPreviousForks;
        canFinishTracker = oldTracker.outer;
    }
}
