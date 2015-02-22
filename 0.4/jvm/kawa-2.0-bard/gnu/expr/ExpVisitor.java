// Copyright (c) 2010  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import gnu.text.SourceMessages;

/** Class for doing a tree-visit over an Expression tree. */

public class ExpVisitor<R,D>
        implements gnu.text.SourceLocator {
    protected SourceMessages messages;
    Compilation comp;

    /** Get the Compilation associated with this visitor. */
    public Compilation getCompilation() { return comp; }

    public Language getLanguage() { return comp.getLanguage(); }

    public SourceMessages getMessages() { return messages; }

    protected R defaultValue(Expression r, D d) {
        return null;
    }

    protected R visitExpression(Expression exp, D d) {
        exp.visitChildren(this, d);
        return defaultValue(exp, d);
    }

    public void setContext(Compilation comp) {
        this.comp = comp;
        messages = comp.getMessages();
    }

    /** Call the visit method of argument Expression.
     * Could call Expression's visit directly, but this allows us to
     * interpose a method call on each Expression.  We use it to note the
     * Expression's line number.  Should not need to be overridden;
     * if you do, you may also want to override visitExps. */
    public R visit(Expression exp, D d) {
        return visit(this, exp, d);
    }

    public static <R,D> R visit(ExpVisitor<R,D> visitor, Expression exp, D d) {
        int line = exp.getLineNumber();
        SourceMessages messages = visitor.messages;
        if (messages != null && line > 0) {
            String saveFile = messages.getFileName();
            int saveLine = messages.getLineNumber();
            int saveColumn = messages.getColumnNumber();
            messages.setLine(exp.getFileName(), line, exp.getColumnNumber());
            R ret = exp.visit(visitor, d);
            messages.setLine(saveFile, saveLine, saveColumn);
            return ret;
        }
        return exp.visit(visitor, d);
    }

    protected Expression update(Expression exp, R r) {
        return exp;
    }

    protected R visitApplyExp(ApplyExp exp, D d) {
        return visitExpression(exp, d);
    }
  
    protected R visitIfExp(IfExp exp, D d) {
        return visitExpression(exp, d);
    }

    protected R visitCaseExp(CaseExp exp, D d) {
        return visitExpression(exp, d);
    }

    protected void visitDeclarationType (Declaration decl)
    {
        Expression texp1 = decl.typeExp;
        if (texp1 != null) {
            Expression texp2 = visitAndUpdate(texp1, null); // FIXME
            if (texp2 != texp1)
                decl.setTypeExp(texp2);
        }
    }

    protected final void visitDeclarationTypes(ScopeExp exp) {
        for (Declaration decl = exp.firstDecl(); decl != null;
             decl = decl.nextDecl()) {
            visitDeclarationType(decl);
        }
    }

    protected R visitScopeExp(ScopeExp exp, D d) {
        visitDeclarationTypes(exp);
        return visitExpression(exp, d);
    }

    protected R visitLetExp(LetExp exp, D d) { return visitScopeExp(exp, d); }
    protected R visitLambdaExp(LambdaExp exp, D d) { return visitScopeExp(exp, d); }
    protected R visitClassExp(ClassExp exp, D d) { return visitLambdaExp(exp, d); }
    protected R visitObjectExp(ObjectExp exp, D d) { return visitClassExp(exp, d); }
    protected R visitModuleExp (ModuleExp exp, D d) { return visitLambdaExp(exp, d); }

    protected R visitSetExp(SetExp exp, D d) {
        exp.new_value = visitAndUpdate(exp.new_value, d);
        return defaultValue(exp, d);
    }

    //protected Expression visitSwitchExp (SwitchExp exp, D d) { return visitExpression(exp, d); }
    protected R visitTryExp(TryExp exp, D d) { return visitExpression(exp, d); }
    protected R visitBeginExp(BeginExp exp, D d) { return visitExpression(exp, d); }
    protected R visitQuoteExp(QuoteExp exp, D d) { return visitExpression(exp, d); }
    protected R visitReferenceExp(ReferenceExp exp, D d) {
        return visitExpression(exp, d);
    }
    protected R visitThisExp(ThisExp exp, D d) {
        return visitReferenceExp(exp, d);
    }
    protected R visitSynchronizedExp(SynchronizedExp exp, D d)
    { return visitExpression(exp, d); }

    protected R visitBlockExp(BlockExp exp, D d) { return visitExpression(exp, d); }
    protected R visitExitExp(ExitExp exp, D d) { return visitExpression(exp, d); }
    protected R visitFluidLetExp(FluidLetExp exp, D d) {
        return visitLetExp(exp, d);
    }
    protected R visitLangExp (LangExp exp, D d)
    { return visitExpression(exp, d); }

    protected LambdaExp currentLambda = null;

    /** If exitValue is set to non-null, the visit stops. */
    protected Object exitValue = null;

    public Object getExitValue() { return exitValue; }

    public final LambdaExp getCurrentLambda() { return currentLambda; }

    public Expression visitAndUpdate(Expression exp, D d) {
        return update(exp, visit(exp, d));
    }

    public Expression[] visitExps(Expression[] exps, D d) {
        return exps == null ? null : visitExps(exps, exps.length, d);
    }

    /** Call visit on the Expressions in an array.
     * However, the visit method is inlined for speed.
     */
    public Expression[] visitExps(Expression[] exps, int n, D d) {
        for (int i = 0;  i < n && exitValue == null;  i++)
            exps[i] = visitAndUpdate(exps[i], d);
        return exps;
    }

    public void visitDefaultArgs(LambdaExp exp, D d) {
        for (Declaration p = exp.firstDecl(); p != null; p = p.nextDecl()) {
            Expression init = p.getInitValue();
            if (init != null)
                p.setInitValue(visitAndUpdate(init, d));
        }
    } 

    public void error(char kind, String message) {
        if (kind == 'w' && comp.warnAsError())
            kind = 'e';
        if (messages != null)
            messages.error(kind, message);
        else
            new Error("internal error: "+message);
    }

    public Expression noteError(String message) {
        if (messages != null)
            messages.error('e', message);
        return new ErrorExp (message);
    }

    public final String getFileName() { return messages.getFileName(); }
    public final int getLineNumber() { return messages.getLineNumber(); }
    public final int getColumnNumber() { return messages.getColumnNumber(); }
    public String getPublicId() { return messages.getPublicId(); }
    public String getSystemId() { return messages.getSystemId(); }
    /** Normally same as getSystemId. */

    public boolean isStableSourceLocation() { return false; }

    public void setFile(String filename) { messages.setFile(filename); }
    public void setLine(int line) { messages.setLine(line); }
    public void setColumn(int column) { messages.setColumn(column); }

    public void setLine(String filename, int line, int column) {
        messages.setLine(filename, line, column);
    }
}
