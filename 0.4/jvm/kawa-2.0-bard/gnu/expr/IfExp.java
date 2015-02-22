// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;
import gnu.mapping.Values;

/**
 * This class represents a conditional.
 * @author	Per Bothner
 */

public class IfExp extends Expression
{
  Expression test;
  Expression then_clause;
  Expression else_clause;

  public IfExp (Expression i, Expression t, Expression e)
  {
    test = i;  then_clause = t;  else_clause = e;
  }

  public Expression getTest () { return test; }
  public Expression getThenClause () { return then_clause; }
  public Expression getElseClause () { return else_clause; }

  protected final Language getLanguage()
  {
    return Language.getDefaultLanguage(); // FIXME
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    if (getLanguage().isTrue(test.eval(ctx)))
      then_clause.apply(ctx);
    else if (else_clause != null)
      else_clause.apply(ctx);
  }

  Expression select (boolean truth)
  {
    return (truth ? then_clause
            : else_clause == null ? QuoteExp.voidExp
            : else_clause);
  }

  public void compile (Compilation comp, Target target)
  {
    compile(test, then_clause,
	    else_clause == null ? QuoteExp.voidExp : else_clause,
	    comp, target);
  }

  public static void compile (Expression test, Expression then_clause,
			      Expression else_clause,
			      Compilation comp, Target target)
  {
    Language language = comp.getLanguage();
    gnu.bytecode.CodeAttr code = comp.getCode();
    Label trueLabel, falseLabel = null;
    BlockExp block;
    boolean trueInherited, falseInherited;
    // A constant else_clause results from the expansion of (and ...),
    // and also if the else_clause if elided, so we optimize this case.
    if (target instanceof ConditionalTarget
	&& else_clause instanceof QuoteExp)
      {
	falseInherited = true;
	Object value = ((QuoteExp) else_clause).getValue();
	if (language.isTrue(value))
	  falseLabel = ((ConditionalTarget) target).ifTrue;
	else
	  falseLabel = ((ConditionalTarget) target).ifFalse;
      }
    /*
    // This is a minor optimization, useful for syntax-case switches.
    // However, there are some tricky bits setting reachabilty correctly,
    // and getting it right doesn't seem worth the complication.
    else if (else_clause instanceof ExitExp
             && ((ExitExp) else_clause).result instanceof QuoteExp
             && (block = ((ExitExp) else_clause).block).exitTarget instanceof IgnoreTarget
             && (falseLabel = block.exitableBlock.exitIsGoto()) != null)
      {
        falseInherited = true;
      }
    */
    else
      falseInherited = false;
    if (falseLabel == null)
      {
	falseLabel = new Label(code);
      }
    // The expansion of "or" creates an IfExp with test==then_clause.
    // In that case, we know that the then_clause must be true.
    // Let's optimize that case.
    if (test == then_clause && target instanceof ConditionalTarget
	&& then_clause instanceof ReferenceExp)
      {
	trueInherited = true;
	trueLabel = ((ConditionalTarget) target).ifTrue;
      }
    else
      {
	trueInherited = false;
	trueLabel = new Label(code); 
      }
    ConditionalTarget ctarget
      = new ConditionalTarget(trueLabel, falseLabel, language);
    if (trueInherited)
      ctarget.trueBranchComesFirst = false;
    test.compile(comp, ctarget);
    code.emitIfThen();
    if (! trueInherited && trueLabel.isUsed())
      {
	trueLabel.define(code);
        // An alternative to saving and restoring callContextVar
        // would be to surround the compile with a pushScope and popScope.
        // That would also release any "dangling" callContextVar.
        Variable callContextSave = comp.callContextVar;
	then_clause.compileWithPosition(comp, target);
        comp.callContextVar = callContextSave;
      }
    if (! falseInherited)
      {
        code.emitElse();
        if (falseLabel.isUsed())
          {
            falseLabel.define(code);
            // See note above for then_clause.
            Variable callContextSave = comp.callContextVar;
            if (else_clause == null)
              comp.compileConstant(Values.empty, target);
            else
              else_clause.compileWithPosition(comp, target);
            comp.callContextVar = callContextSave;
          }
        else
          code.setUnreachable();
      }
    code.emitFi();
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitIfExp(this, d);
  }

  protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d)
  {
    test = visitor.visitAndUpdate(test, d);
    if (visitor.exitValue == null)
      then_clause = visitor.visitAndUpdate(then_clause, d);
    if (visitor.exitValue == null && else_clause != null)
      else_clause = visitor.visitAndUpdate(else_clause, d);
  }

  protected gnu.bytecode.Type calculateType()
  {
    Type t1 = then_clause.getType();
    Type t2 = else_clause == null ? Type.voidType : else_clause.getType();
    return Language.unionType(t1, t2);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(If ", false, ")");
    out.setIndentation(-2, false);
    test.print(out);
    out.writeSpaceLinear();
    then_clause.print (out);
    if (else_clause != null)
      {
	out.writeSpaceLinear();
	else_clause.print (out);
      }
    out.endLogicalBlock(")");
  }

    /* DEBUGGING:
    private static int counter;
    protected int id = ++counter;

    public String toString() { return "IfExp#"+id; }
    */
}
