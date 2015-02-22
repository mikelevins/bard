// Copyright (c) 1999, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;

/**
 * Class used to implement a block that can be exited.
 * @author	Per Bothner
 */

public class BlockExp extends Expression
{
  /** Currently only used for identification. */
  Declaration label;
  Expression body;

  /** If non-null, evaluate this, but only if non-normal exit. */
  Expression exitBody;

  public void setBody(Expression body)
  {
    this.body = body;
  }

  public void setBody(Expression body, Expression exitBody)
  {
    this.body = body;
    this.exitBody = exitBody;
  }

  public void setLabel (Declaration label)
  {
    this.label = label;
  }

  /* Temporary only used during compilation. */
  ExitableBlock exitableBlock;

  Target exitTarget;

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object result;
    try
      {
        result = body.eval(ctx);
      }
    catch (BlockExitException ex)
      {
        if (ex.exit.block != this)
          throw ex;
        result = ex.exit.result;
        if (exitBody != null)
          result = exitBody.eval(ctx);
      }
    ctx.writeValue(result);
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Type rtype = (exitBody == null && target instanceof StackTarget
                  ? target.getType()
                  : null);
    // FIXME - be more clever than "true"
    ExitableBlock bl = code.startExitableBlock(rtype, true);
    exitableBlock = bl;
    this.exitTarget = exitBody == null ? target : Target.Ignore;
    body.compileWithPosition(comp, target);
    Label doneLabel;
    if (exitBody != null && code.reachableHere())
      {
        doneLabel = new Label(code);
        code.emitGoto(doneLabel);
      }
    else
      doneLabel = null;
    code.endExitableBlock();
    if (exitBody != null)
      exitBody.compileWithPosition(comp, target);
    if (doneLabel != null)
      doneLabel.define(code);

    exitableBlock = null;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitBlockExp(this, d);
  }

  protected <R,D> void visitChildren(ExpVisitor<R,D> visitor, D d)
  {
    body = visitor.visitAndUpdate(body, d);
    if (visitor.exitValue == null && exitBody != null)
      exitBody = visitor.visitAndUpdate(exitBody, d);
  }

  int id = ++counter;
  static int counter;
  public String toString() { return "BlockExp#"+id; }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Block#", ")", 2);
    out.print(id);
    if (label != null)
      {
        out.print(' ');
        out.print(label.getName());
      }
    out.writeSpaceLinear();
    body.print(out);
    if (exitBody != null)
      {
	out.writeSpaceLinear();
        out.print("else ");
        exitBody.print(out);
      }
    out.endLogicalBlock(")");
  }
}

class BlockExitException extends RuntimeException
{
  ExitExp exit;
  Object result;
  public BlockExitException (ExitExp exit, Object result)
  {
    this.exit = exit;
    this.result = result;
  }
}
