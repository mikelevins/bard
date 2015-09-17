// Copyright (c) 1999, 2006, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;

/**
 * Expression to exit a lexically surrounding block.
 * @author	Per Bothner
 */

public class ExitExp extends Expression
{
  BlockExp block;
  Expression result;

  public ExitExp(Expression result, BlockExp block)
  {
    this.result = result;
    this.block = block;
  }

  public ExitExp(BlockExp block)
  {
    this.result = QuoteExp.voidExp;
    this.block = block;
  }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    throw new BlockExitException(this, result.eval(ctx));
  }

  public void compile (Compilation comp, Target target)
  {
    CodeAttr code = comp.getCode();
    Expression res = result == null ? QuoteExp.voidExp : result;
    res.compileWithPosition(comp, block.exitTarget);
    block.exitableBlock.exit();
  }

  protected Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    Expression res = deepCopy(result, mapper);
    if (res == null && result != null)
      return null;
    Object b = mapper.get(block);
    ExitExp copy
      = new ExitExp((Expression) res, b == null ? block : (BlockExp) b);
    copy.flags = getFlags();
    return copy;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitExitExp(this, d);
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d)
  {
    result = visitor.visitAndUpdate(result, d);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Exit", false, ")");
    out.writeSpaceFill();
    if (block != null)
      {
        out.print("Block#");
        out.print(block.id);
      }
    else if (block == null)
      out.print("<unknown>");
    if (result != null)
      {
	out.writeSpaceLinear();
	result.print(out);
      }
    out.endLogicalBlock(")");
  }

  protected Type calculateType()
  {
    return Type.neverReturnsType;
  }
}
