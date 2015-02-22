// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Coerces an item sequence to a node sequence.
 * Uses the Nodes class to do the actual work. */

public class CoerceNodes extends Procedure1 implements Inlineable
{
  public static final CoerceNodes coerceNodes = new CoerceNodes();

  public Object apply1 (Object values)
  {
    Nodes nodes = new Nodes();
    Values.writeValues(values, nodes);
    return nodes;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 1)
      ApplyExp.compile(exp, comp, target);
    else
      ConsumerTarget.compileUsingConsumer(args[0], comp, target,
					  makeNodesMethod, null);
  }

  public Type getReturnType (Expression[] args)
  {
    return typeNodes;
  }

  public static final ClassType typeNodes
    = ClassType.make("gnu.kawa.xml.Nodes");
  public static final Method makeNodesMethod
    = typeNodes.getDeclaredMethod("<init>", 0);

}
