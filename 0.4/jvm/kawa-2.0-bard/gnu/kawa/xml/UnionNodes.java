// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.functions.AppendValues;

/** Get the union of two node lists.
 * Implements the XQuery '|' or 'union' operator.
 */

public class UnionNodes extends Procedure2 implements Inlineable
{
  public static final UnionNodes unionNodes = new UnionNodes();

  public Object apply2 (Object vals1, Object vals2)
  {
    SortedNodes nodes = new SortedNodes();
    Values.writeValues(vals1, nodes);
    Values.writeValues(vals2, nodes);
    return nodes;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    exp = new ApplyExp(AppendValues.appendValues, exp.getArgs());
    ConsumerTarget.compileUsingConsumer(exp, comp, target,
					SortNodes.makeSortedNodesMethod, null);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

}
