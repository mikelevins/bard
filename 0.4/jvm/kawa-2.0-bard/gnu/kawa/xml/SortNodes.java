// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Sort argument nodes in document order.
 * Uses the SortedNodes class to do the actual work. */

public class SortNodes extends Procedure1 implements Inlineable
{
  public static final SortNodes sortNodes = new SortNodes();

  public Object apply1 (Object values)
  {
    SortedNodes nodes = new SortedNodes();
    Values.writeValues(values, nodes);
    return nodes.canonicalize();
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 1 || ! comp.mustCompile)
      ApplyExp.compile(exp, comp, target);
    else
      {
        Method resultMethod;
        if (target instanceof ConsumerTarget
            || (target instanceof StackTarget
                && target.getType().isSubtype(Compilation.typeValues)))
          resultMethod = null;
        else
          resultMethod = canonicalizeMethod;
        ConsumerTarget.compileUsingConsumer(args[0], comp, target,
                                            makeSortedNodesMethod,
                                            resultMethod);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  public static final ClassType typeSortedNodes
    = ClassType.make("gnu.kawa.xml.SortedNodes");
  public static final Method makeSortedNodesMethod
    = typeSortedNodes.getDeclaredMethod("<init>", 0);
  public static final Method canonicalizeMethod
    = Compilation.typeValues.getDeclaredMethod("canonicalize", 0);
}
