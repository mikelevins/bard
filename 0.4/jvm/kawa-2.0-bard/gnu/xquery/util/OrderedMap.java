package gnu.xquery.util;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

/** A procedure used to represent a FLWOR expression with
 * an {@code order by} clause.
 * ({@link gnu.kawa.functions.ValuesMap} is used for FLWOR expression
 * that don't have an {@code order by} clause.)
 *
 * As returned by the parser:
 * <pre>
 * for $x1 in exp1, $x2 in exp2 where cond order by comparator1 ... return body
 * </pre>
 * is represented as
 * <pre>
 * ordered-map(tuple-sequence, body-function,
 *             comparator-function1, flags1, collation1, ...)
 * </pre>
 * Here tuple-sequence is an expression that returns a sequence of tuples,
 * which are currently implemnted as Java Object[] arrays.
 * After inlining we get:
 * <pre>
 * ordered-map(tuple-sequence.
 *   OrderedTuples.make$V(body-function,
 *     new Object[]{comparator-function1, flags1, collation1, ...}))
 * </pre>
 *
 * A future optimization would be to create an instance of a new sub-class
 * of OrderedTuples.  Then the body-function and comparator-functions
 * could be compiled as methods to that class.  That wins especially
 * if it saves us having to create extra frame classes.
 */

public class OrderedMap extends MethodProc
  implements Inlineable
{
  public static final OrderedMap orderedMap = new OrderedMap();
  static {
    orderedMap.setProperty(Procedure.validateApplyKey,
                   "gnu.xquery.util.CompileMisc:validateApplyOrderedMap");
 }

  public static Object[] makeTuple$V (Object[] values)
  {
    return values;
  }

  public void apply (CallContext ctx) throws Throwable
  {
    Object[] args = ctx.getArgs();
    Object values = args[0];
    OrderedTuples tuples;
    if (args.length == 2)
      {
        tuples = (OrderedTuples) args[1];
      }
    else
      {
        Object[] comps = new Object[args.length-2];
        System.arraycopy(args, 2, comps, 0, comps.length);
        tuples = OrderedTuples.make$V((Procedure) args[1], comps);
      }
    Values.writeValues(values, tuples);
    tuples.run$X(ctx);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    CompileMisc.compileOrderedMap(exp, comp, target, this);
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return Type.pointer_type; // FIXME
  }
}
