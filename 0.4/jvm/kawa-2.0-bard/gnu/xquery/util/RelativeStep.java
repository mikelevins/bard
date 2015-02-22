// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.math.IntNum;
import gnu.kawa.functions.*;
import gnu.kawa.reflect.OccurrenceType;

/** Implements XPath path expression.
 * The XPath expression E1/E2 is compiled into:
 * (relative-step E1 (lambda (dot position last) E2)).
 */

public class RelativeStep extends MethodProc implements Inlineable
{
  public static final RelativeStep relativeStep = new RelativeStep();

  RelativeStep ()
  {
    setProperty(Procedure.validateApplyKey,
                   "gnu.xquery.util.CompileMisc:validateApplyRelativeStep");
  }

  public int numArgs() { return 0x2002; }

  public void apply (CallContext ctx) throws Throwable
  {
    Object arg = ctx.getNextArg();
    Object next = ctx.getNextArg();
    Procedure proc = (Procedure) next;
    Consumer out = ctx.consumer;
    IntNum countObj;
    Nodes values;
    if (arg instanceof Nodes)
      values = (Nodes) arg;
    else
      {
	values = new Nodes();
	Values.writeValues(arg, values);
      }
    int count = values.size();
    int it = 0;
    countObj = IntNum.make(count);
    RelativeStepFilter filter = new RelativeStepFilter(out);
    for (int pos = 1; pos <= count; pos++)
      {
	it = values.nextPos(it);
	Object dot = values.getPosPrevious(it);
	proc.check3(dot, IntNum.make(pos), countObj, ctx);
        Values.writeValues(ctx.runUntilValue(), filter);
      }
    filter.finish();
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    if (target instanceof IgnoreTarget)
      {
        exp1.compile(comp, target);
        exp2.compile(comp, target);
        return;
      }

    Type rtype = exp.getTypeRaw();
    if (rtype == null) // should never happen
      rtype = Type.pointer_type;
    Type rtypePrime = OccurrenceType.itemPrimeType(rtype);
    int nodeCompare = NodeType.anyNodeTest.compare(rtypePrime);
    // 'A' - atomic; 'N' - nodes; 'S' - pre-sorted nodes; ' ' - unknown.
    char expectedKind;
    if (nodeCompare >= 0)
      expectedKind = 'N';
    else if (nodeCompare == -3)
      expectedKind = 'A';
    else
      expectedKind = ' ';
    TreeScanner step = extractStep(exp2);
    if (step != null)
      {
        Type type1 = exp1.getType();
        if (step instanceof ChildAxis
            || step instanceof AttributeAxis
            || step instanceof SelfAxis)
          {
            if (type1 instanceof NodeSetType
                || (expectedKind == 'N'
                    && OccurrenceType.itemCountIsZeroOrOne(exp1.getType())))
              expectedKind = 'S';
            /*
            // It's presumably more efficient to sort the argument
            // nodes rather than the result nodes.  FIXME
            else
              {
                exp1 = SortNodes(exp1);
                expectedKind = 'S';
              }
            */
          }
      }

    if (! (target instanceof ConsumerTarget))
      {
	ConsumerTarget.compileUsingConsumer(exp, comp, target);
	return;
      }

    CodeAttr code = comp.getCode();
    Target mtarget;
    Scope scope = code.pushScope();
    Variable mconsumer;
    Variable tconsumer;
    ClassType mclass;

    if (expectedKind == 'A' || expectedKind == 'S')
      {
        mtarget = target;
        mclass = null;
        mconsumer = null;
        tconsumer = null;
      }
    else
      {
        // We need a helper consumer.
        Method initMethod;
        if (expectedKind == 'N')
          {
            mclass = ClassType.make("gnu.kawa.xml.SortedNodes");
            initMethod = mclass.getDeclaredMethod("<init>", 0);
          }
        else
          {
            mclass = ClassType.make("gnu.xquery.util.RelativeStepFilter");
            initMethod = mclass.getDeclaredMethod("<init>", 1);
          }
        mconsumer = scope.addVariable(code, mclass, null);
        mtarget = new ConsumerTarget(mconsumer);
        code.emitNew(mclass);
        code.emitDup(mclass);
        tconsumer = ((ConsumerTarget) target).getConsumerVariable();
        if (expectedKind != 'N')
          code.emitLoad(tconsumer);
        code.emitInvoke(initMethod);
        code.emitStore(mconsumer);     
      }

    ValuesMap.compileInlined((LambdaExp) exp2, exp1, 1, null, comp, mtarget);

    // Now finish up from the helper consumer.
    if (expectedKind == 'N')
      {
        code.emitLoad(mconsumer);
        code.emitLoad(tconsumer);
        code.emitInvokeStatic(Compilation.typeValues
                              .getDeclaredMethod("writeValues", 2));
      }
    else if (expectedKind == ' ')
      {
        code.emitLoad(mconsumer);
        code.emitInvoke(mclass.getDeclaredMethod("finish", 0));
      }

    code.popScope();
  }

  public Type getReturnType (Expression[] args)
  {
    // Needlessly convervative, but it shouldn't matter, since this
    // shouldn't be called if the ApplyExp.setType has been done.
    return Type.pointer_type;
  }

  public static TreeScanner extractStep (Expression exp)
  {
    for (;;)
      {
        if (! (exp instanceof ApplyExp))
          return null;
        ApplyExp aexp = (ApplyExp) exp;
        Expression func = aexp.getFunction();
        if (func instanceof QuoteExp)
          {
            Object value = ((QuoteExp) func).getValue();
            if (value instanceof TreeScanner)
              return (TreeScanner) value;
            // This doesn't work, if we've already inlined ValuesFilter. FIXME
            if (value instanceof ValuesFilter)
              {
                exp = aexp.getArgs()[0];
                continue;
              }
          }
        return null;
      }
  }
}
