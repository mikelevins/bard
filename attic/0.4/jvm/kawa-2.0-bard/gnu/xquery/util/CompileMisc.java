package gnu.xquery.util;
import gnu.expr.*;
import gnu.mapping.Procedure;
import gnu.bytecode.*;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.xml.*;
import gnu.math.*;
import gnu.xquery.lang.XQuery;
import gnu.kawa.functions.AddOp;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.functions.ValuesMap;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.OccurrenceType;

public class CompileMisc
{
  /** Inliner for the Compare procedure. */
  public static Expression validateCompare
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression folded = exp.inlineIfConstant(proc, visitor);
    if (folded != exp)
      return folded;
    Compare cproc = (Compare) proc;
    if ((cproc.flags & Compare.VALUE_COMPARISON) != 0)
      {
      }
    else
      {
        exp = new ApplyExp(ClassType.make("gnu.xquery.util.Compare")
                           .getDeclaredMethod("apply", 4),
                           new Expression[] { new QuoteExp(IntNum.make(cproc.flags)),
                                              exp.getArg(0),
                                              exp.getArg(1),
                                              QuoteExp.nullExp });
      }
    if (exp.getTypeRaw() == null)
      exp.setType(XDataType.booleanType);
    return exp;
  }

  /** Inliner for the BooleanValue procedure. */
  public static Expression validateBooleanValue
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        Expression arg = args[0];
        Type type = arg.getType();
        if (type == XDataType.booleanType)
          return arg;
        if (type == null)
          exp.setType(XDataType.booleanType);
        if (arg instanceof QuoteExp)
          {
            Object value = ((QuoteExp) arg).getValue();
            try
              {
                return BooleanValue.booleanValue(value) ? XQuery.trueExp : XQuery.falseExp;
              }
            catch (Exception ex)
              {
                String message = "cannot convert to a boolean";
                visitor.getMessages().error('e', message);
                return new ErrorExp(message);
              }
          }
      }
    return exp;
  }

  /** Inliner for the ArithOp procedure. */
  public static Expression validateArithOp
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    // FUTURE
    return exp;
  }

  /** Inliner for the {@link ValuesFilter} procedure. */
  public static Expression validateApplyValuesFilter
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    ValuesFilter vproc = (ValuesFilter) proc;
    exp.visitArgs(visitor); // FIXME - be smarter about type propagation
    Expression[] args = exp.getArgs();
    Expression exp2 = args[1];
    LambdaExp lexp2;
    if (! (exp2 instanceof LambdaExp)
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    Expression seq = args[0];
    Type seqType = seq.getType();
    if (seqType instanceof OccurrenceType) {
        OccurrenceType occType = (OccurrenceType) seqType;
        Type baseType = occType.getBase();
        if (OccurrenceType.itemCountIsOne(baseType)) {
            int min = occType.minOccurs();
            if (min > 0)
                occType =
                    new OccurrenceType(baseType, min, occType.maxOccurs());
            exp.setType(occType);
        }
    }

    Compilation comp = visitor.getCompilation();

    Declaration dotArg = lexp2.firstDecl();
    Declaration posArg = dotArg.nextDecl();
    Declaration lastArg = posArg.nextDecl();
    dotArg.setCanRead(true);
    posArg.setCanRead(true);

    lexp2.setInlineOnly(true);
    lexp2.returnContinuation = exp;
    lexp2.inlineHome = visitor.getCurrentLambda();

    // Splice out lastArg
    lexp2.remove(posArg, lastArg);
    lexp2.min_args = 2;
    lexp2.max_args = 2;

    if (! lastArg.getCanRead() && vproc.kind != 'R')
      {
        // Don't need to do anything more - lastArg is not needed.
        return exp;
      }
    lastArg.setCanRead(true);

    comp.letStart();
    Method sizeMethod;
    if (vproc.kind == 'P')
      {
        sizeMethod = Compilation.typeValues.getDeclaredMethod("countValues", 1);
      }
    else
      {
        seqType = SortNodes.typeSortedNodes;
        seq = new ApplyExp(SortNodes.sortNodes, new Expression [] {seq});
        sizeMethod = ClassType.make("gnu.lists.AbstractSequence")
            .getDeclaredMethod("size", 0);
      }
    Declaration sequence = comp.letVariable("sequence", seqType, seq);
    comp.letEnter();

    Expression pred = lexp2.body;
    Type predType = lexp2.body.getType();
    if (predType != XDataType.booleanType) // Overly conservative, but simple.
      pred = new ApplyExp(vproc.matchesMethod,
                          new Expression[] { pred,
                                             new ReferenceExp(posArg) });
    if (vproc.kind == 'R')
      {
        Declaration posIncoming = new Declaration(null, Type.intType);
        posIncoming.setCanRead(true);
	Expression init
	  = new ApplyExp(AddOp.$Mn,
			 new Expression[] {
			   new ReferenceExp(lastArg),
			   new ReferenceExp(posIncoming)});
	init
	  = new ApplyExp(AddOp.$Pl,
			 new Expression[] {
			   init,
			   new QuoteExp(IntNum.one())});
        
        comp.letStart();
        lexp2.replaceFollowing(dotArg, posIncoming);
        comp.letVariable(posArg, init);
        comp.letEnter();
        pred = comp.letDone(pred);
      }

    pred = new IfExp(pred,
                     new ReferenceExp(dotArg),
                     QuoteExp.voidExp);
    lexp2.body = pred;

    ApplyExp doMap
      = new ApplyExp(ValuesMap.valuesMapWithPos,
		     new Expression[] { lexp2,
					new ReferenceExp(sequence) });
    doMap.setType(OccurrenceType.getInstance(dotArg.getType(), 0, -1));
    lexp2.returnContinuation = doMap;

    Expression lastInit = new ApplyExp(sizeMethod,
                                       new Expression[] {
                                         new ReferenceExp(sequence)});

    comp.letStart();
    comp.letVariable(lastArg, lastInit);
    LetExp let2 = comp.letDone(gnu.kawa.functions.CompileMisc.validateApplyValuesMap(doMap, visitor, required, ValuesMap.valuesMapWithPos));

    return comp.letDone(let2);
  }

  public static Expression validateApplyRelativeStep
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    // FIXME make use of type of E1 to set dot in E2.
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    Expression exp1 = args[0];
    Expression exp2 = args[1];
    LambdaExp lexp2;
    Compilation comp = visitor.getCompilation();
    if (! (exp2 instanceof LambdaExp)
        // The following optimization breaks when interpreting, because
        // then CoerceToNodes may not work.
        || ! comp.mustCompile
	|| (lexp2 = (LambdaExp) exp2).min_args != 3
	|| lexp2.max_args != 3)
      return exp;

    lexp2.setInlineOnly(true);
    lexp2.returnContinuation = exp;
    lexp2.inlineHome = visitor.getCurrentLambda();

    exp2 = lexp2.body;

    Declaration dotArg = lexp2.firstDecl();
    Declaration posArg = dotArg.nextDecl();
    Declaration lastArg = posArg.nextDecl();
    // Splice out the "last" argument - we'll move it out.
    // The remaining two arguments are suitable for a ValuesMap.
    posArg.setNext(lastArg.nextDecl());
    lastArg.setNext(null);
    lexp2.min_args = 2;
    lexp2.max_args = 2;

    Type type1 = exp1.getType();
    if (type1 != null &&NodeType.anyNodeTest.compare(type1) == -3)
      {
        Language language = visitor.getCompilation().getLanguage();
        String message = "step input is "+language.formatType(type1)+" - not a node sequence";
        visitor.getMessages().error('e', message);
        return new ErrorExp(message);
      }
      
    Type rtype = exp.getTypeRaw();
    Type rtypePrime;
    int nodeCompare;
    if (rtype == null || rtype == Type.pointer_type)
      {
        Type type2 = exp2.getType();
        rtypePrime = OccurrenceType.itemPrimeType(type2);
        nodeCompare = NodeType.anyNodeTest.compare(rtypePrime);
        if (nodeCompare >= 0)
          rtype = NodeSetType.getInstance(rtypePrime);
        else
          rtype = OccurrenceType.getInstance(rtypePrime, 0, -1);
        exp.setType(rtype);
      }
    if (lastArg.getCanRead())
      {
        ClassType typeNodes = CoerceNodes.typeNodes;
        comp.letStart();
        Declaration sequence
          = comp.letVariable(null, typeNodes,
                             new ApplyExp(CoerceNodes.coerceNodes,
                                          new Expression [] { exp1 }));
        comp.letEnter();

        Method sizeMethod = typeNodes.getDeclaredMethod("size", 0);
        Expression lastInit
          = new ApplyExp(sizeMethod,
                         new Expression[] {new ReferenceExp(sequence)});
        comp.letStart();
        comp.letVariable(lastArg, lastInit);
        comp.letEnter();
        LetExp lastLet =
          comp.letDone(new ApplyExp(exp.getFunction(),
                                    new Expression[] { new ReferenceExp(sequence),
                                                       lexp2 }));
        return comp.letDone(lastLet);
      }

    ApplyExp result = exp;

    // Try to rewrite A/B[P] to (A/B)[P].
    // This only works if P doesn't depend in position() or last().
    if (exp2 instanceof ApplyExp)
      {
        ApplyExp aexp2 = (ApplyExp) exp2;
        Object proc2 = aexp2.getFunction().valueIfConstant();
        Expression vexp2;
        if (proc2 instanceof ValuesFilter
            && (vexp2 = aexp2.getArgs()[1]) instanceof LambdaExp)
          {
            LambdaExp lvexp2 = (LambdaExp) vexp2;
            Declaration dot2 = lvexp2.firstDecl();
            Declaration pos2;
            if (dot2 != null && (pos2 = dot2.nextDecl()) != null
                && pos2.nextDecl() == null
                && ! pos2.getCanRead()
                // If the predicate can evaluate to a number, then the
                // optimization is unsafe, since we implicitly
                // compare against position().
                && ClassType.make("java.lang.Number").compare(lvexp2.body.getType()) == -3)
              {
                exp2 = aexp2.getArg(0);
                lexp2.body = exp2;
                aexp2.setArg(0, exp);
                result = aexp2;
              }
          }
      }
    // Now we can rewrite 'descendant-or-self::node()/B' (which is the
    // expansion of the abbreviated syntax '//B') to /descendant::B'.
    if (exp1 instanceof ApplyExp && exp2 instanceof ApplyExp)
      {
        ApplyExp aexp1 = (ApplyExp) exp1;
        ApplyExp aexp2 = (ApplyExp) exp2;
        Object p1 = aexp1.getFunction().valueIfConstant();
        Object p2 = aexp2.getFunction().valueIfConstant();
        Expression exp12;
        if (p1 == RelativeStep.relativeStep && p2 instanceof ChildAxis
            && aexp1.getArgCount() == 2
            && (exp12 = aexp1.getArg(1)) instanceof LambdaExp)
          {
            LambdaExp lexp12 = (LambdaExp) exp12;
            if (lexp12.body instanceof ApplyExp
                && ((ApplyExp) lexp12.body).getFunction().valueIfConstant() == DescendantOrSelfAxis.anyNode)
              {
                exp.setArg(0, aexp1.getArg(0));
                aexp2.setFunction(new QuoteExp(DescendantAxis.make(((ChildAxis) p2).getNodePredicate())));
              }
          }
      }
    return result;
  }

  public static Expression validateApplyOrderedMap
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length > 2)
      {
        Expression[] rargs = new Expression[args.length-1];
        System.arraycopy(args, 1, rargs, 0, rargs.length);
        Expression[] xargs = new Expression[2];
        Method makeTupleMethod = typeTuples.getDeclaredMethod("make$V", 2); 
        xargs[0] = args[0];
        xargs[1] = new ApplyExp(makeTupleMethod, rargs);
        return new ApplyExp(proc, xargs);
      }
    return exp;
  }

  static final ClassType typeTuples
    = ClassType.make("gnu.xquery.util.OrderedTuples");

  public static void compileOrderedMap (ApplyExp exp, Compilation comp, Target target, Procedure proc)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 2)
      {
        ApplyExp.compile(exp, comp, target);
        return;
      }
    CodeAttr code = comp.getCode();
    Scope scope = code.pushScope();
    Variable consumer = scope.addVariable(code, typeTuples, null);
    args[1].compile(comp, Target.pushValue(typeTuples));
    code.emitStore(consumer);
    ConsumerTarget ctarget = new ConsumerTarget(consumer);
    args[0].compile(comp, ctarget);
    Method mm = typeTuples.getDeclaredMethod("run$X", 1);
    code.emitLoad(consumer);
    PrimProcedure.compileInvoke(comp, mm, target, exp.isTailCall(),
                                182/*invokevirtual*/, Type.pointer_type);
    code.popScope();
  }

  static final ClassType typeXDataType =
    ClassType.make("gnu.kawa.xml.XDataType");
  static final Method castMethod = typeXDataType.getDeclaredMethod("cast", 1);

  public static Expression validateApplyCastAs
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp = CompileReflect.inlineClassName(exp, 0, visitor);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[0] instanceof QuoteExp))
      return exp;
    Object type = ((QuoteExp) args[0]).getValue();
    if (type instanceof XDataType)
      return new ApplyExp(castMethod, args);
    return exp;
  }

  static final Method castableMethod
    = typeXDataType.getDeclaredMethod("castable", 1);

  public static Expression validateApplyCastableAs
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp = CompileReflect.inlineClassName(exp, 1, visitor);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[1] instanceof QuoteExp))
      return exp;
    Object type = ((QuoteExp) args[1]).getValue();
    if (type instanceof XDataType)
      return new ApplyExp(castableMethod,
                          new Expression[] { args[1], args[0] });
    return exp;
  }
}
