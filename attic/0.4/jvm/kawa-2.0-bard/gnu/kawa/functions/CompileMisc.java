package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.math.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.lispexpr.LangObjType;
import gnu.lists.LList;
import kawa.standard.Scheme;
import kawa.standard.SchemeCompilation;
import static gnu.expr.InlineCalls.LenientExpectedType;

public class CompileMisc
{
  public static Expression validateApplyConstantFunction0
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    int nargs = exp.getArgCount();
    if (nargs != 0 && visitor != null)
      {
        String message = WrongArguments.checkArgCount(proc, nargs, false);
	return visitor.noteError(message);
      }
    return ((ConstantFunction0) proc).constant;
  }

  public static Expression validateApplyConvert
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Compilation comp = visitor.getCompilation();
    Language language = comp.getLanguage();
    Convert cproc = (Convert) proc;
    Expression[] args = exp.getArgs();
    if (args.length == 2)
      {
        args[0] = visitor.visit(args[0], null);
        Type type = language.getTypeFor(args[0]);
        if (type instanceof Type)
          {
            args[0] = new QuoteExp(type);
            Type xtype = cproc.lenient ? LenientExpectedType.make(type)
                : type;
            if (! args[1].getFlag(Expression.VALIDATED))
                args[1] = ExpVisitor.visit(visitor, args[1], xtype);
            args[1] = visitor.checkType(args[1], xtype);
            CompileReflect.checkKnownClass(type, comp);
            exp.setType(type);
            if (args[1].getType() == type)
                return args[1];
            return exp;
          }
      }
    exp.visitArgs(visitor);
    return exp;
  }

  public static Expression validateApplySimpleBoolean
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(visitor.getCompilation().getLanguage().getTypeFor(Boolean.TYPE));
    return exp.inlineIfConstant(proc, visitor);
  }

  /** Validate-apply handling for "format".
   * Sets the correct return-type, and may replace by call to a static method.
   */
  public static Expression validateApplyFormat
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Type retType = Type.objectType;
    Expression[] args = exp.getArgs();
    if (args.length > 0)
      {
        ClassType typeFormat = ClassType.make("gnu.kawa.functions.Format");
        Object f = args[0].valueIfConstant();
        Type ftype = args[0].getType();
        if (f == Boolean.FALSE || ftype.isSubtype(LangObjType.stringType))
          {
            int skip = f == Boolean.FALSE ? 1 : 0;
            Expression[] xargs = new Expression[args.length+1-skip];
            xargs[0] = new QuoteExp(Integer.valueOf(0), Type.intType);
            System.arraycopy(args, skip, xargs, 1, xargs.length-1);
            ApplyExp ae = new ApplyExp(typeFormat.getDeclaredMethod("formatToString", 2), xargs);
            ae.setLine(exp);
            ae.setType(Type.javalangStringType);
            return ae;
          }
        if (f == Boolean.TRUE
            || ftype.isSubtype(ClassType.make("java.io.Writer")))
          {
            if (f == Boolean.TRUE)
              {
                Expression[] xargs = new Expression[args.length];
                xargs[0] = QuoteExp.nullExp;
                System.arraycopy(args, 1, xargs, 1, args.length-1);
                args = xargs;
              }
            ApplyExp ae = new ApplyExp(typeFormat.getDeclaredMethod("formatToWriter", 3), args);
            ae.setLine(exp);
            ae.setType(Type.voidType);
            return ae;
          }
        if (ftype.isSubtype(ClassType.make("java.io.OutputStream")))
          retType = Type.voidType;
      }
    exp.setType(retType);
    return null;
  }

  public static Expression validateApplyAppendValues
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      return args[0];
    if (args.length == 0)
      return QuoteExp.voidExp;
    Expression folded = exp.inlineIfConstant(proc, visitor);
    if (folded != exp)
      return folded;
    return exp;
  }

  public static Expression validateApplyMakeProcedure
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    int alen = args.length;
    Expression method = null;
    int countMethods = 0;
    String name = null;
    for (int i = 0;  i < alen;  i++)
      {
	Expression arg = args[i];
        Keyword key = arg.checkLiteralKeyword();
	if (key != null)
	  {
	    String keyword = key.getName();
	    Expression next = args[++i];
	    if (keyword == "name")
              {
                if (next instanceof QuoteExp)
                  name = ((QuoteExp) next).getValue().toString();
              }
	    else if (keyword == "method")
              {
                countMethods++;
                method = next;
              }
	    else
	      ; // result.setProperty(keyword, value);
	  }
	else
          {
            countMethods++;
            method = arg;
          }
      }
    if (countMethods == 1 && method instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) method;
        for (int i = 0;  i < alen;  i++)
          {
            Expression arg = args[i];
            Keyword key = arg.checkLiteralKeyword();
            if (key != null)
              {
                String keyword = key.getName();
                Expression next = args[++i];
                if (keyword == "name")
                  lexp.setName(name);
                else if (keyword == "method")
                  ;
                else
                  lexp.setProperty(Namespace.EmptyNamespace.getSymbol(keyword), next);
              }
          }
        return method;
      }
    return exp;
  }

  public static Expression validateApplyValuesMap
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    LambdaExp lexp = ValuesMap.canInline(exp, (ValuesMap) proc);
    // FIXME If required is an OccurrenceType, then we want to validate
    // lexp.body with the same OccurrenceType.  This is tricky - best to
    // do a tree rewrite here, instead of in ValuesMap#compile.
    exp.visitArgs(visitor);
    if (lexp != null)
      {
	lexp.setInlineOnly(true);
        lexp.returnContinuation = exp;
        lexp.inlineHome = visitor.getCurrentLambda();
      }
    return exp;
  }

  static gnu.bytecode.ClassType typeType;
  static gnu.bytecode.Method coerceMethod;

    public static boolean compileConvert (ApplyExp exp, Compilation comp,
                                          Target target, Procedure procedure) {
        Convert proc = (Convert) procedure;
        Expression[] args = exp.getArgs();
        if (args.length != 2 || ! exp.isSimple())
            return false;
        CodeAttr code = comp.getCode();
        Type type = Scheme.getTypeValue(args[0]);
        if (type == Type.neverReturnsType) {
            args[1].compile(comp, Target.Ignore);
            PrimProcedure.compileReachedUnexpected(code);
        } else if (type != null) {
            args[1].compile(comp, Target.pushValue(type));
            if (code.reachableHere() && ! type.isVoid()) {
                target.compileFromStack(comp, type);
            }
        } else {
            if (typeType == null) {
                typeType = ClassType.make("gnu.bytecode.Type");
            }
            if (coerceMethod == null) {
                coerceMethod = typeType.addMethod("coerceFromObject",
                                                  Compilation.apply1args,
                                                  Type.pointer_type,
                                                  gnu.bytecode.Access.PUBLIC);
            }
            args[0].compile(comp, LangObjType.typeClassType);
            args[1].compile(comp, Target.pushObject);
            code.emitInvokeVirtual(coerceMethod);
            target.compileFromStack(comp, Type.pointer_type);
        }
        return true;
    }

    public static boolean compileNot(ApplyExp exp, Compilation comp,
                                     Target target, Procedure procedure) {
        if (! exp.isSimple())
            return false;
        Not proc = (Not) procedure;
        Expression arg = exp.getArgs()[0];
        Language language = proc.language;
        if (target instanceof ConditionalTarget) {
            ConditionalTarget ctarget = (ConditionalTarget) target;
            ConditionalTarget sub_target
                = new ConditionalTarget(ctarget.ifFalse, ctarget.ifTrue,
                                        language);
            sub_target.trueBranchComesFirst = ! ctarget.trueBranchComesFirst;
            arg.compile(comp, sub_target);
            return true;
        }
        CodeAttr code = comp.getCode();
        Type type = target.getType();
        if (target instanceof StackTarget
                && type.getSignature().charAt(0) == 'Z') {
            arg.compile(comp, target);
            code.emitNot(target.getType());
        } else {
            QuoteExp trueExp
                = QuoteExp.getInstance(language.booleanObject(true));
            QuoteExp falseExp
                = QuoteExp.getInstance(language.booleanObject(false));
            IfExp.compile(arg, falseExp, trueExp, comp, target);
        }
        return true;
    }

    public static boolean compileEq(ApplyExp exp, Compilation comp,
                                    Target target, Procedure proc) {
        if (! exp.isSimple())
            return false;
        Expression[] args = exp.getArgs();
        Language language = ((IsEq) proc).language;
        CodeAttr code = comp.getCode();
        Expression arg0 = args[0];
        Expression arg1 = args[1];
        if (arg0==QuoteExp.nullExp) {
            Expression tmp = arg1; arg1 = arg0; arg0 = tmp;
        }
        arg0.compile(comp, Target.pushObject);
        boolean isNull = arg1==QuoteExp.nullExp;
        if (! isNull)
            arg1.compile(comp, Target.pushObject);

        if (target instanceof ConditionalTarget) {
            ConditionalTarget ctarget = (ConditionalTarget) target;
            if (ctarget.trueBranchComesFirst) {
                if (isNull)
                    code.emitGotoIfNonNull(ctarget.ifFalse);
                else
                    code.emitGotoIfNE(ctarget.ifFalse);
            } else {
                if (isNull)
                    code.emitGotoIfNull(ctarget.ifTrue);
                else
                    code.emitGotoIfEq(ctarget.ifTrue);
            }
            ctarget.emitGotoFirstBranch(code);
        }
    else
      {
	Type type;
        if (isNull)
            code.emitIfNull();
        else {
            code.emitIfEq();
        }
	if (target.getType() instanceof ClassType)
	  {
	    Object trueValue = language.booleanObject(true);
	    Object falseValue = language.booleanObject(false);
	    comp.compileConstant(trueValue, Target.pushObject);
	    code.emitElse();
	    comp.compileConstant(falseValue, Target.pushObject);
	    if (trueValue instanceof Boolean && falseValue instanceof Boolean)
	      type = Compilation.scmBooleanType;
	    else
	      type = Type.pointer_type;
	  }
	else
	  {
	    code.emitPushInt(1);
	    code.emitElse();
	    code.emitPushInt(0);
	    type = language.getTypeFor(Boolean.TYPE);
	  }
	code.emitFi();
	target.compileFromStack(comp, type);
      }
      return true;
  }

    public static boolean compileNumberCompare(ApplyExp exp, Compilation comp, Target target, Procedure procedure) {
        NumberCompare proc = (NumberCompare) procedure;
        if (! exp.isSimple())
            return false;
        CodeAttr code = comp.getCode();
        Expression[] args = exp.getArgs();
        if (args.length == 2) {
            Expression arg0 = args[0];
            Expression arg1 = args[1];
            int kind0 = classifyForNumCompare(arg0);
            int kind1 = classifyForNumCompare(arg1);
            if (kind0 > 0 && kind1 > 0
                && kind0 <= Arithmetic.REALNUM_CODE && kind1 <= Arithmetic.REALNUM_CODE
                // Don't optimize if both operands are fractions. FIXME???
                && (kind0 != Arithmetic.RATNUM_CODE || kind1 != Arithmetic.RATNUM_CODE)) {
                if (! (target instanceof ConditionalTarget)) {
                    IfExp.compile(exp, QuoteExp.trueExp, QuoteExp.falseExp,
                                  comp, target);
                    return true;
                }
                int mask = proc.flags;
                if (mask == NumberCompare.TRUE_IF_NEQ)
                    mask = NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_LSS;
                if (kind0 <= Arithmetic.INTNUM_CODE
                    && kind1 <= Arithmetic.INTNUM_CODE
                    && (kind0 > Arithmetic.LONG_CODE
                        || kind1 > Arithmetic.LONG_CODE)) {
                    Type[] ctypes = new Type[2];
                    ctypes[0] = Arithmetic.typeIntNum;
                    if (kind1 <= Arithmetic.LONG_CODE) {
                        ctypes[1] = Type.longType;
                    } else if (kind0 <= Arithmetic.LONG_CODE
                               // Simple check to avoid re-ordering side-effects.
                               && (arg0 instanceof QuoteExp
                                   || arg1 instanceof QuoteExp
                                   || arg0 instanceof ReferenceExp
                                   || arg1 instanceof ReferenceExp)) {
                        ctypes[1] = Type.longType;
                        args = new Expression[2];
                        args[0] = arg1;
                        args[1] = arg0;
                        if (mask != NumberCompare.TRUE_IF_EQU && mask != NumberCompare.TRUE_IF_GRT+NumberCompare.TRUE_IF_LSS)
                            mask ^= NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_LSS;
                    } else
                        ctypes[1] = Arithmetic.typeIntNum;
                    Method cmeth
                        = Arithmetic.typeIntNum.getMethod("compare", ctypes);
                    PrimProcedure compare = new PrimProcedure(cmeth);
                    arg0 = new ApplyExp(compare, args);
                    arg1 = new QuoteExp(IntNum.zero());
                    kind0 = kind1 = Arithmetic.INT_CODE;
                }
                Type commonType;
                if (kind0 <= Arithmetic.INT_CODE && kind1 <= Arithmetic.INT_CODE)
                    commonType = Type.intType;
                else if (kind0 <= Arithmetic.LONG_CODE && kind1 <= Arithmetic.LONG_CODE)
                    commonType = Type.longType;
                else
                    commonType = Type.doubleType;
                StackTarget subTarget = new StackTarget(commonType);
                ConditionalTarget ctarget = (ConditionalTarget) target;
	    
                int opcode;
                if (arg0 instanceof QuoteExp && ! (arg1 instanceof QuoteExp)) {
                    Expression tmp = arg1; arg1 = arg0; arg0 = tmp;
                    if (mask != NumberCompare.TRUE_IF_EQU && mask !=NumberCompare. TRUE_IF_GRT+NumberCompare.TRUE_IF_LSS)
                        mask ^= NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_LSS;
                }
                Label label1 = ctarget.trueBranchComesFirst ? ctarget.ifFalse : ctarget.ifTrue;
                if (ctarget.trueBranchComesFirst)
                    mask ^= NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU;
                switch (mask) {
                case NumberCompare.TRUE_IF_GRT:
                    opcode = 157 /*ifgt*/;  break;
                case NumberCompare.TRUE_IF_EQU:
                    opcode = 153 /*ifeq*/;  break;
                case NumberCompare.TRUE_IF_LSS:
                    opcode = 155 /*iflt*/;  break;
                case NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_LSS:
                    opcode = 154 /*ifne*/;  break;
                case NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU:
                    opcode = 156 /*ifge*/;  break;
                case NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU:
                    opcode = 158 /*ifle*/;  break;
                default:
                    opcode = 0;
                }
                arg0.compile(comp, subTarget);
                Object value;
                if (kind0 <= Arithmetic.INT_CODE && kind1 <= Arithmetic.INT_CODE
                    && arg1 instanceof QuoteExp
                    && (value = ((QuoteExp) arg1).getValue()) instanceof IntNum
                    && ((IntNum) value).isZero()) {
                    code.emitGotoIfCompare1(label1, opcode);
                } else {
                    arg1.compile(comp, subTarget);
                    code.emitGotoIfCompare2(label1, opcode);
                }
                ctarget.emitGotoFirstBranch(code);
                return true;
            }
        }

        String mname;
        switch (proc.flags) {
        case NumberCompare.TRUE_IF_GRT:
            mname = "$Gr";  break;
        case NumberCompare.TRUE_IF_EQU:
            mname = "$Eq";  break;
        case NumberCompare.TRUE_IF_LSS:
            mname = "$Ls";  break;
        case NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU:
            mname = "$Gr$Eq";  break;
        case NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU:
            mname = "$Ls$Eq";  break;
        default:
            mname = null;
        }
        if (mname != null) {
            ClassType compclass =
                ClassType.make("gnu.kawa.functions.NumberCompare");
            Method meth = args.length == 2
                ? compclass.getDeclaredMethod(mname, 2)
                : compclass.getDeclaredMethod(mname+"$V", 4);
            new ApplyExp(meth, args).setLine(exp)
                .compile(comp, target);
            return true;
        }
        return false;
    }

  static int classifyForNumCompare (Expression exp)
  {
    Type type = exp.getType();
    int kind = Arithmetic.classifyType(type);
    Object value;
    if (kind == Arithmetic.INTNUM_CODE && exp instanceof QuoteExp
	&& (value = ((QuoteExp) exp).getValue()) instanceof IntNum)
      {
	int ilength = ((IntNum) value).intLength();
	if (ilength < 32)
	  return Arithmetic.INT_CODE;
	if (ilength < 64)
	  return Arithmetic.LONG_CODE;
      }
    return kind;
  }

    public static boolean compileNumPredicate(ApplyExp exp, Compilation comp,
                                              Target target,
                                              Procedure procedure) {
        NumberPredicate proc = (NumberPredicate) procedure;
        if (! exp.isSimple())
            return false;
        Expression[] args = exp.getArgs();
        int op = proc.op;
        if (args.length == 1
            && (op == NumberPredicate.ODD || op == NumberPredicate.EVEN)) {
            Expression arg0 = args[0];
            int kind = Arithmetic.classifyType(arg0.getType());
            if (kind <= Arithmetic.INTNUM_CODE) {
                PrimType wtype = Type.intType;
                Target wtarget = StackTarget.getInstance(wtype);
                CodeAttr code = comp.getCode();
                if (op == NumberPredicate.EVEN)
                    code.emitPushInt(1);
                arg0.compile(comp, wtarget);
                code.emitPushInt(1);
                code.emitAnd();
                if (op == NumberPredicate.EVEN)
                    code.emitSub(Type.intType);
                target.compileFromStack(comp, Type.booleanType);
                return true;
            }
        }
        return false;
    }

  public static Expression validateApplyCallCC
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    LambdaExp lexp = canInlineCallCC(exp);
    if (lexp != null)
      {
	lexp.setInlineOnly(true);
	lexp.returnContinuation = exp;
        lexp.inlineHome = visitor.getCurrentLambda();
        Declaration contDecl = lexp.firstDecl();
        if (! contDecl.getFlag(Declaration.TYPE_SPECIFIED))
          contDecl.setType(typeContinuation);
        LambdaExp.maybeSetReturnType(lexp, required);
        // FIXME (future): (after visiting lexp), do:
        // exp.setType(lexp.body.getType() UNION continuation-arguments);
      }
    exp.visitArgs(visitor);
    return exp;
  }

  public static final ClassType typeContinuation =
    ClassType.make("kawa.lang.Continuation");

  public static void compileCallCC (ApplyExp exp, Compilation comp, Target target, Procedure proc)
  {
    LambdaExp lambda = canInlineCallCC(exp);
    if (lambda == null)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    CodeAttr code = comp.getCode();
    final Declaration param = lambda.firstDecl();
    if (param.isSimple() && ! param.getCanRead() && ! param.getCanWrite())
      {
        param.setCanCall(false);
        CompileTimeContinuation contProxy = new CompileTimeContinuation();
        Type rtype = target instanceof StackTarget ? target.getType() : null;
        boolean runFinallyBlocks
          = ExitThroughFinallyChecker.check(param, lambda.body);
        ExitableBlock bl = code.startExitableBlock(rtype, runFinallyBlocks);
        contProxy.exitableBlock = bl;
        contProxy.blockTarget = target;
        param.setValue(new QuoteExp(contProxy));
        (new ApplyExp(lambda, QuoteExp.nullExp)).compile(comp, target);
        code.endExitableBlock();
        return;
      }

    Scope sc = code.pushScope();
    Variable contVar = sc.addVariable(code, typeContinuation, null);
    Declaration contDecl = new Declaration(contVar);
    code.emitNew(typeContinuation);
    code.emitDup(typeContinuation);
    comp.loadCallContext();
    code.emitInvokeSpecial(typeContinuation.getDeclaredMethod("<init>", 1));
    code.emitStore(contVar);
    code.emitTryStart(false, target instanceof IgnoreTarget || target instanceof ConsumerTarget ? null : target.getType().getRawType());
    ApplyExp app = new ApplyExp(lambda, new ReferenceExp(contDecl) );
    app.compile(comp, target);
    // Emit: cont.invoked = true
    if (code.reachableHere())
      {
        code.emitLoad(contVar);
        code.emitPushInt(1);
        code.emitPutField(typeContinuation.getField("invoked"));
      }
    code.emitTryEnd();

    // Emit: catch (Throwable ex) { handleException$(ex, cont, ctx); }
    code.emitCatchStart(null);
    code.emitLoad(contVar);
    if (target instanceof ConsumerTarget)
      {
        comp.loadCallContext();
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException$X", 3);
        code.emitInvokeStatic(handleMethod);
      }
    else
      {
        Method handleMethod = typeContinuation.getDeclaredMethod("handleException", 2);
        code.emitInvokeStatic(handleMethod);
        target.compileFromStack(comp, Type.objectType);
      }
    code.emitCatchEnd();

    code.emitTryCatchEnd();
    code.popScope();
  }

  /** If we can inline, return LambdaExp for first arg; otherwise null. */
  private static LambdaExp canInlineCallCC (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    Expression arg0;
    if (args.length == 1 && (arg0 = args[0]) instanceof LambdaExp)
      {
        LambdaExp lexp = (LambdaExp) arg0;
        if (lexp.min_args == 1 && lexp.max_args == 1
            && ! lexp.firstDecl().getCanWrite())
          {
            return lexp;
          }
      }
    return null;
  }

    public static Expression validateApplyWithExceptionHandler
        (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc) {
        Expression[] args = exp.getArgs();
        Expression handler = visitor.visit(args[0], null);
        args[0] = handler;
        Expression thunk = args[1];
        if (thunk instanceof LambdaExp) {
            LambdaExp lthunk = (LambdaExp) thunk;
            if (lthunk.min_args == 0 && lthunk.max_args == 0) {
                LambdaExp.maybeSetReturnType(lthunk, required);
                thunk = visitor.visit(lthunk, required);
                args[1] = thunk;

                /* Rewrite to:
                     (let ((link (HandlerLink:push handler)))
                      (try-catch
                              (let ((v (thunk)))
                               (link:pop)
                               v)
                       (ex java.lang.Throwable
                        (primitive-throw (link:handle ex)))))
                * This allows the thunk to be inlined.
                */
                Compilation comp = visitor.getCompilation();
                comp.letStart();
                ClassType handlerLinkType =
                    ClassType.make("kawa.lib.HandlerLink");
                Method pushMethod =
                    handlerLinkType.getDeclaredMethod("push", 1);
                Method popMethod =
                    handlerLinkType.getDeclaredMethod("pop", 0);
                Declaration linkDecl =
                    comp.letVariable(null, handlerLinkType,
                                     new ApplyExp(pushMethod, handler));
                comp.letEnter();
                Expression tryClause;
                Type bodyType = lthunk.getReturnType();
                Expression bodyCall = new ApplyExp(thunk);
                Expression popHandler =
                    new ApplyExp(popMethod, new ReferenceExp(linkDecl));
                if (bodyType.isVoid()) {
                    tryClause = new BeginExp(bodyCall, popHandler);
                } else {
                    comp.letStart();
                    Declaration resultDecl =
                        comp.letVariable(null, bodyType, bodyCall);
                    comp.letEnter();
                    tryClause =
                        comp.letDone
                        (new BeginExp(popHandler,
                                      new ReferenceExp(resultDecl)));
                }
                TryExp texp = new TryExp(tryClause, null);
                Declaration exDecl =
                    new Declaration(null, Type.javalangThrowableType);

                Expression doHandle =
                    new ApplyExp(handlerLinkType
                                 .getDeclaredMethod("handle", 1),
                                 new ReferenceExp(linkDecl),
                                 new ReferenceExp(exDecl));
                texp.addCatchClause(exDecl,
                                    new ApplyExp(Throw.primitiveThrow,
                                                 doHandle));
                return visitor.visit(comp.letDone(texp), required);
            }
        }
        thunk = visitor.visit(thunk, null);
        args[1] = thunk;
        return exp;
    }


  /** An ExpVisitor class to check if callcc exits through a try-finally. */
  static class ExitThroughFinallyChecker extends ExpVisitor<Expression,TryExp>
  {
    Declaration decl;

    /** Does decl appear in body nested inside a try-finally? */
    public static boolean check (Declaration decl, Expression body)
    {
      ExitThroughFinallyChecker visitor = new ExitThroughFinallyChecker();
      visitor.decl = decl;
      visitor.visit(body, null);
      return visitor.exitValue != null;
    }

    protected Expression defaultValue(Expression r, TryExp d)
    {
      return r;
    }

    protected Expression visitReferenceExp (ReferenceExp exp, TryExp currentTry)
    {
      if (decl == exp.getBinding() && currentTry != null)
        exitValue = Boolean.TRUE;
      return exp;
    }

    protected Expression visitTryExp (TryExp exp, TryExp currentTry)
    {
      visitExpression(exp, exp.getFinallyClause() != null ? exp : currentTry);
      return exp;
    }
  }

  public static Expression validateApplyMap
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure xproc)
  {
    Map mproc = (Map) xproc;
    boolean collect = mproc.collect;
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    if (nargs < 2)
      return exp;  // ERROR
    
    Type r = InlineCalls.typeForCalledFunction(args[0]);
    if (r != null)
      {
        for (int i = 1; i < nargs;  i++)
          visitor.visit(args[i], null);
        // FIXME: We should inline the list arguments first before inlining the
        // procedure argument, for better type inference etc.
        visitor.visit(args[0], r);
     }
    else
      exp.visitArgs(visitor);

    nargs--;

    Expression proc = args[0];
    // If evaluating proc doesn't have side-effects, then we want to do
    // so inside loop, since that turns a "read" info a "call", which
    // may allow better inlining.
    boolean procSafeForMultipleEvaluation = ! proc.side_effects();
    Compilation comp = visitor.getCompilation();

    // First an outer (let ((%proc PROC)) L2), where PROC is args[0].
    if (! procSafeForMultipleEvaluation)
      {
        comp.letStart();
        Declaration procDecl = comp.letVariable("%proc",
                                                Compilation.typeProcedure,
                                                proc);
        proc = new ReferenceExp(procDecl);
      }

    // Then an inner L2=(let ((%loop (lambda (argi ...) ...))) (%loop ...))
    comp.letStart();
    LambdaExp lexp = new LambdaExp(collect ? nargs + 1 : nargs);
    Declaration loopDecl = comp.letVariable("%loop", null, lexp);
    comp.letEnter();

    // Finally an inner L3=(let ((parg1 (as <pair> arg1)) ...) ...)
    comp.letStart();
    Declaration[] largs = new Declaration[nargs];
    Declaration[] pargs = new Declaration[nargs];
    for (int i = 0;  i < nargs;  i++)
      {
	String argName = "arg"+i;
	largs[i] = lexp.addDeclaration(argName);
	pargs[i] = comp.letVariable(argName, Compilation.typePair,
                                    new ReferenceExp(largs[i]));
      }
    Declaration resultDecl = collect ? lexp.addDeclaration("result") : null;
    Expression[] doArgs = new Expression[1+nargs];
    Expression[] recArgs = new Expression[collect ? nargs + 1 : nargs];
    for (int i = 0;  i < nargs;  i++)
      {
        doArgs[i+1] = SlotGet.makeGetField(new ReferenceExp(pargs[i]), "car");
	recArgs[i] = SlotGet.makeGetField(new ReferenceExp(pargs[i]), "cdr");
      }
    doArgs[0] = proc;
    Expression applyFunc = new ReferenceExp(SchemeCompilation.applyFieldDecl);
    Expression doit = new ApplyExp(applyFunc, doArgs);
    if (collect)
      {
	Expression[] consArgs = new Expression[2];
	consArgs[0] = doit;
	consArgs[1] = new ReferenceExp(resultDecl);
	recArgs[nargs] = Invoke.makeInvokeStatic(Compilation.typePair,
						 "make", consArgs);
      }
    Expression rec = new ApplyExp(new ReferenceExp(loopDecl), recArgs);
    lexp.body = comp.letDone(collect ? rec : new BeginExp(doit, rec));
    Expression[] initArgs = new Expression[collect ? nargs + 1 : nargs];
    QuoteExp empty = new QuoteExp(LList.Empty);
    for (int i = nargs;  --i >= 0; )
      {
	Expression[] compArgs = new Expression[2];
	compArgs[0] = new ReferenceExp(largs[i]);
	compArgs[1] = empty;
	Expression result
	  = collect ? (Expression) new ReferenceExp(resultDecl)
	  : (Expression) QuoteExp.voidExp;
	lexp.body = new IfExp(new ApplyExp(mproc.isEq, compArgs),
			      result, lexp.body);
	initArgs[i] = args[i+1];
      }
    if (collect)
      initArgs[nargs] = empty;

    Expression body = new ApplyExp(new ReferenceExp(loopDecl), initArgs);
    if (collect)
      {
	Expression[] reverseArgs = new Expression[1];
	reverseArgs[0] = body;
	body = Invoke.makeInvokeStatic(Compilation.scmListType,
				       "reverseInPlace", reverseArgs);
      }
    LetExp ret = comp.letDone(body);
    if (! procSafeForMultipleEvaluation)
      ret = comp.letDone(ret);
    return visitor.visit(ret, null);
  }

  public static Expression validateApplyMakePromise
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1 && args[0] instanceof LambdaExp)
      {
        boolean forceValueIfPromise = proc == MakePromise.makeLazy;
        Type bodyRequired;
        if (required instanceof LazyType)
          {
            Type valueType = ((LazyType) required).getValueType();
            bodyRequired = forceValueIfPromise ? LazyType.getLazyType(valueType)
                : valueType;
          }
        else
          bodyRequired = null;
        LambdaExp lexp = (LambdaExp) args[0];
        lexp.body = visitor.visit(lexp.body, bodyRequired);
        args[0] = visitor.visit(lexp, null);
        Type rtype = lexp.getReturnType();
        if (forceValueIfPromise)
          {
            rtype = rtype instanceof LazyType
                ? ((LazyType) rtype).getValueType()
                : Type.objectType;
          }
        Type type = LazyType.getLazyType(rtype);
        String mname = forceValueIfPromise ? "makePromiseLazy" : "makePromise";
        Method meth = ClassType.make("gnu.kawa.functions.MakePromise")
            .getDeclaredMethod(mname, 1);
	PrimProcedure mproc
	  = new PrimProcedure(meth);
        mproc.setReturnType(type);
        exp = new ApplyExp(mproc, args);
        exp.setType(type);
      }
    else
      {
        exp.visitArgs(visitor);
      }
    return exp;
  }

    /** A hack to simplify inlining continuation calls.
     */
    static class CompileTimeContinuation extends ProcedureN implements Inlineable {
        Target blockTarget;
        ExitableBlock exitableBlock;

        public Object applyN (Object[] args) throws Throwable {
            throw new Error("internal error");
        }

        public void compile (ApplyExp exp, Compilation comp, Target target) {
            CodeAttr code = comp.getCode();
            Expression[] args = exp.getArgs();
            int nargs = args.length;
            boolean noStack = (blockTarget instanceof IgnoreTarget
                               || blockTarget instanceof ConsumerTarget);
            Type typeNeeded = noStack ? null : target.getType();
            if (noStack || nargs == 1) {
                for (int i = 0;  i < nargs;  i++)
                    args[i].compileWithPosition(comp, blockTarget);
            } else {
                AppendValues app = AppendValues.appendValues;
                app.compile(new ApplyExp(app, args), comp, blockTarget);
            }
            exitableBlock.exit();
        }

        public gnu.bytecode.Type getReturnType (Expression[] args) {
            return Type.neverReturnsType;
        }
    }
}
