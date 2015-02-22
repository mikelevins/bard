package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.*;
import static gnu.kawa.functions.ArithOp.*;

public class CompileArith implements Inlineable
{
  int op;
  Procedure proc;

  public static CompileArith $Pl = new CompileArith(AddOp.$Pl, ADD);
  public static CompileArith $Mn = new CompileArith(AddOp.$Mn, SUB);

  CompileArith(Object proc, int op)
  {
    this.proc = (Procedure) proc;
    this.op = op;
  }

  public static CompileArith forMul(Object proc)
  {
    return new CompileArith(proc, MUL);
  }

  public static CompileArith forDiv(Object proc)
  {
    return new CompileArith(proc, ((DivideOp) proc).op);
  }

  public static CompileArith forBitwise(Object proc)
  {
    return new CompileArith(proc, ((BitwiseOp) proc).op);
  }

  public static boolean appropriateIntConstant(Expression[] args, int iarg, InlineCalls visitor)
  {
    Expression exp = visitor.fixIntValue(args[iarg]);
    if (exp != null)
      {
        args[iarg] = exp;
        return true;
      }
    return false;
  }

  public static boolean appropriateLongConstant(Expression[] args, int iarg, InlineCalls visitor)
  {
    Expression exp = visitor.fixLongValue(args[iarg]);
    if (exp != null)
      {
        args[iarg] = exp;
        return true;
      }
    return false;
  }

  public static Type combineType (Type t1, Type t2)
  {
    int kind1 = Arithmetic.classifyType(t1);
    int kind2 = Arithmetic.classifyType(t2);
    return Arithmetic.kindType(CompileArith.getReturnKind2(kind1, kind2));
  }

  public static Expression validateApplyArithOp
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    ArithOp aproc = (ArithOp) proc;
    int op = aproc.op;
    exp.visitArgs(visitor);

    Expression[] args = exp.getArgs();
    if (args.length > 2)
      return pairwise(proc, exp.getFunction(), args, visitor);

    int rkind = 0;
    if (args.length == 2 || args.length == 1)
      {
        int kind1 = Arithmetic.classifyType(args[0].getType());
        if (args.length == 2
            // Ignore shift count when figuring return type of shifts.
            && (op < ASHIFT_GENERAL || op > LSHIFT_RIGHT))
          {
            int kind2 = Arithmetic.classifyType(args[1].getType());
            rkind = getReturnKind(kind1, kind2, op);
            if (rkind == Arithmetic.INTNUM_CODE)
              {
                if (kind1 == Arithmetic.INT_CODE && appropriateIntConstant(args, 1, visitor))
                  rkind = Arithmetic.INT_CODE;
                else if (kind2 == Arithmetic.INT_CODE && appropriateIntConstant(args, 0, visitor))
                  rkind = Arithmetic.INT_CODE;
                else if (kind1 == Arithmetic. LONG_CODE && appropriateLongConstant(args, 1, visitor))
                  rkind = Arithmetic.LONG_CODE;
                else if (kind2 == Arithmetic.LONG_CODE && appropriateLongConstant(args, 0, visitor))
                  rkind = Arithmetic.LONG_CODE;
              }
          }
        else
          {
            rkind = kind1;
          }
        rkind = adjustReturnKind(rkind, op);
        exp.setType(Arithmetic.kindType(rkind));
      }

    Expression folded = exp.inlineIfConstant(proc, visitor);
    if (folded != exp)
      return folded;

    // Inlining may yield PrimProcedure instructions of bytecode instructions
    // which we don't know how to interpret (yet).
    if (! visitor.getCompilation().mustCompile)
      return exp;

    switch (op)
      {
      case ADD:
      case SUB:
        return validateApplyAdd((AddOp) proc, exp, visitor);
      case DIVIDE_GENERIC:
      case DIVIDE_INEXACT:
      case QUOTIENT:
      case QUOTIENT_EXACT:
      case MODULO:
        return validateApplyDiv((DivideOp) proc, exp, visitor);
      case NOT:
        if (rkind > 0)
          return validateApplyNot(exp, rkind, visitor);
        // else fall through ...
      default:
        return exp;
      }
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int len = args.length;
    if (len == 0)
      {
	comp.compileConstant(((ArithOp) proc).defaultResult(), target);
	return;
      }
    if (len == 1 || target instanceof IgnoreTarget)
      {
	// FIXME implement optimization for unary
	ApplyExp.compile(exp, comp, target);
	return;
      }
    // We know len >= 2 from above.
    // We expect len == 2, assuming inline has been run.
    int kind1 = Arithmetic.classifyType(args[0].getType());
    int kind2 = Arithmetic.classifyType(args[1].getType());
    int kind = getReturnKind(kind1, kind2, op);
    Type type = Arithmetic.kindType(kind);
    if (kind == 0 || len != 2 /* just in case */)
      {
	ApplyExp.compile(exp, comp, target);
	return;
      }
    Type targetType = target.getType();
    int tkind = Arithmetic.classifyType(targetType);
    Type wtype;
    if ((tkind == Arithmetic.INT_CODE || tkind == Arithmetic.LONG_CODE)
        && kind >= Arithmetic.INT_CODE && kind <= Arithmetic.INTNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.INT_CODE ? LangPrimType.intType
          : LangPrimType.longType;
      }
    else if ((tkind == Arithmetic.DOUBLE_CODE
              || tkind == Arithmetic.FLOAT_CODE)
             && kind > Arithmetic.LONG_CODE && kind <= Arithmetic.REALNUM_CODE)
      {
        kind = tkind;
        wtype = tkind == Arithmetic.FLOAT_CODE ? LangPrimType.floatType
          : LangPrimType.doubleType;

      }
    else if (kind == Arithmetic.FLOAT_CODE)
      wtype = LangPrimType.floatType;
    else if (kind == Arithmetic.DOUBLE_CODE || kind == Arithmetic.FLONUM_CODE)
      {
        kind = Arithmetic.DOUBLE_CODE;
        wtype = LangPrimType.doubleType;
      }
    else
      wtype = type;

    if (op >= DIVIDE_GENERIC && op <= MODULO)
      {
        DivideOp dproc = (DivideOp) proc;
        if (dproc.op == DivideOp.DIVIDE_GENERIC
            && (kind <= Arithmetic.INTNUM_CODE
                || (kind >= Arithmetic.RATNUM_CODE || kind <= Arithmetic.FLONUM_CODE)))
          ;
        else if ((dproc.op == DivideOp.DIVIDE_INEXACT
                  && kind <= Arithmetic.REALNUM_CODE && kind != Arithmetic.FLOAT_CODE)
                 || (dproc.op == DivideOp.DIVIDE_GENERIC && kind == Arithmetic.REALNUM_CODE))
          kind = Arithmetic.DOUBLE_CODE;
        else if ((dproc.op == DivideOp.QUOTIENT_EXACT
                  || (dproc.op == DivideOp.QUOTIENT
                      && kind <= Arithmetic.INTNUM_CODE))
                 && (dproc.getRoundingMode() == Numeric.TRUNCATE
                     || kind == Arithmetic.INTNUM_CODE
                     || kind == Arithmetic.FLOAT_CODE
                     || kind == Arithmetic.DOUBLE_CODE)) 
          // FIXME Should optimize primitive non-TRUNCATE quotient.
          ;
        else if (dproc.op == DivideOp.MODULO
                 && (dproc.getRoundingMode() == Numeric.TRUNCATE
                     || kind == Arithmetic.INTNUM_CODE))
          // FIXME Should optimize primitive non-TRUNCATE modulo.
          ;
        else
          {
            ApplyExp.compile(exp, comp, target);
            return;
          }
      }
    if (op == DIVIDE_GENERIC
        && kind <= Arithmetic.REALNUM_CODE 
        && kind != Arithmetic.DOUBLE_CODE && kind != Arithmetic.FLOAT_CODE)
      {
        Method meth;
        if (kind == Arithmetic.RATNUM_CODE
            || kind > Arithmetic.INTNUM_CODE)
          {
            
            LangObjType ctype = kind == Arithmetic.RATNUM_CODE
              ? Arithmetic.typeRatNum
              : Arithmetic.typeRealNum;
            wtype = ctype;
            meth = ctype.getDeclaredMethod("divide", 2);
          }
        else // if (kind <= Arithmetic.INTNUM_CODE)
          {
            wtype = Arithmetic.typeIntNum;
            meth = Arithmetic.typeRatNum.getDeclaredMethod("make", 2);
          }
        Target wtarget = StackTarget.getInstance(wtype);
        args[0].compile(comp, wtarget);
        args[1].compile(comp, wtarget);
        comp.getCode().emitInvokeStatic(meth);
      }
    else if (kind == Arithmetic.INTNUM_CODE
             && (op == ADD || op == MUL || op == SUB
                 || op == AND || op == IOR || op == XOR
                 || op == QUOTIENT_EXACT || op == MODULO
                 || (op >= ASHIFT_GENERAL && op <= ASHIFT_RIGHT)))
      {
        compileIntNum(args[0], args[1], kind1, kind2, comp);
      }
    else if (kind == Arithmetic.INT_CODE
             || kind == Arithmetic.LONG_CODE
             || ((kind == Arithmetic.FLOAT_CODE
                  || kind == Arithmetic.DOUBLE_CODE)
                 && (op <= MODULO || op >= AND)))
      {
        Target wtarget = StackTarget.getInstance(wtype);

        CodeAttr code = comp.getCode();
        for (int i = 0;  i < len;  i++)
          {
            if (i == 1 && op >= ASHIFT_GENERAL && op <= LSHIFT_RIGHT)
              wtarget = StackTarget.getInstance(Type.intType);
            args[i].compile(comp, wtarget);
            if (i == 0)
              continue;
            switch (kind)
              {
              case Arithmetic.INT_CODE:
              case Arithmetic.LONG_CODE:
              case Arithmetic.FLOAT_CODE:
              case Arithmetic.DOUBLE_CODE:
                if (op == ASHIFT_GENERAL)
                  {
                    Type[] margs = { wtype, Type.intType };
                    Method method = ClassType.make("gnu.math.IntNum").getDeclaredMethod("shift", margs);
                    code.emitInvokeStatic(method);
                  }
                else
                  code.emitBinop(primitiveOpcode(), (PrimType) wtype.getImplementationType());
                break;
              }
          }
      }
    else
      {
        ApplyExp.compile(exp, comp, target);
        return;
      }
    target.compileFromStack(comp, wtype);
  }

  public boolean compileIntNum (Expression arg1, Expression arg2, int kind1, int kind2, Compilation comp)
  {
    // Check if we can replace ARG1-CONSTANT by ARG1+(-CONSTANT),
    // where (-CONSTANT) is an int, so we can use IntNum.add(IntNum,int).
    if (op == SUB && arg2 instanceof QuoteExp)
      {
        Object val = arg2.valueIfConstant();
        long lval;
        boolean negateOk;
        if (kind2 <= Arithmetic.LONG_CODE)
          {
            lval = ((Number) val).longValue();
            negateOk = lval > Integer.MIN_VALUE && lval <= Integer.MAX_VALUE;
          }
        else if (val instanceof IntNum)
          {
            IntNum ival = (IntNum) val;
            lval =  ival.longValue();
            negateOk = ival.inRange(Integer.MIN_VALUE+1, Integer.MAX_VALUE);
          }
        else
          {
            negateOk = false;
            lval = 0;
          }
        if (negateOk)
          return $Pl.compileIntNum(arg1,
                                   QuoteExp.getInstance(Integer.valueOf((int) - lval)),
                                   kind1, Arithmetic.INT_CODE, comp);
      }
    boolean swap;
    boolean addOrMul = op == ADD || op == MUL;
    Type type1, type2;
    Method meth;
    if (addOrMul)
      {
        if (InlineCalls.checkIntValue(arg1) != null)
          kind1 = Arithmetic.INT_CODE;
        if (InlineCalls.checkIntValue(arg2) != null)
          kind2 = Arithmetic.INT_CODE;
        swap = kind1 == Arithmetic.INT_CODE && kind2 != Arithmetic.INT_CODE;
        if (swap && ! (arg1.side_effects() && arg2.side_effects()))
          return compileIntNum(arg2, arg1, kind2, kind1, comp);
        type1 = kind1 == Arithmetic.INT_CODE ? Type.intType :  Arithmetic.typeIntNum;
        type2 = kind2 == Arithmetic.INT_CODE ? Type.intType :  Arithmetic.typeIntNum;
      }
    else if (op >= ASHIFT_GENERAL && op <= LSHIFT_RIGHT)
      {
        type1 = Arithmetic.typeIntNum;
        type2 = Type.intType;
        swap = false;
      }
    else
      {
        type1 = type2 = Arithmetic.typeIntNum;
        swap = false;
      }
    arg1.compile(comp, type1);
    arg2.compile(comp, type2);
    CodeAttr code = comp.getCode();
    if (swap)
      {
        code.emitSwap();
        type1 = Arithmetic.typeIntNum;
        type2 = LangPrimType.intType;
      }
    String mname = null;
    Type[] argTypes = null;
    ObjectType mclass = Arithmetic.typeIntNum;
    switch (op)
      {
      case ADD: mname = "add";  break;
      case SUB: mname = "sub";  break;
      case MUL: mname = "times";  break;
      case AND:
        mname = "and";
        /* ... fall through ... */
      case IOR:
        if (mname == null)
          mname = "ior";
        /* ... fall through ... */
      case XOR:
        if (mname == null)
          mname = "xor";
        mclass = ClassType.make("gnu.math.BitOps");
        break;
      case DIVIDE_GENERIC:
      case DIVIDE_INEXACT:
      case QUOTIENT:
      case QUOTIENT_EXACT:
      case MODULO:
        mname = op == MODULO ? "remainder" : "quotient";
        DivideOp dproc = (DivideOp) proc;
        if (op == MODULO && dproc.rounding_mode == Numeric.FLOOR)
          mname = "modulo";
        else if (dproc.rounding_mode != Numeric.TRUNCATE)
          {
            code.emitPushInt(dproc.rounding_mode);
            argTypes = new Type[] { type1, type2, Type.intType };
          }
        break;
      case ASHIFT_LEFT:
      case ASHIFT_RIGHT:
        mname = op == ASHIFT_LEFT ? "shiftLeft" : "shiftRight";
        mclass = ClassType.make("gnu.kawa.functions.BitwiseOp");
        break;
      case ASHIFT_GENERAL:
        mname = "shift";
        break;
      default: throw new Error();
      }
    if (argTypes == null)
      argTypes = new Type[] { type1, type2 };
    meth = mclass.getMethod(mname, argTypes);
    code.emitInvokeStatic(meth);
    return true;
  }

  public static int getReturnKind (int kind1, int kind2, int op)
  {
    if (op >= ASHIFT_GENERAL && op <= LSHIFT_RIGHT)
      return kind1;
    return getReturnKind2(kind1, kind2);
  }

    private static int getReturnKind2 (int kind1, int kind2) {
        /* FIXME: Possible future optimization.
           Would need to resolve print-out of Double vs DFloNum.
        if (kind1 <= 0)
            return kind1;
        if (kind2 <= 0)
            return kind2;
        if ((kind1 == Arithmetic.FLONUM_CODE
             && kind2 <= Arithmetic.DOUBLE_CODE) ||
            (kind2 == Arithmetic.FLONUM_CODE
             && kind1 <= Arithmetic.DOUBLE_CODE))
            return Arithmetic.DOUBLE_CODE;
        return kind1 > kind2 ? kind1 : kind2;
        */
        return kind1 <= 0 || (kind1 > kind2 && kind2 > 0) ? kind1 : kind2;
  }

  /** This actually returns the "promoted argument type".
   * The result kind is different for divide.
   */
  public int getReturnKind (Expression[] args)
  {
    int len = args.length;
    if (len == 0)
      return Arithmetic.INTNUM_CODE;
    Type type = Type.pointer_type;
    int kindr = 0;
    for (int i = 0;  i < len;  i++)
      {
	Expression arg = args[i];
	int kind = Arithmetic.classifyType(arg.getType());

	if (i == 0 || kind == 0 || kind > kindr)
	  kindr = kind;
      }
    return kindr;
  }

  // semi-deprecated.
  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return Arithmetic.kindType(adjustReturnKind(getReturnKind(args), op));
  }

  static int adjustReturnKind (int rkind, int op)
  {
    if (op >= DIVIDE_GENERIC && op <= QUOTIENT_EXACT && rkind > 0)
      {
        switch (op)
          {
          case DivideOp.DIVIDE_GENERIC:
            if (rkind <= Arithmetic.INTNUM_CODE)
              rkind = Arithmetic.RATNUM_CODE;
            break;
          case DivideOp.DIVIDE_INEXACT:
            if (rkind <= Arithmetic.REALNUM_CODE
                && rkind != Arithmetic.FLOAT_CODE)
              rkind = Arithmetic.DOUBLE_CODE;
            break;
          case DivideOp.QUOTIENT_EXACT:
            if (rkind <= Arithmetic.REALNUM_CODE)
              rkind = Arithmetic.INTNUM_CODE;
          default: ;
          }
      }
    return rkind;
  }

  public static Expression validateApplyAdd (AddOp proc, ApplyExp exp, InlineCalls visitor)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1 && proc.plusOrMinus < 0)
      {
        Type type0 = args[0].getType();
        if (type0 instanceof PrimType)
          {
            char sig0 = type0.getSignature().charAt(0);
            Type type = null;
            int opcode = 0;
            if (sig0 == 'V' || sig0 == 'Z' || sig0 == 'C')
              {
                // error
              }
            else if (sig0 == 'D')
              {
                opcode = 119 /* dneg */;
                type = LangPrimType.doubleType;
              }
            else if (sig0 == 'F')
              {
                opcode = 118 /* fneg */;
                type = LangPrimType.floatType;
              }
            else if (sig0 == 'J')
              {
                opcode = 117 /* lneg */;
                type = LangPrimType.longType;
              }
            else
              {
                opcode = 116 /* ineg */;
                type = LangPrimType.intType;
              }
            if (type != null)
              {
                PrimProcedure prim
                  = PrimProcedure.makeBuiltinUnary(opcode, type);
                return new ApplyExp(prim, args);
              }
          }
      }
    return exp;
  }

  public static Expression validateApplyDiv (DivideOp proc,
                                      ApplyExp exp, InlineCalls visitor)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        args = new Expression[] { QuoteExp.getInstance(IntNum.one()), args[0] };
        exp = new ApplyExp(exp.getFunction(), args);
      }
    return exp;
  }

  public static Expression validateApplyNot (ApplyExp exp, int kind, InlineCalls visitor)
  {
    if (exp.getArgCount() == 1)
      {
        Expression arg = exp.getArg(0);
        if (kind == Arithmetic.INT_CODE || kind == Arithmetic.LONG_CODE)
          {
            Expression[] args = {arg, QuoteExp.getInstance(IntNum.minusOne())};
            return visitor.visitApplyOnly(new ApplyExp(BitwiseOp.xor, args), null); // FIXME
          }
        String cname;
        if (kind == Arithmetic.INTNUM_CODE)
          cname = "gnu.math.BitOps";
        else if (kind == Arithmetic.BIGINTEGER_CODE)
          cname = "java.meth.BigInteger";
        else
          cname = null;
        if (cname != null)
          return new ApplyExp(ClassType.make(cname).getDeclaredMethod("not", 1),
                              exp.getArgs());
      }
    return exp;
  }

  public int primitiveOpcode ()
  {
    switch (op)
      {
      case ADD:    return 96; /* iadd */
      case SUB:    return 100; /* isub */
      case MUL:    return 104;
      case DIVIDE_GENERIC:
      case DIVIDE_INEXACT:
      case QUOTIENT:
      case QUOTIENT_EXACT:
        return 108;
      case MODULO:    return 112;
      case ASHIFT_LEFT:  return 120; // ishl
      case ASHIFT_RIGHT:  return 122; // ishr
      case LSHIFT_RIGHT:  return 124; // iushr
      case AND:  return 126; // iand
      case IOR:  return 128; // ior
      case XOR:  return 130; // ixor
      default:     return -1;
      }
  }

  /** Convert (PROC A B C) to (PROC (PROC A B) C) etc.
   */
  public static Expression pairwise(Procedure proc,
                                    Expression rproc, Expression[] args,
				    InlineCalls visitor)
  {
    int len = args.length;
    Expression prev = args[0];
    for (int i = 1;  i < len;  i++)
      {
        Expression[] args2 = new Expression[2];
        args2[0] = prev;
        args2[1] = args[i];
        ApplyExp next = new ApplyExp(rproc, args2);
        Expression inlined = visitor.maybeInline(next, null/*FIXME*/, proc);
        prev = inlined != null ? inlined : next;
      }
    return prev;
  }
}
