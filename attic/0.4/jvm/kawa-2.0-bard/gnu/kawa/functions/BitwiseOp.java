// Copyright (c) 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.expr.*;
import java.math.BigInteger;
import gnu.kawa.lispexpr.LangObjType;

public class BitwiseOp extends ArithOp
{
  public static final BitwiseOp and
  = new BitwiseOp("bitwise-and", AND);
  public static final BitwiseOp ior
  = new BitwiseOp("bitwise-ior", IOR);
  public static final BitwiseOp xor
  = new BitwiseOp("bitwise-xor", XOR);
  public static final BitwiseOp ashift
  = new BitwiseOp("bitwise-arithmetic-shift", ASHIFT_GENERAL);
  public static final BitwiseOp ashiftl
  = new BitwiseOp("bitwise-arithmetic-shift-left", ASHIFT_LEFT);
  public static final BitwiseOp ashiftr
  = new BitwiseOp("bitwise-arithmetic-shift-right", ASHIFT_RIGHT);
  public static final BitwiseOp not
  = new BitwiseOp("bitwise-not", NOT);

  public BitwiseOp (String name, int op)
  {
    super(name, op);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileArith:validateApplyArithOp");
    Procedure.compilerKey.set(this, "*gnu.kawa.functions.CompileArith:forBitwise");
  }

  public Object defaultResult ()
  {
    if (op == AND)
      return IntNum.minusOne();
    else
      return IntNum.zero();
  }

  public Object adjustResult (IntNum value, int code)
  {
    switch (code)
      {
      case Arithmetic.INT_CODE:
        return Integer.valueOf(value.intValue());
      case Arithmetic.LONG_CODE:
        return Long.valueOf(value.longValue());
      case Arithmetic.BIGINTEGER_CODE:
        return new BigInteger(value.toString());
      default:
        return value;
      }
  }

  public Object apply1 (Object arg1)
  {
    if (op == NOT)
      {
        int code1 = Arithmetic.classifyValue(arg1);
        IntNum iarg1 = LangObjType.coerceIntNum(arg1);
        return adjustResult( BitOps.not(iarg1), code1);
      }
    else
      return apply2(defaultResult(), arg1);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    int kind1 = Arithmetic.classifyValue(arg1);
    int kind2 = Arithmetic.classifyValue(arg2);
    int kind = (op >= ASHIFT_GENERAL && op <= LSHIFT_RIGHT) || kind1 <= 0 || (kind1 > kind2 && kind2 > 0) ? kind1 : kind2;
    IntNum iarg1 = LangObjType.coerceIntNum(arg1);
    IntNum iarg2 = LangObjType.coerceIntNum(arg2);
    IntNum result;
    switch (op)
      {
      case AND: result = BitOps.and(iarg1, iarg2);  break;
      case IOR: result = BitOps.ior(iarg1, iarg2);  break;
      case XOR: result = BitOps.xor(iarg1, iarg2);  break;
      case ASHIFT_LEFT:
      case ASHIFT_RIGHT:
      case ASHIFT_GENERAL:
        int amount = iarg2.intValue();
        if (op == ASHIFT_RIGHT || op == ASHIFT_LEFT)
          {
            checkNonNegativeShift(this, amount);
            if (op == ASHIFT_RIGHT)
              amount = -amount;
          }
        result = IntNum.shift(iarg1, amount);
        break;
      default:
        throw new Error();
      }
    return adjustResult(result, kind);
  }

  public Object applyN (Object[] args)
  {
    int alen = args.length;
    if (alen == 0)
      return defaultResult();
    else if (alen == 1)
      return apply1(args[0]);
    else
      {
        Object r = args[0];
        for (int i = 1;  i < alen;  i++)
          r = apply2(r, args[i]);
        return r;
      }
  }

  public static int checkNonNegativeShift(Procedure proc, int amount)
  {
    if (amount < 0)
      throw new WrongType(proc, 2, Integer.valueOf(amount),
                          "non-negative integer");
    return amount;
  }

  public static IntNum shiftLeft (IntNum value, int count)
  {
    return IntNum.shift(value, checkNonNegativeShift(ashiftl, count));
  }

  public static IntNum shiftRight (IntNum value, int count)
  {
    return IntNum.shift(value, - checkNonNegativeShift(ashiftr, count));
  }

  public int numArgs()
  {
    if (op >= ASHIFT_GENERAL && op <= LSHIFT_RIGHT)
      return 0x2002;
    if (op == NOT)
      return 0x1001;
    return 0xfffff000;
  }
}
