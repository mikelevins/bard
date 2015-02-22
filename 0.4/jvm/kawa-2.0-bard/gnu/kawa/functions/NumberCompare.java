package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import java.math.*;

/** This implements the numeric comparison relations: {@code <}, {@code <=}, etc. */

public class NumberCompare extends ProcedureN
{
  Language language;

  // Return codes from Numeric.compare:
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  // One flag bit for each of the above RESULT_XXX codes:
  public static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  public static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  public static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  public static final int TRUE_IF_NAN = 1 << (RESULT_NAN + 3);
  public static final int TRUE_IF_NEQ = 1 << (RESULT_NEQ + 3);
  int flags;

  public int numArgs() { return (-1 << 12) | 2; }

  public static boolean $Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_EQU, arg1, arg2);
  }

  public static boolean $Gr(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_GRT, arg1, arg2);
  }

  public static boolean $Gr$Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_GRT|TRUE_IF_EQU, arg1, arg2);
  }

  public static boolean $Ls(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_LSS, arg1, arg2);
  }

  public static boolean $Ls$Eq(Object arg1, Object arg2)
  {
    return apply2(TRUE_IF_LSS|TRUE_IF_EQU, arg1, arg2);
  }
 
  public static boolean $Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Eq(arg1, arg2) && $Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Eq(arg3, rest[0]) && applyN(TRUE_IF_EQU, rest))));
  }

  public static boolean $Gr$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Gr(arg1, arg2) && $Gr(arg2, arg3)
	    && (rest.length == 0
		|| ($Gr(arg3, rest[0]) && applyN(TRUE_IF_GRT, rest))));
  }

  public static boolean $Gr$Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Gr$Eq(arg1, arg2) && $Gr$Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Gr$Eq(arg3, rest[0])
		    && applyN(TRUE_IF_GRT|TRUE_IF_EQU, rest))));
  }

  public static boolean $Ls$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Ls(arg1, arg2) && $Ls(arg2, arg3)
	    && (rest.length == 0
		|| ($Ls(arg3, rest[0]) && applyN(TRUE_IF_LSS, rest))));
  }

  public static boolean $Ls$Eq$V (Object arg1, Object arg2,
			       Object arg3, Object[] rest)
  {
    return ($Ls$Eq(arg1, arg2) && $Ls$Eq(arg2, arg3)
	    && (rest.length == 0
		|| ($Ls$Eq(arg3, rest[0])
		    && applyN(TRUE_IF_LSS|TRUE_IF_EQU, rest))));
  }

  public static NumberCompare make(Language language, String name, int flags)
  {
    NumberCompare proc = new NumberCompare();
    proc.language = language;
    proc.setName(name);
    proc.flags = flags;
    proc.setProperty(Procedure.validateApplyKey,
                     "gnu.kawa.functions.CompileMisc:validateApplySimpleBoolean");
    proc.setProperty(Procedure.compilerXKey,
                     "gnu.kawa.functions.CompileMisc:compileNumberCompare");
    return proc;
  }

  protected final Language getLanguage ()
  {
    return language;
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    return getLanguage().booleanObject(apply2(flags, arg1, arg2));
  }

  static public boolean apply2 (int flags, Object arg1, Object arg2)
  {
    return ((1 << (3 + compare(arg1, arg2, true))) & flags) != 0;
  }
  
  public static boolean checkCompareCode (int code, int flags)
  {
    return ((1 << (3 + code)) & flags) != 0;
  }

  static public boolean applyWithPromotion (int flags, Object arg1, Object arg2)
  {
    return checkCompareCode(compare(arg1, arg2, false), flags);
  }
  
  /** Compare two numbers.
   * @param exact true if we should compare exact/inexact numbers exactly
   *   (by converting the inexact number to exact), or inexactly (by
   *   "promoting" the exact to inexact) (as required for XQuery).
   * @return 1 if {@code arg1>arg2}; 0 if {@code arg1==arg2};
   * -1 if {@code arg1<arg2}; -2 if either is {@code NaN};
   * -3 if not comparable (either is not a number). */
  static public int compare (Object arg1, Object arg2, boolean exact)
  {
    int code1 = Arithmetic.classifyValue(arg1);
    int code2 = Arithmetic.classifyValue(arg2);
    return compare(arg1, code1, arg2, code2, exact);
  }

  static public int compare (Object arg1, int code1,
                             Object arg2, int code2,
                             boolean exact)
  {
    if (code1 < 0 || code2 < 0)
      return -3;
    int code = code1 < code2 ? code2 : code1;
    int comp; // A Numeric.compare return code: -1, 0, 1, or rarely: -2, or -3.
    switch (code)
      {
      case Arithmetic.INT_CODE:
	int i1 = Arithmetic.asInt(arg1);
	int i2 = Arithmetic.asInt(arg2);
        comp = i1 < i2 ? -1 : i1 > i2 ? 1 : 0;
	break;
      case Arithmetic.LONG_CODE:
	long l1 = Arithmetic.asLong(arg1);
	long l2 = Arithmetic.asLong(arg2);
        comp = l1 < l2 ? -1 : l1 > l2 ? 1 : 0;
        break;
      case Arithmetic.BIGINTEGER_CODE:
	BigInteger bi1 = Arithmetic.asBigInteger(arg1);
	BigInteger bi2 = Arithmetic.asBigInteger(arg2);
	comp = bi1.compareTo(bi2);
        break;
      case Arithmetic.INTNUM_CODE:
	comp = IntNum.compare(Arithmetic.asIntNum(arg1),
                              Arithmetic.asIntNum(arg2));
        break;
      case Arithmetic.BIGDECIMAL_CODE:
	BigDecimal bd1 = Arithmetic.asBigDecimal(arg1);
	BigDecimal bd2 = Arithmetic.asBigDecimal(arg2);
	comp = bd1.compareTo(bd2);
        break;
      case Arithmetic.RATNUM_CODE:
	comp = RatNum.compare(Arithmetic.asRatNum(arg1),
                              Arithmetic.asRatNum(arg2));
        break;
      case Arithmetic.FLOAT_CODE:
        if (! exact
            || (code1 > Arithmetic.RATNUM_CODE
                && code2 > Arithmetic.RATNUM_CODE))
          {
            float f1 = Arithmetic.asFloat(arg1);
            float f2 = Arithmetic.asFloat(arg2);
            comp = f1 > f2 ? 1 : f1 < f2 ? -1 : f1 == f2 ? 0 : -2;
            break;
          }
        // else fall through, to handle exact-inexact comparison
      case Arithmetic.DOUBLE_CODE:
      case Arithmetic.FLONUM_CODE:
        if (! exact
            || (code1 > Arithmetic.RATNUM_CODE
                && code2 > Arithmetic.RATNUM_CODE))
          {
            double d1 = Arithmetic.asDouble(arg1);
            double d2 = Arithmetic.asDouble(arg2);
            comp = d1 > d2 ? 1 : d1 < d2 ? -1 : d1 == d2 ? 0 : -2;
            break;
          }
        // else fall through, to handle exact-inexact comparison
      default:
	Numeric num1 = Arithmetic.asNumeric(arg1);
	Numeric num2 = Arithmetic.asNumeric(arg2);
        comp = ((Numeric) num1).compare(num2);
      }
    return comp;
  }

  static boolean applyN (int flags, Object[] args)
  {
    //  if (args.length < 2)
    //  throw new WrongArguments(this.name(),2,"(< x1 x2 ...)");
    for (int i = 0;  i < args.length - 1;  i++)
      {
	Object arg1 = args[i];
	Object arg2 = args[i+1];
	if (! apply2(flags, arg1, arg2))
	  return false;
      }
    return true;
  }

  public Object applyN (Object[] args)
  {
    //  if (args.length < 2)
    //  throw new WrongArguments(this.name(),2,"(< x1 x2 ...)");
    return getLanguage().booleanObject(applyN(flags, args));
  }
}
