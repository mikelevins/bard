// Copyright (c) 2001, 2003, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.util;
import gnu.kawa.functions.NumberCompare;
import gnu.kawa.xml.*;
import gnu.math.*;
import gnu.mapping.*;

/** Compares two values (or sequences) according to XPath semantics. */

public class Compare extends Procedure2
{
  static final int RESULT_GRT = 1;
  static final int RESULT_EQU = 0;
  static final int RESULT_LSS = -1;
  static final int RESULT_NAN = -2;
  static final int RESULT_NEQ = -3;

  // One flag bit for each of the above RESULT_XXX codes:
  static final int TRUE_IF_GRT = 1 << (RESULT_GRT + 3);
  static final int TRUE_IF_EQU = 1 << (RESULT_EQU + 3);
  static final int TRUE_IF_LSS = 1 << (RESULT_LSS + 3);
  static final int TRUE_IF_NAN = 1 << (RESULT_NAN + 3);
  static final int TRUE_IF_NEQ = 1 << (RESULT_NEQ + 3);
  static final int VALUE_COMPARISON = 1 << 5;
  static final int LENIENT_COMPARISON = 1 << 6;
  static final int LENIENT_EQ = (TRUE_IF_EQU|LENIENT_COMPARISON);

  int flags;

  public static Compare make(String name, int flags)
  {
    Compare proc = new Compare();
    proc.setName(name);
    proc.setProperty(Procedure.validateApplyKey,
                     "gnu.xquery.util.CompileMisc:validateCompare");
    proc.flags = flags;
    return proc;
  }

  /*
  private static Object toString(object value)
  {
    if (value instanceof TreeList)
      return = ((TreeList) value).stringValue(0, sbuf);
    else if (value instanceof SeqPosition && ! (node instanceof TreePosition))
      {
	SeqPosition pos = (SeqPosition) value;
	if (pos.sequence instanceof TreeList)
	  {
	    ((TreeList) pos.sequence).stringValue(pos.ipos >> 1, sbuf);
	    return;
	  }
      }
  }
  */

  public static boolean apply(int flags, Object arg1, Object arg2,
                              NamedCollator collator)
  {
    if (arg1 instanceof Values)
      {
	Values values1 = (Values) arg1;
	int iter = 0;
	for (;;)
	  {
	    iter = values1.nextPos(iter);
	    if (iter == 0)
	      return false;
	    if (apply(flags, values1.getPosPrevious(iter), arg2, collator))
	      return true;
	  }
      }
    if (arg2 instanceof Values)
      {
	Values values2 = (Values) arg2;
	int iter = 0;
	for (;;)
	  {
	    iter = values2.nextPos(iter);
	    if (iter == 0)
	      return false;
	    if (apply(flags, arg1, values2.getPosPrevious(iter), collator))
	      return true;
	  }
      }
    return atomicCompare(flags,
                         KNode.atomicValue(arg1),
                         KNode.atomicValue(arg2),
                         collator);
  }

  public static boolean equalityComparison (int flags)
  {
    return ((flags & TRUE_IF_GRT) != 0) == ((flags & TRUE_IF_LSS) != 0);
  }

  public static boolean atomicCompare(int flags, Object arg1, Object arg2,
                                      NamedCollator collator)
  {
    if (arg1 instanceof UntypedAtomic)
      {
        String str = arg1.toString();
        if ((flags & VALUE_COMPARISON) != 0)
          arg1 = str;
        else if (arg2 instanceof DateTime)
          arg1 = XTimeType.parseDateTime(str, ((DateTime) arg2).components());
        else if (arg2 instanceof Duration)
          arg1 = Duration.parse(str, ((Duration) arg2).unit());
        else if (arg2 instanceof Number)
          arg1 = new DFloNum(str);
        else if (arg2 instanceof Boolean)
          arg1 = XDataType.booleanType.valueOf(str);
        else
          arg1 = str;
      }
    if (arg2 instanceof UntypedAtomic)
      {
        String str = arg2.toString();
        if ((flags & VALUE_COMPARISON) != 0)
          arg2 = str;
        else if (arg1 instanceof DateTime)
          arg2 = XTimeType.parseDateTime(str, ((DateTime) arg1).components());
        else if (arg1 instanceof Duration)
          arg2 = Duration.parse(str, ((Duration) arg1).unit());
        else if (arg1 instanceof Number)
          arg2 = new DFloNum(str);
        else if (arg1 instanceof Boolean)
          arg2 = XDataType.booleanType.valueOf(str);
        else
          arg2 = str;
      }
    int comp;
    if (arg1 instanceof Number || arg2 instanceof Number)
      {
        if (arg1 instanceof Duration)
          {
            if (! (arg2 instanceof Duration))
              comp = -3;
            else
              {
                Duration d1 = (Duration) arg1;
                Duration d2 = (Duration) arg2;
                if ((d1.unit != d2.unit || d1.unit == Unit.duration)
                    && ! equalityComparison(flags))
                  comp = -3;
                else
                  comp = Duration.compare(d1, d2);
              }
          }
        else if (arg1 instanceof DateTime)
          {
            if (! (arg2 instanceof DateTime))
              comp = -3;
            else
              {
                DateTime d1 = (DateTime) arg1;
                DateTime d2 = (DateTime) arg2;
                int m1 = d1.components();
                int m2 = d2.components();
                if (m1 != m2)
                  comp = -3;
                else if (! equalityComparison(flags)
                         && m1 != DateTime.TIME_MASK
                         && m1 != DateTime.DATE_MASK
                         && m1 != (DateTime.DATE_MASK|DateTime.TIME_MASK))
                  comp = -3;
                else
                  comp = DateTime.compare(d1, d2);
              }
          }
        else if (arg2 instanceof Duration || arg2 instanceof DateTime)
          comp = -3;
        else
          comp = NumberCompare.compare(arg1, arg2, false);
        if (comp == -3 && (flags & LENIENT_COMPARISON) == 0)
          throw new IllegalArgumentException("values cannot be compared");
        return NumberCompare.checkCompareCode(comp, flags);
      }
    if (arg1 instanceof Symbol)
      {
        if (arg2 instanceof Symbol && equalityComparison(flags))
          comp = arg1.equals(arg2) ? 0 : -2;
        else
          comp = -3;
      }
    else if (arg1 instanceof Boolean)
      {
        if (arg2 instanceof Boolean)
          {
            boolean b1 = ((Boolean) arg1).booleanValue();
            boolean b2 = ((Boolean) arg2).booleanValue();
            comp = b1 == b2 ? 0 : b2 ? -1 : 1;
          }
        else
          comp = -3;
      }
    else if (arg2 instanceof Boolean || arg2 instanceof Symbol)
      comp = -3;
    else
      {
        String str1 = arg1.toString();
        String str2 = arg2.toString();
        /* #ifdef JAVA2 */
        if (collator != null)
          comp = collator.compare(str1, str2);
        else
        /* #endif */
          comp = NamedCollator.codepointCompare(str1, str2);
        comp = comp < 0 ? -1 : comp > 0 ? 1 : 0;
      }
    if (comp == -3 && (flags & LENIENT_COMPARISON) == 0)
      throw new IllegalArgumentException("values cannot be compared");
    return NumberCompare.checkCompareCode(comp, flags);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    if ((flags & VALUE_COMPARISON) != 0)
      {
        if (arg1 == null || arg1 == Values.empty) return arg1;
        if (arg2 == null || arg2 == Values.empty) return arg2;
        return atomicCompare(flags,
                             KNode.atomicValue(arg1),
                             KNode.atomicValue(arg2),
                             null) ? Boolean.TRUE: Boolean.FALSE;
      }
    return apply(flags, arg1, arg2, null) ? Boolean.TRUE : Boolean.FALSE;
  }

  public static final Compare $Eq   = make("=",TRUE_IF_EQU);
  public static final Compare $Ex$Eq
  = make("!=",TRUE_IF_GRT|TRUE_IF_LSS|TRUE_IF_NAN|TRUE_IF_NEQ);
  public static final Compare $Gr   = make(">",TRUE_IF_GRT);
  public static final Compare $Gr$Eq= make(">=",TRUE_IF_GRT|TRUE_IF_EQU);
  public static final Compare $Ls   = make("<",TRUE_IF_LSS);
  public static final Compare $Ls$Eq= make("<=",TRUE_IF_LSS|TRUE_IF_EQU);

  public static final Compare valEq =
    make("eq",TRUE_IF_EQU|VALUE_COMPARISON);
  public static final Compare valNe =
    make("ne",TRUE_IF_GRT|TRUE_IF_LSS|TRUE_IF_NAN|TRUE_IF_NEQ|VALUE_COMPARISON);
  public static final Compare valGt =
    make("gt",TRUE_IF_GRT|VALUE_COMPARISON);
  public static final Compare valGe =
    make("ge",TRUE_IF_GRT|TRUE_IF_EQU|VALUE_COMPARISON);
  public static final Compare valLt =
    make("lt",TRUE_IF_LSS|VALUE_COMPARISON);
  public static final Compare valLe =
    make("le",TRUE_IF_LSS|TRUE_IF_EQU|VALUE_COMPARISON);
}
