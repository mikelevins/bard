package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangPrimType;

public abstract class ArithOp extends ProcedureN
{
  static final int ADD = 1;
  static final int SUB = 2;
  static final int MUL = 3;

  /** Implement's Scheme {@code /} operation. */
  public static final int DIVIDE_GENERIC = 4;

  /** Implements a division operation.
   * Like Scheme's {@code (exact->inexact (/ x y))}.
   */
  public static final int DIVIDE_INEXACT = 5;

  /** Implements a quotient operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands are real; result is an integer.
   * Inexact operands yield inexact integer result.
   */
  public static final int QUOTIENT = 6;
  /** Implements a quotient operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands and result are real.
   * Operands are real; result is an integer.
   * Inexact operands yield exact integer result.
   */
  public static final int QUOTIENT_EXACT = 7;

  /** Implements a modulo/remainder operation.
   * Depends on the value of {@code getRoundingMode()}.
   * Operands and result are real.
   */
  public static final int MODULO = 8;

  public static final int ASHIFT_GENERAL = 9;
  public static final int ASHIFT_LEFT = 10;
  public static final int ASHIFT_RIGHT = 11;
  public static final int LSHIFT_RIGHT = 12; // Not implemented yet.
  public static final int AND = 13;
  public static final int IOR = 14;
  public static final int XOR = 15;
  public static final int NOT = 16;

  final int op;

  public ArithOp (String name, int op)
  {
    super(name);
    this.op = op;
  }

  public Object defaultResult ()
  {
    return IntNum.zero();
  }

  public boolean isSideEffectFree ()
  {
    return true;
  }
}
