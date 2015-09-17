package gnu.ecmascript;
import gnu.mapping.*;

public class Reserved
{
  String name;
  int prio;
  Procedure proc;

  public static final int VAR_TOKEN = 30;
  public static final int IF_TOKEN = 31;
  public static final int WHILE_TOKEN = 32;
  public static final int FOR_TOKEN = 33;
  public static final int CONTINUE_TOKEN = 34;
  public static final int BREAK_TOKEN = 35;
  public static final int RETURN_TOKEN = 36;
  public static final int WITH_TOKEN = 37;
  public static final int ELSE_TOKEN = 38;
  public static final int NEW_TOKEN = 39;
  public static final int THIS_TOKEN = 40;
  public static final int FUNCTION_TOKEN = 41;

  public static final int PLUS_OP = 1;
  public static final int MINUS_OP = 2;
  public static final int TIMES_OP = 3;
  public static final int LSHIFT_OP = 4;
  public static final int LESS_OP = 5;

  public Reserved (String name, int prio, Procedure proc)
  {
    this.name = name;
    this.prio = prio;
    this.proc = proc;
  }

  public Reserved (String name, int prio)
  {
    this.name = name;
    this.prio = prio;
  }

  public Reserved (String name, int prio, int op)
  {
    this.name = name;
    this.prio = prio;
    this.proc = new BinaryOp(name, op);
  }

  final static Reserved opBoolOr = new Reserved("||", 1, 0);
  final static Reserved opBoolAnd = new Reserved("&&", 2, 0);
  final static Reserved opBitOr = new Reserved("|", 3, 0);
  final static Reserved opBitXor = new Reserved("^", 4, 0);
  final static Reserved opBitAnd = new Reserved("&", 5, 0);
  final static Reserved opEqual = new Reserved("=", 6, 0);
  final static Reserved opNotEqual = new Reserved("!=", 6, 0);
  final static Reserved opLess = new Reserved("<", 7, LESS_OP);
  final static Reserved opGreater = new Reserved(">", 7, 0);
  final static Reserved opLessEqual = new Reserved("<=", 7, 0);
  final static Reserved opGreaterEqual = new Reserved(">=", 7, 0);
  final static Reserved opLshift = new Reserved("<<", 8, LSHIFT_OP);
  final static Reserved opRshiftSigned = new Reserved(">>", 8, 0);
  final static Reserved opRshiftUnsigned = new Reserved(">>>", 8, 0);
  final static Reserved opPlus = new Reserved("+", 9, PLUS_OP);
  final static Reserved opMinus = new Reserved("-", 9, MINUS_OP);
  final static Reserved opTimes = new Reserved("*", 10, TIMES_OP);
  final static Reserved opDivide = new Reserved("/", 10, 0);
  final static Reserved opRemainder = new Reserved("%", 10, 0);

  static Reserved opPlusPlus, opMinusMinus;  // FIXME

  public String toString() { return "[Reserved \""+name+"\" prio:"+prio+"]"; }

  public boolean isAssignmentOp() { return false; }
}
