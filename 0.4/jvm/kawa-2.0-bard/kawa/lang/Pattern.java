package kawa.lang;
import gnu.bytecode.*;
import gnu.expr.Compilation;
import gnu.lists.Consumer;
import gnu.text.Printable;

/**
 * A Pattern is used to match against objects.
 * E.g. it can be used to match against macro arguments.
 * @author	Per Bothner
 */

abstract public class Pattern implements Printable
{
  /**
   * Match this Pattern against an object.
   * @param obj object to match against this pattern
   * @return null on failure, or an array of bound pattern variables.
   */
  public Object[] match (Object obj)
  {
    Object[] vars = new Object [varCount ()];
    return match (obj, vars, 0) ? vars : null;
  }

  /** Match this Pattern against an Object.
   * @param obj the Object to match against
   * @param vars the "pattern variable" values extracted from obj go here
   * @param start_vars where in vars to strt putting the varCount() values
   * @return true iff the match succeeded.
   */
  abstract public boolean match (Object obj, Object[] vars, int start_vars);

  abstract public int varCount ();

  static public ClassType typePattern = ClassType.make("kawa.lang.Pattern");
  private static Type[] matchArgs =
  { Type.pointer_type, Compilation.objArrayType, Type.intType  };
  static public final Method matchPatternMethod
  = typePattern.addMethod("match", matchArgs, Type.booleanType, Access.PUBLIC);

    public void print(Consumer out) {
        out.write(toString());
    }
}
