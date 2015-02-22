package gnu.kawa.functions;
import gnu.math.*;
import gnu.mapping.*;
import gnu.text.Char;
import gnu.expr.Language;
import gnu.mapping.Promise;

/** Implement that standard Scheme function "eqv?". */

public class IsEqv extends Procedure2
{
  Language language;
  IsEq isEq;

  public IsEqv(Language language, String name, IsEq isEq)
  {
    this.language = language;
    this.isEq = isEq;
    setName(name);
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompilationHelpers:validateIsEqv");
  }

  public static boolean apply (Object arg1, Object arg2) 
  {
    arg1 = Promise.force(arg1);
    arg2 = Promise.force(arg2);
    if (arg1==arg2)
      return true;
    if (arg1 instanceof Number && arg2 instanceof Number)
      return IsEqual.numberEquals((Number) arg1, (Number) arg2);
    if (arg1 instanceof Char
        || arg1 instanceof Boolean
        // Symbols can now be equals even if not ==, due to namespace support.
        || arg1 instanceof Symbol)
      return arg1.equals (arg2);
    return false;
   }

  public Object apply2 (Object arg1, Object arg2)
  {
    return language.booleanObject(apply(arg1, arg2));
   }
}
