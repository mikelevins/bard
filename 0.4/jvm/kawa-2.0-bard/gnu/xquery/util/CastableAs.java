package gnu.xquery.util;
import gnu.kawa.reflect.*;
import gnu.xquery.lang.XQuery;
import gnu.bytecode.*;
import gnu.kawa.xml.*;
import gnu.expr.*;
import gnu.mapping.Procedure;

public class CastableAs extends InstanceOf
{
  public static CastableAs castableAs = new CastableAs();

  CastableAs ()
  {
    super(XQuery.getInstance(), "castable as");
    setProperty(Procedure.validateApplyKey,
                   "gnu.xquery.util.CompileMisc:validateApplyCastableAs");
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = language.asType(arg2);
    boolean result;
    if (type instanceof XDataType)
      result = ((XDataType) type).castable(arg1);
    else
      result = type.isInstance(arg1);
    return language.booleanObject(result);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    // To override InstanceOf.compile.
    ApplyExp.compile(exp, comp, target);
  }
}
