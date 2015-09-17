package gnu.kawa.functions;
import gnu.expr.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangObjType;

public class NumberPredicate extends Procedure1
{
  public static final int ODD = 1;
  public static final int EVEN = 2;
  final int op;

  Language language;

  protected final Language getLanguage ()
  {
    return language;
  }

  public Object apply1 (Object arg1)
  {
    IntNum iarg1 = LangObjType.coerceIntNum(arg1);
    boolean result;
    switch (op)
      {
      case ODD: result = iarg1.isOdd(); break;
      case EVEN: result = ! iarg1.isOdd(); break;
      default: throw new Error();
      }
    return getLanguage().booleanObject(result);
  }

  public NumberPredicate (Language language, String name, int op)
  {
    super(name);
    this.language = language;
    this.op = op;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileMisc:validateApplySimpleBoolean");
    setProperty(Procedure.compilerXKey,
                "gnu.kawa.functions.CompileMisc:compileNumPredicate");
  }

  public int numArgs()
  {
    return 0x1001;
  }

}
