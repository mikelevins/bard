package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.expr.Language;

/** Implement the standard Scheme procedure "not". */

public class Not extends Procedure1
{
  Language language;

  public Not(Language language)
  {
    this.language = language;
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.functions.CompileMisc:validateApplySimpleBoolean");
    setProperty(Procedure.compilerXKey,
                "gnu.kawa.functions.CompileMisc:compileNot");
 }

  public Not(Language language, String name)
  {
    this(language);
    setName(name);
  }

  public Object apply1 (Object arg1)
   {
     return language.booleanObject(! language.isTrue(arg1));
   }
}
