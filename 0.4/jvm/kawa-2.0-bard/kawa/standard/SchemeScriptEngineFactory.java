package kawa.standard;
import javax.script.*;
import gnu.expr.*;
import java.util.List;

public class SchemeScriptEngineFactory extends AbstractScriptEngineFactory
{
  public SchemeScriptEngineFactory ()
  {
    super(Scheme.instance);
  }

  protected void getNames (List<String> names)
  {
    names.add("scheme");
    names.add("kawa");
    names.add("kawa-scheme");
  }
}
