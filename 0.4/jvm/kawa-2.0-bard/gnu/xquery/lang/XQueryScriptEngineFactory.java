package gnu.xquery.lang;
import javax.script.*;
import gnu.expr.*;
import java.util.List;

public class XQueryScriptEngineFactory
 extends AbstractScriptEngineFactory
{
  public XQueryScriptEngineFactory ()
  {
    super(XQuery.instance);
  }

  protected void getNames (List<String> names)
  {
    names.add("xquery");
    names.add("qexo");
    names.add("kawa-xquery");
  }
}
