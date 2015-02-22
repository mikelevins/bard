package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.lists.UnescapedData;

public class MakeUnescapedData extends Procedure1
{
  public static final MakeUnescapedData unescapedData
    = new MakeUnescapedData();

  public MakeUnescapedData ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.xml.CompileXmlFunctions:validateApplyMakeUnescapedData");
  }

  public Object apply1(Object arg)
  {
    return new UnescapedData(arg == null ? "" : arg.toString());
  }
}
