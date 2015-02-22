package gnu.kawa.xml;
import gnu.kawa.reflect.*;
import gnu.bytecode.*;
import gnu.expr.*;

public class NodeSetType extends OccurrenceType
{
  public NodeSetType (Type itemType)
  {
    super(itemType, 0, -1);
  }

  public static Type getInstance (Type base)
  {
    return new NodeSetType(base);
  }

  public String toString ()
  {
    return super.toString()+"node-set";
  }
}
