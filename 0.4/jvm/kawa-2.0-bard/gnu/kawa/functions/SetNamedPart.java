package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;

/** Procedure to get the value of a named component of an object. */

public class SetNamedPart extends Procedure3 implements HasSetter
{
  public static final SetNamedPart setNamedPart = new SetNamedPart();
  static {
    setNamedPart.setName("setNamedPart");
    setNamedPart.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileNamedPart:validateSetNamedPart");
  }

  public Object apply3 (Object container, Object part, Object value)
  {
    /*
    if (container implements HasNamedParts)
      return ((HasNamedParts) container).getNamedPart(part);
    */
    if (container instanceof Namespace)
      {
        Namespace ns = (Namespace) container;
        String uri = ns.getName();
        if (uri.startsWith("class:"))
          container = ClassType.make(uri.substring(6));
        else
          {
            Symbol sym = ns.getSymbol(part.toString());
            Environment env = Environment.getCurrent();
            Environment.getCurrent().put(sym, value);
            return Values.empty;
          }
      }
    if (container instanceof Class)
      container = (ClassType) Type.make((Class) container);
    if (container instanceof ClassType)
      {
        try
          {
            gnu.kawa.reflect.SlotSet.setStaticField(container, part.toString(), value);
            return Values.empty;
          }
        catch (Exception ex)
          {
            // FIXME!
          }
      }

    gnu.kawa.reflect.SlotSet.setField(container, part.toString(), value); 
    return Values.empty;
  }
}
