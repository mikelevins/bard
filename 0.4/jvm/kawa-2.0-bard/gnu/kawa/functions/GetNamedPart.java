package gnu.kawa.functions;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import gnu.expr.Compilation;
import gnu.expr.Language;

/** Procedure to get the value of a named component of an object. */

public class GetNamedPart extends Procedure2 implements HasSetter
{
  public static final GetNamedPart getNamedPart = new GetNamedPart();
  static {
      getNamedPart.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileNamedPart:validateGetNamedPart");
  }


  /** {@code PREFIX:<>} is equivalent to the {@code ClassType} bound to {@code PREFIX}. */
  public static final String CLASSTYPE_FOR = "<>";

  /** Pseudo-method-name for the cast operation. */
  public static final String CAST_METHOD_NAME = "@";

  /** Pseudo-method-name for class-membership-test (instanceof) operation. */
  public static final String INSTANCEOF_METHOD_NAME = "instance?";

  public Object apply2 (Object container, Object part)
    throws Throwable
  {
    if (container instanceof Values)
      {
        Object[] values = ((Values) container).getValues();
        Values.FromTreeList result = new Values.FromTreeList();
        for (int i = 0;  i < values.length;  i++)
          {
            result.writeObject(apply2(values[i], part));
          }
        return result.canonicalize();
      }
    Symbol sym;
    if (part instanceof Symbol)
      sym = (Symbol) part;
    else
      sym = Namespace.EmptyNamespace.getSymbol(part.toString().intern());
    return getNamedPart(container, sym);
  }

  public static Object getTypePart (Type type, String name)
    throws Exception
  {
    if (name.equals(CLASSTYPE_FOR))
      return type;

    if (type instanceof ObjectType)
      {
        if (name.equals(INSTANCEOF_METHOD_NAME))
          return new NamedPart(type, name, 'I');
        if (name.equals(CAST_METHOD_NAME))
          return new NamedPart(type, name, 'C');
        if (name.equals("new"))
          return new NamedPart(type, name, 'N');
        if (name.equals(".length")
            || (name.length() > 1 && name.charAt(0) == '.'
                && type instanceof ClassType))
          return new NamedPart(type, name, 'D');
      }

    if (type instanceof ClassType)
      {
        try
          {
            return gnu.kawa.reflect.SlotGet.staticField(type, name);
          }
        catch (Exception ex)
          {
            // FIXME!
          }
        return ClassMethods.apply(ClassMethods.classMethods, type, name);
      }
    return getMemberPart(type, name);
  }

  public static Object getNamedPart (Object container, Symbol part)
    throws Exception
  {
    String name = part.getName();
    container = Promise.force(container);
    if (container instanceof HasNamedParts)
      return ((HasNamedParts) container).get(name);
    if (container instanceof Class)
      container = Type.make((Class) container);
    if (container instanceof Package)
      {
        try
          {
            String pname = ((Package) container).getName();
            return ClassType.getContextClass(pname + '.' + name);
          }
        catch (Exception ex)
          {
          }
      }
    if (container instanceof Type)
      return getTypePart((Type) container, name);
    return getMemberPart(container, part.toString());
  }

  public static Object getMemberPart(Object container, String name)
    throws Exception
  {
    try
      {
        return gnu.kawa.reflect.SlotGet.field(container, name);
      }
    catch (Exception ex)
      {
        // FIXME!
      }
    MethodProc methods = ClassMethods.apply((ClassType) ClassType.make(container.getClass()),
                                            Compilation.mangleName(name), '\0',
                                            Language.getDefaultLanguage());
    if (methods != null)
      return new NamedPart(container, name, 'M', methods);
    throw new RuntimeException("no part '"+name+"' in "+container);
  }

  public Procedure getSetter()
  {
    return SetNamedPart.setNamedPart;
  }
}
