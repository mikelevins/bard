package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.lists.FString;
import gnu.expr.*;

public class SlotSet extends Procedure3 implements Inlineable
{
  /** True if this is a "static-field" operation. */
  boolean isStatic;

  public static final SlotSet set$Mnfield$Ex = new SlotSet("set-field!", false);
  public static final SlotSet set$Mnstatic$Mnfield$Ex
  = new SlotSet("set-static-field!", true);

  public SlotSet(String name, boolean isStatic)
  {
    super(name);
    this.isStatic = isStatic;
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.reflect.CompileReflect:validateApplySlotSet");
  }

  public static void setField (Object obj, String name, Object value)
  {
    apply(false, obj, name, value);
  }

  public static void setStaticField (Object obj, String name, Object value)
  {
    apply(true, obj, name, value);
  }

  public static void apply (boolean isStatic, Object obj, Object member, Object value)
  {
    Language language = Language.getDefaultLanguage();
    boolean illegalAccess = false;
    String name;
    String fname;
    Class clas;
    if (member instanceof String
        || member instanceof FString
        || member instanceof SimpleSymbol) {
      name = member.toString();
      fname = gnu.expr.Compilation.mangleNameIfNeeded(name);
      clas = isStatic ? SlotGet.coerceToClass(obj) : obj.getClass();
    }
    else {
      fname = name = ((Member) member).getName();
      clas = null;
    }
    try
      {
        java.lang.reflect.Field field
          = member instanceof Field ? ((Field) member).getReflectField()
          : clas.getField(fname);
	Class ftype = field.getType();
        field.set(obj, language.coerceFromObject(ftype, value));
        return;
      }
    catch (java.lang.NoSuchFieldException ex)
      {
      }
    catch (IllegalAccessException ex)
      {
	illegalAccess = true;
      }

    // Try looking for a method "setFname" instead.
    // First look for "getName" or "isName", to get the "field type".
    try
      {
        java.lang.reflect.Method getmethod = null;
    
        boolean haveSetter = member instanceof Method;
	String setName = haveSetter ? fname
          : ClassExp.slotToMethodName("set", name);
        if (haveSetter && ! setName.startsWith("set"))
          haveSetter = false;

        try {
          String getName = haveSetter ? "get" + setName.substring(3)
            : ClassExp.slotToMethodName("get", name);
          getmethod = clas.getMethod(getName, SlotGet.noClasses);
        } catch (Exception getEx) {
          String getName = haveSetter ? "is" + setName.substring(3)
            : ClassExp.slotToMethodName("is", name);
          getmethod = clas.getMethod(getName, SlotGet.noClasses);
        }
        
        Class[] setArgTypes = new Class[1];
        setArgTypes[0] = getmethod.getReturnType();
        java.lang.reflect.Method setmethod
          = clas.getMethod(setName, setArgTypes);
        Object[] args = new Object[1];
        args[0] = language.coerceFromObject(setArgTypes[0], value);
        setmethod.invoke(obj, args);
        return;
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
        WrappedException.rethrow(ex.getTargetException());
      }
    catch (IllegalAccessException ex)
      {
        illegalAccess = true;
      }
    catch (java.lang.NoSuchMethodException ex)
      {
      }

    if (illegalAccess)
      throw new RuntimeException("illegal access for field "+name);
    else
      throw new RuntimeException ("no such field "+name
                                  +" in "+clas.getName());
  }

  public Object apply3 (Object obj, Object fname, Object value)
  {
    apply(isStatic, obj, fname, value);
    return Values.empty;
  }

  static final Type[] type1Array = new Type[1];

  /** Get a setter property - field or 'set' accessor method.
   * @param clas the class type declaring the property.
   * @param name the source (unmangled) name of the property.
   * Note if a method is returned it may not be the most specific/appropriate,
   * since we don't take the setter value into account here.
   * Therefore it is best to just use the method name, and do a second lookup
   * that takes arguments into account, as in CompileReflect#makeSetterCall.
   */
  public static Member
  lookupMember (ObjectType clas, String name, ClassType caller)
  {
    gnu.bytecode.Field field
      = clas.getField(Compilation.mangleNameIfNeeded(name), -1);
    if (field != null)
      {
        if (caller == null)
          caller = Type.pointer_type;
        if (caller.isAccessible(field, clas))
          return field;
      }

    String setName = ClassExp.slotToMethodName("set", name);
    gnu.bytecode.Method method = clas.getMethod(setName, type1Array);
    if (method == null)
      return field;
    else
      return method;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Expression value = args[2];
    Type type = isStatic ? kawa.standard.Scheme.exp2Type(arg0)
      : arg0.getType();
    if (type instanceof ObjectType && arg1 instanceof QuoteExp)
      {
        Object val1 = ((QuoteExp) arg1).getValue();
        ObjectType ctype = (ObjectType) type;
	if (val1 instanceof Field)
	  {
            Field field = (Field) val1;
            CodeAttr code = comp.getCode();
	    int modifiers = field.getModifiers();
	    boolean isStaticField = (modifiers & Access.STATIC) != 0;
	    args[0].compile(comp,
			    isStaticField ? Target.Ignore
			    : Target.pushValue(ctype));
            Type ftype = comp.getLanguage().getLangTypeFor(field.getType());
            args[2].compile(comp, CheckedTarget.getInstance(ftype));
            if (isStaticField)
              code.emitPutStatic(field); 
            else
              code.emitPutField(field);
            comp.compileConstant(Values.empty, target);
	    return;
	  }
      }
    ApplyExp.compile(exp, comp, target);
  }
}
