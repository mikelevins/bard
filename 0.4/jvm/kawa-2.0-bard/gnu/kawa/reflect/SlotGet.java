package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangPrimType;

public class SlotGet extends Procedure2
  implements HasSetter, Inlineable
{
  static Class[] noClasses = { };

  /** True if this is a "static-field" operation. */
  boolean isStatic;

  Procedure setter;
  public static final SlotGet field
    = new SlotGet("field", false, SlotSet.set$Mnfield$Ex);
  public static final SlotGet slotRef
    = new SlotGet("slot-ref", false, SlotSet.set$Mnfield$Ex);
  public static final SlotGet staticField
    = new SlotGet("static-field", true, SlotSet.set$Mnstatic$Mnfield$Ex);

  public SlotGet(String name, boolean isStatic)
  {
    super(name);
    this.isStatic = isStatic;
    setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.reflect.CompileReflect:validateApplySlotGet");
  }

  public SlotGet(String name, boolean isStatic, Procedure setter)
  {
    this(name, isStatic);
    this.setter = setter;
  }

  public static Object field(Object obj, String fname)
  {
    return field.apply2(obj, fname);
  }

  public static Object staticField(Object obj, String fname)
  {
    return staticField.apply2(obj, fname);
  }

  public Object apply2 (Object arg1, Object arg2)
  {
    String name, fname;
    String getName = null, isName = null;
    if (arg2 instanceof gnu.bytecode.Field)
      {
        fname = ((gnu.bytecode.Field) arg2).getName();
        name = Compilation.demangleName(fname, true);
      }
    else if (arg2 instanceof gnu.bytecode.ClassType)
      {
        return arg2;
      }
    else if (arg2 instanceof gnu.bytecode.Method)
      {
        String mname = ((gnu.bytecode.Method) arg2).getName();
        name = Compilation.demangleName(mname, false);
        if (mname.startsWith("get"))
          getName = mname;
        else if (mname.startsWith("is"))
          isName = mname;
        fname = null;
      }
    else if (arg2 instanceof SimpleSymbol
             /* #ifdef use:java.lang.CharSequence */
             || arg2 instanceof CharSequence
             /* #else */
             // || arg2 instanceof gnu.lists.CharSeq || arg2 instanceof String
             /* #endif */
             )
      {
        name = arg2.toString();
        fname = gnu.expr.Compilation.mangleNameIfNeeded(name);
      }
    else
      throw new WrongType(this, 2, arg2, "string");
    // "intern" fname if it is "class" or "length":
    if ("class".equals(fname))
      fname = "class";
    else if ("length".equals(fname))
      fname = "length";
    return getSlotValue(isStatic, arg1, name, fname, getName, isName,
                         Language.getDefaultLanguage());
  }

  /** The actual gets of finding the field value.
   * The compiler emits calls to this method if the field name is literal
   * but the actual field is not known at compile time.
   * This speeds lookup a bit.
   * If fname equals "length" or "class", it is assumed to be interned.
   */
  public static Object
  getSlotValue (boolean isStatic, Object obj, String name, String fname,
                String getName, String isName, Language language)
  {
    Class clas = isStatic ? coerceToClass(obj) : obj.getClass();
    if (fname == "length" && clas.isArray())
      {
	int length = java.lang.reflect.Array.getLength(obj);
        /* #ifdef JAVA5 */
        return Integer.valueOf(length);
        /* #else */
        // return new Integer(length);
        /* #endif */
      }
    if (fname == "class")
      return clas;
    boolean illegalAccess = false;
    if (fname != null)
      {
        java.lang.reflect.Field field;
        try
          {
            field = clas.getField(fname);
          }
        catch (Exception ex)
          {
            // Check for a member class.
            Class[] memberClasses = clas.getClasses();
            for (int i = memberClasses.length;  --i >= 0; )
              {
                Class memberClass = memberClasses[i];
                if (memberClass.getSimpleName().equals(fname))
                  return memberClass;
              }
            field = null;
          }
        if (field != null)
          {
            if (isStatic
                && (field.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
              throw new RuntimeException("cannot access non-static field '"
                                         + fname + '\'');
            try
              {
                return language.coerceToObject(field.getType(), field.get(obj));
              }
            catch (IllegalAccessException ex)
              {
                illegalAccess = true;
              }
            catch (Exception ex)
              {
                ex.printStackTrace();  // FIXME?
              }
          }
      }

    // Try looking for a method "getFname" or "isFname" instead:
    try
      {
        String mname = null;
        java.lang.reflect.Method getmethod = null;
        
        try {
          mname = getName != null ? getName
            : ClassExp.slotToMethodName("get", name);
          getmethod = clas.getMethod(mname, noClasses);
        } catch (Exception getEx) {
          mname = isName != null ? isName
            : ClassExp.slotToMethodName("is", name);
          getmethod = clas.getMethod(mname, noClasses);
        }

        if (isStatic
            && (getmethod.getModifiers() & java.lang.reflect.Modifier.STATIC) == 0)
          throw new RuntimeException("cannot call non-static getter method '"
                                     + mname + '\'');
        Object result = getmethod.invoke(obj, Values.noArgs);
        result = language.coerceToObject(getmethod.getReturnType(), result);
        return result;
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
      throw new RuntimeException("illegal access for field "+fname);
    else
      throw new RuntimeException ("no such field "+fname
                                  +" in "+clas.getName());
  }

  static Class coerceToClass(Object obj)
  {
    if (obj instanceof Class)
      return (Class) obj;
    if (obj instanceof gnu.bytecode.Type)
      return ((gnu.bytecode.Type) obj).getReflectClass();
    throw new RuntimeException("argument is neither Class nor Type");
  }

  public void setN (Object[] args)
  {
    int nargs = args.length;
    if (nargs != 3)
      throw new WrongArguments(getSetter(), nargs);
    set2(args[0], args[1], args[2]);
  }

  public void set2 (Object obj, Object name, Object value)
  {
    SlotSet.apply(isStatic, obj, (String) name, value);
  }

  /** Get a named property - field or member class or 'get' accessor method.
   * @param clas the class type declaring the property.
   * @param name the source (unmangled) name of the property.
   */
  public static Member
  lookupMember (ObjectType clas, String name, ClassType caller)
  {
    String mname = Compilation.mangleNameIfNeeded(name);
    Member member = clas.getField(mname, -1);
    if (member == null && clas instanceof ClassType)
      member = ((ClassType) clas).getDeclaredClass(mname);
    if (member != null)
      {
        if (caller == null)
          caller = Type.pointer_type;
        if (caller.isAccessible(member, clas))
          return member;
      }

    // Try looking for a method "getFname" instead:
    String getname = ClassExp.slotToMethodName("get", name);
    gnu.bytecode.Method method = clas.getMethod(getname, Type.typeArray0);
    if (method == null)
      method = clas.getMethod(ClassExp.slotToMethodName("is", name),
                              Type.typeArray0);
    if (method == null)
      return member;
    else
      return method;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    Expression arg0 = args[0];
    Expression arg1 = args[1];
    Language language = comp.getLanguage();
    Type type = isStatic ? language.getTypeFor(arg0)
      : arg0.getType();
    CodeAttr code = comp.getCode();

    if (type instanceof ObjectType && arg1 instanceof QuoteExp)
      {
	ObjectType ctype = (ObjectType) type;
        Object part = ((QuoteExp) arg1).getValue();
        if (part instanceof gnu.bytecode.Field)
          {
            gnu.bytecode.Field field = (gnu.bytecode.Field) part;
            int modifiers = field.getModifiers();
            boolean isStaticField = (modifiers & Access.STATIC) != 0;
            args[0].compile(comp,
                            isStaticField ? Target.Ignore
                            : Target.pushValue(ctype));
            if (isStaticField)
              {
                boolean inlined = false;
                /*
                FIXME This should be done at inline-time, not compile-time.
                See CompileReflect#validateApplySlotGet.
                FIXME This isn't quite safe.  We should only "inline"
                the value if the field whose initializer is a constant
                expression (JLS 2nd ed 15.28).  We cannot determine this
                using reflection instead we have to parse the .class file.

                Type ftype = field.getType();
                if ((modifiers & Access.FINAL) != 0
                    && ftype instanceof PrimType)
                  {
                    // We inline int final fields.
                    // Other kinds of final fields are less obviously a win.
                    char sig = ftype.getSignature().charAt(0);
                    if (sig != 'F' && sig != 'D' && sig != 'J')
                      {
                        try
                          {
                            java.lang.reflect.Field rfield
                              = field.getReflectField();
                            int val = rfield.getInt(null);
                            code.emitPushInt(val);
                            inlined = true;
                          }
                        catch (Exception ex)
                          {
                          }
                      }
                  }
                */
                if (! inlined)
                  code.emitGetStatic(field); 
              }
            else
              code.emitGetField(field);
            Type ftype = language.getLangTypeFor(field.getType());
            target.compileFromStack(comp, ftype);
            return;
          }
        if (part instanceof Method)
          {
            gnu.bytecode.Method method = (gnu.bytecode.Method) part;
	    int modifiers = method.getModifiers();
            boolean isStaticMethod = method.getStaticFlag();
            args[0].compile(comp,
                            isStaticMethod ? Target.Ignore
                            : Target.pushValue(ctype));
            if (isStaticMethod)
              code.emitInvokeStatic(method);
            else
              code.emitInvoke(method);
	    target.compileFromStack(comp, method.getReturnType());
            return;
          }
      }
    String name = ClassMethods.checkName(arg1);
    if (type instanceof ArrayType && "length".equals(name) && ! isStatic)
      {
	args[0].compile(comp, Target.pushValue(type));
	code.emitArrayLength();
	target.compileFromStack(comp, LangPrimType.intType);  // FIXME
	return;
      }
    ApplyExp.compile(exp, comp, target);
  }

  public Procedure getSetter()
  {
    return setter == null ? super.getSetter() : setter;
  }

  /**
   * Convenience method to make an Expression that gets the value of a field.
   * @param value evaluates to object that has the named field
   * @param fieldName name of field in value
   * @return expression that get the name field from value
   */
  public static ApplyExp makeGetField(Expression value, String fieldName)
  {
    Expression[] args = new Expression[2];
    args[0] = value;
    args[1] = new QuoteExp(fieldName);
    return new ApplyExp(gnu.kawa.reflect.SlotGet.field, args);
  }
}
