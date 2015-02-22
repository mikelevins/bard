package gnu.bytecode;

public class PrimType extends Type {

  public PrimType (String nam, String sig, int siz, Class reflectClass) {
    super(nam, sig);
    size = siz;
    this.reflectClass = reflectClass;
    Type.registerTypeForClass(reflectClass, this);
  }

  protected PrimType(PrimType type)
  {
    super(type.this_name, type.signature);
    size = type.size;
    reflectClass = type.reflectClass;
  }

  public Object coerceFromObject (Object obj)
  {
    if (obj.getClass() == reflectClass)
      return obj;
    char sig1 = (signature == null || signature.length() != 1) ? ' '
      : signature.charAt(0);
    /* #ifdef JAVA5 */
    switch (sig1)
      {
      case 'B':  return Byte.valueOf(((Number) obj).byteValue());
      case 'S':  return Short.valueOf(((Number) obj).shortValue());
      case 'I':  return Integer.valueOf(((Number) obj).intValue());
      case 'J':  return Long.valueOf(((Number) obj).longValue());
      case 'F':  return Float.valueOf(((Number) obj).floatValue());
      case 'D':  return Double.valueOf(((Number) obj).doubleValue());
      case 'Z':  return Boolean.valueOf(((Boolean) obj).booleanValue());
      }
    /* #else */
    // switch (sig1)
    //   {
    //   case 'B':  return new Byte(((Number) obj).byteValue());
    //   case 'S':  return new Short(((Number) obj).shortValue());
    //   case 'I':  return new Integer(((Number) obj).intValue());
    //   case 'J':  return new Long(((Number) obj).longValue());
    //   case 'F':  return new Float(((Number) obj).floatValue());
    //   case 'D':  return new Double(((Number) obj).doubleValue());
    //   case 'Z':  return ((Boolean) obj).booleanValue() ? Boolean.TRUE : Boolean.FALSE;
    //   }
    /* #endif */
    throw new ClassCastException("don't know how to coerce "
				 + obj.getClass().getName() + " to "
				 + getName());
  }

    public Object convertToRaw(Object obj) {
        return obj;
    }

  /** Coerce value to a char.
   * Only defined if getSignature() is "C". */
  public char charValue (Object value)
  {
    return ((Character) value).charValue();
  }

  /** Coerce value to a boolean.
   * Only defined if getSignature() is "Z". */
  public static boolean booleanValue (Object value)
  {
    return ! (value instanceof Boolean) || ((Boolean) value).booleanValue();
  }

  public ClassType boxedType ()
  {
    char sig1 = getSignature().charAt(0);
    String cname;
    switch (sig1)
      {
      case 'Z':  cname = "java.lang.Boolean";   break;
      case 'C':  cname = "java.lang.Character"; break;
      case 'B':  cname = "java.lang.Byte";      break;
      case 'S':  cname = "java.lang.Short";     break;
      case 'I':  cname = "java.lang.Integer";   break;
      case 'J':  cname = "java.lang.Long";      break;
      case 'F':  cname = "java.lang.Float";     break;
      case 'D':  cname = "java.lang.Double";    break;
      case 'V':  cname = "java.lang.Void";    break;
      default:   cname = null; // Should never happen.
      }
    return ClassType.make(cname);
 }

  public static PrimType unboxedType(Type type)
  {
    if (type instanceof PrimType)
      return (PrimType) type;
    if (!(type instanceof ClassType))
      return null;
    String name = type.getName();
    if ("java.lang.Boolean".equals(name))
      return Type.booleanType;
    else if ("java.lang.Character".equals(name))
      return Type.charType;
    else if ("java.lang.Byte".equals(name))
      return Type.byteType;
    else if ("java.lang.Short".equals(name))
      return Type.shortType;
    else if ("java.lang.Integer".equals(name))
      return Type.intType;
    else if ("java.lang.Long".equals(name))
      return Type.longType;
    else if ("java.lang.Float".equals(name))
      return Type.floatType;
    else if ("java.lang.Double".equals(name))
      return Type.doubleType;
    else if ("java.lang.Void".equals(name))
      return Type.voidType;
    else return null;
  }

  public void emitCoerceToObject (CodeAttr code)
  {
    char sig1 = getSignature().charAt(0);
    ClassType clas = boxedType();
    if (sig1 == 'Z')
      {
	code.emitIfIntNotZero();
	code.emitGetStatic(clas.getDeclaredField("TRUE"));
	code.emitElse();
	code.emitGetStatic(clas.getDeclaredField("FALSE"));
	code.emitFi();
	return;
      }
    Method method;
    Type[] args = new Type[1];
    args[0] = this;
    if (code.getMethod().getDeclaringClass().classfileFormatVersion >= ClassType.JDK_1_5_VERSION)
        method = clas.getDeclaredMethod("valueOf", args);
    else
      {
        method = clas.getDeclaredMethod("<init>", args);
        code.emitNew(clas);
        code.emitDupX();
        code.emitSwap();
      }
    code.emitInvoke(method);
  }

  public void emitIsInstance (CodeAttr code)
  {
    char sig1 = (signature == null || signature.length() != 1) ? ' '
      : signature.charAt(0);
    if (sig1 == 'Z')  // boolean
      javalangBooleanType.emitIsInstance(code);
    else if (sig1 == 'V')
      {
	code.emitPop(1);
	code.emitPushInt(1);
      }
    // Have left out Character -> char, since not used by Kawa.
    else
      javalangNumberType.emitIsInstance(code);
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    char sig1 = (signature == null || signature.length() != 1) ? ' '
      : signature.charAt(0);
    if (sig1 == 'Z')  // boolean
      {
	code.emitCheckcast(javalangBooleanType);
	code.emitInvokeVirtual(booleanValue_method);
      }
    else if (sig1 == 'V')
      code.emitPop(1);
    else
      {
	code.emitCheckcast(javalangNumberType);
	if (sig1 == 'I' || sig1 == 'S' || sig1 == 'B')
	  code.emitInvokeVirtual(intValue_method);
	else if (sig1 == 'J')
	  code.emitInvokeVirtual(longValue_method);
	else if (sig1 == 'D')
	  code.emitInvokeVirtual(doubleValue_method);
	else if (sig1 == 'F')
	  code.emitInvokeVirtual(floatValue_method);
	// Have left out Character -> char, since not used by Kawa.
	else
	  super.emitCoerceFromObject(code);
      }
  }

  public static int compare(PrimType type1, PrimType type2)
  {
    char sig1 = type1.signature.charAt(0);
    char sig2 = type2.signature.charAt(0);

    if (sig1 == sig2)
      return 0;

    // Anything can be converted to void, but not vice versa.
    if (sig1 == 'V')
      return 1;
    if (sig2 == 'V')
      return -1;

    // In Java, no other type can be converted to/from boolean.
    // Other languages, including C and Scheme are different:
    // "everything" can be converted to a boolean.
    if (sig1 == 'Z' || sig2 == 'Z')
      return -3;

    if (sig1 == 'C')
      return type2.size > 2 ? -1 : -3;
    if (sig2 == 'C')
      return type1.size > 2 ? 1 : -3;

    if (sig1 == 'D')
      return 1;
    if (sig2 == 'D')
      return -1;
    if (sig1 == 'F')
      return 1;
    if (sig2 == 'F')
      return -1;
    if (sig1 == 'J')
      return 1;
    if (sig2 == 'J')
      return -1;
    if (sig1 == 'I')
      return 1;
    if (sig2 == 'I')
      return -1;
    if (sig1 == 'S')
      return 1;
    if (sig2 == 'S')
      return -1;
    // Can we get here?
    return -3;
  }

  public Type promotedType ()
  {
    switch (signature.charAt(0))
      {
      case 'B': case 'S':  case 'I':  case 'Z':  case 'C':
        return Type.intType;
      default:
        return getImplementationType();
      }
  }

  /** An encoding of the subset/priority order of Number class.
   * Each entry is a letter, then ':', then a class name, then ';'.
   * Classes with letters later in alphabet are more general.
   */
  private static final String numberHierarchy =
    "A:java.lang.Byte;" +
    "B:java.lang.Short;" +
    "C:java.lang.Integer;" +
    "D:java.lang.Long;" +
    "E:gnu.math.IntNum;E:java.gnu.math.BitInteger;" +
    "G:gnu.math.RatNum;" +
    "H:java.lang.Float;" +
    "I:java.lang.Double;I:gnu.math.DFloNum;" +
    "J:gnu.math.RealNum;" +
    "K:gnu.math.Complex;" +
    "L:gnu.math.Quantity;" +
    "K:gnu.math.Numeric;" +
    "N:java.lang.Number;";

  /** Map a class name to a priority letter, using the numberHierarchy table. */
  private static char findInHierarchy (String cname)
  {
    int pos = numberHierarchy.indexOf(cname) - 2;
    return pos < 0 ? '\0' : numberHierarchy.charAt(pos);
  }

  public int compare(Type other)
  {
    if (other instanceof PrimType)
      {
        // Catches the case that other is a gnu.kawa.listpexpr.LangPrimType,
        // in which case it's best that LangPrimType's compare handle it.
        // For example intType.compare(Scheme.booleanType) should be -1.
        if (other.getImplementationType() != other)
          return swappedCompareResult(other.compare(this));
        return compare(this, (PrimType) other);
      }
    if (! (other instanceof ClassType))
      {
        if (other instanceof ArrayType)
          return -3;
        else
          return swappedCompareResult(other.compare(this));
      }
    char sig1 = signature.charAt(0);
    String otherName = other.getName();
    if (otherName == null)
       return -1;
    char thisPriority = '\0';
    switch (sig1)
      {
      case 'V':
        return 1;
      case 'Z':
        if (otherName.equals("java.lang.Boolean"))
          return 0;
      case 'C':
        if (otherName.equals("java.lang.Character"))
          return 0;
        break;
      case 'B': thisPriority = 'A'; break;
      case 'S': thisPriority = 'B'; break;
      case 'I': thisPriority = 'C'; break;
      case 'J': thisPriority = 'D'; break;
      case 'F': thisPriority = 'H'; break;
      case 'D': thisPriority = 'I'; break;
      }
    if (thisPriority != '\0')
      {
        char otherPriority = findInHierarchy(otherName);
        if (otherPriority != '\0')
          return otherPriority == thisPriority ? 0
            : otherPriority < thisPriority ? 1 : -1;
      }
    if (otherName.equals("java.lang.Object")
	|| other == toStringType)
      return -1;
    return -3;
  }
}
