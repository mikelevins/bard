// Copyright (c) 1997, 2000, 2003, 2004, 2006, 2009  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;
import gnu.kawa.util.AbstractWeakHashTable;

/** An abstract type as used by both gnu.bytecode and gnu.expr. */

public abstract class Type
/* #ifdef JAVA5 */
 implements java.lang.reflect.Type
/* #endif */
{
  String signature;
  String genericSignature;
  // Fully-qualified name (in external format, i.e. using '.' to separate).
  String this_name;
  /**
   * Nominal unpromoted size in bytes.
   */
  int size;

  ArrayType array_type;

  protected Type () { }

  /** Return Java-level implementation type.
   * The type used to implement types not natively understood by the JVM
   * or the Java language.
   * Usually, the identity function.  However, a language might handle
   * union types or template types or type expressions calculated at
   * run time.  In that case return the type used at the Java level,
   * and known at compile time.
   */
  public Type getImplementationType()
  {
    return this;
  }

    /** Return JVM-level implementation type. */
    public Type getRawType() {
	Type t = getImplementationType();
	if (t != this)
	    t = t.getRawType();
	return t;
    }

    /** If this is a type alias, get the aliased type.
     * This is semi-deprecated.
     */
    public Type getRealType() {
	return this;
    }

    public boolean isInterface() {
        Type raw = getRawType();
        return raw != this && raw.isInterface();
    }

  public boolean isExisting()
  {
    // Overridden in ObjectType.
    return true;
  }

  static ClassToTypeMap mapClassToType;

  /** Maps Java type name (e.g. "java.lang.String[]") to corresponding Type. */
  /* #ifdef JAVA5 */
  static java.util.HashMap<String,Type> mapNameToType;
  /* #else */
  // static java.util.Hashtable mapNameToType;
  /* #endif */

  public static Type lookupType (String name)
  {
    /* #ifdef JAVA5 */
    java.util.HashMap<String,Type> map = mapNameToType;
    synchronized (map) { return map.get(name); }
    /* #else */
    // return (Type) mapNameToType.get(name);
    /* #endif */
  }

  /** Find an Type with the given name, or create a new one.
   * Use this for "library classes", where you need the field/method types,
   * but not one where you are about to generate code for.
   * @param name the name of the class (e..g. "java.lang.String").
   */
  public static Type getType (String name)
  {
    /* #ifdef JAVA5 */
    java.util.HashMap<String,Type> map = mapNameToType;
    /* #else */
    // java.util.Hashtable map = mapNameToType;
    /* #endif */
    synchronized (map)
      {
        Type type = (Type) map.get(name);
        if (type == null)
          {
            if (name.endsWith("[]"))
              type = ArrayType.make(name);
            else
              {
                ClassType cl = new ClassType(name);
                cl.setExisting(true);
                type = cl;
              }
            map.put(name, type);
          }
        return type;
      }
  }

  /** Register that the Type for class is type. */
  public synchronized static void registerTypeForClass(Class clas, Type type)
  {
    ClassToTypeMap map = mapClassToType;
    if (map == null)
      mapClassToType = map = new ClassToTypeMap();
    type.reflectClass = clas;
    map.put(clas, type);
  }

    /** Try to map java.lang.reflect.Type to gnu.bytecode.Type.
     * If we can't handle that, resolve the Class instead.
     */
    public static Type make(Class reflectClass, java.lang.reflect.Type type) {
	Type t = make(type);
	return t != null ? t : make(reflectClass);
    }

    /** Resolve a java.lang.reflect.Type to gnu.bytecode.Type.
     * Handles simple parameterized types, but does not handle type variables.
     * @return a translated Type, or null for unhandled types.
     */
    static Type make(java.lang.reflect.Type type) {
	if (type instanceof Class)
	    return make((Class) type);
	if (type instanceof java.lang.reflect.GenericArrayType)
	    return null;
	if (type instanceof java.lang.reflect.ParameterizedType) {
	    java.lang.reflect.ParameterizedType ptype
		= (java.lang.reflect.ParameterizedType) type;
	    java.lang.reflect.Type typeArguments[]
		= ptype.getActualTypeArguments();
	    Type rt = Type.make(ptype.getRawType());
	    if (rt instanceof ClassType) {
		ClassType rawType = (ClassType) rt;
		int nargs = typeArguments.length;
		Type[] typeArgumentTypes = new Type[nargs];
		char[] bounds = new char[nargs];
		for (int i = 0;  i < nargs;  i++) {
		    java.lang.reflect.Type ti = typeArguments[i];
		    if (ti instanceof java.lang.reflect.WildcardType) {
			java.lang.reflect.WildcardType wi =
			    (java.lang.reflect.WildcardType) ti;
			java.lang.reflect.Type[] lower = wi.getLowerBounds();
			java.lang.reflect.Type[] upper = wi.getUpperBounds();
			if (lower.length + upper.length != 1)
			    return null;
			else if (lower.length == 1) {
			    bounds[i] = '-';
			    ti = lower[0];
			}
			else /* if (upper.length == 1) */ {
			    bounds[i] = '+';
			    ti = upper[0];
			}
		    }
		    typeArgumentTypes[i] = Type.make(ti);
		}
		ParameterizedType ret = new ParameterizedType(rawType, typeArgumentTypes);
		ret.setTypeArgumentBounds(bounds);
		return ret;
	    }
	}
	if (type instanceof java.lang.reflect.TypeVariable) {
	    return TypeVariable.make((java.lang.reflect.TypeVariable) type);
	}
	return null;
    }

  public synchronized static Type make(Class reflectClass)
  {
    Type type;
    
    if (mapClassToType != null)
      {
	Type t = mapClassToType.get(reflectClass);
	if (t != null)
	  return t;
      }
    if (reflectClass.isArray())
      type = ArrayType.make(Type.make(reflectClass.getComponentType()));
    else if (reflectClass.isPrimitive())
      throw new Error("internal error - primitive type not found");
    else
      {
	String name = reflectClass.getName();
        /* #ifdef JAVA5 */
        java.util.HashMap<String,Type> map = mapNameToType;
        /* #else */
        // java.util.Hashtable map = mapNameToType;
        /* #endif */
        synchronized (map)
          {
            type = (Type) map.get(name);
            if (type == null
                || (type.reflectClass != reflectClass
                    && type.reflectClass != null))
              {
                ClassType cl = new ClassType(name);
                cl.setExisting(true);
                type = cl;
                mapNameToType.put(name, type);
              }
          }
      }
    registerTypeForClass(reflectClass, type);
    return type;
  }

    public String getSignature() { return signature; }
    protected void setSignature(String sig) { this.signature = sig; }
    public String getGenericSignature() { return genericSignature; }
    protected void setGenericSignature(String sig) { this.genericSignature = sig; }
    public String getMaybeGenericSignature() {
	String s = getGenericSignature();
	return s != null ? s : getSignature();
    }

  Type (String nam, String sig) {
    this_name = nam;
    signature = sig;
  }

  public Type (Type type)
  {
    this_name = type.this_name;
    signature = type.signature;
    size = type.size;
    reflectClass = type.reflectClass;
  }

  public Type promote () {
    return size < 4 ? intType : this;
  }

  public final int getSize() { return size; }
  public int getSizeInWords () { return size > 4 ? 2 : 1; }

  public final boolean isVoid () { return size == 0; }

  /** Returns the primitive type corresponding to a signature character.
   * @return a primitive type, or null if there is no such type. */
  public static PrimType signatureToPrimitive(char sig)
  {
    switch(sig)
      {
      case 'B':  return Type.byteType;
      case 'C':  return Type.charType;
      case 'D':  return Type.doubleType;
      case 'F':  return Type.floatType;
      case 'S':  return Type.shortType;
      case 'I':  return Type.intType;
      case 'J':  return Type.longType;
      case 'Z':  return Type.booleanType;
      case 'V':  return Type.voidType;
      }
    return null;
  }

  /** Get a Type corresponding to the given signature string. */
  public static Type signatureToType(String sig, int off, int len)
  {
    if (len == 0)
      return null;
    char c = sig.charAt(off);
    Type type;
    if (len == 1)
      {
	type = signatureToPrimitive(c);
	if (type != null)
	  return type;
      }
    if (c == '[')
      {
	type = signatureToType(sig, off+1, len-1);
	return type == null ? null : ArrayType.make(type);
      }
    if (c == 'L' && len > 2 && sig.indexOf(';', off) == len-1+off)
      return ClassType.make(sig.substring(off+1,len-1+off).replace('/', '.'));
    return null;
  }

  /** Get a Type corresponding to the given signature string. */
  public static Type signatureToType(String sig)
  {
    return signatureToType(sig, 0, sig.length());
  }

  public static void printSignature (String sig, int off, int len,
                                     java.io.PrintWriter out)
  {
    if (len == 0)
      return;
    char c = sig.charAt(off);
    Type type;
    if (len == 1)
      {
	type = signatureToPrimitive(c);
	if (type != null)
	  out.print(type.getName());
      }
    else if (c == '[')
      {
        printSignature(sig, off+1, len-1, out);
        out.print("[]");
      }
    else if (c == 'L' && len > 2 && sig.indexOf(';', off) == len-1+off)
      out.print(sig.substring(off+1,len-1+off).replace('/', '.'));
    else
      out.append(sig, off, len-off);
  }

  /** Return the length of the signature starting at a given string position.
   * Returns -1 for an invalid signature. */
  public static int signatureLength (String sig, int pos)
  {
    int len = sig.length();
    if (len <= pos)
      return -1;
    char c = sig.charAt(pos);
    int arrays = 0;
    while (c == '[')
      {
	arrays++;
	pos++;
	c = sig.charAt(pos);
      }
    if (signatureToPrimitive(c) != null)
      return arrays+1;
    if (c == 'L')
      {
	int end = sig.indexOf(';', pos);
	if (end > 0)
	  return arrays + end + 1 - pos;
      }
    return -1;
  }

  public static int signatureLength (String sig)
  {
    return signatureLength(sig, 0);
  }

  /** Returns the Java-level type name from a given signature.
   * Returns null for an invalid signature. */
  public static String signatureToName(String sig)
  {
    int len = sig.length();
    if (len == 0)
      return null;
    char c = sig.charAt(0);
    Type type;
    if (len == 1)
      {
	type = signatureToPrimitive(c);
	if (type != null)
	  return type.getName();
      }
    if (c == '[')
      {
	int arrays = 1;
	if (arrays < len && sig.charAt(arrays) == '[')
	  arrays++;
	sig = signatureToName(sig.substring(arrays));
	if (sig == null)
	  return null;
	StringBuffer buf = new StringBuffer(50);
	buf.append(sig);
	while (--arrays >= 0)
	  buf.append("[]");
	return buf.toString();
      }
    if (c == 'L' && len > 2 && sig.indexOf(';') == len-1)
      return sig.substring(1,len-1).replace('/', '.');
    return null;
  }

  public String getName ()
  {
    return this_name;
  }

  protected void setName (String name)
  {
    this_name = name;
  }

  public static boolean isValidJavaTypeName (String name)
  {
    boolean in_name = false;
    int i;
    int len = name.length();
    while (len > 2 && name.charAt(len-1) == ']'
	   && name.charAt(len-2) == '[')
      len -= 2;
    for (i = 0;  i < len; i++)
      {
	char ch = name.charAt(i);
	if (ch == '.')
	  {
	    if (in_name)
	      in_name = false;
	    else
	      return false;
	  }
	else if (in_name ? Character.isJavaIdentifierPart(ch)
		 : Character.isJavaIdentifierStart(ch))
	  in_name = true;
	else
	  return false;
      }
    return i == len;
  }

  public boolean isInstance (Object obj)
  {
    return getReflectClass().isInstance(obj);
  }

  /** Return true if this is a "subtype" of other. */
  public final boolean isSubtype (Type other)
  {
    int comp = compare(other);
    return comp == -1 || comp == 0;
  }

    /** If this is the target type, is a given source type compatible?
     * Return -1 if no; 1 if yes; 0 if need to check at run-time.
     * @return -1 if not compatible; 0 if need to check at run-time;
     *   1 if compatible; 2 if compatible and no conversion or cast needed.
     */
    public int isCompatibleWithValue(Type valueType) {
        if (this == toStringType)
            return valueType == javalangStringType ? 2 : 1;
        // Hack for LangPrimType.charType.
        if (this == charType && valueType.getImplementationType()== this)
            return 2;
        int comp = compare(valueType);
        return comp >= 0 ? 1 : comp == -3 ? -1 : 0;
    }

  /**
   * Computes the common supertype
   *
   * Interfaces are not taken into account.
   * This would be difficult, since interfaces allow multiple-inheritance.
   * This means that there may exists multiple common supertypes
   * to t1 and t2 that are not comparable.
   *
   * @return the lowest type that is both above t1 and t2,
   *  or null if t1 and t2 have no common supertype.
   */
  public static Type lowestCommonSuperType(Type t1, Type t2)
  {
    if (t1 == neverReturnsType)
      return t2;
    if (t2 == neverReturnsType)
      return t1;
    if (t1 == null || t2 == null)
      return null;
    if (t1 == t2)
      return t1;
    if (t1 instanceof PrimType && t2 instanceof PrimType)
      {
        t1 = ((PrimType) t1).promotedType();
        t2 = ((PrimType) t2).promotedType();
        return t1 == t2 ? t1 : null;
      }
    if (t1.isSubtype(t2))
      return t2;
    else if (t2.isSubtype(t1))
      return t1;
    else
      {
       // the only chance left is that t1 and t2 are ClassTypes.
       if (!(t1 instanceof ClassType && t2 instanceof ClassType))
         return Type.objectType;
       ClassType c1 = (ClassType) t1;
       ClassType c2 = (ClassType) t2;
       if (! c1.isInterface() && ! c2.isInterface())
         {
           ClassType s1 = c1.getSuperclass();
           ClassType s2 = c2.getSuperclass();
           if (s1 != null && s2 != null)
             return lowestCommonSuperType(s1, s2);
         }
      }
    return Type.objectType;
  }

  /** Return a numeric code showing "subtype" relationship:
   *  1: if other is a pure subtype of this;
   *  0: if has the same values;
   * -1: if this is a pure subtype of other;
   * -2: if they have values in common but neither is a subtype of the other;
   * -3: if the types have no values in common.
   * "Same values" is rather loose;  by "A is a subtype of B"
   * we mean that all instance of A can be "widened" to B.
   * More formally, A.compare(B) returns:
   *  1: all B values can be converted to A without a coercion failure
   *     (i.e. a ClassCastException or overflow or major loss of information),
   *     but not vice versa.
   *  0: all A values can be converted to B without a coercion failure
   *     and vice versa;
   * -1: all A values can be converted to B without a coercion failure
   *     but not vice versa;
   * -2: there are (potentially) some A values that can be converted to B,
   *     and some B values can be converted to A;
   * -3: there are no A values that can be converted to B, and neither
   *     are there any B values that can be converted to A.
   */
  public abstract int compare(Type other);

  /** Change result from compare to compensate for argument swapping. */
  protected static int swappedCompareResult(int code)
  {
    return code == 1 ? -1 : code == -1 ? 1 : code;
  }

  /** Return true iff t1[i].isSubtype(t2[i]) for all i. */
  public static boolean isMoreSpecific (Type[] t1, Type[] t2)
  {
    if (t1.length != t2.length)
      return false;
    for (int i = t1.length; --i >= 0; )
      {
	if (! t1[i].isSubtype(t2[i]))
	  return false;
      }
    return true;
  }

  public void emitIsInstance (CodeAttr code)
  {
    code.emitInstanceof(this);
  }

    /** Convert an object to a value of this Type.
     * The result is actually of the implementation type, boxed as appropriate,
     * so it is suitable for standard reflective operations.
     * Throw a ClassCastException when this is not possible.
     */
    public abstract Object coerceFromObject (Object obj);

    /** Given a raw JVM value convert it to an object of this type.
     * I.e. the argument is an object of the type returned by
     * {@code getRawType()}, boxed as needed.  The result may be
     * a language-specific (boxed) value. Generally a no-op.
     */
    public Object coerceToObject(Object obj) {
        return obj;
    }

  /** Convert from stackType (usually PrimType) to this type.
   * However, we might only convert part-way, to some object type.
   * If converting to this type might fail at run-time, only convert
   * to Object (as by emitCoerceToObject); a caller can use
   * {@code stackType.emitConvertFromObject} to convert the rest,
   * but that might throw an exception. (This is a bit of a kludge.)
   */
  public void emitConvertFromPrimitive (Type stackType, CodeAttr code)
  {
    stackType.emitCoerceToObject(code);
  }

  /** Compile code to convert a object of this type on the stack to Object. */
  public void emitCoerceToObject (CodeAttr code)
  {
  }

  /** Compile code to coerce/convert from Object to this type. */
  public void emitCoerceFromObject (CodeAttr code)
  {
    throw new Error ("unimplemented emitCoerceFromObject for "+this);
  }

  public static final PrimType byteType
    = new PrimType ("byte", "B", 1, java.lang.Byte.TYPE);
  public static final PrimType shortType
    = new PrimType ("short", "S", 2, java.lang.Short.TYPE);
  public static final PrimType intType
    = new PrimType ("int", "I", 4, java.lang.Integer.TYPE);
  public static final PrimType longType
    = new PrimType ("long", "J", 8, java.lang.Long.TYPE);
  public static final PrimType floatType
    = new PrimType ("float", "F", 4, java.lang.Float.TYPE);
  public static final PrimType doubleType
    = new PrimType ("double", "D", 8, java.lang.Double.TYPE);
  public static final PrimType booleanType
    = new PrimType ("boolean", "Z", 1, java.lang.Boolean.TYPE);
  public static final PrimType charType
    = new PrimType ("char", "C", 2, java.lang.Character.TYPE);
  public static final PrimType voidType
    = new PrimType ("void", "V", 0, java.lang.Void.TYPE);

  /* @deprecated */  public static final PrimType byte_type = byteType;
  /* @deprecated */  public static final PrimType short_type = shortType;
  /* @deprecated */  public static final PrimType int_type = intType;
  /* @deprecated */  public static final PrimType long_type = longType;
  /* @deprecated */  public static final PrimType float_type = floatType;
  /* @deprecated */  public static final PrimType double_type = doubleType;
  /* @deprecated */  public static final PrimType boolean_type = booleanType;
  /* @deprecated */  public static final PrimType char_type = charType;
  /* @deprecated */  public static final PrimType void_type = voidType;

  static
  {
    /* #ifdef JAVA5 */
    mapNameToType = new java.util.HashMap<String,Type>();
    /* #else */
    // mapNameToType = new java.util.Hashtable();
    /* #endif */
	mapNameToType.put("byte",    byteType);
	mapNameToType.put("short",   shortType);
	mapNameToType.put("int",     intType);
	mapNameToType.put("long",    longType);
	mapNameToType.put("float",   floatType);
	mapNameToType.put("double",  doubleType);
	mapNameToType.put("boolean", booleanType);
	mapNameToType.put("char",    charType);
	mapNameToType.put("void",    voidType);
  }

    /** The return type of an expression that never returns, such as a throw. */
    public static final Type neverReturnsType
        = ClassType.make("gnu.bytecode.Type$NeverReturns");

  public static final ClassType javalangObjectType
    = ClassType.make("java.lang.Object");
  public static final ClassType objectType = javalangObjectType;
  public static final ClassType javalangBooleanType
    = ClassType.make("java.lang.Boolean");
  public static final ClassType javalangCharacterType
    = ClassType.make("java.lang.Character");
  public static final ClassType javalangThrowableType
    = ClassType.make("java.lang.Throwable");
  public static final ClassType javalangannotationAnnotationType
    = ClassType.make("java.lang.annotation.Annotation");
  public static final Type[] typeArray0 = new Type[0];
  public static final Method toString_method
    = objectType.getDeclaredMethod("toString", 0);
  public static final ClassType javalangNumberType
    = ClassType.make("java.lang.Number");
  public static final Method clone_method
    = Method.makeCloneMethod(objectType);
  public static final Method intValue_method
    = javalangNumberType.addMethod("intValue", typeArray0,
			    intType, Access.PUBLIC);
  public static final Method longValue_method
    = javalangNumberType.addMethod("longValue", typeArray0,
			    longType, Access.PUBLIC);
  public static final Method floatValue_method
    = javalangNumberType.addMethod("floatValue", typeArray0,
			    floatType, Access.PUBLIC);
  public static final Method doubleValue_method
    = javalangNumberType.addMethod("doubleValue", typeArray0,
			    doubleType, Access.PUBLIC);
  public static final Method booleanValue_method
    = javalangBooleanType.addMethod("booleanValue", typeArray0,
			      booleanType, Access.PUBLIC);
  public static final ClassType javalangClassType
    = ClassType.make("java.lang.Class");

    public static final ClassType javalanginvokeMethodHandleType
        = ClassType.make("java.lang.invoke.MethodHandle");

    /** The magic type of null. */
    public static final ObjectType nullType
    //= new ObjectType("(type of null)");
    = new SpecialObjectType("(type of null)", objectType);
    public static final ObjectType errorType = new ObjectType("(error type)");

    static public ClassType javalangStringType = ClassType.make("java.lang.String");
    /* The String type. but coercion is handled by toString. */
    public static final ObjectType toStringType
        = new SpecialObjectType("String", javalangStringType);
    //= new ClassType("java.lang.String");

  /** @deprecated */
  public static final ClassType pointer_type = javalangObjectType;
  /** @deprecated */
  public static final ClassType string_type = javalangStringType;
  /** @deprecated */
  public static final ObjectType tostring_type = toStringType;
  /** @deprecated */
  public static final ClassType java_lang_Class_type = javalangClassType;
  /** @deprecated */
  public static final ClassType boolean_ctype = javalangBooleanType;
  /** @deprecated */
  public static final ClassType throwable_type = javalangThrowableType;
  /** @deprecated */
  public static final ClassType number_type = javalangNumberType;
  
  protected Class reflectClass;

  /** Get the java.lang.Class object for the representation type. */
  public java.lang.Class getReflectClass()
  {
    return reflectClass;
  }

  public void setReflectClass(java.lang.Class rclass)
  {
    reflectClass = rclass;
  }

  public String toString()
  {
    return "Type " + getName();
  }

  public int hashCode()
  {
    String name = toString();
    return name == null ? 0 : name.hashCode ();
  }

    /** A marker class, used for {@code Type.neverReturnsType}. */
    public static class NeverReturns {
        private NeverReturns() { }
    }

  static class ClassToTypeMap extends AbstractWeakHashTable<Class,Type>
  {
    protected Class getKeyFromValue (Type type)
    {
      return type.reflectClass;
    }

    protected boolean matches (Class oldValue, Class newValue)
    {
      // Minor optimization.
      return oldValue == newValue;
    }
  }
}
