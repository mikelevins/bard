// Copyright (c) 1997, 2000, 2007  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.util.*;

/**
  * Semi-abstract class object reference types.
  * <p>
  * Extended by ClassType and ArrayType. */

public class ObjectType extends Type
{
  protected ObjectType ()
  {
    size = 4;
  }

  public ObjectType (String name)
  {
    this_name = name;
    size = 4;
  }

  // Miscellaneous bits:
  final static int ADD_FIELDS_DONE  = 1;
  final static int ADD_METHODS_DONE = 2;
  final static int ADD_MEMBERCLASSES_DONE = 4;
  final static int ADD_ENCLOSING_DONE = 8;
  // A ClassType that we can expect to have a corresponding reflectClass.
  final static int EXISTING_CLASS = 16;

  final static int HAS_OUTER_LINK = 32;

  /* */ public int flags;

  public final boolean isExisting()
  {
    Type t = getImplementationType();
    if (t instanceof ArrayType)
      t = ((ArrayType) t).getComponentType();
    if (t == this)
      return (flags & EXISTING_CLASS) != 0;
    else
      return t.isExisting();
  }

  public final void setExisting(boolean existing)
  {
    if (existing) flags |= EXISTING_CLASS;
    else flags &= ~ EXISTING_CLASS;
  }

  /** Returns class name if a class type, signature if an array type.
   * In both cases, uses '/' rather than '.' after packages prefixes.
   * Seems rather arbitrary - but that is how classes are represented
   * in the constant pool (CONSTANT_Class constants).
   * Also, Class.forName is the same, except using '.'.
   */
  public String getInternalName()
  {
    return getName().replace('.', '/');
  }

  /* #ifdef JAVA2 */
  /* #ifndef JAVA5 */
  // static ClassLoader thisClassLoader;
  // static
  // {
  //   try
  //     {
  //       thisClassLoader
  //         = Class.forName("gnu.mapping.ObjectType").getClassLoader();
  //     }
  //   catch (Exception ex)
  //     {
  //     }
  // }
  /* #endif */
  /* #endif */

  /** Get named class using context class loader.
   * If the security policy prohibits that, fall back to this class's loader.
   */
  public static Class getContextClass (String cname)
    throws java.lang.ClassNotFoundException
  {
    return Class.forName(cname, false, getContextClassLoader());
  }

  public static ClassLoader getContextClassLoader ()
  {
    try
      {
        return Thread.currentThread().getContextClassLoader();
      }
    catch (java.lang.SecurityException ex)
      {
        /* The .class syntax below also works for JDK 1.4, but it's just
           syntactic sugar, so there is no benefit in using it. */
        /* #ifdef JAVA5 */
        return ObjectType.class.getClassLoader();
        /* #else */
        // return thisClassLoader;
        /* #endif */
      }
  }

  /** Get the java.lang.Class object for the representation type. */
  public Class getReflectClass()
  {
    try
      {
	if (reflectClass == null)
          reflectClass = getContextClass(getInternalName().replace('/', '.'));
        setExisting(true);
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        if ((flags & EXISTING_CLASS) != 0)
          {
            throw new RuntimeException("no such class: "+getName(), ex);
          }
      }
    return reflectClass;
  }

  public Type promote ()
  {
    return this == nullType ? objectType : this;
  }

  public boolean isInstance (Object obj)
  {
    if (this == nullType)
      return obj == null;
    return super.isInstance(obj);
  }

  public Field getField(String name, int mask)
  {
    return null;
  }

  public Method getMethod(String name, Type[] arg_types)
  {
    return Type.objectType.getMethod(name, arg_types);
  }

  /** @deprecated */
  public final int getMethods (Filter filter, int searchSupers,
                               Vector result, String context)
  {
    return getMethods(filter, searchSupers, result);
  }

    public int getMethods (Filter filter, int searchSupers,
                           List<Method> result) {
        return Type.objectType.getMethods(filter, searchSupers, result);
    }

    public int compare(Type other) {
	if (this == other)
	    return 0;
	else if (this == nullType)
	    return -1;
	else
	    return -3;
    }

  /* #ifdef JAVA5 */
  @SuppressWarnings("unchecked")
  /* #endif */
  /** Convert an object to a value of this Type.
   * Throw a ClassCastException when this is not possible. */
  public Object coerceFromObject (Object obj)
  {
    if (obj != null)
      {
	if (this == Type.toStringType)
	  return obj.toString();
        Class clas = getReflectClass();
        Class objClass = obj.getClass();
        if (! clas.isAssignableFrom(objClass))
          throw new ClassCastException("don't know how to coerce "
                                       + objClass.getName() + " to "
                                       + getName());
      }
    return obj;
  }

  /** Compile (in given method) cast from Object to this Type. */
  public void emitCoerceFromObject (CodeAttr code)
  {
    if (this == Type.toStringType)
      {
	// This would be nice but it doesn't verify, alas!
	// code.reserve(4);
	// code.emitDup();
	// code.put1(198); // ifnull
	// code.put2(6);  // skip after emitInvokeVirtual.
	// code.emitInvokeVirtual(Type.toString_method);
	code.emitDup();
	code.emitIfNull();
	code.emitPop(1);
	code.emitPushNull();
	code.emitElse();
	code.emitInvokeVirtual(Type.toString_method);
	code.emitFi();
      }
    else if (this != Type.objectType)
      code.emitCheckcast(this);
  }
}
