// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class SingletonType extends ObjectType // implements TypeValue
{
  static final SingletonType instance = new SingletonType("singleton");

  public SingletonType (String name)
  {
    super(name);
  }

  public static final SingletonType getInstance () { return instance; }

  public java.lang.Class getReflectClass()
  {
    return getImplementationType().getReflectClass();
  }

  public Type getImplementationType ()
  {
    return Type.pointer_type;
  }

  public int compare(Type other)
  {
    int otherRange = OccurrenceType.itemCountRange(other);
    int otherMin = otherRange & 0xfff;
    int otherMax = otherRange >> 12;
    if (otherMax == 0 || otherMin > 1)
      return -3;
    if (otherMin == 1 && otherMax == 1)
      return Type.pointer_type.compare(other);
    int cmp = Type.pointer_type.compare(other);
    if (cmp == 0 || cmp == -1)
      return -1;
    return -2;
  }

  public static Object coerceToSingleton (Object obj)
  {
    if (obj instanceof Values)
      obj = ((Values) obj).canonicalize();
    if (obj == null || obj instanceof Values)
      throw new ClassCastException("value is not a singleton");
    return obj;
  }

  public Object coerceFromObject (Object obj)
  {
    return coerceToSingleton(obj);
  }

  public void emitCoerceFromObject (CodeAttr code)
  {
    code.emitInvokeStatic(ClassType.make("gnu.kawa.reflect.SingletonType")
                          .getDeclaredMethod("coerceToSingleton", 1));
  }

  public boolean isInstance (Object obj)
  {
    return obj != null && ! (obj instanceof Values);
  }
}
