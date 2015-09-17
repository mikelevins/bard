// Copyright (c) 2001  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;

// Should be moved to some other package?  FIXME

/** A class type implemented as a pair of an interface and a class.
 * This is how true multiple inheritance can be implemented.
 */

public class PairClassType extends ClassType
{
  // FIXME should probably inherit from ObjectType or even Type
  // rather than ClassType, and have an interfaceType field,
  // which getImplementationType would return
  /*
  public ClassType interfaceType;
  public Type getImplementationType()
  {
    return interfaceType;
  }
  String name;
  */

  Object staticLink;

  public ClassType instanceType;

  public PairClassType()
  {
  }

  PairClassType(Class reflectInterface, Class reflectInstanceClass)
  {
    super(reflectInterface.getName());
    setExisting(true);
    reflectClass = reflectInterface;
    Type.registerTypeForClass(reflectInterface, this);
    this.instanceType = (ClassType) Type.make(reflectInstanceClass);
  }

  public static PairClassType make(Class reflectInterface,
				   Class reflectInstanceClass)
  {
    return new PairClassType(reflectInterface, reflectInstanceClass);
  }

  public static PairClassType make(Class reflectInterface,
				   Class reflectInstanceClass,
				   Object staticLink)
  {
    PairClassType type
      = new PairClassType(reflectInterface, reflectInstanceClass);
    type.staticLink = staticLink;
    return type;
  }

  public Object getStaticLink()
  {
    return staticLink;
  }

  /** This method is called from compiled code. */
  public static Object extractStaticLink(ClassType type)
  {
    return ((PairClassType) type).staticLink;
  }
}
