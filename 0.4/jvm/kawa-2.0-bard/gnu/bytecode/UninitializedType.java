// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

/** A pseudo-type used for allocated but uninitialized objects. */

public class UninitializedType extends ObjectType
{
  ClassType ctype;
  /** If non-null, the location of the 'new' instruction. */
  Label label;

  UninitializedType (ClassType ctype)
  {
    super(ctype.getName());
    setSignature(ctype.getSignature());
    this.ctype = ctype;
  }

  UninitializedType (ClassType ctype, Label label)
  {
    this(ctype);
    this.label = label;
  }

  static UninitializedType uninitializedThis (ClassType ctype)
  {
    return new UninitializedType(ctype);
  }

  public Type getImplementationType()
  {
    return ctype;
  }

  public String toString()
  {
    return "Uninitialized<" + ctype.getName() + '>';
  }
}
