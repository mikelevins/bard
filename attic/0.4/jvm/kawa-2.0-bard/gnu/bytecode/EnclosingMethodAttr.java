// Copyright (c) 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;

public class EnclosingMethodAttr extends Attribute
{
  int class_index;
  int method_index;

  Method method;

  public EnclosingMethodAttr (ClassType cl)
  {
    super("EnclosingMethod");
    addToFrontOf(cl);
  }

  public EnclosingMethodAttr (int class_index, int method_index, ClassType ctype)
  { 
    this(ctype);
    this.class_index = class_index;
    this.method_index = method_index;
  }

  public static EnclosingMethodAttr getFirstEnclosingMethod (Attribute attr)
  {
    for (; ; attr = attr.getNext())
      {
        if (attr == null || attr instanceof EnclosingMethodAttr)
          return (EnclosingMethodAttr) attr;
      }
  }

  public int getLength() { return 4; }

  public void assignConstants (ClassType cl)
  {
    super.assignConstants(cl);
    if (method != null)
      {
        ConstantPool constants = cl.getConstants();
        class_index = constants.addClass(method.getDeclaringClass()).getIndex();
        method_index = constants.addNameAndType(method).getIndex();
      }
  }

  public void write (java.io.DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.writeShort(class_index);
    dstr.writeShort(method_index);
  }

  public void print (ClassTypeWriter dst)
  {
    ClassType ctype = (ClassType) container;
    ConstantPool constants = ctype.getConstants();
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.println(getLength());
    dst.print("  class: ");
    dst.printOptionalIndex(class_index);
    CpoolEntry centry = constants.getForced(class_index, ConstantPool.CLASS);
    dst.print(((CpoolClass) centry).getStringName());
    dst.print(", method: ");
    dst.printOptionalIndex(method_index);
    centry = constants.getForced(method_index, ConstantPool.NAME_AND_TYPE);
    centry.print(dst, 0);
    dst.println();
  }

}
