// Copyright (c) 1998, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
public class InnerClassesAttr  extends Attribute
{
  /** Number of entries. */
  int count;
  short[] data;

  /** Add a new InnerClassesAttr to a ClassType. */
  public InnerClassesAttr (ClassType cl)
  {
    super("InnerClasses");
    addToFrontOf(cl);
  }

  public InnerClassesAttr (short[] data, ClassType cl)
  {
    this(cl);
    count = (short) (data.length >> 2);
    this.data = data;
  }

  public static InnerClassesAttr getFirstInnerClasses (Attribute attr)
  {
    for (; ; attr = attr.getNext())
      {
        if (attr == null || attr instanceof InnerClassesAttr)
          return (InnerClassesAttr) attr;
      }
  }

  /** Add a class from a constant pool entry.
   * Doesn't check for duplicates, but there shouldn't be any
   * duplicates in a constant pool.. */
  void addClass (CpoolClass centry, ClassType owner)
  {
    int i = 4 * count++;
    if (data == null)
      data = new short[16];
    else if (i >= data.length)
      {
        short[] tmp = new short[2*i];
        System.arraycopy(data, 0, tmp, 0, i);
        data = tmp;
      }
    ConstantPool constants = owner.constants;
    ClassType clas = (ClassType) centry.getClassType();

    String name = clas.getSimpleName();
    int name_index = name == null || name.length() == 0 ? 0
      : constants.addUtf8(name).index;
    data[i] = (short) centry.index;
    ClassType outer = clas.getDeclaringClass();
    data[i+1] = outer == null ? 0 :
      (short) constants.addClass(outer).index;
    data[i+2] = (short) name_index;
    int flags = clas.getModifiers();
    flags &= ~ Access.SUPER;
    data[i+3] = (short) flags;
  }

  public void assignConstants (ClassType cl)
  {
    super.assignConstants(cl);
    // Real work handled when addClass is called.
  }

  /** Return the length of the attribute in bytes.
    * Does not include the 6-byte header (for the name_index and the length).*/
    public int getLength() { return 2 + 8 * count; }

  /** Write out the contents of the Attribute.
    * Does not write the 6-byte attribute header. */
  public void write (java.io.DataOutputStream dstr)
    throws java.io.IOException
  {
    dstr.writeShort(count);
    for (int i = 0;  i < count;  i++)
      {
	dstr.writeShort(data[4 * i]);     // inner_class_info_index
	dstr.writeShort(data[4 * i + 1]); // outer_class_info_index
	dstr.writeShort(data[4 * i + 2]); // inner_name_index
	dstr.writeShort(data[4 * i + 3]); // inner_class_access_flags
      }
  }

  public void print (ClassTypeWriter dst) 
  {
    ClassType ctype = (ClassType) container;
    ConstantPool constants = data == null ? null : ctype.getConstants();
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", count: ");
    dst.println(count);
    for (int i = 0;  i < count;  i++)
      {
        int inner_index = constants == null ? 0 : data[4*i] & 0xFFFF;
        CpoolClass centry = constants == null || inner_index == 0 ? null
          : constants.getForcedClass(inner_index);
        ClassType clas = centry != null && centry.clas instanceof ClassType
          ? (ClassType) centry.clas
          : null;
        dst.print(' ');
        int access = inner_index == 0 && clas != null ? clas.getModifiers()
          : data[4*i+3] & 0xFFFF;
        dst.print(Access.toString(access, Access.INNERCLASS_CONTEXT));
        dst.print(' ');
        int index;
        String name;
        if (inner_index == 0 && clas != null)
          name = clas.getSimpleName();
        else
          {
            index = data[4*i+2] & 0xFFFF; // inner_name_index
            if (constants == null || index == 0)
              name = "(Anonymous)";
            else
              {
                dst.printOptionalIndex(index);
                name = ((CpoolUtf8)
                        (constants.getForced(index, ConstantPool.UTF8))).string;
              }
          }
        dst.print(name);
        dst.print(" = ");
        if (centry != null)
          {
            dst.printOptionalIndex(inner_index);
            name = centry.getClassName();
          }
        else
          name = "(Unknown)";
        dst.print(name);
        dst.print("; ");
        if (inner_index == 0 && clas != null)
          {
            String iname = clas.getName();
            int dot = iname.lastIndexOf('.');
            if (dot > 0)
              iname = iname.substring(dot+1);
            int start = iname.lastIndexOf('$') + 1;
            char ch;
            if (start < iname.length()
                && ((ch = iname.charAt(start)) >= '0' && ch <= '9'))
              dst.print("not a member");
            else
              {
                dst.print("member of ");
                dst.print(ctype.getName());
              }
          }
        else
          {
            CpoolEntry oentry;
            index = data[4*i+1] & 0xFFFF; // outer_class_info_index
            if (index == 0)
              dst.print("not a member");
            else
              {
                dst.print("member of ");
                oentry = constants.getForced(index, ConstantPool.CLASS);
                dst.print(((CpoolClass) oentry).getStringName());
              }
          }
	dst.println();
      }
  }
}
