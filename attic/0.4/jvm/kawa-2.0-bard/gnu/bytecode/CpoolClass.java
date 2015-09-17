// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_Class entry in the constant pool. */

public class CpoolClass extends CpoolEntry {
  CpoolUtf8 name;
  ObjectType clas;

  CpoolClass () { }

  CpoolClass (ConstantPool cpool, int hash, CpoolUtf8 n)
  {
    super (cpool, hash);
    name = n;
  }

  public int getTag() { return ConstantPool.CLASS; }

  public final CpoolUtf8 getName()
  {
    return name;
  }

  /** Get name of the class as a String. */
  public final String getStringName()
  {
    return name.string;
  }

  public final String getClassName()
  {
    return name.string.replace('/', '.');
  }

  /** Get corresponding ObjectType (ClassType or ArrayType). */
  public final ObjectType getClassType ()
  {
    ObjectType otype = clas;
    if (otype != null)
      return otype;
    String name = this.name.string;
    if (name.charAt (0) == '[')
      otype = (ObjectType)Type.signatureToType (name);
    else
      otype = ClassType.make (name.replace ('/', '.'));
    clas = otype;
    return otype;
  }
  
  final static int hashCode (CpoolUtf8 name)
  {
    return name.hashCode() ^ 0xF0F;
  }

  public int hashCode ()
  {
    if (hash == 0)
      hash = hashCode(name);
    return hash;
  }

  void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeByte (ConstantPool.CLASS);
    dstr.writeShort (name.index);
  }

  public void print (ClassTypeWriter dst, int verbosity)
  {
    if (verbosity == 1)
      dst.print("Class ");
    else if (verbosity > 1)
      {
	dst.print("Class name: ");
	dst.printOptionalIndex(name);
      }
    String str = name.string;
    int nlen = str.length();
    if (nlen > 1 && str.charAt(0) == '[')
      Type.printSignature(str, 0, nlen, dst);
    else
      dst.print(str.replace('/', '.'));
  }
}
