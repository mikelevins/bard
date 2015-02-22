// Copyright (c) 2014  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_InvokeDynamic entry in the constant pool. */

public class CpoolInvokeDynamic extends CpoolEntry {
    int bootstrapMethodIndex;
    CpoolNameAndType nameAndType;

    public CpoolInvokeDynamic() {
    }

    public int getTag() { return ConstantPool.INVOKE_DYNAMIC; }

    void write (DataOutputStream dstr) throws java.io.IOException {
	dstr.writeByte(ConstantPool.INVOKE_DYNAMIC);
	dstr.writeShort(bootstrapMethodIndex);
	dstr.writeShort(nameAndType.index);
    }

    public void print (ClassTypeWriter dst, int verbosity) {
	if (verbosity > 0) {
	    dst.print("InvokeDynamic ");
	    //if (verbosity == 2)
	    //dst.printOptionalIndex(str);
	}
	dst.print("bootstrap_method: #");
	dst.print(bootstrapMethodIndex);
	dst.print(' ');
	nameAndType.print(dst, 0);
    }
}

