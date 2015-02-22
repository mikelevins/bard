// Copyright (c) 2014  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_MethodType entry in the constant pool. */

public class CpoolMethodType extends CpoolEntry {
    CpoolUtf8 descriptor;

    public CpoolMethodType() {
    } 

    public int getTag() { return ConstantPool.METHOD_TYPE; }

    void write (DataOutputStream dstr) throws java.io.IOException {
	throw new Error();
    }

    public void print (ClassTypeWriter dst, int verbosity) {
	if (verbosity > 0) {
	    dst.print("MethodType");
	    if (verbosity == 2) {
		dst.print(" descriptor: ");
		dst.printOptionalIndex(descriptor);
	    } else
		dst.print( ' ');
	}
	descriptor.print(dst, 0);
    }
}
