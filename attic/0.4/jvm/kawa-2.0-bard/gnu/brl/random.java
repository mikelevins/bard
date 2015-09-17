package gnu.brl;

// read.java -- procedure to read a BRL expression
// Copyright (C) 2000  Bruce R. Lewis and Eaton Vance Management
// See the file COPYING for license terms.

import kawa.lang.*;
import gnu.mapping.Procedure1;
import gnu.mapping.WrongType;
import gnu.kawa.io.InPort;
import gnu.math.*;

public class random extends Procedure1 {

    static java.util.Random prg = new java.util.Random();

    public final Object apply1 (Object arg1)
    {
	IntNum retval = new IntNum();

	// When passed an integer N, return an int M s.t. 0 <= M < N.
	if (arg1 instanceof IntNum)
	    {
		IntNum.divide(IntNum.makeU(Math.abs(prg.nextLong()) >> 1),
			      (IntNum) arg1, null, retval, Numeric.FLOOR);
		return retval;
	    }

	// InPort, e.g. /dev/random, for initializing generator
	if (arg1 instanceof InPort)
	    try
		{
		    InPort i = (InPort)arg1;
		    prg.setSeed((long)
				i.read()
				+ i.read() << 8
				+ i.read() << 16
				+ i.read() << 24);
		    i.close();
		    return retval;
		}
	    catch (java.io.IOException e)
		{
		    throw new GenericError ("I/O exception in brl-random: "
					    + e.toString ());
		}
	throw new WrongType (this, 1, arg1, "real");
    }
}
