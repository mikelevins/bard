// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.io.*;

/** General Cartesian Quaternion quantity. */

public class CQuantity extends Quantity implements Externalizable {
    Quaternion num;
    Unit unt;

    public CQuantity (Quaternion num, Unit unit) {
        this.num = num;
        this.unt = unit;
    }

    public CQuantity (RealNum real, RealNum imag, Unit unit) {
        this.num = new CComplex (real, imag);
        this.unt = unit;
    }

    public CQuantity (RealNum real, RealNum imag, RealNum jmag,
                      RealNum kmag, Unit unit) {
        this.num = new CQuaternion(real, imag, jmag, kmag);
        this.unt = unit;
    }

    public Quaternion number() { return num; }
    public Unit unit() { return unt; }

    public boolean isExact () { return num.isExact(); }

    public boolean isZero () { return num.isZero(); }

    /**
     * @serialData Write the complex value (using writeObject) followed
     *   by the Unit (also using writeUnit).
     */

    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(num);
        out.writeObject(unt);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        num = (Quaternion) in.readObject();
        unt = (Unit) in.readObject();
    }
}
