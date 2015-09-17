// Copyright (c) 2014 Jamison Hope
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.io.*;

/**
 * General Cartesian Quaternion number (a four-dimensional extension
 * of complex numbers).
 * Use this instead of DQuaternion if you want exact quaternion
 * numbers.
 * @author Jamison Hope
 */
public class CQuaternion extends Quaternion implements Externalizable {
    RealNum real;
    RealNum imag;
    RealNum jmag;
    RealNum kmag;

    public CQuaternion() {
    }

    public CQuaternion(RealNum real, RealNum imag, RealNum jmag,
                       RealNum kmag) {
        this.real = real;
        this.imag = imag;
        this.jmag = jmag;
        this.kmag = kmag;
    }

    public RealNum re() { return real; }
    public RealNum im() { return imag; }
    public RealNum jm() { return jmag; }
    public RealNum km() { return kmag; }

    /**
     * @serialData Write the real and imaginary parts, as Objects.
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeObject(real);
        out.writeObject(imag);
        out.writeObject(jmag);
        out.writeObject(kmag);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        real = (RealNum) in.readObject();
        imag = (RealNum) in.readObject();
        jmag = (RealNum) in.readObject();
        kmag = (RealNum) in.readObject();
    }
}
