// Copyright (c) 2014 Jamison Hope
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.io.*;

/**
 * A quaternion number using plain double values.
 * @author Jamison Hope
 */
public class DQuaternion extends Quaternion implements Externalizable {
    double real;
    double imag;
    double jmag;
    double kmag;

    public DQuaternion() {
    }

    public DQuaternion(double real, double imag, double jmag,
                       double kmag) {
        this.real = real;
        this.imag = imag;
        this.jmag = jmag;
        this.kmag = kmag;
    }

    public RealNum re () { return new DFloNum (real); }
    public double doubleValue() { return real; }
    public RealNum im () { return new DFloNum (imag); }
    public double doubleImagValue () { return imag; }
    public RealNum jm () { return new DFloNum (jmag); }
    public double doubleJmagValue () { return jmag; }
    public RealNum km () { return new DFloNum (kmag); }
    public double doubleKmagValue () { return kmag; }

    public boolean isExact () {
        return false;
    }

    public Quaternion toExact() {
        return new CQuaternion(DFloNum.toExact(real),
                               DFloNum.toExact(imag),
                               DFloNum.toExact(jmag),
                               DFloNum.toExact(kmag));
    }

    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Quaternion))
            return false;
        Quaternion y = (Quaternion)obj;
        return y.unit() == Unit.Empty
            && (Double.doubleToLongBits(real)
                == Double.doubleToLongBits(y.reValue()))
            && (Double.doubleToLongBits(imag)
                == Double.doubleToLongBits(y.imValue()))
            && (Double.doubleToLongBits(jmag)
                == Double.doubleToLongBits(y.jmValue()))
            && (Double.doubleToLongBits(kmag)
                == Double.doubleToLongBits(y.kmValue()));
    }

    public String toString() {
        String reString = DFloNum.toString(real);

        if ((Double.doubleToLongBits(imag) == 0) &&
            (Double.doubleToLongBits(jmag) == 0) &&
            (Double.doubleToLongBits(kmag) == 0))
            return reString;

        StringBuilder sbuf = new StringBuilder();
        if (!reString.equals("0.0"))
            sbuf.append(reString);

        if (Double.doubleToLongBits(imag) != 0) {
            String imString = DFloNum.toString(imag);
            char ch0 = imString.charAt(0);
            if (ch0 != '-' && ch0 != '+')
                sbuf.append('+');
            sbuf.append(imString);
            sbuf.append('i');
        }

        if (Double.doubleToLongBits(jmag) != 0) {
            String jmString = DFloNum.toString(jmag);
            char ch0 = jmString.charAt(0);
            if (ch0 != '-' && ch0 != '+')
                sbuf.append('+');
            sbuf.append(jmString);
            sbuf.append('j');
        }

        if (Double.doubleToLongBits(kmag) != 0) {
            String kmString = DFloNum.toString(kmag);
            char ch0 = kmString.charAt(0);
            if (ch0 != '-' && ch0 != '+')
                sbuf.append('+');
            sbuf.append(kmString);
            sbuf.append('k');
        }
        return sbuf.toString();
    }

    public String toString(int radix) {
        if (radix == 10)
            return toString();
        return "#d" + toString();
    }

    public final Numeric neg() {
        return new DQuaternion(-real, -imag, -jmag, -kmag);
    }

    public Numeric add (Object y, int k) {
        if (y instanceof Quaternion) {
            Quaternion yq = (Quaternion)y;
            if (yq.dimensions() != Dimensions.Empty)
                throw new ArithmeticException ("units mis-match");
            return Quaternion.make(real + k * yq.reValue(),
                                   imag + k * yq.imValue(),
                                   jmag + k * yq.jmValue(),
                                   kmag + k * yq.kmValue());
        }
        return ((Numeric)y).addReversed(this, k);
    }

    public Numeric mul (Object y) {
        if (y instanceof Quaternion) {
            Quaternion yq = (Quaternion)y;
            if (yq.unit() == Unit.Empty) {
                double y_re = yq.reValue();
                double y_im = yq.imValue();
                double y_jm = yq.jmValue();
                double y_km = yq.kmValue();
                return Quaternion.make
                    (real * y_re - imag * y_im - jmag * y_jm - kmag * y_km,
                     real * y_im + imag * y_re + jmag * y_km - kmag * y_jm,
                     real * y_jm - imag * y_km + jmag * y_re + kmag * y_im,
                     real * y_km + imag * y_jm - jmag * y_im + kmag * y_re);
            }
            return Quaternion.times(this, yq);
        }
        return ((Numeric)y).mulReversed(this);
    }

    public Numeric div(Object y) {
        if (y instanceof Quaternion) {
            Quaternion yq = (Quaternion) y;
            return DQuaternion.div(real, imag, jmag, kmag,
                                   yq.doubleValue(), yq.doubleImagValue(),
                                   yq.doubleJmagValue(), yq.doubleKmagValue());
        }
        return ((Numeric)y).divReversed(this);
    }

    public static double hypot4(double w, double x, double y, double z) {
        return Math.hypot(Math.hypot(w,x),Math.hypot(y,z));
    }

    public static double hypot3(double x, double y, double z) {
        return Math.hypot(Math.hypot(x,y),z);
    }

    public static Quaternion power(double x_re, double x_im,
                                   double x_jm, double x_km,
                                   double y_re, double y_im,
                                   double y_jm, double y_km) {
        if (x_jm == 0.0 && x_km == 0.0 && y_jm == 0.0 && y_km == 0.0)
            return DComplex.power(x_re, x_im, y_re, y_im);

        // ln(x)
        double qmag = hypot4(x_re, x_im, x_jm, x_km);
        double vmag = hypot3(x_im, x_jm, x_km);

        double atv = Math.atan2(vmag, x_re) / vmag;

        double ln_r = Math.log(qmag);
        double ln_i = atv * x_im;
        double ln_j = atv * x_jm;
        double ln_k = atv * x_km;

        // ln(x)*y
        double p_r = ln_r * y_re - ln_i * y_im - ln_j * y_jm - ln_k * y_km;
        double p_i = ln_r * y_im + ln_i * y_re + ln_j * y_km - ln_k * y_jm;
        double p_j = ln_r * y_jm - ln_i * y_km + ln_j * y_re + ln_k * y_im;
        double p_k = ln_r * y_km + ln_i * y_jm - ln_j * y_im + ln_k * y_re;

        // exp(ln(x)*y)
        double pvmag = hypot3(p_i,p_j,p_k);
        double sinpvmag = Math.sin(pvmag);
        double expr = Math.exp(p_r);

        if (pvmag == 0.0 || sinpvmag == 0.0)
            return DFloNum.make(expr * Math.cos(pvmag));

        return Quaternion.make(expr * Math.cos(pvmag),
                               expr * sinpvmag * p_i / pvmag,
                               expr * sinpvmag * p_j / pvmag,
                               expr * sinpvmag * p_k / pvmag);
    }

    public static Quaternion exp(double x_re, double x_im,
                                 double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return Complex.polar(Math.exp(x_re), x_im);

        double vmag = hypot3(x_im,x_jm,x_km);
        double sinvmag = Math.sin(vmag);
        double expr = Math.exp(x_re);
        return Quaternion.make(expr * Math.cos(vmag),
                               expr * sinvmag * x_im / vmag,
                               expr * sinvmag * x_jm / vmag,
                               expr * sinvmag * x_km / vmag);
    }

    public static Quaternion log(double x_re, double x_im,
                                 double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return DComplex.log(x_re, x_im);

        double qmag = hypot4(x_re,x_im,x_jm,x_km);
        double vmag = hypot3(x_im,x_jm,x_km);

        double atv = Math.atan2(vmag, x_re) / vmag;

        double r = Math.log(qmag);
        double i = atv * x_im;
        double j = atv * x_jm;
        double k = atv * x_km;

        return Quaternion.make(r, i, j, k);
    }

    public static Quaternion div(double x_re, double x_im,
                                 double x_jm, double x_km,
                                 double y_re, double y_im,
                                 double y_jm, double y_km) {
        if (x_jm == 0.0 && x_km == 0.0 && y_jm == 0.0 && y_km == 0.0)
            return DComplex.div(x_re, x_im, y_re, y_im);

        double y_norm = y_re*y_re + y_im*y_im + y_jm*y_jm + y_km*y_km;

        // This computes (y^-1 * x), which is different from (x * y^-1).
        double r = x_re*y_re + x_im*y_im + x_jm*y_jm + x_km*y_km;
        double i = x_im*y_re - x_re*y_im + x_km*y_jm - x_jm*y_km;
        double j = x_jm*y_re - x_re*y_jm + x_im*y_km - x_km*y_im;
        double k = x_km*y_re - x_re*y_km + x_jm*y_im - x_im*y_jm;

        return Quaternion.make(r/y_norm, i/y_norm, j/y_norm, k/y_norm);
    }

    public static Quaternion sqrt(double x_re, double x_im,
                                  double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return DComplex.sqrt(x_re, x_im);

        double qmag = hypot4(x_re,x_im,x_jm,x_km);
        double vmag = hypot3(x_im,x_jm,x_km);

        double t = Math.acos(x_re/qmag);

        double y_mag = Math.sqrt(qmag);
        double s = Math.sin(t/2);

        return Quaternion.make(y_mag * Math.cos(t/2),
                               y_mag * s * x_im / vmag,
                               y_mag * s * x_jm / vmag,
                               y_mag * s * x_km / vmag);
    }

    public static Quaternion sin(double x_re, double x_im,
                                 double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return DComplex.sin(x_re, x_im);

        double vmag = hypot3(x_im,x_jm,x_km);

        double r = Math.sin(x_re) * Math.cosh(vmag);
        double v = Math.cos(x_re) * Math.sinh(vmag);

        return Quaternion.make(r, v * x_im/vmag, v * x_jm/vmag, v * x_km/vmag);
    }

    public static Quaternion cos(double x_re, double x_im,
                                 double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return DComplex.cos(x_re, x_im);

        double vmag = hypot3(x_im,x_jm,x_km);

        double r =  Math.cos(x_re) * Math.cosh(vmag);
        double v = -Math.sin(x_re) * Math.sinh(vmag);

        return Quaternion.make(r, v * x_im/vmag, v * x_jm/vmag, v * x_km/vmag);
    }

    public static Quaternion tan(double x_re, double x_im,
                                 double x_jm, double x_km) {
        if (x_jm == 0.0 && x_km == 0.0)
            return DComplex.tan(x_re, x_im);

        double vmag = hypot3(x_im,x_jm,x_km);

        double sin_re = Math.sin(x_re);
        double cos_re = Math.cos(x_re);
        double sinh_v = Math.sinh(vmag);
        double cosh_v = Math.cosh(vmag);
        // tan = sin/cos
        return DQuaternion.div(sin_re*cosh_v,
                               cos_re*sinh_v*x_im/vmag,
                               cos_re*sinh_v*x_jm/vmag,
                               cos_re*sinh_v*x_km/vmag,
                               cos_re*cosh_v,
                               -sin_re*sinh_v*x_im/vmag,
                               -sin_re*sinh_v*x_jm/vmag,
                               -sin_re*sinh_v*x_km/vmag);
    }

    /**
     * @serialData Writes the real part, followed by the imaginary parts.
     *   All are written as doubles (using writeDouble).
     */
    public void writeExternal(ObjectOutput out) throws IOException {
        out.writeDouble(real);
        out.writeDouble(imag);
        out.writeDouble(jmag);
        out.writeDouble(kmag);
    }

    public void readExternal(ObjectInput in)
        throws IOException, ClassNotFoundException {
        real = in.readDouble();
        imag = in.readDouble();
        jmag = in.readDouble();
        kmag = in.readDouble();
    }
}
