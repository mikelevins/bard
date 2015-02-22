// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;

public abstract class Complex extends Quaternion
{
    @Override public final RealNum jm() { return IntNum.zero(); }
    @Override public final RealNum km() { return IntNum.zero(); }

    @Override public final Complex complexPart() { return this; }

    @Override public Quaternion vectorPart() {
        return Complex.make(IntNum.zero(), im());
    }

    @Override public Quaternion unitVector() {
        int imSign = im().sign();

        switch (imSign) {
        case 1:
            return Complex.imOne();
        case 0:
            return IntNum.zero();
        case -1:
            return Complex.imMinusOne();
        case -2: default:
            return Complex.make(0, Double.NaN);
        }
    }

    @Override public Quaternion unitQuaternion() {
        if (im().isZero())
            return re().unitQuaternion();
        if (re().isZero())
            return Complex.make(IntNum.zero(), (RealNum)im().unitQuaternion());
        return DComplex.unitQuaternion(doubleRealValue(), doubleImagValue());
    }

    @Override public Quaternion conjugate() {
        return Complex.make(re(), im().rneg());
    }

  public boolean isExact ()
  {
    // Should we return false if unit() != unit.Empty ?
    return re().isExact() && im().isExact();
  }

    /** Check if value is finite, infinite, or NaN.
     * @return 1 if finite; 0 if infinite; -1 if NaN.
     */
    public int classifyFinite() {
        int r = re().classifyFinite();
        if (r < 0)
            return r;
        int i = im().classifyFinite();
        return r < i ? r : i;
    }

  public Complex toExact ()
  {
    RealNum re = re();
    RealNum im = im();
    RatNum xre = re.toExact();
    RatNum xim = im.toExact();
    if (xre == re && xim == im)
      return this;
    else
      return new CComplex(xre, xim);
  }

  public Complex toInexact ()
  {
    if(isExact())
      return this;
    return new DComplex(re().doubleValue(), im().doubleValue());
  }

  private static CComplex imOne;
  private static CComplex imMinusOne;

  public static CComplex imOne()
  {
    if (imOne == null)
      imOne = new CComplex (IntNum.zero(), IntNum.one());
    return imOne;
  }

  public static CComplex imMinusOne()
  {
    if (imMinusOne == null)
      imMinusOne = new CComplex (IntNum.zero(), IntNum.minusOne());
    return imMinusOne;
  }

  public static Complex make (RealNum re, RealNum im)
  {
    if (im.isZero() && im.isExact())
      return re;
    if (! re.isExact() && ! im.isExact())
      return new DComplex(re.doubleValue(), im.doubleValue());
    return new CComplex (re, im);
  }

  public static Complex make (double re, double im)
  {
    if (im == 0.0)
      return new DFloNum(re);
    return new DComplex(re, im);
  }

  public static DComplex polar (double r, double t)
  {
    return new DComplex(r * Math.cos(t), r * Math.sin(t));
  }

  public static DComplex polar (RealNum r, RealNum t)
  {
    return polar(r.doubleValue(), t.doubleValue());
  }

  public static Complex power (Complex x, Complex y)
  {
    if (y instanceof IntNum)
      return (Complex) x.power((IntNum) y);
    double x_re = x.doubleRealValue();
    double x_im = x.doubleImagValue();
    double y_re = y.doubleRealValue();
    double y_im = y.doubleImagValue();
    if (x_im == 0.0 && y_im == 0
	&& (x_re >= 0 || Double.isInfinite(x_re) || Double.isNaN(x_re)))
      return new DFloNum (Math.pow (x_re, y_re));
    return DComplex.power (x_re, x_im, y_re, y_im);
  }

  public Numeric abs ()
  {  
    /* #ifdef JAVA5 */
    return new DFloNum(Math.hypot(doubleRealValue(), doubleImagValue()));
    /* #else */
    // return new DFloNum(DComplex.hypot(doubleRealValue(), doubleImagValue()));
    /* #endif */
  }

  public RealNum angle()
  {
    return new DFloNum(Math.atan2(doubleImagValue(), doubleRealValue()));
  }

    @Override public final RealNum colatitude() { return IntNum.zero(); }
    @Override public final RealNum longitude() { return IntNum.zero(); }

  public static boolean equals (Complex x, Complex y)
  {
    return x.re().equals(y.re())
      && x.im().equals(y.im());
  }

  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof Complex))
      return false;
    return Complex.equals (this, (Complex) obj);
  }

  public static int compare (Complex x, Complex y)
  {
    int code = x.im().compare(y.im());
    if (code != 0)
      return code;
    return x.re().compare(y.re());
  }

  public int compare (Object obj)
  {
    if (! (obj instanceof Complex))
      return ((Numeric) obj).compareReversed(this);
    return compare(this, (Complex) obj);
  }

  public boolean isZero ()
  {
    return re().isZero () && im().isZero();
  }

  //  public abstract Complex neg ();

  /*
  Unit unit () { return Unit.Empty; }
  Dimesions dims() { return unit().dims; }
  */

  
  public String toString (int radix)
  {
    // Note: The r4rs read syntax does not allow unsigned pure
    // imaginary numbers, i.e. you must use +5i, not 5i.
    // Although our reader allows the sign to be dropped, we always
    // print it so that the number may be read by any r4rs system.
    if (im().isZero ())
      return re().toString(radix);
    String imString = im().toString(radix) + "i";
    char ch0 = imString.charAt(0);
    if (ch0 != '-' && ch0 != '+')
      imString = "+" + imString;
    if (re().isZero())
      return imString;
    return re().toString(radix) + imString;
  }

  public static Complex neg (Complex x)
  {
    return Complex.make (x.re().rneg(), x.im().rneg());
  }

  public Numeric neg () { return neg (this); }

  public static Complex add (Complex x, Complex y, int k)
  {
    return Complex.make (RealNum.add(x.re(), y.re(), k),
			 RealNum.add(x.im(), y.im(), k));
  }

  public Numeric add (Object y, int k)
  {
    if (y instanceof Complex)
      return add (this, (Complex) y, k);
    return ((Numeric)y).addReversed(this, k);
  }

  public Numeric addReversed (Numeric x, int k)
  {
    if (x instanceof Complex)
      return add ((Complex)x, this, k);
    throw new IllegalArgumentException ();
  }

  public static Complex times (Complex x, Complex y)
  {
    RealNum x_re = x.re();
    RealNum x_im = x.im();
    RealNum y_re = y.re();
    RealNum y_im = y.im();
    return Complex.make (RealNum.add (RealNum.times(x_re, y_re),
				      RealNum.times(x_im, y_im), -1),
			 RealNum.add (RealNum.times(x_re, y_im),
				      RealNum.times(x_im, y_re), 1));
  }

  public Numeric mul (Object y)
  {
    if (y instanceof Complex)
      return times(this, (Complex) y);
    return ((Numeric)y).mulReversed(this);
  }

  public Numeric mulReversed (Numeric x)
  {
    if (x instanceof Complex)
      return times((Complex)x, this);
    throw new IllegalArgumentException ();
  }

  public static Complex divide (Complex x, Complex y)
  {
    if (! x.isExact () || ! y.isExact ())
      return DComplex.div (x.doubleRealValue(), x.doubleImagValue(),
			   y.doubleRealValue(), y.doubleImagValue());

    RealNum x_re = x.re();
    RealNum x_im = x.im();
    RealNum y_re = y.re();
    RealNum y_im = y.im();

    RealNum q = RealNum.add (RealNum.times(y_re, y_re),
			     RealNum.times(y_im, y_im), 1);
    RealNum n = RealNum.add(RealNum.times(x_re, y_re),
			    RealNum.times(x_im, y_im), 1);
    RealNum d = RealNum.add(RealNum.times(x_im, y_re),
			    RealNum.times(x_re, y_im), -1);
    return Complex.make(RealNum.divide(n, q), RealNum.divide(d, q));
  }

  public Numeric div (Object y)
  {
    if (y instanceof Complex)
      return divide(this, (Complex) y);
    return ((Numeric)y).divReversed(this);
  }

  public Numeric divReversed (Numeric x)
  {
    if (x instanceof Complex)
      return divide((Complex)x, this);
    throw new IllegalArgumentException ();
  }

  public Complex exp ()
  {
    return polar (Math.exp(doubleRealValue()), doubleImagValue());
  }


  public Complex log ()
  {
    return DComplex.log(doubleRealValue(), doubleImagValue());
  }
  
  public Complex sqrt ()
  {
    return DComplex.sqrt(doubleRealValue(), doubleImagValue());
  }

    @Override public Complex sin() {
        return DComplex.sin(doubleRealValue(), doubleImagValue());
    }

    @Override public Complex cos() {
        return DComplex.cos(doubleRealValue(), doubleImagValue());
    }

    @Override public Complex tan() {
        return DComplex.tan(doubleRealValue(), doubleImagValue());
    }
}
