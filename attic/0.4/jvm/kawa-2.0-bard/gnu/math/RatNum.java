// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.math;
import java.math.BigDecimal;

/** The abstract class of rational numbers. */

public abstract class RatNum extends RealNum
{
  public abstract IntNum numerator ();
  public abstract IntNum denominator ();

    @Override
    public int classifyFinite() {
        return ! denominator().isZero() ? 1 : numerator().isZero() ? -1 : 0;
    }

  public static RatNum make(IntNum num, IntNum den)
  {
    IntNum g = IntNum.gcd (num, den);
    if (den.isNegative ())
      g = IntNum.neg (g);
    if (! g.isOne ())
      {
	num = IntNum.quotient (num, g);
	den = IntNum.quotient (den, g);
      }
    return den.isOne () ? (RatNum)num : (RatNum)(new IntFraction (num, den));
  }

  public static final IntNum ten_exp_9 = IntNum.make(1000000000);

  public static RatNum valueOf (BigDecimal value)
  {
    RatNum v = IntNum.valueOf(value.unscaledValue().toString(), 10);
    int scale = value.scale();
    for (; scale >= 9; scale -= 9)
      v = RatNum.divide(v, ten_exp_9);
    for (; scale <= -9; scale += 9)
      v = RatNum.times(v, ten_exp_9);
    IntNum scaleVal;
    switch (scale > 0 ? scale : -scale)
      {
      case 1: scaleVal = IntNum.make(10);  break;
      case 2: scaleVal = IntNum.make(100);  break;
      case 3: scaleVal = IntNum.make(1000);  break;
      case 4: scaleVal = IntNum.make(10000);  break;
      case 5: scaleVal = IntNum.make(100000);  break;
      case 6: scaleVal = IntNum.make(1000000);  break;
      case 7: scaleVal = IntNum.make(10000000);  break;
      case 8: scaleVal = IntNum.make(100000000);  break;
      default:
        return v;
      }
    if (scale > 0)
      return RatNum.divide(v, scaleVal);
    else
      return RatNum.times(v, scaleVal);
  }

  public static RatNum asRatNumOrNull (Object value)
  {
    if (value instanceof RatNum)
      return (RatNum) value;
    if (value instanceof BigDecimal)
      return valueOf((BigDecimal) value);
    return IntNum.asIntNumOrNull(value);
  }

  public boolean isExact ()
  {
    return true;
  }

  public boolean isZero ()
  {
    return numerator().isZero();
  }

  public final RatNum rneg() { return RatNum.neg(this); }

  /** Return exact "rational" infinity.
  * @param sign either 1 or -1 for positive or negative infinity */
  public static RatNum infinity(int sign)
  {
    return new IntFraction (IntNum.make(sign), IntNum.zero());
  }

    public static int compare(RatNum x, RatNum y) {
        IntNum xn = x.numerator( );
        IntNum xd = x.denominator();
        IntNum yn = y.numerator();
        IntNum yd = y.denominator();
        IntNum left, right;
        if (xd.isZero() && yd.isZero()) {
            left = xn;
            right = yn;
        } else {
            left = IntNum.times(xn, yd);
            right = IntNum.times(yn, xd);
        }
    return IntNum.compare (left, right);
  }

  /* Assumes x and y are both canonicalized. */
  public static boolean equals (RatNum x, RatNum y)
  {
    return IntNum.equals (x.numerator(), y.numerator())
      && IntNum.equals (x.denominator(), y.denominator());
  }

  /* Assumes this and obj are both canonicalized. */
  public boolean equals (Object obj)
  {
    if (obj == null || ! (obj instanceof RatNum))
      return false;
    return RatNum.equals (this, (RatNum) obj);
  }

  public static RatNum add (RatNum x, RatNum y, int k)
  {
    IntNum x_num = x.numerator();
    IntNum x_den = x.denominator();
    IntNum y_num = y.numerator();
    IntNum y_den = y.denominator();
    if (IntNum.equals (x_den, y_den))
      return RatNum.make (IntNum.add (x_num, y_num, k), x_den);
    return RatNum.make (IntNum.add (IntNum.times (y_den, x_num),
				    IntNum.times (y_num, x_den), k),
			IntNum.times (x_den, y_den));
  }

  public static RatNum neg (RatNum x)
  {
    IntNum x_num = x.numerator();
    IntNum x_den = x.denominator();
    return RatNum.make(IntNum.neg(x_num), x_den);
  }

  public static RatNum times (RatNum x, RatNum y)
  {
    return RatNum.make (IntNum.times (x.numerator(), y.numerator()),
			IntNum.times (x.denominator(), y.denominator()));
  }

  public static RatNum divide (RatNum x, RatNum y)
  {
    return RatNum.make (IntNum.times (x.numerator(), y.denominator()),
			IntNum.times (x.denominator(), y.numerator()));
  }

  public Numeric power (IntNum y)
  {
    boolean inv;
    if (y.isNegative())
      {
	inv = true;
	y = IntNum.neg(y);
      }
    else
      inv = false;
    if (y.words == null)
      {
	IntNum num = IntNum.power (numerator(), y.ival);
	IntNum den = IntNum.power (denominator(), y.ival);
	return inv ? RatNum.make (den, num) : RatNum.make (num, den);
      }
    double d = doubleValue();
    boolean neg = d < 0.0 && y.isOdd();
    d = Math.pow (d, y.doubleValue());
    if (inv)
      d = 1.0/d;
    return new DFloNum (neg ? -d : d);
  }

  public final RatNum toExact ()
  {
    return this;
  }

  public RealNum toInt (int rounding_mode)
  {
    return IntNum.quotient (numerator(), denominator(), rounding_mode);
  }

  public IntNum toExactInt (int rounding_mode)
  {
    return IntNum.quotient (numerator(), denominator(), rounding_mode);
  }

  /** Calcaulte the simplest rational between two reals. */
  public static RealNum rationalize (RealNum x, RealNum y)
  {
    // This algorithm is by Alan Bawden.  It has been transcribed
    // with permission from C-Gambit, copyright Marc Feeley.
    if (x.grt (y))
      return simplest_rational2 (y, x);
    else if (! (y.grt(x)))
      return x;
    else if (x.sign() > 0)
      return simplest_rational2 (x, y);
    else if (y.isNegative ())
      return (RealNum) (simplest_rational2 ((RealNum)y.neg(),
					    (RealNum)x.neg())).neg();
    else
      return IntNum.zero ();
  }

  private static RealNum simplest_rational2 (RealNum x, RealNum y)
  {
    RealNum fx = x.toInt (FLOOR);
    RealNum fy = y.toInt (FLOOR);
    if (! x.grt(fx))
      return fx;
    else if (fx.equals(fy))
      {
	RealNum n = (RealNum) IntNum.one().div(y.sub(fy));
	RealNum d = (RealNum) IntNum.one().div(x.sub(fx));
	return (RealNum) fx.add(IntNum.one().div(simplest_rational2 (n, d)),
				1);
      }
    else
      return (RealNum) fx.add(IntNum.one(), 1);
  }
}
