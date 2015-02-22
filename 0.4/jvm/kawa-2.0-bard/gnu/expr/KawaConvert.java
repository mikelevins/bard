// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.lists.Convert;

/** Override gnu.lists.Convert to use Kawa number and Char classes. */

public class KawaConvert extends Convert
{
  public static Convert instance = new KawaConvert();

  public static Convert getInstance() { return instance; }

  public static void setInstance(Convert value) { instance = value; };

  public Object charToObject(char ch)
  {
    return gnu.text.Char.make(ch);
  }

  public char objectToChar(Object obj)
  {
    return ((gnu.text.Char) obj).charValue();
  }

  public Object byteToObject(byte value)
  {
    return gnu.math.IntNum.make(value);
  }

  public Object shortToObject(short value)
  {
    return gnu.math.IntNum.make(value);
  }

  public Object intToObject(int value)
  {
    return gnu.math.IntNum.make(value);
  }

  public Object longToObject(long value)
  {
    return gnu.math.IntNum.make(value);
  }

  public Object byteToObjectUnsigned(byte value)
  {
    return gnu.math.IntNum.make(value & 0xFF);
  }

  public Object shortToObjectUnsigned(short value)
  {
    return gnu.math.IntNum.make(value & 0xFFFF);
  }

  public Object intToObjectUnsigned(int value)
  {
    return gnu.math.IntNum.make((long) value & 0xFFFFFFFFL);
  }

  public Object longToObjectUnsigned(long value)
  {
    return gnu.math.IntNum.makeU(value);
  }

  public Object floatToObject(float value)
  {
    return gnu.math.DFloNum.make(value);
  }

  public Object doubleToObject(double value)
  {
    return gnu.math.DFloNum.make(value);
  }


}
