// Copyright (c) 2006  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;
import gnu.math.IntNum;

/** An integer that is an instance of a more specific integer type.
 * I.e. it has a type annotation and restrictions, in the XML Schema sense.
 */

public class XInteger extends IntNum
{
  // Alternatively have a different subclass for each type.
  private XIntegerType type;

  public XIntegerType getIntegerType ()
  {
    return type;
  }

  XInteger (IntNum value, XIntegerType type)
  {
    words = value.words;
    ival = value.ival;
    this.type = type;
  }
}
