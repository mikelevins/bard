// Copyright (c) 2007  Per M.A. Bothner.
// This is free software;  for specifics see ../../../COPYING.

package gnu.kawa.xml;

public class XString
  /* #ifdef use:java.lang.CharSequence */
  implements CharSequence
  /* #endif */
{
  public String text;

  // Alternatively have a different subclass for each type.
  private XStringType type;

  public XStringType getStringType ()
  {
    return type;
  }

  public char charAt (int index) { return text.charAt(index); }

  public int length () { return text.length(); }

  /* #ifdef use:java.lang.CharSequence */
  public CharSequence subSequence(int start, int end)
  { return text.substring(start, end); }
  /* #endif */

  public String toString () { return text; }

  XString (String text, XStringType type)
  {
    this.text = text;
    this.type = type;
  }
}
