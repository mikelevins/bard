package gnu.kawa.xml;

/** A loosely typed string.
 * Can be cast as needed to numbers and other types.
 * Implements the {@code xs:untypedAtomic} type of XPath/XQuery, which use
 * it to represent attribute and text values of unvalidated XML.
 */

public class UntypedAtomic
{
  String text;

  public String toString ()
  {
    return text;
  }

  public UntypedAtomic (String text)
  {
    this.text = text;
  }

  public int hashCode ()
  {
    return text.hashCode();
  }

  public boolean equals (Object arg)
  {
    return arg instanceof UntypedAtomic
      && text.equals(((UntypedAtomic) arg).text);
  }
}
