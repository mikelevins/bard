package gnu.xquery.util;
import java.io.*;

public class NamedCollator
/* #ifdef JAVA2 */
extends java.text.Collator
/* #endif */
implements Externalizable
{
  /* #ifdef JAVA2 */
  java.text.Collator collator;
  /* #endif */

  String name;

  public static final String UNICODE_CODEPOINT_COLLATION
  = "http://www.w3.org/2005/xpath-functions/collation/codepoint";

  public static NamedCollator make (String name)
  {
    NamedCollator coll = new NamedCollator();
    coll.name = name;
    coll.resolve();
    return coll;
  }

  public String getName ()
  {
    return name;
  }

  public static NamedCollator find (String name)
  {
    return make(name);
  }

  public static final NamedCollator codepointCollation = new NamedCollator();
  static { codepointCollation.name = UNICODE_CODEPOINT_COLLATION; }

  public void resolve ()
  {
    if (name != null && ! name.equals(UNICODE_CODEPOINT_COLLATION))
      {
	// FIXME!
	throw new RuntimeException("unknown collation: "+name);
      }
  }

  /** Compares two strings lexicographically by codepoint.
   * Same as {@code String.compareTo} but handles surrogate characters.
   * @return -1, 0, or 1 depending on their relative order.
   */
  public static int codepointCompare (String str1, String str2)
  {
    int i1 = 0, i2 = 0;
    int len1 = str1.length();
    int len2 = str2.length();
    for (;;)
      {
        if (i1 == len1)
          return i2 == len2 ? 0 : -1;
        if (i2 == len2)
          return 1;
        int c1 = str1.charAt(i1++);
        if (c1 >= 0xD800 && c1 < 0xDC00 && i1 < len1)
          c1 =  (c1 - 0xD800) * 0x400
            + (str1.charAt(i1++) - 0xDC00) + 0x10000;
        int c2 = str2.charAt(i2++);
        if (c2 >= 0xD800 && c2 < 0xDC00 && i2 < len2)
          c2 =  (c2 - 0xD800) * 0x400
            + (str2.charAt(i2++) - 0xDC00) + 0x10000;
        if (c1 != c2)
          return c1 < c2 ? -1 : 1;
      }
  }

  public int compare (String str1, String str2)
  {
    /* #ifdef JAVA2 */
    if (collator != null)
      return collator.compare(str1, str2);
    /* #endif */
    return codepointCompare(str1, str2);
  }

  /* #ifdef JAVA2 */
  public java.text.CollationKey getCollationKey (String source)
  {
    return collator.getCollationKey(source);
  }
  /* #endif */

  public int hashCode ()
  {
    /* #ifdef JAVA2 */
    if (collator != null)
      return collator.hashCode();
    /* #endif */
    return 0;
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeUTF(name);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    name = in.readUTF();
    resolve();
  }
}
