package gnu.kawa.io;
import gnu.text.*;
import gnu.lists.*;

/** An Inport for reading from a char array.
  * Essentially the same as an InPort wrapped around a CharArrayReader, but
  * more efficient because it uses the char array as the InPort's buffer. */

public class CharArrayInPort extends InPort
{
  static final Path stringPath = Path.valueOf("<string>");

  public static CharArrayInPort make
  /* #ifdef use:java.lang.CharSequence */
  (CharSequence seq)
  /* #else */ 
  // (CharSeq seq)
  /* #endif */
  {
    if (seq instanceof FString)
      return new CharArrayInPort((FString) seq);
    else
      {
        int len = seq.length();
        char[] buf = new char[len];
        /* #ifdef use:java.lang.CharSequence */
        if (seq instanceof String)
          ((String) seq).getChars(0, len, buf, 0);
        else if (! (seq instanceof CharSeq))
          for (int i = len; --i >= 0; )
            buf[i] = seq.charAt(i);
        else
        /* #endif */
          ((CharSeq) seq).getChars(0, len, buf, 0);
        return new CharArrayInPort(buf, len);
      }
  }

  public CharArrayInPort (char[] buffer, int len)
  {
    super(NullReader.nullReader, stringPath);
    try
      {
	setBuffer(buffer);
      }
    catch (java.io.IOException ex)
      {
	throw new Error(ex.toString()); // Can't happen.
      }
    limit = len;
  }

  public CharArrayInPort (char[] buffer)
  {
    this(buffer, buffer.length);
  }

  public CharArrayInPort (String string)
  {
    this(string.toCharArray());
  }

    public CharArrayInPort(FString string) {
        this(string.data, string.size());
    }

  public int read () throws java.io.IOException
  {
    if (pos >= limit)
      return -1;
    return super.read();
  }
}
