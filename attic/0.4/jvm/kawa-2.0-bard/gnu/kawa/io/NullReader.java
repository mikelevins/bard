package gnu.kawa.io;

/** A reader for an empty stream (similar to /dev/null). */

public class NullReader extends java.io.Reader
{
  public static final NullReader nullReader = new NullReader();

  public int read (char[] buffer, int offset, int length) { return -1; }

  public boolean ready () { return true; }

  public void close () { }
}
