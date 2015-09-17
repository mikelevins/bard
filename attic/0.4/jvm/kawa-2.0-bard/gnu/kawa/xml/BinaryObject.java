package gnu.kawa.xml;

/** Generic binary data with application-specific interpretation. */

public abstract class BinaryObject
{
  byte[] data;

  public byte[] getBytes ()
  {
    return data;
  }
}
