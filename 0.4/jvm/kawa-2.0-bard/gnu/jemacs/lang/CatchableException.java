package gnu.jemacs.lang;

/** An exception thrown by (throw tag value) and caught by (catch tag body). */

public class CatchableException extends RuntimeException
{
  Object tag;
  Object value;

  public CatchableException (Object tag, Object value)
  {
    this.tag = tag;
    this.value = value;
  }

  public Object match (Object catchTag)
  {
    if (tag != catchTag)
      throw this;
    return value;
  }
}
