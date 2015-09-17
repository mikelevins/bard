package gnu.jemacs.buffer;

/** Used to signal that an action was cancelled. */

public class CancelledException extends RuntimeException
{
  public CancelledException(String msg)
  {
    super(msg);
  }

  public CancelledException()
  {
    super();
  }
}
