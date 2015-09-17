package kawa.lang;

// FIXME code should just use RuntimeException!
public class GenericError extends RuntimeException
{
  public GenericError (String message)
  {
    super (message);
  }
}
