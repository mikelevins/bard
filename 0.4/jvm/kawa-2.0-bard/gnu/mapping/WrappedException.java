// Copyright (c) 1999, 2003, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;

/** Encapsulate some Exception inside a RuntimeException.
 * Inspired by org.xml.sax.SAXException written by David Megginson.
 */

public class WrappedException extends RuntimeException
{
  /**
   * Create a new WrappedException.
   */
  public WrappedException ()
  {
  }

  /**
   * Create a new WrappedException.
   *
   * @param message The error or warning message.
   */
  public WrappedException (String message)
  {
    super(message);
  }

  /**
   * Create a new WrappedException wrapping an existing exception.
   *
   * <p>The existing exception will be embedded in the new
   * one, and its message will become the default message for
   * the WrappedException.</p>
   *
   * @param e The exception to be wrapped in a WrappedException.
   */
  public WrappedException (Throwable e)
  {
    this(e.toString(), e);
  }

  /**
   * Create a new WrappedException from an existing exception.
   *
   * <p>The existing exception will be embedded in the new
   * one, but the new exception will have its own message.</p>
   *
   * @param message The detail message.
   * @param e The exception to be wrapped in a WrappedException.
   */
  public WrappedException (String message, Throwable e)
  {
    /* #ifdef use:java.lang.Throwable.getCause */
    super(message, e);
    /* #else */
    // super(message);
    // initCause(e);
    /* #endif */
  }

  /**
   * Return the embedded exception, if any.
   *
   * @return The embedded exception, or null if there is none.
   */
  public Throwable getException ()
  {
    return getCause();
  }

  /**
   * Convert this exception to a string.
   *
   * @return A string version of this exception.
   */
  public String toString ()
  {
    return getMessage();
  }

  // The initCause/getCause functionality was added in JDK 1.4.
  /* #ifndef use:java.lang.Throwable.getCause */
  // public Throwable initCause(Throwable cause)
  // {
  //   exception = cause;
  //   return this;
  // }

  // public Throwable getCause()
  // {
  //   return exception;
  // }

  // private Throwable exception;
  /* #endif */

  /** Coerce argument to a RuntimeException.
   * Using rethrow may be preferable as it doesn't require wrapping an Error.
   */
  public static RuntimeException wrapIfNeeded (Exception ex)
  {
    if (ex instanceof RuntimeException)
      return (RuntimeException) ex;
    else
      return new WrappedException(ex);
  }

    /** Re-throw as a non-checked exception.
     * This method never returns, in spite of the return type.
     * This allows the call to be written as:
     * {@code throw WrappedExcepton.rethrow(ex)}
     * so javac and the verifier can know the code doesn't return.
     */
    public static RuntimeException rethrow (Throwable ex) {
	if (ex instanceof Error)
	    throw (Error) ex;
	else if (ex instanceof RuntimeException)
	    throw (RuntimeException) ex;
	else
	    throw new WrappedException(ex);
    }
}
