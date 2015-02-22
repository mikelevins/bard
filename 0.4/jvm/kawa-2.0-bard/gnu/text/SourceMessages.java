// Copyright (c) 1999, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;

/** A collection of (zero or more) SourceErrors.
 * Has a "current line number" which clients can use as the default line
 * number, or clients can explicitly provide a line number.
 * Does not handle localization of messages.
 *
 * Similar functionality as JAVA6's javax.tools.DiagnosticCollector.
 */

public class SourceMessages implements SourceLocator
{
  // Number of errors (not counting warnings).  A value of 1000 is "fatal".
  private int errorCount = 0;

  /** The first error or warning in a linked list. */
  SourceError firstError;
  /** The last error or warning in a linked list. */
  SourceError lastError;

  public SourceError getErrors() { return firstError; }

  SourceLocator locator;

  String current_filename;
  int current_line;
  int current_column;

  public static boolean stripDirectoriesDefault = false;
  public boolean stripDirectories = stripDirectoriesDefault;

  /** If true, print out stack trace with any warning. */
  public static boolean debugStackTraceOnWarning = false;

  /** If true, print out stack trace with any error. */
  public static boolean debugStackTraceOnError = false;


  /** Return true iff errors (not warnings) have been seen. */
  public boolean seenErrors() { return errorCount > 0; }

  public boolean seenErrorsOrWarnings() { return firstError != null; }

  /** Get the number of errors (not counting warnings). */
  public int getErrorCount() { return errorCount; }

  /** Get number of diagnostics whose severity is one of the characters in the argument. */
  public int getCount (String severities)
  {
    int count = 0;
    for (SourceError err = firstError; err != null;  err = err.next)
      {
        if (severities.indexOf(err.severity) >= 0)
          count++;
      }
    return count;
  }

  /** Clear the error count (only). */
  public void clearErrors() { errorCount = 0; }

  /** Clear the contained errors and warnings. */
  public void clear()
  {
    firstError = lastError = null;
    errorCount = 0;
  }

  // The last SourceError with a *differnt* filename than prev has.
  SourceError lastPrevFilename = null;

  /** True if we should sort messages by line number. */
  public boolean sortMessages;

  /** Link in an error. */
  public void error(SourceError error)
  {
    if (error.severity == 'f')
      errorCount = 1000;
    else if (error.severity != 'w' && error.severity != 'i')
      errorCount++;
    if ((SourceMessages.debugStackTraceOnError
         && (error.severity == 'e' || error.severity == 'f'))
        || SourceMessages.debugStackTraceOnWarning && error.severity == 'w')
      {
        error.fakeException = new Throwable();
      }

    // Insert the next error so that line numbers are increasing.
    if (lastError != null && lastError.filename != null
	&& ! lastError.filename.equals(error.filename))
      lastPrevFilename = lastError;
    SourceError prev = lastPrevFilename;
    if (! sortMessages || error.severity == 'f')
      prev = lastError;
    else
      {
        for (;;)
          {
            SourceError next;
            if (prev == null)
              next = firstError;
            else
              next = prev.next;
            if (next == null)
              break;
            if (error.line != 0 && next.line != 0)
              {
                if (error.line < next.line)
                  break;
                if (error.line == next.line
                    && error.column != 0 && next.column != 0)
                  {
                    if (error.column < next .column)
                      break;
                  }
              }
            prev = next;
          }
      }
    if (prev == null)
      {
	error.next = firstError;
	firstError = error;
      }
    else
      {
	error.next = prev.next;
	prev.next = error;
      }
    if (prev == lastError) 
      lastError = error;
  }

  /** Record a new error.
   * @param severity is the seriousness of the error
   *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
   * @param filename the name or URL of the file containing the error
   * @param line the (1-origin) line number or 0 if unknown
   * @param column the (1-origin) column number or 0 if unknown
   * @param message the error message
   */
  public void error(char severity, String filename, int line, int column,
		    String message)
  {
    error(new SourceError(severity, filename, line, column, message));
  }

  public void error(char severity, SourceLocator location, String message)
  {
    error(new SourceError(severity, location, message));
  }

  public void error(char severity, String filename, int line, int column,
		    String message, String code)
  {
    SourceError err = new SourceError(severity, filename, line, column,
                                      message);
    err.code = code;
    error(err);
  }

  public void error(char severity, SourceLocator location,
		    String message, String code)
  {
    SourceError err = new SourceError(severity, location,  message);
    err.code = code;
    error(err);
  }

  /** Record a new error at the current default source file location.
   * @param severity is the seriousness of the error
   *  - one of 'w' (for warning), 'e' (for error), or 'f' (for fatal error)
   * @param message the error message
   */
  public void error(char severity, String message)
  {
    error(new SourceError(severity, current_filename,
			  current_line, current_column, message));
  }

  public void error(char severity, String message, Throwable exception)
  {
    SourceError err = new SourceError(severity, current_filename,
                                      current_line, current_column, message);
    err.fakeException = exception;
    error(err);
  }

  public void error(char severity, String message, String code)
  {
    SourceError err = new SourceError(severity, current_filename,
                                      current_line, current_column, message);
    err.code = code;
    error(err);
  }

  /** Return {@code 2*(max_non_fatal_messages_to_display)+(should_warnings_be_skipped?1:0)}.
   * @param max maximum total messages to display
   */
  int adjustDisplayMax (int max)
  {
    int count = getCount("iwef");
    boolean skipWarnings = false;
    if (count > max)
      {
        skipWarnings = true;
        max = max - getCount("f");
      }
    return (max << 1) | (skipWarnings?1:0);
  }

  boolean skipDisplayMessage (int max, SourceError error)
  {
    char severity = error.severity;
    if (max <= 0 && severity != 'f')
      return true;
    boolean skipWarnings = (max & 1) != 0;
    if (skipWarnings && (severity == 'i' || severity == 'w'))
      return true;
    return false;
  }

  /** Print all the error messages to a PrintStream. */
  public void printAll(java.io.PrintStream out, int max)
  {
    max = adjustDisplayMax(max);
    for (SourceError err = firstError;  err != null;  err = err.next)
      {
        if (skipDisplayMessage(max, err))
          continue;
	err.println(out, stripDirectories);
        max -= 2;
      }
  }

  /** Print all the error messages to a PrintWriter. */
  public void printAll(java.io.PrintWriter out, int max)
  {
    max = adjustDisplayMax(max);
    for (SourceError err = firstError;  err != null;  err = err.next)
      {
        if (skipDisplayMessage(max, err))
          continue;
	err.println(out, stripDirectories);
        max -= 2;
      }
  }

  /** Convert this to a String containing the recorded errors.
   * @param max the maximum number of error error to list
   * @return a String with one '\n'-terminated line per recorded error
   */
  public String toString(int max)
  {
    if (firstError == null)
      return null;
    StringBuffer buffer = new StringBuffer ();
    for (SourceError err = firstError;
	 err != null && --max >= 0;  err = err.next)
      {
	buffer.append(err.toString(stripDirectories));
	buffer.append('\n');
      }
    return buffer.toString();
  }

  /** Checks if an error was seen; if so, prints and clears the messages.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0)
   */
  public boolean checkErrors(java.io.PrintWriter out, int max)
  {
    if (firstError != null)
      {
	printAll(out, max);
	firstError = lastError = null;
        int saveCount = errorCount;
	errorCount = 0;
	return saveCount > 0;
      }
    return false;
  }

  /** Checks if an error was seen; if so, prints and clears the messages.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0) */
  public boolean checkErrors(java.io.PrintStream out, int max)
  {
    if (firstError != null)
      {
	printAll(out, max);
	firstError = lastError = null;
        int saveCount = errorCount;
	errorCount = 0;
	return saveCount > 0;
      }
    return false;
  }

  /** Links our location to the one give. */
  public final void setSourceLocator (SourceLocator locator)
  {
    this.locator = locator == this ? null : locator;
  }

  public final SourceLocator swapSourceLocator (SourceLocator locator)
  {
    SourceLocator save = this.locator;
    this.locator = locator;
    return save;
  }

  /** Copies the current position of locator. */
  public final void setLocation (SourceLocator locator)
  {
    this.locator = null;
    current_line = locator.getLineNumber();
    current_column = locator.getColumnNumber();
    current_filename = locator.getFileName();
  }

  public String getPublicId ()
  {
    return locator == null ? null : locator.getPublicId();
  }
  public String getSystemId ()
  {
    return locator == null ? current_filename : locator.getSystemId();
  }

  public boolean isStableSourceLocation () { return false; }

  /** The default filename to use for a new error. */
  public final String getFileName() { return current_filename; }

  /** The default line number to use for a new error. */
  public final int getLineNumber ()
  {
    return locator == null ? current_line : locator.getLineNumber();
  }

  /** The default column number to use for a new error. */
  public final int getColumnNumber ()
  {
    return locator == null ? current_column : locator.getColumnNumber();
  }

  /** Set the default filename to use for a new error. */
  public void setFile(String filename) { current_filename = filename; }
  /** Set the default line number to use for a new error. */
  public void setLine(int line) { current_line = line; }
  /** Set the default column number to use for a new error. */
  public void setColumn(int column) { current_column = column; }

  /** Set the default filename, line and column to use for a new error. */
  public void setLine(String filename, int line, int column)
  {
    current_filename = filename;
    current_line = line;
    current_column = column;
  }

}
