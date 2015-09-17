// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;

/** Used to signal a non-recoverable (fatal) syntax error.
 * Can also be used to signal a syntax error from embedded parsers,
 * such as an <code>eval</code> or </code>execute</code> method,
 * or well-formedness errors when reading an XML document.
 */

public class SyntaxException extends RuntimeException
{
  /** If non-null, an extra one-line message printed before the messages.
   * Can be used to provide extra context.  */
  String header;

  /** One or more error messages that triggered this exception. */
  SourceMessages messages;

  public SyntaxException(SourceMessages messages)
  {
    this.messages = messages;
  }

  public SyntaxException(String header, SourceMessages messages)
  {
    this.header = header;
    this.messages = messages;
  }

  /** If non-null, an extra one-line message printed before the messages.
   * Can be used to provide extra context.  */
  public final String getHeader() { return header; }

  /** Set the header returned by <code>getHeader</code>. */
  public final void setHeader(String header) { this.header = header; }

  public SourceMessages getMessages () { return messages; }

  public void printAll(java.io.PrintWriter out, int max)
  {
    if (header != null)
      out.println(header);
    messages.printAll(out, max);
  }

  public void clear()
  {
    messages.clear();
  }

  public int maxToPrint = 10;

  public String getMessage ()
  {
    int max = messages.adjustDisplayMax(maxToPrint);
    StringBuffer buffer = new StringBuffer ();
    if (header != null)
      buffer.append(header);
    for (SourceError err = messages.firstError;
	 err != null;  err = err.next)
      {
        if (messages.skipDisplayMessage(max, err))
          continue;
	buffer.append('\n');
	buffer.append(err);
        max -= 2;
      }
    return buffer.toString();
  }
}
