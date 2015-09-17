// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.text;
import gnu.kawa.io.InPort;
import java.io.*;

/**
 * Framework for implementing lexical scanners and parsers.
 * @author	Per Bothner
 */

public class Lexer extends Reader
{
  protected InPort port;
  protected boolean interactive;

  public Lexer(InPort port)
  {
    this.port = port;
  }

  public Lexer(InPort port, SourceMessages messages)
  {
    this.port = port;
    this.messages = messages;
  }

  /** Enter a nested expression.
   * This is used in interactive mode to control whether to continue
   * past end of line, depending on whether the expression is incomplete.
   * @param promptChar Used in prompt string to indicate type of nesting.
   * @return The previous value of promptChar, to be passed to popNesting.
   */
  public char pushNesting (char promptChar)
  {
    nesting++;
    InPort port = getPort();
    char save = port.readState;
    port.readState = promptChar;
    return save;
  }

  /** Exit a nested expression, reversing pushNesting
   * @param save Saved values return by prior pushNesting
   */
  public void popNesting (char save)
  {
    InPort port = getPort();
    port.readState = save;
    nesting--;
  }

  protected int nesting;

  public final InPort getPort() { return port; }

  public void close() throws java.io.IOException
  {
    port.close();
  }

  public int read() throws java.io.IOException
  {
    return port.read();
  }
 
    /** Read a Unicode character (codepoint) by checking for surrogates.
     * @deprecated Use {@link #readCodePoint}.
     */
    public int readUnicodeChar() throws java.io.IOException {
        return port.readCodePoint();
    }

    /** Read a Unicode character (codepoint) by checking for surrogates.
     */
    public int readCodePoint() throws java.io.IOException {
        return port.readCodePoint();
    }

   public int read(char[] buf, int offset, int length)
    throws java.io.IOException
  {
    return port.read(buf, offset, length);
  }

  public void unread(int ch) throws java.io.IOException
  {
    if (ch >= 0)
      port.unread();
  }

  public int peek() throws java.io.IOException
  {
    return port.peek();
  }

  public void skip() throws java.io.IOException
  {
    port.skip();
  }

  protected void unread() throws java.io.IOException
  {
    port.unread();
  }

  protected void unread_quick() throws java.io.IOException
  {
    port.unread_quick();
  }

  /**
   * Check if the next character matches a given character.
   * @param ch The character to match against.
   * @return if the character read matches
   * On a match, the position is advanced following that character.
   */
  public boolean checkNext(char ch)
      throws java.io.IOException
  {
    int r = port.read();
    if (r == ch)
	return true;
    if (r >= 0)
      port.unread_quick();
    return false;
  }

  protected void skip_quick() throws java.io.IOException
  {
    port.skip_quick();
  }

  SourceMessages messages = null;

  public SourceMessages getMessages () { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }

  /** Returns true if any error were seen.  Prints and clears the errors.
   * @param out where to write the error message to
   * @param max maximum number of messages to print (can be 0) */
  public boolean checkErrors(PrintWriter out, int max)
  {
    return messages != null && messages.checkErrors(out, max);
  }

  public SourceError getErrors()
  { return messages == null ? null : messages.getErrors(); }

  public boolean seenErrors()
  { return messages != null && messages.seenErrors(); }

  public void clearErrors() { if (messages != null) messages.clearErrors(); }

  public void error(char severity, String filename, int line, int column,
		    String message)
  {
    if (messages == null)
      messages = new SourceMessages();
    messages.error(severity, filename, line, column, message);
  }

  public void error(char severity, String message)
  {
    int line = port.getLineNumber();
    int column = port.getColumnNumber();
    error(severity, port.getName(), line + 1, column >= 0 ? column + 1 : 0,
	  message);
  }

  public void error(String message)
  {
    error('e', message);
  }

  public void fatal(String message) throws SyntaxException
  {
    error('f', message);
    throw new SyntaxException(messages);
  }

  public void eofError(String msg) throws SyntaxException
  {
    fatal(msg);
  }

  public void eofError(String message, int startLine, int startColumn)
    throws SyntaxException
  {
    error('f', port.getName(), startLine, startColumn, message);
    throw new SyntaxException(messages);
  }

  /** Read an optional signed integer.
   * If there is no integer in the input stream, return 1.
   * For excessively large exponents, return Integer.MIN_VALUE
   * or Integer.MAX_VALUE.
   */
  public int readOptionalExponent()
       throws java.io.IOException
  {
    int sign = read();
    boolean overflow = false;
    int c;
    if (sign == '+' || sign == '-')
      c = read();
    else
      {
	c = sign;
	sign = 0;
      }
    int value;
    if (c < 0 || (value = Character.digit ((char)c, 10)) < 0)
      {
	if (sign != 0)
	  error("exponent sign not followed by digit");
	value = 1;
      }
    else
      {
	int max = (Integer.MAX_VALUE - 9) / 10;
	for (;;)
	  {
	    c = read();
	    int d = Character.digit ((char)c, 10);
	    if (d < 0)
	      break;
	    if (value > max)
	      overflow = true;
	    value = 10 * value + d;
	  }
      }
    if (c >= 0)
      unread(c);
    if (sign == '-')
      value = -value;
    if (overflow)
      return sign == '-' ? Integer.MIN_VALUE : Integer.MAX_VALUE;
    return value;
  }

  /** Scan until a given delimiter.
   * On success, text upto the delimiter is in then tokenBuffer (with
   * tokenBufferLength marking its length); the delimiter is not included.
   */
  public boolean readDelimited(String delimiter)
      throws java.io.IOException, SyntaxException
  {
    tokenBufferLength = 0;
    int dlen = delimiter.length();
    char last = delimiter.charAt(dlen-1);
    for (;;)
      {
	int ch = read();
	if (ch < 0)
	  return false;
	int dstart, j;
	// Look for a match for the last delimiter character.
	if (ch == last
	    && (dstart = tokenBufferLength - (j = dlen - 1)) >= 0)
	  {
	    // Check that the initial part of the delimiter has also been seen.
	    do
	      {
		if (j == 0)
		  {
		    tokenBufferLength = dstart;
		    return true;
		  }
		j--;
	      }
	    while (tokenBuffer[dstart+j] == delimiter.charAt(j));
	  }
	tokenBufferAppend((char) ch);
      }
  }

  /** Read digits, up to the first non-digit or the buffer limit
    * @return the digits seen as a non-negative long, or -1 on overflow
    */
  public static long readDigitsInBuffer (InPort port, int radix)
  {
    long ival = 0;
    boolean overflow = false;
    long max_val = Long.MAX_VALUE / radix;
    int i = port.pos;
    if (i >= port.limit)
      return 0;
    for (;;)
      {
	char c = port.buffer[i];
	int dval = Character.digit(c, radix);
	if (dval < 0)
	  break;
	if (ival > max_val)
	  overflow = true;
	else
	  ival = ival * radix + dval;
	if (ival < 0)
	  overflow = true;
	if (++i >= port.limit)
	  break;
      }
    port.pos = i;
    return overflow ? -1 : ival;
  }

  public String getName() { return port.getName(); }

  /** Get the current line number.
    * The "first" line is number number 0. */
  public int getLineNumber() { return port.getLineNumber(); }

  /** Return the current (zero-based) column number. */
  public int getColumnNumber() { return port.getColumnNumber(); }

  public boolean isInteractive() { return interactive; }
  public void setInteractive(boolean v) { interactive = v; }

  /** For building tokens of various kinds. */
  public char[] tokenBuffer = new char[100];

  /** The number of chars of tokenBuffer that are used. */
  public int tokenBufferLength = 0;

  /** Append one character to tokenBuffer, resizing it if need be. */
  public void tokenBufferAppend(int ch)
  {
    if (ch >= 0x10000)
      {
        tokenBufferAppend(((ch - 0x10000) >> 10) + 0xD800);
        ch = (ch & 0x3FF) + 0xDC00;
        // fall through to append low surrogate.
      }
    int len = tokenBufferLength;
    char[] buffer = tokenBuffer;
    if (len == tokenBuffer.length)
      {
	tokenBuffer = new char[2 * len];
	System.arraycopy(buffer, 0, tokenBuffer, 0, len);
	buffer = tokenBuffer;
      }
    buffer[len] = (char) ch;
    tokenBufferLength = len + 1;
  }

  public String tokenBufferString ()
  {
    return new String(tokenBuffer, 0, tokenBufferLength);
  }

  private int saveTokenBufferLength = -1;

  /** Start tentative parsing.  Must be followed by a reset. */
  public void mark ()
    throws java.io.IOException
  {
    if (saveTokenBufferLength >= 0)
      throw new Error("internal error: recursive call to mark not allowed");
    port.mark(Integer.MAX_VALUE);
    saveTokenBufferLength = tokenBufferLength;
  }

  /** Stop tentative parsing.  Return to position where we called mark. */
  public void reset ()
    throws java.io.IOException
  {
    if (saveTokenBufferLength < 0)
      throw new Error("internal error: reset called without prior mark");
    port.reset();
    saveTokenBufferLength = -1;
  }
}
