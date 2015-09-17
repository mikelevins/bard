package gnu.jemacs.buffer;
import gnu.kawa.io.InPort;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.expr.*;

public final class Marker extends SeqPosition
{
  Buffer buffer;

  /** Is this the special point marker? */
  public final boolean isPoint() { return buffer != null && sequence == null; }

  public Marker()
  {
  }

  public Marker(Marker marker)
  {
    buffer = marker.buffer;
    if (buffer != null)
      {
	if (marker.isPoint())
	  set(buffer, buffer.getDot(), true);
	else
	  set(marker);
      }
  }

  public Marker (Buffer buffer, int offset, boolean isAfter)
  {
    super(buffer, offset, isAfter);
    this.buffer = buffer;
  }

  public String evalLastSexp()
  {
    int leftparen = 0;
    int rightparen = 0;
    int quotecount = 0;
    Language language = Language.getDefaultLanguage();
    StringBuilder sb = new StringBuilder();
    boolean inString = false;
    boolean inParen = false;
    boolean inNumber = false;
    boolean inOther = false;
    boolean skipChar = false;
    boolean numberFinished = false;
    boolean finished = false;
    if (this.buffer.getDot() == 0)
      {
        return "End of file during parsing";
      }
    for (int i=buffer.getDot()-1; i >= 0; i--)
      {
        char c = buffer.charAt(i);
        if (Character.isWhitespace(c) && !inParen)
          {
            finished = true;
          }
        else if (c == '\"' && !inParen && !inNumber)
          {
            if (i > 0)
              {
                if (buffer.charAt(i-1) != '\\')
                  {
                    quotecount++;
                    inString = !inString;
                  }
              }
            else
              {
                quotecount++;
                inString = !inString;
              }
          }
        else if (Character.isDigit(c) && !inParen && !inString)
          {
            inNumber = false;
          }
        else if (inNumber && Character.isWhitespace(c))
          {
            inNumber = false;
            numberFinished = true;
          }
        else if (c == ')')
          {
            if (inOther)
              {
                finished = true;
                inOther = false;
                skipChar = true;
              }
            else
              {
                rightparen++;
                inParen = true;
              }
          }
        else if (c == '(')
          {
            leftparen++;
          }
        else if (!inParen && !inString)
          {
            inOther = true;
          }
        if (!skipChar)
          sb.insert(0,c);
        if  ((quotecount == 2) || (rightparen != 0 && rightparen == leftparen) | i == 0 || numberFinished || finished)
          {
            try
              {
                return language.eval(sb.toString()).toString();
              }
            catch (Error ex)
              {
                throw ex;
              }
            catch (Throwable ex)
              {
                return "Lisp error: " + ex;
              }
          }
      }
    return "";
  }



  public int getOffset()
  {
    if (buffer == null)
      return -1;
    else if (isPoint())
      return buffer.getDot();
    return nextIndex();
  }

  public int getPoint()
  {
    return 1 + getOffset();
  }

  public Buffer getBuffer()
  {
    return buffer;
  }

  public void setBuffer(Buffer buf)
  {
    this.buffer = buf;
  }

  public void setDot(int newPosition)
  {
    set(buffer, newPosition);
  }

  public void set(Buffer newBuffer, int newPosition)
  {
    if (isPoint())
      {
        if (newBuffer != buffer)
          {
            String msg;
            if (newBuffer == null)
              msg = "Can't make point-marker point nowhere: ";
            else
              msg = "Can't change buffer of point-marker: ";
            throw new Error(msg+this);
          }
	buffer.setDot(newPosition);
      }
    else
      {
        if (sequence != null)
          release();
	sequence = null;
        if (newBuffer == null)
          {
            buffer = null;
            return;
          }

        if (newPosition < 0)
          newPosition = 0;
        else
          {
            int newLength = newBuffer.length();
            if (newPosition > newLength)
              newPosition = newLength;
          }
	set(newBuffer, newPosition, false);
      }
  }

  public void removeChar(int count)
  {
    if (isPoint())
      buffer.removeChar(count);
    else
      buffer.removePos(ipos, count);
  }

  public void insert (char[] data, int off, int len, Object style)
  {
    int point = getOffset();
    buffer.insert(data, off, len, style, ipos);
    point += len;
    setDot(point);
  }

  public void insert (String string, Object style)
  {
    int point = getOffset();
    if (isPoint())
      buffer.insert(string, style);
    else
      buffer.insert(string, style, ipos);
    point += string.length();
    setDot(point);
  }

  public static final String repeatChar (int ch, int n)
  {
    char[] cbuf;
    if (ch >= 0x10000)
      {
        char c1 = (char) (((ch - 0x10000) >> 10) + 0xD800);
        char c2 = (char) ((ch & 0x3FF) + 0xDC00);
        int i = 2*n;
        cbuf = new char[i];
        while ((i -= 2) >= 0)
          {
            cbuf[i] = c1;
            cbuf[i+1] = c2;
          }
      }
    else
      {
        cbuf = new char[n];
        for (int i = n;  --i >= 0; )
          {
            cbuf[i] = (char) ch;
          }
      }
    return new String(cbuf);
  }

  /** Insert count copies of ch at the current position. */
  public void insertChar (int ch, int count, Object style)
  {
    if (count <= 0)
      return;
    int n = count > 500 ? 500 : count;
    String str = repeatChar(ch, n);
    for (;;)
      {
	insert(str, style);
	count -= n;
	if (count == 0)
	  break;
	if (count < 500)
	  {
	    n = count;
	    str = str.substring(0, ch >= 0x10000 ? 2*n : n);
	  }
      }
  }

  public void forwardChar(int i)
  {
    int point = getOffset();
    int max = buffer.maxDot();
    if (point + i > max)
      {
	point = max;
	Signal.signal("End of buffer");
      }
    point += i;
    setDot(point);
  }

  public void backwardChar(int i)
  {
    int point = getOffset();
    if (point < i)
      {
	point = 0;
	Signal.signal("Beginning of buffer");
      }
    point -= i;
    setDot(point);
  }

  public int currentColumn()
  {
    return buffer.currentColumn(getOffset());
  }

  // force is currently ignored FIXME
  public int moveToColumn(int column, boolean force)
  { 
    int lineStart = buffer.lineStartOffset(getOffset());
    InPort port = buffer.openReader(lineStart, buffer.maxDot() - lineStart);
    int resultColumn = 0;
    try
      {
	int offset = lineStart;
	for (;;)
	  {
	    int ch = port.read();
	    if (ch < 0 || ch == '\n')
	      {
		if (force)
		  {
		    // FIXME
		  }
		break;
	      }
	    int width = buffer.charWidth((char) ch, resultColumn);
	    offset++;
	    resultColumn += width;
	    if (resultColumn >= column)
	      {
		if (resultColumn > column && force)
		  {
		    // FIXME
		  }
		break;
	      }
	  }
	setDot(offset);
	return resultColumn;
      }
    catch (java.io.IOException ex)
      {
	throw new WrappedException(ex);
      }
  }

  public int forwardLine(int lines)
  {
    long value = buffer.forwardLine(lines, getOffset());
    setDot((int) value);
    return (int) (value >> 32);
  }

  /** Move to start of frame line COUNTs down.
   * Assume window width is WIDTH.
   * If LINES is negative, this is moving up. */

  /*
  public int verticalMotion(int count, int width)
  {
    if (count == 0)
      {
	moveToColumn ((currentColumn() / width) * width, false);
	return 0;
      }
    if (count > 0)
      {
	int todo = count + currentColumn() / width;
	endOfLine();
	// The loop iterates over buffer lines;
	// H is the number of screen lines in the current line, i.e.
	// the ceiling of dividing the buffer line width by width.
	for (;;)
	  {
	    int h = (currentColumn() + width - 1) / width;
	    if (h <= 0) h = 1;
	    if (h > todo)
	      break;
	    if (eobp())
	      break;
	    todo -= h;
	    forwardChar(1);  // move past '\n'.
	    endOfLine();  // and on to the end of the next line.
	  }
	if (todo >= h && todo > 0)
	  return count - todo + h - 1; // Hit end of buffer.
      }
    else // count > 0 -- Similar algorithm, but for upward motion.
      {
	int todo = - count;
	for (;;)
	  {
	    int h = (currentColumn() + width - 1) / width;
	    if (h <= 0) h = 1;
	    if (h > todo)
	      break;
	    beginningOfLine();
	    if (bobp())
	      break;
	    todo -= h;
	    backwardChar(1); // Move to end of previous line
	  }
	if (todo >= h && todo > 0)
	  return count + todo - 1 + h; // Hit beginning of buffer.
	todo = h - todo - 1;
      }
    moveToColumn(todo * width, false);
    return count;
  }
  */

  public boolean isBeginningOfLine()
  {
    int offset = getOffset();
    return offset == 0 || buffer.charAt(offset - 1) == '\n';
  }

  public boolean isEndOfLine()
  {
    int offset = getOffset();
    return offset == buffer.length() || buffer.charAt(offset) == '\n';
  }

  public int hashCode()
  {
    if (buffer == null)
      return 0;
    return buffer.hashCode() ^ getOffset();
  }

  public boolean equals (Object other)
  {
    if (! (other instanceof Marker))
      return false;
    Marker m2 = (Marker) other;
    return buffer == m2.buffer && getOffset() == m2.getOffset();
  }

  public String toString()
  {
    if (buffer == null)
      return "#<marker in no buffer>";
    StringBuffer sbuf = new StringBuffer(80);
    sbuf.append("#<marker at ");
    sbuf.append(getPoint());
    sbuf.append(" in ");
    sbuf.append(buffer.getName());
    sbuf.append('>');
    return sbuf.toString();
  }
}
