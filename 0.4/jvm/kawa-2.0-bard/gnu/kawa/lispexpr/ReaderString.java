// Copyright (c) 2001, 2009 Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.kawa.io.InPort;

public class ReaderString extends ReadTableEntry
{
  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    return readString(in, ch, count);
  }
  
  public static
  /* #ifdef use:java.lang.CharSequence */
  String
  /* #else */
  // gnu.lists.FString
  /* #endif */
  readString (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    int startPos = in.tokenBufferLength;
    InPort port = in.getPort();
    char saveReadState = '\0';
    int c = ch;
    int prev;
    if (port instanceof InPort)
      {
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = (char) ch;
      }
    try
      {
	for (;;)
	  {
	    prev = c;

	    // Read next char - inline the common case.
	    if (prev == '\r')
	      {
		c = port.read();
		if (c == '\n')
		  continue;
	      }
	    else if (port.pos < port.limit && prev != '\n')
	      c = port.buffer[port.pos++];
	    else
	      c = port.read();
	    if (c == ch)
	      {
		break;
	      }
	    switch (c)
	      {
	      case '\r':
                int t;
                if (port.getConvertCR())
                  t = '\n';
                else
                  {
                    t = '\r';
                    // To suppress possible "\r\n"-conversion.
                    c = ' ';
                  }
		in.tokenBufferAppend(t);
		continue;
	      case '\\':
		if (in instanceof LispReader)
		  c = ((LispReader) in).readEscape();
		else
		  c = port.read();
		if (c == -2)
		  {
		    c = '\n'; // So prev gets set ...
		    continue;
		  }
		/* ... fall through ... */
	      default:
		if (c < 0)
		  in.eofError("unexpected EOF in string literal");
		in.tokenBufferAppend(c);
		break;
	      }
	  }
        /* #ifdef use:java.lang.CharSequence */
	return new String(in.tokenBuffer, startPos,
                          in.tokenBufferLength - startPos).intern();
        /* #else */
        // return new gnu.lists.FString (in.tokenBuffer, startPos,
        //                               in.tokenBufferLength - startPos);
        /* #endif */
      }
    finally
      {
	in.tokenBufferLength = startPos;
	if (port instanceof InPort)
	  ((InPort) port).readState = saveReadState;
      }
  }
}
