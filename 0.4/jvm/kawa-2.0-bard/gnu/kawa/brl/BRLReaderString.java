package gnu.kawa.brl;

// BRLRead.java -- a class to read BRL forms
// Copyright (C) 2001  Bruce R. Lewis and Eaton Vance Management
// See the file COPYING for license terms.

import gnu.text.*;
import gnu.kawa.io.InPort;
import gnu.lists.*;

public class BRLReaderString extends gnu.kawa.lispexpr.ReadTableEntry
{
  public Object read (Lexer in, int ch, int count)
      throws java.io.IOException
  {
	int startPos = in.tokenBufferLength;
	InPort port = in.getPort();
	char saveReadState = '\0';
	int c = ch;
	int prev;
	char endch;
	String startFile = port.getName();
	int startLine = port.getLineNumber();
	int startColumn = port.getColumnNumber();

	switch((char)ch)
	    {
	    case ']':		// normal BRL case
		endch = '[';
		break;
	    case '}':
		endch = '{';
		break;
	    case '{':
		endch = '}';
		break;
	    case '[':
		endch = ']';
		break;
	    default:
		// By default, symmetric string delimiters
		endch = (char)ch;
		break;
	    }

	if (port instanceof InPort)
	    {
		saveReadState = ((InPort) port).readState;
		((InPort) port).readState = (char) ch;
	    }

	try
	    {
		boolean inString = true;
		while (inString)
		    {
			int next;

			prev = c;

			if (port.pos < port.limit
			    && prev != '\r'
			    && prev != '\n')
			    c = port.buffer[port.pos++];
			else
			    /* If no buffered data, or if port
			       might update lineNumber */
			    c = port.read();
			if (c == endch)
			    {
				if (port.peek() == endch)
				    {
					in.tokenBufferAppend(c);
					port.skip();
				    }
				else
				  {
				    inString = false;
				    saveReadState = '\n';
				    if (in instanceof BRLRead)
				      ((BRLRead) in).saveExpressionStartPosition();
				  }
			    }
			else if (c == '\n' && in.isInteractive()
				 && ((BRLRead) in).nesting == 0)
			  {
			    port.unread();
			    inString = false;
			    in.tokenBufferAppend(c);
			  }
			else
			    {
				if (c < 0)
				  {
				    inString = false;
				    if (! in.isInteractive()
					&& ((BRLRead) in).nesting > 0)
				      in.error('e',
					       startFile, startLine + 1,
					       startColumn,
					       "nested literal text starting here was not ended by a '['");
				  }
				else
				    in.tokenBufferAppend(c);
			    }
		    }
	
		int length = in.tokenBufferLength - startPos;
		if (length == 0)
		  return BRL.emptyForm;
                String str = new String(in.tokenBuffer, startPos, length);
                /* FIXME
                if (((BRLRead) in).isBrlCompatible())
                  return str;
                else
                */
                  return new UnescapedData(str);
	    }
	finally
	    {
		in.tokenBufferLength = startPos;
		if (port instanceof InPort)
		    ((InPort) port).readState = saveReadState;
	    }
    }
}
