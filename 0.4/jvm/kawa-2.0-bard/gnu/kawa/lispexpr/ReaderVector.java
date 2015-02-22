// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.kawa.io.InPort;
import gnu.mapping.Values;
import gnu.lists.FVector;
import gnu.lists.ConstVector;
import gnu.lists.LList;
import gnu.lists.Pair;

public class ReaderVector extends ReadTableEntry
{
  char close;

  public ReaderVector(char close)
  {
    this.close = close;
  }

  public Object read (Lexer in, int ch, int count, int sharingIndex)
    throws java.io.IOException, SyntaxException
  {
    return readVector((LispReader) in, in.getPort(), count, close, sharingIndex);
  }

    public static FVector readVector(LispReader lexer, InPort port, int count, char close, int sharingIndex)
    throws java.io.IOException, SyntaxException
  {
    char saveReadState = ' ';
    if (port instanceof InPort)
      {	
	saveReadState = ((InPort) port).readState;
	((InPort) port).readState = close == ']' ? '[' : '(';
      }
    int startLine = port.getLineNumber();
    int startColumn = port.getColumnNumber()-1;
     try
       {
         ConstVector result = new ConstVector();
         lexer.bindSharedObject(sharingIndex, result);

         ReadTable rtable = ReadTable.getCurrent();
         Pair head = new Pair(null, LList.Empty);
         Pair last = head;
	 for (;;)
	   {
	     int ch = lexer.read();
	     if (ch < 0)
	       lexer.eofError("unexpected EOF in vector starting here",
			      startLine + 1, startColumn);
	     if (ch == close)
	       break;
             last = lexer.readValuesAndAppend(ch, rtable, last);
	   }
         result.setDataBackDoor(((LList) head.getCdr()).toArray());
	 return result;

       }
     finally
       {
	 if (port instanceof InPort)
	   ((InPort) port).readState = saveReadState;
       }
  }
}
