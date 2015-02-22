// Copyright (c) 2010  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.lists.*;

public class ReaderColon extends ReadTableEntry
{
  public int getKind() { return ReadTable.NON_TERMINATING_MACRO; }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    ReadTable rtable = ReadTable.getCurrent();
    int startPos = reader.tokenBufferLength;
    if (ch == rtable.postfixLookupOperator)
      {
        int next = reader.read();
        if (next == ':')
          return rtable.makeSymbol("::");
        // Force an initial ':' to be treated as a CONSTITUENT.
        reader.tokenBufferAppend(ch);
        ch = next;
      }
    return reader.readAndHandleToken(ch, startPos, rtable);
  }
}
