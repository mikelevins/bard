// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.*;
import gnu.bytecode.Type;
import gnu.lists.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.util.GeneralHashTable;
/* #ifdef use:java.util.regex */
import java.util.regex.*;
/* #endif */

public class ReaderDispatchMisc extends ReadTableEntry
{
  /** A code which specifies which particular reader-action to perform.
   * The code is one the CommonLisp or Scheme '#' reader characters.
   * For example, if code=='x' then read a hexadecimal integer.
   * If code==-1, perform the standard action for the character read. */
  protected int code;

  private static ReaderDispatchMisc instance = new ReaderDispatchMisc();

  public static ReaderDispatchMisc getInstance() { return instance; }

  public ReaderDispatchMisc()
  {
    code = -1;
  }

  public ReaderDispatchMisc(int code)
  {
    this.code = code;
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    char saveReadState = '\0';
    InPort port;
    int length;
    String name;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case ':':
	// Handle Guile-style keyword syntax: '#:KEYWORD'
	// Note this conflicts with Common Lisp uninterned symbols.  FIXME
        name = reader.readTokenString(-1, ReadTable.getCurrent());
        return gnu.expr.Keyword.make(name.intern());
      case '\\':
	return LispReader.readCharacter(reader);
      case '!':
	return LispReader.readSpecial(reader);
      case 'T':
      case 'F':
          name = reader.readTokenString(ch, ReadTable.getCurrent());
          String nameLC = name.toLowerCase();
          if (nameLC.equals("t") || nameLC.equals("true"))
              return Boolean.TRUE;
          if (nameLC.equals("f") || nameLC.equals("false"))
              return Boolean.FALSE;
          int size;
          if (nameLC.equals("f32")) size = 32;
          else if (nameLC.equals("f64")) size = 64;
          else
            {
              in.error("unexpected characters following '#'");
              return Boolean.FALSE;
            }
          return LispReader.readSimpleVector(reader, 'F',
                                             reader.read(), size);
      case 'S':
      case 'U':
	return LispReader.readSimpleVector(reader, (char) ch);
      case 'R':
	if (count > 36)
	  {
	    in.error("the radix "+count+" is too big (max is 36)");
	    count = 36;
	  }
	return LispReader.readNumberWithRadix(0, reader, count);
      case 'X':
	return LispReader.readNumberWithRadix(0, reader, 16);
      case 'D':
	return LispReader.readNumberWithRadix(0, reader, 10);
      case 'O':
	return LispReader.readNumberWithRadix(0, reader, 8);
      case 'B':
	return LispReader.readNumberWithRadix(0, reader, 2);
      case 'I':
      case 'E':
	reader.tokenBufferAppend('#');
	reader.tokenBufferAppend(ch);
	return LispReader.readNumberWithRadix(2, reader, 0);
      /* #ifdef use:java.util.regex */
      case '/':
      return readRegex(in, ch, count);
      /* #endif */
      case '|':
        readNestedComment(reader);
	return Values.empty;
      case ';':
	port = reader.getPort();
	if (port instanceof InPort)
	  {
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = ';';
	  }
	try
	  {
            reader.readObject();
	  }
	finally
	  {
	    if (port instanceof InPort)
	      ((InPort) port).readState = saveReadState;
	  }
	return Values.empty;
      case ',':
        return ReaderDispatchSyntaxQuote.readNamedConstructor(reader);
      case '=':
        return reader.readObject(count, false);
      case '#':
        if (in instanceof LispReader)
          {
            GeneralHashTable<Integer,Object> map
                = ((LispReader) in).sharedStructureTable;
            if (map != null)
              {
                Integer key = Integer.valueOf(count);
                Object object = map.get(key, in);
                if (object != in)
                  return object;
              }
          }
        in.error("an unrecognized #n# back-reference was read");
	return Boolean.FALSE;
      default:
	in.error("An invalid #-construct was read.");
	return Values.empty;
      }
  }

    public static void readNestedComment(LispReader reader)
            throws java.io.IOException, SyntaxException {
	InPort port = reader.getPort();
        char saveReadState = '\0';
	if (port instanceof InPort) {
	    saveReadState = ((InPort) port).readState;
	    ((InPort) port).readState = '|';
        }
	try {
	    reader.readNestedComment('#', '|');
        } finally {
	    if (port instanceof InPort)
                ((InPort) port).readState = saveReadState;
        }
    }

  /* #ifdef use:java.util.regex */
  public static Pattern readRegex (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    int startPos = in.tokenBufferLength;
    InPort port = in.getPort();
    char saveReadState = '\0';
    int flags = 0;
    if (port instanceof InPort)
      {
        saveReadState = ((InPort) port).readState;
        ((InPort) port).readState = '/';
      }
    try
      {
        for (;;)
          {
            int next;

            int c = port.read();
            if (c < 0)
              in.eofError("unexpected EOF in regex literal");
	    if (c == ch)
              break;
            if (c == '\\')
              {
                c = port.read();
                if ((c == ' ' ||  c == '\t' || c == '\r' || c == '\n')
                    && in instanceof LispReader)
                  {
                    c = ((LispReader) in).readEscape(c);
                    if (c == -2)
                      continue;
                  }
                if (c < 0)
                  in.eofError("unexpected EOF in regex literal");
                if (c != ch)
                  in.tokenBufferAppend('\\');
              }
            in.tokenBufferAppend(c);
          }
        String pattern = new String(in.tokenBuffer, startPos,
                                    in.tokenBufferLength - startPos);
        for (;;)
          {
            int c = in.peek();
            if (c == 'i' || c == 'I')
              flags |= Pattern.CASE_INSENSITIVE|Pattern.UNICODE_CASE;
            else if (c == 's' || c == 'S')
              flags |= Pattern.DOTALL;
            else if (c == 'm' || c == 'M')
              flags |= Pattern.MULTILINE;
            /* Think this through more before adding this feature:
            Perhaps we should use the 'x' handling from
            gnu.xquery.util.StringUtils.makePattern (which is
            smart enogh to handle space in character classes).
            Perhaps we should handle Scheme comments?

            else if (c == 'x' || c == 'X')
              flags |= Pattern.COMMENTS;
            */
            else if (Character.isLetter(c))
              {
                in.error("unrecognized regex option '"+((char) c)+'\'');
              }
            else
              break;
            in.skip();
          }
        return Pattern.compile(pattern, flags);
      }
    finally
      {
        in.tokenBufferLength = startPos;
        if (port instanceof InPort)
          ((InPort) port).readState = saveReadState;
      }
  }
  /* #endif */
}
