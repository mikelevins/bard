// Copyright (c) 2001  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.kawa.util.RangeTable;
import gnu.text.*;
import gnu.mapping.Values;

public class ReaderDispatch extends ReadTableEntry
{
  RangeTable table = new RangeTable();
  int kind;

  public int getKind()
  {
    return kind;
  }

  public void set(int key, Object value)
  {
    table.set(key, key, value);
  }

  public ReadTableEntry lookup(int key)
  {
    return (ReadTableEntry) table.lookup(key, null);
  }

  public ReaderDispatch()
  {
    kind = ReadTable.TERMINATING_MACRO;
  }

  public ReaderDispatch(boolean nonTerminating)
  {
    this.kind = nonTerminating ? ReadTable.NON_TERMINATING_MACRO
      : ReadTable.TERMINATING_MACRO;
  }

  /** Create a fresh instance and initialize it appropriately for Common Lisp.
   */
  public static ReaderDispatch create(ReadTable rtable, boolean nonTerminating)
  {
    ReaderDispatch tab = new ReaderDispatch(nonTerminating);
    ReaderDispatchMisc entry = ReaderDispatchMisc.getInstance();
    tab.set(':', entry);
    tab.set('B', entry);
    tab.set('D', entry);
    tab.set('E', entry);
    tab.set('F', entry);
    tab.set('I', entry);
    tab.set('O', entry);
    tab.set('R', entry);
    tab.set('S', entry);
    tab.set('T', entry); 
    tab.set('U', entry);
    tab.set('X', entry);
    tab.set('|', entry);
    tab.set(';', entry);
    tab.set('!', entry);
    tab.set('\\', entry);
    tab.set('=', entry);
    tab.set('#', entry);
    /* #ifdef use:java.util.regex */
    tab.set('/', entry);
    /* #endif */
    tab.set('\'', new ReaderQuote(rtable.makeSymbol("function")));
    tab.set('(', new ReaderVector(')'));
    /* #ifdef enable:XML */
    tab.set('<', new ReaderXmlElement());
    /* #endif */
    return tab;
  }

  public Object read (Lexer in, int ch, int count, int sharingIndex)
    throws java.io.IOException, SyntaxException
  {
    count = -1;
    for (;;)
      {
	ch = in.read();
	if (ch < 0)
	  in.eofError("unexpected EOF after "+ (char) ch);
	if (ch > 0x10000)
	  break;
	int digit = Character.digit((char) ch, 10);
	if (digit < 0)
	  {
	    ch = Character.toUpperCase((char) ch);
	    break;
	  }
	count = count < 0 ? digit : count * 10 + digit;
      }
    ReadTableEntry entry = (ReadTableEntry) table.lookup(ch, null);
    if (entry == null)
      {
	// Effectively subtracting 1 from the column number.
	in.error('e', in.getName(),
                 in.getLineNumber() + 1, in.getColumnNumber(),
                 "invalid dispatch character '"+((char) ch)+'\'');
	return Char.make('?');
      }
    return entry.read(in, ch, count, sharingIndex);
  }
}
