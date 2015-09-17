// Copyright (c) 2001, 2003, 2006  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.kawa.util.RangeTable;
import gnu.mapping.*;
import gnu.expr.Language;
import gnu.kawa.reflect.StaticFieldLocation;
import gnu.bytecode.Type;

public class ReadTable extends RangeTable
{
  /** Kinds of characters. */
  public static final int ILLEGAL = 0;
  public static final int WHITESPACE = 1;
  public static final int CONSTITUENT = 2;
  public static final int SINGLE_ESCAPE = 3;
  public static final int MULTIPLE_ESCAPE = 4;
  public static final int TERMINATING_MACRO = 5;
  public static final int NON_TERMINATING_MACRO = 6;

  /** Default value to pass to setBracketMode() unless overridden. */
  public static int defaultBracketMode = -2;

  /** A character X such that PreXWord -> ($lookup$ Pre 'Word), if > 0. */
  public char postfixLookupOperator = (char) (-1);

  /** True if ":IDENTIFIER" should be treated as a keyword. */
  protected boolean initialColonIsKeyword;

  /** True if "IDENTIFIER:" should be treated as a keyword. */
  protected boolean finalColonIsKeyword;

  /** Control whether we should handle R6RS inline hex escape.
   * I.e. "\x"<hexdigits>";".
   */
  protected boolean hexEscapeAfterBackslash = true;

  /** Set whether ":IDENTIFIER" should be treated as a keyword. */
  public void setInitialColonIsKeyword (boolean whenInitial)
  {
    initialColonIsKeyword = whenInitial;
  }

  /** Set whether "IDENTIFIER:" should be treated as a keyword. */
  public void setFinalColonIsKeyword (boolean whenFinal)
  {
    finalColonIsKeyword = whenFinal;
  }

  static final ThreadLocation current = new ThreadLocation("read-table");

  public ReadTable()
  {
  }

  public void initialize(boolean sharpIsTerminating)
  {
    ReadTableEntry entry;
    entry = ReadTableEntry.getWhitespaceInstance();
    set(' ',  entry);
    set('\t', entry);
    set('\n', entry);
    set('\r', entry);
    set('\f', entry);
    //set('\v', entry);
    set('|',  ReadTableEntry.getMultipleEscapeInstance());
    set('\\', ReadTableEntry.getSingleEscapeInstance());
    set('0',  '9',  ReadTableEntry.getDigitInstance());
    entry = ReadTableEntry.getConstituentInstance();
    set('a',  'z',  entry);
    set('A',  'Z',  entry);
    set('!',  entry);
    set('$',  entry);
    set('%',  entry);
    set('&',  ReadTableEntry.ampersand);
    set('*',  entry);
    set('+',  entry);
    set('-',  entry);
    set('.',  entry);
    set('/',  entry);
    set('=',  entry);
    set('>',  entry);
    set('?',  entry);
    set('@',  entry);
    set('^',  entry);
    set('_',  entry);
    set('{',  ReadTableEntry.brace);
    set('~',  entry);
    set('\177',entry);
    set('\b', entry);
    set(':',  new ReaderColon());
    set('\"', new ReaderString());
    set('#',  ReaderDispatch.create(this, ! sharpIsTerminating));
    set(';',  ReaderIgnoreRestOfLine.getInstance());
    set('(',  ReaderParens.getInstance('(', ')'));

    set('\'', new ReaderQuote(makeSymbol(LispLanguage.quote_str)));
    set('`',  new ReaderQuote(makeSymbol(LispLanguage.quasiquote_str)));
    ReaderQuote unquoteEntry =
        new ReaderQuote(makeSymbol(LispLanguage.unquote_str),
                        '@', makeSymbol(LispLanguage.unquotesplicing_str));
    set(',',  unquoteEntry);

    setBracketMode();  // Sets the entries for '[', ']', and '<'.
  }

    /** Create a new ReadTable and initialize it appropriately for Common Lisp. */
    public static ReadTable createInitial ()
    {
        ReadTable tab = new ReadTable();
        tab.initialize(true);
        return tab;
    }

    /** Specify how '[' and ']' (and '<') are handled.
     * The value -2 means {@code [a b c]} is {@code ($bracket-list$ a b c)}
     * and {@code f[a b]} is {@code ($bracket-apply$ f a b)}.
     * The value -1 means that '[' and ']' are plain token constituents.
     * The value 0 means that '[' and ']' are equivalent to '(' and ')'.
     * The value 1 means that '[' and ']' are equivalent to '(' and ')', except
   * within a token starting with '<', in which case they are constituents.
   * This is so '[' is non-terminating when reading say '<char[]>'
   */
  public void setBracketMode(int mode)
  {
    if (mode == -2)
      {
        set('[',  ReaderParens.getInstance('[', ']', ReadTable.TERMINATING_MACRO,
                                           LispLanguage.bracket_list_sym));
        set('<', new ReaderTypespec());
      }
    else if (mode <= 0)
      {
	ReadTableEntry token = ReadTableEntry.getConstituentInstance();
	set('<', token);
	if (mode < 0)
	  {
	    set('[', token);
	    set(']', token);
	  }
      }
    else
      set('<', new ReaderTypespec());
    if (mode >= 0)
      {
	set('[', ReaderParens.getInstance('[', ']'));
	remove(']');
      }
  }

  /** Specify how '[' and ']' are handled.
   * Unless overridden, uses defaultBracketMode. */
  public void setBracketMode()
  {
    setBracketMode(defaultBracketMode);
  }
  
  /** A table mapping constructor tags to functions, as in SRFI-10. */
  Environment ctorTable = null;

  void initCtorTable ()
  {
    if (ctorTable == null)
      ctorTable = Environment.make();
  }

  /** Add a mapping for a SRFI-10 constructor tag. */
  public synchronized void putReaderCtor (String key, Procedure proc)
  {
    initCtorTable();
    ctorTable.put(key, proc);
  }

  /** Add a mapping for a SRFI-10 constructor tag. */
  public synchronized void putReaderCtor (String key, Type type)
  {
    initCtorTable();
    ctorTable.put(key, type);
  }

  /** Map a SRFI-10 constructor tag to Procedure-valued lazy field  */
  public synchronized void putReaderCtorFld (String key,
                                             String cname, String fname)
  {
    initCtorTable();
    Symbol symbol = ctorTable.getSymbol(key);
    StaticFieldLocation.define(ctorTable, symbol, null, cname, fname);
  }

  /** Resolve a SRFI-10 constructor tags to a functions. */
  public synchronized Object getReaderCtor (String key)
  {
    initCtorTable();
    return ctorTable.get(key, null);
  }

  public static ReadTable getCurrent()
  {
    ReadTable table = (ReadTable) current.get(null);
    if (table == null)
      {
	Language language = Language.getDefaultLanguage();
	if (language instanceof LispLanguage)
          {
            LispLanguage llanguage = (LispLanguage) language;
            synchronized (llanguage)
              {
                table = llanguage.defaultReadTable;
                if (table == null)
                  {
                    table = llanguage.createReadTable();
                    llanguage.defaultReadTable = table;
                  }
              }
          }
	else
	  table = ReadTable.createInitial();
	current.set(table);
      }
    return table;
  }

  public static void setCurrent(ReadTable rt)
  {
    current.set(rt);
  }

  public ReadTableEntry lookup (int ch)
  {
    ReadTableEntry entry = (ReadTableEntry) lookup(ch, null);
    if (entry == null && ch >= 0 && ch < 0x10000)
      {
	if (Character.isDigit((char) ch))
	  entry = (ReadTableEntry) lookup('0', null);
	else if (Character.isLowerCase((char) ch))
	  entry = (ReadTableEntry) lookup('a', null);
	else if (Character.isLetter((char) ch))
	  entry = (ReadTableEntry) lookup('A', null);
	else if (Character.isWhitespace((char) ch))
	  entry = (ReadTableEntry) lookup(' ', null);
	// Current code assumes lookup(')') returns null.
	if (entry == null && ch >= 128)
	  entry = ReadTableEntry.getConstituentInstance();
        if (entry == null)
          entry = ReadTableEntry.getIllegalInstance();
      }
    return entry;
  }

  protected Object makeSymbol (String name)
  {
    return Symbol.valueOf(name);
  }
}
