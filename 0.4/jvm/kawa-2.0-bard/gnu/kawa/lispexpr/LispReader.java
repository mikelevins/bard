package gnu.kawa.lispexpr;
import gnu.text.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.math.*;
import gnu.expr.*;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.InPort;
import gnu.kawa.util.GeneralHashTable;
import java.util.regex.*;

/** A Lexer for reading S-expressions in generic Lisp-like syntax.
 * This class may have outlived its usefulness: It's mostly just a
 * wrapper around an InPort plus a helper token-buffer.
 * The functionality should be moved to ReadTable, though it is
 * unclear what to do about the tokenBuffer.
 */

public class LispReader extends Lexer
{
  public LispReader(InPort port)
  {
    super(port);
  }

  public LispReader(InPort port, SourceMessages messages)
  {
    super(port, messages);
  }

    boolean returnMutablePairs;
    /** Set whether returned pairs are mutable or not (the default). */
    public void setReturnMutablePairs(boolean v) { returnMutablePairs = v; }

  GeneralHashTable<Integer,Object> sharedStructureTable;

    /** Bind value to index in sharingStructuretable.
     * @param value The object being defined.
     * @param sharingIndex Back-reference index.
     *   I.e. the value N in a @code{#N=} form.  If negative, do nothing.
     * @return The value unchanged.
     */
    public Object bindSharedObject(int sharingIndex, Object value) {
	if (sharingIndex >= 0) {
	    GeneralHashTable<Integer,Object> map = sharedStructureTable;
	    if (map == null) {
		map = new GeneralHashTable<Integer,Object>();
		sharedStructureTable = map;
	    }
	    Integer key = Integer.valueOf(sharingIndex);
	    if (map.get(key, this) != this)
              error('w', "a duplicate #n= definition was read");
	    map.put(key, value);
	}
	return value;
    }

    /** Read a #|...|#-style comment (which may contain other nested comments).
     * Assumes the initial "#|" has already been read.
     */
    final public void readNestedComment (char c1, char c2)
        throws java.io.IOException, SyntaxException {
        int commentNesting = 1;
        int startLine = port.getLineNumber();
        int startColumn = port.getColumnNumber();
        StringBuilder buf = null;
        if (port instanceof BinaryInPort && (startLine == 0 || startLine == 1))
            buf = new StringBuilder();
        do {
            int c = read ();
            if (buf != null)
                buf.append((char) c);
            if (c == '|') {
                c = read();
                if (buf != null)
                    buf.append((char) c);
                if (c == c1)
                    commentNesting--;
            } else if (c == c1) {
                c = read();
                if (c == c2)
                    commentNesting++;
            }
            if (c < 0) {
                eofError("unexpected end-of-file in " + c1 + c2
                         + " comment starting here",
                         startLine + 1, startColumn - 1);
                return;
            }
        } while (commentNesting > 0);
        if (buf != null)
            checkEncodingSpec(buf.toString());
    }

    public void checkEncodingSpec(String line) {
        Matcher m = Pattern.compile("coding[:=]\\s*([-a-zA-Z0-9]+)")
            .matcher(line);
        if (m.find()) {
            String enc = m.group(1);
            try {
                ((BinaryInPort) getPort()).setCharset(enc);
            } catch (java.nio.charset.UnsupportedCharsetException ex) {
                error('e', "unrecognized encoding name "+enc);
            } catch (Exception ex) {
                error('e', "cannot set encoding name here");
            }
        }
    }

    boolean inQuasiSyntax;

  char readCase = lookupReadCase();

  /** Get specification of how symbols should be case-folded.
    * @return Either '\0' (unspecified - defaults to preserve case),
    * 'P' (means preserve case), 'U' (upcase),
    * 'D' (downcase), or 'I' (invert case).
    */
  public char getReadCase () { return readCase; }
  public void setReadCase(char readCase) { this.readCase = readCase; }

  static char lookupReadCase()
  {
    try
      {
	String read_case_string
	  = Environment.getCurrent().get("symbol-read-case", "P").toString();
        if (read_case_string.length() > 0)
          {
            char read_case = read_case_string.charAt(0);
            if (read_case == 'P') ;
            else if (read_case == 'u')
              read_case = 'U';
            else if (read_case == 'd' || read_case == 'l' || read_case == 'L')
              read_case = 'D';
            else if (read_case == 'i')
              read_case = 'I';
            return read_case;
          }
      }
    catch (Exception ex)
      {
      }
    return '\0';
  }

    public Object readValues (int ch,  ReadTable rtable, int sharingIndex)
            throws java.io.IOException, SyntaxException {
        return readValues(ch, rtable.lookup(ch), rtable, sharingIndex);
    }

    /** May return zero or multiple values.
     * Returns no values if looking at whitespace or a comment. */
    public Object readValues (int ch, ReadTableEntry entry, ReadTable rtable,
                              int sharingIndex)
        throws java.io.IOException, SyntaxException {
        seenEscapes = false;
        return entry.read(this, ch, -1, sharingIndex);
    }

    public Pair readValuesAndAppend(int ch, ReadTable rtable, Pair last)
            throws java.io.IOException, SyntaxException {
        int line = port.getLineNumber();
        int column = port.getColumnNumber();
        Object values = readValues(ch, rtable, -1);
        int index = 0;
        int next = Values.nextIndex(values, index);
        if (next >= 0) {
            for (;;) {
                Object value = Values.nextValue(values, index);
                index = next;
                if (value == gnu.expr.QuoteExp.voidExp)
                    value = Values.empty;
                next = Values.nextIndex(values, index);
                if (next < 0)
                    value = handlePostfix(value, rtable, line, column);
                Pair pair = makePair(value, line, column);
                setCdr(last, pair);
                last = pair;
                if (next < 0)
                    break;
            }
        }
        return last;
    }

  protected Object readAndHandleToken(int ch, int startPos, ReadTable rtable)
    throws java.io.IOException, SyntaxException
  {
    char readCase = getReadCase();
    readToken(ch, rtable);
    int endPos = tokenBufferLength;
    if (! seenEscapes)
      {
        Object value = parseNumber(tokenBuffer, startPos, endPos - startPos,
                                   '\0', 0, SCM_NUMBERS);
        if (value != null && ! (value instanceof String))
          {
            tokenBufferLength = startPos;
            return value;
          }
        /* Common Lisp only?  FIXME
        if (isPotentialNumber(tokenBuffer, startPos, endPos))
          {
            error(value == null ? "not a valid number"
                  : "not a valid number: " + value);
            return IntNum.zero();
          }
        */
      }

    if (readCase == 'I')
      {
	int upperCount = 0;
	int lowerCount = 0;
	for (int i = startPos;  i < endPos;  i++)
	  {
	    char ci = tokenBuffer[i];
	    if (ci == TOKEN_ESCAPE_CHAR)
	      i++;
	    else if (Character.isLowerCase(ci))
	      lowerCount++;
	    else if (Character.isUpperCase(ci))
	      upperCount++;
	  }
	if (lowerCount == 0)
	  readCase = 'D';
	else if (upperCount == 0)
	  readCase = 'U';
	else
	  readCase = 'P';
      }

    boolean handleUri =
      (endPos >= startPos + 2
       && tokenBuffer[endPos-1] == '}'
       && tokenBuffer[endPos-2] != TOKEN_ESCAPE_CHAR
       && peek() == ':');
    int packageMarker = -1;
    int lbrace = -1, rbrace = -1, braceNesting = 0;
    int j = startPos;
    boolean uriBad = false;
    for (int i = startPos;  i < endPos;  i++)
      {
	char ci = tokenBuffer[i];
	if (ci == TOKEN_ESCAPE_CHAR)
	  {
	    if (++ i < endPos)
	      tokenBuffer[j++] = tokenBuffer[i];
	    continue;
	  }
        if (handleUri)
          {
            if (ci == '{')
              {
                if (lbrace < 0)
                  lbrace = j;
                else if (braceNesting == 0)
                  uriBad = true;
                braceNesting++;
              }
            else if (ci == '}')
              {
                braceNesting--;
                if (braceNesting < 0)
                   uriBad = true;
                else if (braceNesting == 0)
                  {
                    if (rbrace < 0)
                      rbrace = j;
                    else
                      uriBad = true;
                  }
              }
          }
        if (braceNesting > 0)
          ;
	else if (ci == ':')
	  packageMarker = packageMarker >= 0 ? -1 : j;
	else if (readCase == 'U')
	  ci = Character.toUpperCase(ci);
	else if (readCase == 'D')
	  ci = Character.toLowerCase(ci);
	tokenBuffer[j++] = ci;
      }
    endPos = j;

    int len = endPos - startPos;

    Object result;
    if (lbrace >= 0 && rbrace > lbrace)
      {
        String prefix = lbrace > 0 ? new String(tokenBuffer, startPos, lbrace-startPos) : null;
        lbrace++;
        String uri = new String(tokenBuffer, lbrace, rbrace-lbrace);
        ch = read(); // skip ':' - previously peeked.
        ch = read();
        Object rightOperand = readValues(ch, rtable.lookup(ch), rtable, -1);
        if (! (rightOperand instanceof SimpleSymbol))
          error("expected identifier in symbol after '{URI}:'");
        // FIXME should allow "compound keyword" - for attribute names
        result = Symbol.valueOf(rightOperand.toString(), uri, prefix);
      }
    else if (rtable.initialColonIsKeyword && packageMarker == startPos && len > 1)
      {
	startPos++;
	String str = new String(tokenBuffer, startPos, endPos-startPos);
	result = Keyword.make(str.intern());
    }
    else if (rtable.finalColonIsKeyword && packageMarker != -1 && packageMarker == endPos - 1
        && (len > 1 || seenEscapes))
      {
	String str = new String(tokenBuffer, startPos, len - 1);
	result = Keyword.make(str.intern());
      }
    else {
      if (len == 1 && tokenBuffer[startPos] == '.' && !seenEscapes)
          error("invalid use of '.' token");
      result = rtable.makeSymbol(new String(tokenBuffer, startPos, len));
    }
    tokenBufferLength = startPos;
    return result;
  }

  public static final char TOKEN_ESCAPE_CHAR = '\uffff';

  /** If true, then tokenbuffer contains escaped characters.
   * These are prefixed (in the buffer) by TOKEN_ESCAPE_CHAR.
   */
  protected boolean seenEscapes;

    /** Read token, leaving characters in tokenBuffer.
     * Sets seenEscapes if escape characters are seen.
     */
  void readToken(int ch, ReadTable rtable)
      throws java.io.IOException, SyntaxException
  {
    boolean inEscapes = false;
    int braceNesting = 0;
    for (;; ch = read())
      {
	if (ch < 0)
	  {
	    if (inEscapes)
	      eofError("unexpected EOF between escapes");
	    else
	      break;
	  }
	ReadTableEntry entry = rtable.lookup(ch);
	int kind = entry.getKind();
	if (kind == ReadTable.ILLEGAL)
	  {
	    if (inEscapes)
	      {
		tokenBufferAppend(TOKEN_ESCAPE_CHAR);
		tokenBufferAppend(ch);
		continue;
	      }
            if (ch == '}' && --braceNesting >= 0)
              {
                tokenBufferAppend(ch);
		continue;
              }
	    unread(ch);
	    break;
	  }
        if (ch == rtable.postfixLookupOperator && ! inEscapes)
          {
            int next = port.peek();
            if (next == rtable.postfixLookupOperator)
              { // Looking at '::'
                unread(ch);
                break;
              }
            if (validPostfixLookupStart(next, rtable))
              kind = ReadTable.TERMINATING_MACRO;
          }
                  
	if (kind == ReadTable.SINGLE_ESCAPE)
	  {
	    ch = read();
	    if (ch < 0)
	      eofError("unexpected EOF after single escape");
            if (rtable.hexEscapeAfterBackslash
                // We've allowed hex escapes for a while.
                // Allow R7RS general escapes - but only inside |bars|.
                && (inEscapes || ch == 'x' || ch == 'X'))
                ch = readEscape(ch);
            if (ch >= 0)
              {
                tokenBufferAppend(TOKEN_ESCAPE_CHAR);
                tokenBufferAppend(ch);
              }
	    seenEscapes = true;
	    continue;
	  }
	if (kind == ReadTable.MULTIPLE_ESCAPE)
	  {
	    inEscapes = ! inEscapes;
	    seenEscapes = true;
	    continue;
	  }
	if (inEscapes)
	  {
	    // Step 9:
	    tokenBufferAppend(TOKEN_ESCAPE_CHAR);
	    tokenBufferAppend(ch);
	  }
	else
	  {
	    // Step 8:
	    switch (kind)
	      {
	      case ReadTable.CONSTITUENT:
                if (ch == '{' && entry == ReadTableEntry.brace)
                  braceNesting++;
                /* ... fall through ... */
	      case ReadTable.NON_TERMINATING_MACRO:
		tokenBufferAppend(ch);
		continue;
	      case ReadTable.MULTIPLE_ESCAPE:
		inEscapes = true;
		seenEscapes = true;
		continue;
	      case ReadTable.TERMINATING_MACRO:
		unread(ch);
		return;
	      case ReadTable.WHITESPACE:
		// if (readPreservingWhitespace) FIXME
		unread(ch);
		return;
	      }
	  }
      }
  }

    public String readTokenString(int ch, ReadTable rtable)
            throws java.io.IOException, SyntaxException {
        int startPos = tokenBufferLength;
        if (ch >= 0)
            tokenBufferAppend(ch);
        readToken(read(), rtable);
        int length = tokenBufferLength - startPos;
        String str = new String(tokenBuffer, startPos, length);
        tokenBufferLength = startPos;
        return str;
    }

    public Object readObject() throws java.io.IOException, SyntaxException {
	return readObject(-1, false);
    }

    public Object readObject(int sharingIndex, boolean topLevel)
	throws java.io.IOException, SyntaxException
    {
    char saveReadState = ((InPort) port).readState;
    int startPos = tokenBufferLength;
    ((InPort) port).readState = ' ';
    try
      {
        ReadTable rtable = ReadTable.getCurrent();
	for (;;)
	  {
	    int line = port.getLineNumber();
	    int column = port.getColumnNumber();
	    int ch = port.read();
	    if (ch < 0)
	      return Sequence.eofValue; // FIXME
            Object value = readValues(ch, rtable, sharingIndex);
	    if (value == Values.empty)
	      continue;
	    value = handlePostfix(value, rtable, line, column);
            if (topLevel && ! (value instanceof Pair))
              {
                // Wrap in begin form so top-level forms have position info.
                value = makePair(kawa.standard.begin.begin,
                                 makePair(value, line, column), line, column);
              }
	    return value;
	  }
      }
    finally
      {
	tokenBufferLength = startPos;
	((InPort) port).readState = saveReadState;
      }
  }

    protected boolean validPostfixLookupStart (int ch, ReadTable rtable)
            throws java.io.IOException {
        if (ch < 0 || ch == rtable.postfixLookupOperator)
            return false;
        if (ch == ',')
            return true;
        if (ch == '@')
            return true; // To support deprecated (TYPE:@ EXP)
        int kind = rtable.lookup(ch).getKind();
        return kind == ReadTable.CONSTITUENT
            || kind == ReadTable.NON_TERMINATING_MACRO
            || kind == ReadTable.MULTIPLE_ESCAPE
            || kind == ReadTable.SINGLE_ESCAPE;
    }

    /** After reading a value check for following {@code '['} or {@code ':'}.
     */
    Object handlePostfix (Object value, ReadTable rtable, int line, int column)
        throws java.io.IOException, SyntaxException {
        if (value == QuoteExp.voidExp)
            value = Values.empty;
        for (;;) {
            int ch = port.peek();
            String str; int slen;
            if (ch == '[' && rtable.defaultBracketMode == -2) {
                port.read();
                Object lst = ReaderParens.readList(this, null, ch, 1, ']', -1);
                value = makePair(value, lst, line, column);
                value = makePair(LispLanguage.bracket_apply_sym, value,
                                              line, column);
            } else if (ch == rtable.postfixLookupOperator) {
                // A kludge to map PreOpWord to ($lookup$ Pre 'Word).
                port.read();
                int ch2 = port.peek();
                Object rightOperand;
                if (ch2 == '@') {
                    error('w',
                          "deprecated cast syntax TYPE:@ (use ->TYPE instead)");
                    rightOperand = readAndHandleToken('\\', 0, rtable);
                } else {
                    if (! validPostfixLookupStart(ch2, rtable)) {
                        unread();
                        break;
                    }
                    ch = port.read();
                    rightOperand = readValues(ch, rtable.lookup(ch), rtable, -1);
                }
                value = LList.list2(value,
                                    LList.list2(LispLanguage.quasiquote_sym, rightOperand));
                value = makePair(LispLanguage.lookup_sym, value,
                                 line, column);
            }
            else
                break;
        }
        return value;
    }

  private boolean isPotentialNumber (char[] buffer, int start, int end)
  {
    int sawDigits = 0;
    for (int i = start;  i < end;  i++)
      {
	char ch = buffer[i];
	if (Character.isDigit(ch))
	  sawDigits++;
	else if (ch == '-' || ch == '+')
	  {
	    if (i + 1 == end)
	      return false;
	  }
	else if (ch == '#')
	  return true;
	else if (Character.isLetter(ch) || ch == '/'
		 || ch == '_' || ch == '^')
 	  {
	    // CommonLisp defines _123 (and ^123) as a "potential number";
	    // most implementations seem to define it as a symbol.
	    // Scheme does defines it as a symbol.
	    if (i == start)
	      return false;
	  }
	else if (ch != '.')
	  return false;
      }
    return sawDigits > 0;
  }

  static final int SCM_COMPLEX = 1;
  public static final int SCM_NUMBERS = SCM_COMPLEX;
  public static final int SCM_ANGLE = SCM_NUMBERS << 1;
  public static final int SCM_COLATITUDE = SCM_ANGLE << 1;

  public static Object parseNumber
  /* #ifdef use:java.lang.CharSequence */
  (CharSequence str, int radix)
  /* #else */
  // (CharSeq str, int radix)
  /* #endif */
  {
    char[] buf;
    if (str instanceof FString)
      buf = ((FString) str).data;
    else
      buf = str.toString().toCharArray();
    int len = str.length();
    return parseNumber(buf, 0, len, '\0', radix, LispReader.SCM_NUMBERS);
  }

  /** Parse a number.
   * @param buffer contains the characters of the number
   * @param start startinging index of the number in the buffer
   * @param count number of characters in buffer to use
   * @param exactness either 'i' or 'I' force an inexact result,
   *   either 'e' or 'E' force an exact result,
   *   '\0' yields an inact or inexact depending on the form of the literal,
   *   while ' ' is like '\0' but does not allow more exactness specifiers.
   * @param radix the number base to use or 0 if unspecified
   *   A negative radix is an overideable default.
   * @return the number if a valid number; null or a String-valued error
   *   message if if there was some error parsing the number.
   */
  public static Object parseNumber(char[] buffer, int start, int count,
				   char exactness, int radix, int flags)
  {
    int end = start + count;
    int pos = start;
    if (pos >= end)
      return "no digits";
    char ch = buffer[pos++];
    while (ch == '#')
      {
	if (pos >= end)
	  return "no digits";
	ch = buffer[pos++];
	switch (ch)
	  {
	  case 'b':  case 'B':
	    if (radix > 0)
	      return "duplicate radix specifier";
	    radix = 2;
	    break;
	  case 'o':  case 'O':
	    if (radix > 0)
	      return "duplicate radix specifier";
	    radix = 8;
	    break;
	  case 'd':  case 'D':
	    if (radix > 0)
	      return "duplicate radix specifier";
	    radix = 10;
	    break;
	  case 'x':  case 'X':
	    if (radix > 0)
	      return "duplicate radix specifier";
	    radix = 16;
	    break;
	  case 'e':  case 'E':
	  case 'i':  case 'I':
	    if (exactness != '\0')
	      {
		if (exactness == ' ')
		  return "non-prefix exactness specifier";
		else
		  return "duplicate exactness specifier";
	      }
	    exactness = ch;
	    break;
	  default:
	    int value = 0;
	    for (;;)
	      {
		int dig = Character.digit(ch, 10);
		if (dig < 0)
		  break;
		value = 10 * value + dig;
		if (pos >= end)
		  return "missing letter after '#'";
		ch = buffer[pos++];
	      }
	    if (ch == 'R' || ch == 'r')
	      {
		if (radix > 0)
		  return "duplicate radix specifier";
		if (value < 2 || value > 35)
		  return "invalid radix specifier";
		radix = value;
		break;
	      }
	    return "unknown modifier '#" + ch + '\'';
	  }
	if (pos >= end)
	  return "no digits";
	ch = buffer[pos++];
      }
    if (exactness == '\0')
      exactness = ' ';
    if (radix < 0)
        radix = -radix;
    else if (radix == 0)
      {
        radix = 10;
        /*
	for (int i = count;  ; )
	  {
	    if (--i < 0)
	      {
		// FIXME - should get *read-base* in CommonLisp:
		// radix = *read_base*;
		radix = 10;
		break;
	      }
	    if (buffer[start+i] == '.')
	      {
		radix = 10;
		break;
	      }
	  }
        */
      }

    boolean negative = ch == '-';
    boolean numeratorNegative = negative;
    boolean sign_seen = ch == '-' || ch == '+';
    if (sign_seen)
      {
	if (pos >= end)
	  return "no digits following sign";
	ch = buffer[pos++];
      }

    // Special case for '+i' and '-i'.
    if ((ch == 'i' || ch == 'I') &&
        (pos == end || buffer[pos] == '+' || buffer[pos] == '-') &&
        start == pos - 2 && (flags & SCM_COMPLEX) != 0) {
        char sign = buffer[start];
        if (sign != '+' && sign != '-')
            return "no digits";
        if (pos < end) {
            Object jmag = parseNumber(buffer, pos, end-pos, exactness,
                                      10, flags);
            if (jmag instanceof String)
                return jmag;
            if (! (jmag instanceof Quaternion))
                return "invalid numeric constant ("+jmag+")";
            Quaternion qjmag = (Quaternion) jmag;
            RealNum re = qjmag.re();
            RealNum im = qjmag.im();
            if (!(re.isZero() && im.isZero()))
                return "invalid numeric constant";
            if (exactness == 'i' || exactness == 'I')
                return Quaternion.make(0, negative ? -1 : 1,
                                       qjmag.doubleJmagValue(),
                                       qjmag.doubleKmagValue());
            return Quaternion.make(IntNum.zero(), negative ?
                                   IntNum.minusOne() : IntNum.one(),
                                   qjmag.jm(), qjmag.km());
        }
        if (exactness == 'i' || exactness == 'I')
            return new DComplex(0, negative ? -1 : 1);
        return negative ? Complex.imMinusOne() : Complex.imOne();
    }
    // Special case for '+j' and '-j'.
    if ((ch == 'j' || ch == 'J') &&
        (pos == end || buffer[pos] == '+' || buffer[pos] == '-') &&
        start == pos - 2 && (flags & SCM_COMPLEX) != 0) {
        char sign = buffer[start];
        if (sign != '+' && sign != '-')
            return "no digits";
        if (pos < end) {
            Object kmag = parseNumber(buffer, pos, end-pos, exactness,
                                      10, flags);
            if (kmag instanceof String)
                return kmag;
            if (! (kmag instanceof Quaternion))
                return "invalid numeric constant ("+kmag+")";
            Quaternion qkmag = (Quaternion) kmag;
            RealNum re = qkmag.re();
            RealNum im = qkmag.im();
            RealNum jm = qkmag.jm();
            if (!(re.isZero() && im.isZero() && jm.isZero()))
                return "invalid numeric constant";
            if (exactness == 'i' || exactness == 'I')
                return Quaternion.make(0, 0, negative ? -1 : 1,
                                       qkmag.doubleKmagValue());
            return Quaternion.make(IntNum.zero(), IntNum.zero(),
                                   negative ? IntNum.minusOne() : IntNum.one(),
                                   qkmag.km());
        }
        if (exactness == 'i' || exactness == 'I')
            return new DQuaternion(0, 0, 0, negative ? -1 : 1);
        return negative ? Quaternion.jmMinusOne() : Quaternion.jmOne();
    }
    // Special case for '+k' and '-k'.
    if ((ch == 'k' || ch == 'K') && pos == end && start == pos - 2
	&& (flags & SCM_COMPLEX) != 0) {
        char sign = buffer[start];
        if (sign != '+' && sign != '-')
            return "no digits";
        if (exactness == 'i' || exactness == 'I')
            return new DQuaternion(0, 0, 0, negative ? -1 : 1);
        return negative ? Quaternion.kmMinusOne() : Quaternion.kmOne();
    }

    int realStart = pos - 1;
    boolean hash_seen = false;
    int exp_seen = -1;
    int digits_start = -1;
    int decimal_point = -1;
    boolean copy_needed = false;
    boolean underscore_seen = false;
    IntNum numerator = null;
    long lvalue = 0;
  loop:
    for (;;)
      {
	int digit = Character.digit(ch, radix);
	if (digit >= 0)
	  {
	    if (hash_seen && decimal_point < 0)
	      return "digit after '#' in number";
	    if (digits_start < 0)
	      digits_start = pos - 1;
	    lvalue = radix * lvalue + digit;
	  }
	else
	  {
	    switch (ch)
	      {
		/*
	      case '_':
		underscore_seen = true;
		break;
		*/
		/*
	      case '#':
		if (radix != 10)
		  return "'#' in non-decimal number";
		if (digits_start < 0)
		  return "'#' with no preceeding digits in number";
		hash_seen = true;
		break;
		*/
	      case '.':
		if (decimal_point >= 0)
		  return "duplicate '.' in number";
		if (radix != 10)
		  return "'.' in non-decimal number";
		decimal_point = pos - 1;
		break;
	      case 'e': case 's': case 'f': case 'd': case 'l':
	      case 'E': case 'S': case 'F': case 'D': case 'L':
		if (pos == end || radix != 10)
		  {
		    pos--;
		    break loop;
		  }
		char next = buffer[pos];
                int exp_pos = pos-1;
		if (next == '+' || next == '-')
		  {
		    if (++ pos >= end
			|| Character.digit(buffer[pos], 10) < 0)
		      return "missing exponent digits";
		  }
		else if (Character.digit(next, 10) < 0)
		  {
		    pos--;
		    break loop;
		  }
		if (exp_seen >= 0)
		  return "duplicate exponent";
		if (radix != 10)
		  return "exponent in non-decimal number";
		if (digits_start < 0)
		  return "mantissa with no digits";
		exp_seen = exp_pos;
		for (;;)
		  {
		    pos++;
		    if (pos >= end || Character.digit(buffer[pos], 10) < 0)
		      break loop;
		  }
	      case '/':
		if (numerator != null)
		  return "multiple fraction symbol '/'";
		if (digits_start < 0)
		  return "no digits before fraction symbol '/'";
		if (exp_seen >= 0 || decimal_point >= 0)
		  return "fraction symbol '/' following exponent or '.'";
		numerator = valueOf(buffer, digits_start, pos - digits_start,
				    radix, negative, lvalue);
		digits_start = -1;
		lvalue = 0;
		negative = false;
		hash_seen = false;
		underscore_seen = false;
		break;
	      default:
		pos--;
		break loop;
	      }
	  }
	if (pos == end)
	  break;
	ch = buffer[pos++];
      }

    char infnan = '\0';

    if (digits_start < 0)
      {
        if (sign_seen
            && pos + 4 < end && buffer[pos+3] == '.' && buffer[pos+4] == '0')
          {
            char b0 = buffer[pos];
            char b1, b2;
            if ((b0 == 'i' || b0 == 'I')
                && ((b1 = buffer[pos+1]) == 'n' || b1 == 'N')
                && ((b2 = buffer[pos+2]) == 'f' || b2 == 'F'))
              {
                infnan = 'i';
              }
            else if ((b0 == 'n' || b0 == 'N')
                     && ((b1 = buffer[pos+1]) == 'a' || b1 == 'A')
                     && ((b2 = buffer[pos+2]) == 'n' || b2 == 'N'))
              {
                infnan = 'n';
              }
          }
        if (infnan == '\0')
          return "no digits";
        pos += 5;
      }

    if (hash_seen || underscore_seen)
      {
	// FIXME make copy, removing '_' and replacing '#' by '0'.
      }

    boolean inexact = (exactness == 'i' || exactness == 'I'
		       || (exactness == ' ' && hash_seen));
    RealNum number = null;
    char exp_char = '\0';
    if (infnan != '\0')
      {
        inexact = true;
	double d = infnan == 'i' ? Double.POSITIVE_INFINITY : Double.NaN;
	number = new DFloNum(negative ? - d : d);
      }
    else if (exp_seen >= 0 || decimal_point >= 0)
      {
	if (digits_start > decimal_point && decimal_point >= 0)
	  digits_start = decimal_point;
	if (numerator != null)
	  return "floating-point number after fraction symbol '/'";
	String str = new String(buffer, digits_start, pos - digits_start);
        if (exp_seen >= 0)
          {
            exp_char = Character.toLowerCase(buffer[exp_seen]);
            if (exp_char != 'e')
              {
                int prefix = exp_seen - digits_start;
                str = str.substring(0, prefix)+'e'+str.substring(prefix+1);
              }
          }
	double d = Convert.parseDouble(str);
	number = new DFloNum(negative ? - d : d);
      }
    else
      {
	IntNum iresult = valueOf(buffer, digits_start, pos - digits_start,
				 radix, negative, lvalue);
	if (numerator == null)
	  number = iresult;
	else
	  {
	    // Check for zero denominator values: 0/0, n/0, and -n/0
	    // (i.e. NaN, Infinity, and -Infinity).
	    if (iresult.isZero ())
	      {
		boolean numeratorZero = numerator.isZero();
		if (inexact)
		  number =  new DFloNum ((numeratorZero ? Double.NaN
					  : numeratorNegative ? Double.NEGATIVE_INFINITY
					  : Double.POSITIVE_INFINITY));
		else if (numeratorZero)
		  return "0/0 is undefined";
		else
		  number = RatNum.make(numerator, iresult);
	      }
	    else
	      {
		number = RatNum.make(numerator, iresult);
	      }
	  }
	if (inexact && number.isExact())
	  // We want #i-0 or #i-0/1 to be -0.0, not 0.0.
	  number = new DFloNum(numeratorNegative && number.isZero() ? -0.0
			       : number.doubleValue());
      }

    if (exactness == 'e' || exactness == 'E')
      number = number.toExact();

    if (pos < end)
      {
	ch = buffer[pos++];

	if (ch == '@')
	  { /* polar notation */
	    Object angle = parseNumber(buffer, pos, end - pos,
				       exactness, 10, flags|SCM_ANGLE);
	    if (angle instanceof String)
	      return angle;
	    if (! (angle instanceof RealNum) && ! (angle instanceof RealNum[]))
	      return "invalid complex polar constant";
            if (angle instanceof RealNum[]) {
                RealNum[] polars = (RealNum[]) angle;
                if (number.isZero() &&
                    (!polars[0].isExact() || !polars[1].isExact() ||
                     !polars[2].isExact()))
                    return new DFloNum(0.0);
                return Quaternion.polar(number, polars[0], polars[1],
                                        polars[2]);
            }
	    RealNum rangle = (RealNum) angle;
	    /* r4rs requires 0@1.0 to be inexact zero, even if (make-polar
	     * 0 1.0) is exact zero, so check for this case.  */
	    if (number.isZero () && !rangle.isExact ())
	      return new DFloNum (0.0);

	    return Complex.polar (number, rangle);
	  }
        if (ch == '%') {
            /* extended polar notation */
            Object colatitude = parseNumber(buffer, pos, end - pos,
                                            exactness, 10,
                                            flags|SCM_COLATITUDE);
            if (colatitude instanceof String)
                return colatitude;
            if (!(colatitude instanceof RealNum) &&
                !(colatitude instanceof RealNum[]))
                return "invalid quaternion polar constant";
            if ((flags & SCM_ANGLE) == 0) {
                // number%colatitude or number%colatitude&longitude
                RealNum rangle = IntNum.zero();
                RealNum rcolatitude, rlongitude;
                if (colatitude instanceof RealNum) {
                    rcolatitude = (RealNum) colatitude;
                    rlongitude = IntNum.zero();
                } else {
                    RealNum[] polars = (RealNum[]) colatitude;
                    rcolatitude = polars[1];
                    rlongitude = polars[2];
                }
                /* r4rs requires 0@1.0 to be inexact zero, even if
                   (make-polar 0 1.0) is exact zero, so check for this
                   case.  */
                if (number.isZero() &&
                    (!rcolatitude.isExact() || !rlongitude.isExact()))
                    return new DFloNum(0.0);
                return Quaternion.polar(number, rangle, rcolatitude,
                                        rlongitude);
            }
            if (colatitude instanceof RealNum[]) {
                RealNum[] polars = (RealNum[]) colatitude;
                polars[0] = number;
                return polars;
            }
            return new RealNum[] { number, (RealNum)colatitude, IntNum.zero() };
        }
        if (ch == '&') {
            /* extended polar notation */
            Object longitude = parseNumber(buffer, pos, end - pos,
                                           exactness, 10, flags);
            if (longitude instanceof String)
                return longitude;
            if (! (longitude instanceof RealNum))
                return "invalid quaternion polar constant";
            RealNum rlongitude = (RealNum) longitude;
            if ((flags & (SCM_ANGLE|SCM_COLATITUDE)) == 0) {
                // number&longitude
                /* r4rs requires 0@1.0 to be inexact zero, even if
                   (make-polar 0 1.0) is exact zero, so check for this
                   case.  */
                if (number.isZero() && !rlongitude.isExact())
                    return new DFloNum(0.0);
                return Quaternion.polar(number, IntNum.zero(),
                                        IntNum.zero(), rlongitude);
            }
            if ((flags & SCM_COLATITUDE) != 0)
                return new RealNum[] { IntNum.zero(), number, rlongitude };
            return new RealNum[] { number, IntNum.zero(), rlongitude };
        }

	if (ch == '-' || ch == '+')
	  {
	    pos--;
	    Object imag = parseNumber(buffer, pos, end - pos,
				      exactness, 10, flags);
	    if (imag instanceof String)
	      return imag;
	    if (! (imag instanceof Quaternion))
	      return "invalid numeric constant ("+imag+")";
	    Quaternion cimag = (Quaternion) imag;
	    RealNum re = cimag.re();
	    if (! re.isZero())
	      return "invalid numeric constant";
	    return Quaternion.make(number, cimag.im(), cimag.jm(), cimag.km());
	  }

	int lcount = 0;
	for (;;)
	  {
	    if (! Character.isLetter(ch))
	      {
		pos--;
		break;
	      }
	    lcount++;
	    if (pos == end)
	      break;
	    ch = buffer[pos++];
	  }

	if (lcount == 1) {
            char prev = buffer[pos-1];
            if (prev == 'i' || prev == 'I') {
                if (pos < end) {
                    Object jmag = parseNumber(buffer, pos, end-pos,
                                              exactness, 10, flags);
                    if (jmag instanceof String)
                        return jmag;
                    if (! (jmag instanceof Quaternion))
                        return "invalid numeric constant ("+jmag+")";
                    Quaternion qjmag = (Quaternion) jmag;
                    RealNum re = qjmag.re();
                    RealNum im = qjmag.im();
                    if (!(re.isZero() && im.isZero()))
                        return "invalid numeric constant";
                    return Quaternion.make(IntNum.zero(), number,
                                           qjmag.jm(), qjmag.km());
                }
                return Complex.make(IntNum.zero(), number);
            }
            if (prev == 'j' || prev == 'J') {
                if (pos < end) {
                    Object kmag = parseNumber(buffer, pos, end-pos,
                                              exactness, 10, flags);
                    if (kmag instanceof String)
                        return kmag;
                    if (! (kmag instanceof Quaternion))
                        return "invalid numeric constant ("+kmag+")";
                    Quaternion qkmag = (Quaternion) kmag;
                    RealNum re = qkmag.re();
                    RealNum im = qkmag.im();
                    RealNum jm = qkmag.jm();
                    if (!(re.isZero() && im.isZero() && jm.isZero()))
                        return "invalid numeric constant";
                    return Quaternion.make(IntNum.zero(), IntNum.zero(),
                                           number, qkmag.km());
                }
                return Quaternion.make(IntNum.zero(), IntNum.zero(),
                                       number, IntNum.zero());
            }
            if (prev == 'k' || prev == 'K') {
                if (pos < end)
                    return "junk after imaginary suffix 'k'";
                return Quaternion.make(IntNum.zero (), IntNum.zero(),
                                       IntNum.zero(), number);
            }
        }
        return "excess junk after number";
      }
    else if (number instanceof DFloNum && exp_char > 0 && exp_char != 'e')
      {
        double d = number.doubleValue();
        switch (exp_char)
          {
          case 'f':  case 's':
            return Float.valueOf((float) d);
          case 'd':
            return Double.valueOf(d);
          case 'l':
            return java.math.BigDecimal.valueOf(d);
          }
      }
    return number;
  }

  private static IntNum valueOf (char[] buffer, int digits_start,
				 int number_of_digits,
				 int radix, boolean negative,
				 long lvalue)
  {
    // It turns out that if number_of_digits + radix <= 28
    // then the value will fit in a long without overflow,
    // so we can use the value calculated in lvalue.
    if (number_of_digits + radix <= 28)
      return IntNum.make(negative ? - lvalue : lvalue);
    else
      return IntNum.valueOf(buffer, digits_start, number_of_digits,
			    radix, negative);
  }

  /** Reads a C-style String escape sequence.
   * Assume '\\' has already been read.
   * Return the converted character, or -1 on EOF, or -2 to ignore. */
  public int readEscape()
    throws java.io.IOException, SyntaxException 
  {
    int c = read();
    if (c < 0)
      {
	eofError("unexpected EOF in character literal");
	return -1;
      }
    return readEscape(c);
  }

  public final int readEscape(int c)
    throws java.io.IOException, SyntaxException 
  {
    switch ((char) c)
      {
      case 'a':  c =  7;  break;  // alarm/bell
      case 'b':  c =  8;  break;  // backspace
      case 't':  c =  9;  break;  // tab
      case 'n':  c = 10;  break;  // newline
      case 'v':  c = 11;  break;  // vertical tab
      case 'f':  c = 12;  break;  // formfeed
      case 'r':  c = 13;  break;  // carriage return
      case 'e':  c = 27;  break;  // escape
      case '\"': c = 34;  break;  // quote
      case '|':  c = '|';  break;  // vertical bar
      case '\\': c = 92;  break;  // backslash
      case ' ':  // Skip to end of line, inclusive.
      case '\n': // Skip initial whitespace on following line.
      case '\r':
      case '\t':
	for (;;)
	  {
	    if (c < 0)
	      {
		eofError("unexpected EOF in literal");
		return -1;
	      }
	    if (c == '\n')
	      break;
	    if (c == '\r')
	      {
		if (peek() == '\n')
		  skip();
                c = '\n';
                break;
	      }
	    if (c != ' ' && c != '\t')
	      {
		unread(c);
		break;
	      }
	    c = read();
	  }
        if (c != '\n')
          break; // ERROR
        // FIXME: if legacy-compatible non-R6RS-mode: return -2;
        for (;;)
          {
            c = read();
            if (c < 0)
              {
                eofError("unexpected EOF in literal");
                return -1;
              }
            if (c != ' ' && c != '\t')
              {
                unread(c);
                return -2;
              }
          }
      case 'M':
	c = read();
	if (c != '-')
	  {
	    error("Invalid escape character syntax");
	    return '?';
	  }
	c = read();
	if (c == '\\')
	  c = readEscape();
	return c | 0200;
      case 'C':
	c = read();
	if (c != '-')
	  {
	    error("Invalid escape character syntax");
	    return '?';
	  }
	/* ... fall through ... */
      case '^':
	c = read();
	if (c == '\\')
	  c = readEscape();
	if (c == '?')
	  return 0177;
	return c & (0200 | 037);
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
	/* An octal escape, as in ANSI C.  */
	c = c - '0';
	for (int count = 0;  ++count < 3; )
	  {
	    int d = read();
	    int v = Character.digit((char) d, 8);
	    if (v >= 0)
	      c = (c << 3) + v;
	    else
	      {
		if (d >= 0)
		  unread(d);
		break;
	      }
	  }
	break;
      case 'u':
	c = 0;
	for (int i = 4;  --i >= 0; )
	  {
	    int d = read ();
	    if (d < 0)
	      eofError("premature EOF in \\u escape");
	    int v = Character.digit ((char) d, 16);
	    if (v < 0)
	      error("non-hex character following \\u");
	    c = 16 * c + v;
	  }
	break;
      case 'x':
      case 'X':
	return readHexEscape();
      default:  break;
      }
    return c;
  }

  public int readHexEscape ()
    throws java.io.IOException, SyntaxException 
  {
    int c = 0;
    /* A hex escape, as in ANSI C.  */
    for (;;)
      {
        int d = read();
        int v = Character.digit((char) d, 16);
        if (v >= 0)
          c = (c << 4) + v;
        else
          {
            if (d != ';')
              {
                // FIXME: if strict-R6RS: ERROR
                if (d >= 0)
                  unread(d);
              }
            break;
          }
      }
    return c;
  }

  public final Object readObject (int c)
      throws java.io.IOException, SyntaxException
  {
    unread(c);
    return readObject();
  }

  /** Read a "command" - a top-level expression or declaration.
   * Return Sequence.eofValue at end of file. */
  public Object readCommand ()
      throws java.io.IOException, SyntaxException
  {
    return readObject(-1, true);
  }

  protected Object makeNil ()
  {
    return LList.Empty;
  }

  protected Pair makePair (Object car, int line, int column)
  {
    return makePair(car, LList.Empty, line, column);
  }

  protected Pair makePair (Object car, Object cdr, int line, int column)
  {
    String pname = port.getName();
    if (! returnMutablePairs && pname != null && line >= 0)
      return PairWithPosition.make(car, cdr,
                                   pname, line + 1, column + 1);
    else
      return Pair.make(car, cdr);
  }

    protected Pair makePair2 (Object car, Object cadr, Object cddr,
                              int line, int column) {
        return makePair(car, makePair(cadr, cddr, line, column), line, column);
    }

  protected void setCar (Object pair, Object car)
  {
    ((Pair) pair).setCarBackdoor(car);
  }

  protected void setCdr (Object pair, Object cdr)
  {
    ((Pair) pair).setCdrBackdoor(cdr);
  }

  /** Read a number from a LispReader
   * @param previous number of characters already pushed on tokenBuffer
   * @param reader LispReader to read from
   * @param radix base to use or -1 if unspecified
   */
  public static Object readNumberWithRadix(int previous, LispReader reader, int radix)
    throws java.io.IOException, SyntaxException
  {
    int startPos = reader.tokenBufferLength - previous;
    ReadTable rtable = ReadTable.getCurrent();
    for (;;) {
        reader.readToken(reader.read(), rtable);
        // '#' is a terminating-macro character so we have to add it "manually"
        int ch = reader.peek();
        if (ch != '#')
            break;
        reader.tokenBufferAppend(ch);
        reader.skip();
    }
    int endPos = reader.tokenBufferLength;
    if (startPos == endPos)
      {
	reader.error("missing numeric token");
	return IntNum.zero();
      }
    Object result = LispReader.parseNumber(reader.tokenBuffer, startPos,
					   endPos - startPos, '\0', radix, 0);
    if (result instanceof String)
      {
	reader.error((String) result);
	return IntNum.zero();
      }
    else if (result == null)
      {
	reader.error("invalid numeric constant");
	return IntNum.zero();
      }
    else
      return result;
  }

  public static Object readCharacter (LispReader reader)
    throws java.io.IOException, SyntaxException
  {
    int ch = reader.read();
    if (ch < 0)
      reader.eofError("unexpected EOF in character literal");
    int startPos = reader.tokenBufferLength;
    reader.tokenBufferAppend(ch);
    reader.readToken(reader.read(), ReadTable.getCurrent());
    char[] tokenBuffer = reader.tokenBuffer;
    int length = reader.tokenBufferLength - startPos;
    if (length == 1 || length == 2) {
        ch = Character.codePointAt(tokenBuffer, startPos,
                                   reader.tokenBufferLength);
        if (ch > 0xFFFF || length == 1)
            return Char.make(ch);
    }
    String name = new String(tokenBuffer, startPos, length);
    ch = Char.nameToChar(name);
    if (ch >= 0)
      return Char.make(ch);
    ch = tokenBuffer[startPos];
    if (ch == 'x' || ch == 'X')
      {
        int value = 0;
        for (int i = 1; ;  i++)
          {
             if (i == length)
	      return Char.make(value);
             int v = Character.digit (tokenBuffer[startPos + i], 16);
             if (v < 0)
               break;
             value = 16 * value + v;
             if (value > 0x10FFFF) {
                 reader.error("character scalar value greater than #x10FFFF");
                 return Char.make('?');
             }
          }
      }
    // FIXME remove - only used for BRL Perhaps a deprecation warning?
    ch = Character.digit(ch, 8);
    if (ch >= 0)
      {
	int value = ch;
	for (int i = 1;  ;  i++)
	  {
	    if (i == length)
	      return Char.make(value);
	    ch = Character.digit(tokenBuffer[startPos + i], 8);
	    if (ch < 0)
	      break;
	    value = 8 * value + ch;
	  }
      }
    reader.error("unknown character name: " + name);
    return Char.make('?');
  }

  public static Object readSpecial (LispReader reader)
    throws java.io.IOException, SyntaxException
  {
    int ch = reader.read();
    if (ch < 0)
      reader.eofError("unexpected EOF in #! special form");

    /* Handle Unix #!PROGRAM line at start of file. */
    if ((ch == '/' || ch == ' ')
	&& reader.getLineNumber() == 0
	&& reader.getColumnNumber() == 3)
      {
        String filename = reader.getName();
        if (filename != null
            && ApplicationMainSupport.commandName.get(null) == null)
          {
            ApplicationMainSupport.commandName.set(filename);
          }

        boolean sawBackslash = false;
        for (;;)
          {
            ch = reader.read();
            if (ch < 0)
              break;
            if (ch == '\\')
              sawBackslash = true;
            else if (ch == '\n' || ch == '\r')
              {
                if (! sawBackslash)
                  break;
                sawBackslash = false;
              }
            else if (sawBackslash && ch != ' ' && ch != '\t')
              sawBackslash = false;
          }
        return Values.empty;
      }

    String name = reader.readTokenString(ch, ReadTable.getCurrent());
    if (name.equals("optional"))
      return Special.optional;
    if (name.equals("rest"))
      return Special.rest;
    if (name.equals("key"))
      return Special.key;
    if (name.equals("eof"))
      return Special.eof;
    if (name.equals("void"))
      //return Values.empty;
      return QuoteExp.voidExp;
    if (name.equals("default"))
      return Special.dfault;
    if (name.equals("undefined"))
      return Special.undefined;
    if (name.equals("abstract"))
      return Special.abstractSpecial;
    if (name.equals("native"))
      return Special.nativeSpecial;
    if (name.equals("null"))
      return null;
    if (name.equals("fold-case"))
      {
        reader.readCase = 'D';
        return Values.empty;
      }
    if (name.equals("no-fold-case"))
      {
        reader.readCase = 'P';
        return Values.empty;
      }
    reader.error("unknown named constant #!"+name);
    return null;
  }

  public static SimpleVector
  readSimpleVector(LispReader reader, char kind)
    throws java.io.IOException, SyntaxException
  {
    int size = 0;
    int ch;
    for (;;)
      {
	ch = reader.read();
	if (ch < 0)
	  reader.eofError("unexpected EOF reading uniform vector");
	int digit = Character.digit((char) ch, 10);
	if (digit < 0)
	  break;
	size = size * 10 + digit;
      }
    return readSimpleVector(reader, kind, ch, size);
  }

  public static SimpleVector
      readSimpleVector(LispReader reader, char kind, int ch, int size)
    throws java.io.IOException, SyntaxException
  {
    if (! (size == 8 || size == 16 || size == 32 || size == 64)
        || (kind == 'F' && size < 32)
        || ch != '(')
      {
        reader.error("invalid uniform vector syntax");
        return null;
      }
    Object list = ReaderParens.readList(reader, null, '(', -1, ')', -1);
    int len = LList.listLength(list, false);
    if (len < 0)
      {
        reader.error("invalid elements in uniform vector syntax");
        return null;
      }
    Sequence q = (Sequence) list;
    switch (kind)
      {
      case 'F':
        switch (size)
          {
          case 32:  return new F32Vector(q);
          case 64:  return new F64Vector(q);
          }
      case 'S':
        switch (size)
          {
          case  8:  return new S8Vector(q);
          case 16:  return new S16Vector(q);
          case 32:  return new S32Vector(q);
          case 64:  return new S64Vector(q);
          }
      case 'U':
        switch (size)
          {
          case  8:  return new U8Vector(q);
          case 16:  return new U16Vector(q);
          case 32:  return new U32Vector(q);
          case 64:  return new U64Vector(q);
          }
      }
    return null;
  }

    boolean deprecatedXmlEnlosedReported;
}
