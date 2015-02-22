// Copyright (c) 2012, 2013  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.lispexpr;
import gnu.kawa.io.InPort;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.*;
import java.io.*;
import gnu.xml.XName; // FIXME - not available if --disable-xml

public class ReaderExtendedLiteral extends ReaderConstituent {
    static final Symbol qstringSymbol = Symbol.valueOf("$string$");
    static final Symbol formatSymbol = Symbol.valueOf("$format$");
    static final Symbol sprintfSymbol = Symbol.valueOf("$sprintf$");
    static final Symbol startEnclosedSymbol = Symbol.valueOf("$<<$");
    static final Symbol endEnclosedSymbol = Symbol.valueOf("$>>$");

    public ReaderExtendedLiteral() { super(ReadTable.CONSTITUENT); }

    public Object read(Lexer in, int ch, int count)
	throws java.io.IOException, SyntaxException {
        LispReader reader = (LispReader) in;
        int startPos = reader.tokenBufferLength;
        ReadTable rtable = ReadTable.getCurrent();
        int startLine = reader.getLineNumber() + 1;
        int startColumn = reader.getColumnNumber() - 2;
        in.tokenBufferAppend(ch);
        int next = reader.read();
        next = scanTag(reader, next);
        Object result;
        if (next == '{' || next == '[') {
            int len = reader.tokenBufferLength - startPos - 1;
            String tag = len == 0 ? null
                : new String(reader.tokenBuffer, startPos+1, len);
            reader.tokenBufferLength = startPos;
            result = readNamedLiteral(reader, rtable, tag,  next, startLine, startColumn);
        } else {
            result = reader.readAndHandleToken(next, startPos, rtable);
        }
        return result;
    }

    protected int enclosedExprDelim(int ch, LispReader reader) {
        return ch == '[' ? ']' : -1;
    }

    public Object readNamedLiteral(LispReader reader, ReadTable rtable,
                                   String tag, int next,
                                   int startLine, int startColumn)
            throws java.io.IOException, SyntaxException {
        Object operator = tag == null ? qstringSymbol
            : LispLanguage.constructNamespace.getSymbol(tag);
        Pair result = PairWithPosition.make(operator, null,
                                           reader.getName(),
                                 startLine, startColumn);
        Pair rtail = result;
        int endDelimiter = enclosedExprDelim(next, reader);
        if (endDelimiter >= 0 && tag != null) {
            int line = reader.getLineNumber() + 1;
            int column = reader.getColumnNumber();
            rtail = readEnclosed(reader, rtable, rtail, next, endDelimiter);
            Pair endMarker = reader.makePair(endEnclosedSymbol, LList.Empty,
                                             reader.getLineNumber() + 1,
                                             reader.getColumnNumber());
            rtail.setCdrBackdoor(endMarker);
            rtail = endMarker;
            next = reader.read();
        }
        if (next == '{') {
            readContent(reader, '}', rtail);
        }
        else if (tag == null) {
            reader.error("unexpected character after &");
        } else
             reader.unread(next);
        return result;
    }

    protected Object checkDelim(LispReader reader, int next, int delimiter)
        throws java.io.IOException, SyntaxException {
        return next == delimiter || next < 0 ? Special.eof : null;
    }

    protected boolean isNestableStartDelim(int next) {
        return next == '{';
    }

    protected boolean isNestableEndDelim(int next) {
        return next == '}';
    }

    public Pair readContent(LispReader reader, char delimiter, Pair head)
        throws java.io.IOException, SyntaxException {
        Pair resultTail = head;
        reader.tokenBufferLength = 0;
        int braceNesting = 1;
        // If lineStart >= 0 then it is an index into tokenBuffer
        // such that &| should delete up to lineStart.
        // However, there is an error if we've seen a non-space,
        // and so we use nonSpace to mark the first non-space character
        // on the line.
        int lineStart = -1;
        int nonSpace = -1;
        for (;;) {
            Object item = null;
            int line = reader.getLineNumber() + 1;
            int column = reader.getColumnNumber();
            int next = reader.readCodePoint();
            if (next == '\r' || next == '\n') {
                // As a special case, if this is the first newline
                // since the start of the literal text, and we haven't
                // seen any whitespace, then &| also delete this newline.
                if (lineStart < 0 && nonSpace < 0)
                    lineStart = 0;
                else
                    lineStart = reader.tokenBufferLength + 1;
                nonSpace = -1;
            }
            else if (nonSpace < 0 && next != ' ' && next != '\t') {
                nonSpace = reader.tokenBufferLength;
            }
            if (next < 0) {
                reader.error("unexpected end-of-file");
                item = Special.eof;
            }
            else if (next == delimiter
                     && (! isNestableEndDelim(next)
                         || --braceNesting == 0))
                item = Special.eof;
            else if (next == '&') {
                int next1 = reader.peek();
                if (next1 == '|') {
                    int skipped = 0;
                    int blen = reader.tokenBufferLength;
                    if (lineStart < 0) {
                        reader.error('e', reader.getName(),
                                     line, column+1,
                                     "invalid '&|'");
                    } else if (nonSpace != reader.tokenBufferLength) {
                        reader.error('e', reader.getName(),
                                     line,
                                     nonSpace - lineStart + 1,
                                     "non-whitespace before '&|'");
                    }
                    else
                        reader.tokenBufferLength = lineStart;
                    reader.skip();
                    continue;
                } else if (next1 == '-') {
                    reader.skip();
                    boolean complained = false;
                    for (;;) {
                        next = reader.read();
                        if (next == '\r' || next == '\n')
                            break;
                        if (! complained && next != ' ' && next != '\t') {
                            reader.error('e', reader.getName(),
                                         reader.getLineNumber() + 1,
                                         reader.getColumnNumber(),
                                         "non-whitespace after '&-'");
                            complained = true;
                        }
                    }
                    lineStart = reader.tokenBufferLength;
                    nonSpace = -1;
                    continue;
                } else if (next1 == '#') {
                    reader.skip();
                    next = reader.read();
                    if (next == '|') {
                        ReaderDispatchMisc.readNestedComment(reader);
                    } else
                        readCharRef(reader, next);
                }
            } else {
                if (isNestableStartDelim(next))
                    braceNesting++;
                reader.tokenBufferAppend(next);
                if (next == ']' && delimiter == '<') {
                    if (reader.peek() == ']') {
                        reader.skip();
                        reader.tokenBufferAppend(']');
                        if (reader.peek() == '>') {
                            reader.error('w', reader.getName(), line, column+1,
                                         "literal ']]>' is only valid following '<![CDATA['");
                        }
                    }
                }
                next = ' ';
            }
            if (reader.tokenBufferLength > 0
                    && (next == delimiter || next == '&' || next < 0)) {
                String text = reader.tokenBufferString();
                reader.tokenBufferLength = 0;
                Object tnode = wrapText(text);
                Pair pair = PairWithPosition.make(tnode,  reader.makeNil(),
                                                  null, -1, -1); // FIXME
                resultTail.setCdrBackdoor(pair);
                resultTail = pair;
            }
            if (next == '&') {
                ReadTable rtable = ReadTable.getCurrent();
                next = reader.read();
                int endDelimiter = enclosedExprDelim(next, reader);
                if (endDelimiter >= 0 || next == '(') {
                    Pair qq =
                        reader.makePair(startEnclosedSymbol, LList.Empty,
                                        line, column);
                    resultTail.setCdrBackdoor(qq);
                    resultTail = qq;
                    resultTail = readEnclosed(reader, rtable, resultTail, next, endDelimiter);
                    item = endEnclosedSymbol;
                }
                else if (next == '~' || next == '%') {
                    boolean sawQuote = false;
                    boolean printfStyle = next == '%';
                    boolean needEnclosed;
                    int magic = next;
                    for (;;) { 
                        reader.tokenBufferAppend(next);
                        next = reader.read();
                        if (next < 0 || next == '\n') {
                            reader.error('e', "non-terminated format specifier");
                            needEnclosed = false;
                            break;
                        }
                        if (sawQuote)
                            sawQuote = false;
                        else if (next == '\'' && magic == '~')
                            sawQuote = true;
                        // Prefix characters allowed in a format directive.
                        // We should probably be more restrictive.
                        else if ((next >= '0' && next <= '9')
                                 || next == '+' || next == '-' || next == ' '
                                 || (printfStyle
                                     ? (next == '.' || next == '*')
                                     : (next == ',' || next == '#' 
                                        || next == 'v' || next == 'V'
                                        || next == ':' || next == '@')))
                            ; // prefix directive part
                        else {
                            // next is (hopefully) a directive character
                            reader.tokenBufferAppend(next);
                            next = reader.read();
                            if (next == '[' || next == '(') {
                                needEnclosed = true;
                                break;
                            } else if (next != magic) {
                                reader.unread(next);
                                needEnclosed = false;
                                break;
                            }
                            // if next==magic continue to read next specifier.
                        }
                    }
                    String fmt = reader.tokenBufferString();
                    endDelimiter = enclosedExprDelim(next, reader);
                    reader.tokenBufferLength = 0;
                    Pair ffmt = reader.makePair(fmt, LList.Empty, line, column);
                    Object fun = printfStyle ? sprintfSymbol : formatSymbol;
                    Pair fhead = reader.makePair(fun, ffmt,
                                                 line, column);
                    if (needEnclosed)
                        readEnclosed(reader, rtable, ffmt, next, endDelimiter);
                    item = fhead;
                }
                else {
                    int startPos = reader.tokenBufferLength;
                    next = scanTag(reader, next);
                    String str = new String(reader.tokenBuffer, startPos,
                                         reader.tokenBufferLength-startPos);
                    reader.tokenBufferLength = startPos;
                    reader.unread(next);
                    if (next == '[' || next == '{') {
                        item = readNamedLiteral(reader, rtable, str, reader.read(),
                                                line, column);
                    } else if (next == ';') {
                        item = checkEntity(reader, str);
                    } else {
                        reader.error('e', "expected '[', '{', or ';'");
                    }
                }
            }
            else {
                item = checkDelim(reader, next, delimiter);
            }
            if (item == Special.eof)
                break;
            if (item != null) {
                Pair pair = PairWithPosition.make(item,  reader.makeNil(),
                                                  reader.getName(),
                                                  line, column+1);
                resultTail.setCdrBackdoor(pair);
                resultTail = pair;
            }
        }
        return resultTail;
    }

    protected Object wrapText(String text) {
        return text;
    }

    protected Object readEnclosedSingleExpression (LispReader reader, ReadTable readTable, int ch)
            throws IOException, SyntaxException {
        if (ch == '(')  {
            reader.unread(ch);
            return reader.readObject();
        } else {
            int endDelimiter = enclosedExprDelim(ch, reader);
            Pair head = new Pair(null, LList.Empty);
            int line = reader.getLineNumber() + 1;
            int column = reader.getColumnNumber() + 1; // Column after '['
            Pair tail = readEnclosedExpressions(reader, readTable, head, endDelimiter);
            if (head == tail) {
                reader.error('e', reader.getName(), line, column,
                             "missing expression");
              return "<missing>";
            }
            Pair first = (Pair) head.getCdr();
            if (first.getCdr() != LList.Empty)
                reader.error('e', reader.getName(), line, column,
                             "too many expressions");
            return first.getCar();
        }
    }

    protected Pair readEnclosed(LispReader reader, ReadTable readTable, Pair last, int startDelimiter, int endDelimiter)
            throws IOException, SyntaxException {
        if (startDelimiter == '(') {
            return reader.readValuesAndAppend('(', readTable, last);
        } else {
            return readEnclosedExpressions(reader, readTable, last,
                                           endDelimiter);
        }
    }

    /** Read expressions enclosed by '[' and ']'.
     * Assume '[' has already been read.
     */
    protected Pair readEnclosedExpressions(LispReader reader, ReadTable readTable, Pair last, int endDelimiter)
            throws IOException, SyntaxException {
        InPort port = reader.getPort();
        char saveReadState = reader.pushNesting('[');
        int startLine = port.getLineNumber();
        int startColumn = port.getColumnNumber();
        try {
            for (;;) {
                int line = port.getLineNumber();
                int column = port.getColumnNumber();
                int ch = port.read();
                if (ch == endDelimiter)
                  break;
                if (ch < 0)
                  reader.eofError("unexpected EOF in list starting here",//FIXME
                                 startLine + 1, startColumn);
                last = reader.readValuesAndAppend(ch, readTable, last);
            }
            return last;
          }
        finally
          {
            reader.popNesting(saveReadState);
          }
    }

    private int scanTag(LispReader reader, int next)
        throws IOException, SyntaxException {
        if (XName.isNameStart(next)) {
            for (;;) {
                reader.tokenBufferAppend(next);
                next = reader.read();
                if (! XName.isNamePart(next)) {
                    break;
                }
            }
        } else if (next == '`' || next == '<' || next == '>') {
            int nextnext = reader.peek();
            if (next == '>' && nextnext == '>') {
                reader.tokenBufferAppend(next);
                reader.skip();
                nextnext = reader.peek();
            }
            if (nextnext == '{' || nextnext == '[') {
                reader.tokenBufferAppend(next);
                next = reader.read();
            }
        }
        return next;
    }

    Object checkEntity(LispReader reader, String str)
            throws IOException, SyntaxException {
        int next = reader.read();
        if (next != ';') {
            reader.unread(next);
            reader. error("invalid entity reference");
        }
        return LispLanguage.entityNamespace.getSymbol(str);
    }

    /** Read a character reference, assuming {@code "&#"} have been read. */
    void readCharRef (LispReader reader, int next)
        throws IOException, SyntaxException {
        int base;
        if (next == 'x') {
            base = 16;
            next = reader.read();
        } else
            base = 10;
        int value = 0;
        while (next >= 0) {
            char ch = (char) next;
            int digit = Character.digit((char) ch, base);
            if (digit < 0)
                break;
            if (value >= 0x8000000)
                break; // Overflow likely.
            value = value * base;
            value += digit;
            next = reader.read();
        }
        if (next != ';') {
            reader.unread(next);
            reader.error("invalid character reference");
        }
        // See definition of 'Char' in XML 1.1 2nd ed Specification.
        else if ((value > 0 && value <= 0xD7FF)
                 || (value >= 0xE000 && value <= 0xFFFD)
                 || (value >= 0x10000 && value <= 0x10FFFF)) {
            reader.tokenBufferAppend(value);
        }
        else
            reader.error("invalid character value "+value);
    }
}
