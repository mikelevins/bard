// Copyright (c) 2010, 2013  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.xml.*;
import gnu.kawa.xml.*;
import gnu.expr.*;

public class ReaderXmlElement extends ReaderExtendedLiteral
{
    static final Symbol xmlTextSymbol = Symbol.valueOf("$xml-text$");
    static final Symbol xmlCommentSymbol = Symbol.valueOf("$xml-comment$");
    static final Symbol xmlElementSymbol = Symbol.valueOf("$xml-element$");
    static final Symbol xmlCDATASymbol = Symbol.valueOf("$xml-CDATA$");
    static final Symbol xmlProcInstSymbol = Symbol.valueOf("$xml-processing-instruction$");
    static final Symbol xmlAttributeSymbol = Symbol.valueOf("$xml-attribute$");
    static final Symbol resolveQNameSymbol = Symbol.valueOf("$resolve-qname$");

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    return readXMLConstructor(reader, reader.readUnicodeChar(), false);
  }

  public static Pair quote (Object obj)
  {
    Symbol q = Namespace.EmptyNamespace.getSymbol(LispLanguage.quote_str);
    return LList.list2(q, obj);
  }

    static final String DEFAULT_ELEMENT_NAMESPACE = "$default-element-namespace$";
    public static final Symbol defaultElementNamespaceSymbol
        = Symbol.valueOf(DEFAULT_ELEMENT_NAMESPACE);

  /** Read either a QName literal or an enclosed QName-producing form.
   * If literal, returns a quoted symbol, and the source literal
   * in the non-empty token-buffer.
   * If non-literal, tokenBufferLength is set to 0.
   */
  public Object readQNameExpression (LispReader reader, int ch, boolean forElement)
      throws java.io.IOException, SyntaxException
  {
    String file = reader.getName();
    int line = reader.getLineNumber() + 1;
    int column = reader.getColumnNumber();
    reader.tokenBufferLength = 0;
    if (XName.isNameStart(ch))
      {
        int colon = -1;
        for (;;)
          {
            reader.tokenBufferAppend(ch);
            ch = reader.readUnicodeChar();
            if (ch == ':' && colon < 0)
              colon = reader.tokenBufferLength;
            else if (! XName.isNamePart(ch))
              {
                reader.unread(ch);
                break;
              }
          }
        if (colon >= 0 || forElement)
          {
            int llen = reader.tokenBufferLength - colon - 1;
            String local
              = new String(reader.tokenBuffer, colon+1, llen).intern();
            Object ns = LList.Empty;
            if (colon >= 0) {
                String prefix = new String(reader.tokenBuffer, 0, colon).intern();
                ns = new Pair(Symbol.valueOf(prefix), ns);
            }

            return new Pair(resolveQNameSymbol,
                            PairWithPosition.make(Symbol.valueOf(local), ns,
                                                  reader.getName(), line, column));
          }
        else
          {
            Symbol symbol = Namespace.getDefaultSymbol(reader.tokenBufferString().intern());
            return quote(symbol);
          }
      }
    else if (enclosedExprDelim(ch, reader) >= 0 || ch == '(')
      {
        return readEnclosedSingleExpression(reader, ReadTable.getCurrent(), ch);
      }
    else
      {
        reader.error("missing element name");
        return null;
      }
  }

  /** Parse an Element or other constructs starting with '<'.
   * Assume initial '<' has been processed.
   * @param next next character (after '<').
   */
  Object readXMLConstructor (LispReader reader, int next, boolean inElementContent)
      throws java.io.IOException, SyntaxException
  {
    Object exp;
    int startLine = reader.getLineNumber() + 1;
    int startColumn = reader.getColumnNumber() - 2;
    if (next == '!')
      {
	next = reader.read();
	if (next == '-' && (next = reader.peek()) == '-')
	  {
	    reader.skip();
	    if (! reader.readDelimited("-->"))
              reader.error('f', reader.getName(), startLine, startColumn, "unexpected end-of-file in XML comment starting here - expected \"-->\"");
            String str = reader.tokenBufferString();
            exp = LList.list2(xmlCommentSymbol, str);
	  }
        else if (next == '['
                 && (next = reader.read()) == 'C'
                 && (next = reader.read()) == 'D'
                 && (next = reader.read()) == 'A'
                 && (next = reader.read()) == 'T'
                 && (next = reader.read()) == 'A'
                 && (next = reader.read()) == '[')
	  {
	    if (! reader.readDelimited("]]>"))
              reader.error('f', reader.getName(), startLine, startColumn,
                           "unexpected end-of-file in CDATA starting here - expected \"]]>\"");
            String str = reader.tokenBufferString();
            exp = LList.list2(xmlCDATASymbol, str);
	  }
        else
          {
            reader.error('f', reader.getName(), startLine, startColumn,
                         "'<!' must be followed by '--' or '[CDATA['");
            while (next >= 0 && next != '>'
                   && next != '\n' && next != '\r')
              {
                next = reader.read();
              }
            exp = null;
          }
      }
    else if (next == '?')
      {
        next = reader.readUnicodeChar();
	if (next < 0 || ! XName.isNameStart(next))
	  reader.error("missing target after '<?'");
        for (;;)
          {
            reader.tokenBufferAppend(next);
            next = reader.readUnicodeChar();
            if (! XName.isNamePart(next))
              break;
          }
	String target = reader.tokenBufferString();
        int nspaces = 0;
        while (next >= 0 && Character.isWhitespace(next))
          {
            nspaces++;
            next = reader.read();
         }
        reader.unread(next);
        char saveReadState = reader.pushNesting('?');
        try
          {
            if (! reader.readDelimited("?>"))
              reader.error('f', reader.getName(), startLine, startColumn,
                           "unexpected end-of-file looking for \"?>\"");
          }
        finally
          {
            reader.popNesting(saveReadState);
          }
        if (nspaces == 0 && reader.tokenBufferLength > 0)
          reader.error("target must be followed by space or '?>'");
	String content = reader.tokenBufferString();
        exp = LList.list3(xmlProcInstSymbol, target, content);
      }
    else {
      exp = readElementConstructor(reader, next);
    }
    return exp;
  }

    /** Parse ElementConstructor.
     * Assume initial {@code '<'} has been processed,
     * and we're read the next character.
     * Reads through end of the end tag.
     */
    public Object readElementConstructor(LispReader reader, int ch)
        throws java.io.IOException, SyntaxException {
        int startLine = reader.getLineNumber() + 1;
        int startColumn = reader.getColumnNumber() - 2;
        Object tag = readQNameExpression(reader, ch, true);
        // Note that we cannot do namespace resolution at reader time,
        // because of constructs like this:  <a x="&{x:v}" xmlns:x="xx"/>
        // Instead we defer namespace lookup until rewrite-time.
        String startTag = reader.tokenBufferLength == 0 ? null
            : reader.tokenBufferString();
        Pair tagPair = PairWithPosition.make(tag, LList.Empty,
                                             reader.getName(),
                                             startLine, startColumn);
        Pair resultTail = tagPair;
        LList namespaceList = LList.Empty;
        NamespaceBinding nsBindings = null; // ???
        boolean sawSpace = false;
        for (;;) {
            ch = reader.readUnicodeChar();
            while (ch >= 0 && Character.isWhitespace(ch)) {
                ch = reader.readUnicodeChar();
                sawSpace = true;
            }
            if (ch < 0 || ch == '>' || ch == '/')
                break;
            if (! sawSpace)
                reader.error("missing space before attribute");
            Object attrName = readQNameExpression(reader, ch, false);
            int line = reader.getLineNumber() + 1;
            int column = reader.getColumnNumber() + 1 - reader.tokenBufferLength;
            String definingNamespace = null;
            if (reader.tokenBufferLength >= 5
                && reader.tokenBuffer[0] == 'x'
                && reader.tokenBuffer[1] == 'm'
                && reader.tokenBuffer[2] == 'l'
                && reader.tokenBuffer[3] == 'n'
                && reader.tokenBuffer[4] == 's') {
                if (reader.tokenBufferLength == 5)
                    definingNamespace = "";
                else if (reader.tokenBuffer[5] == ':')
                    definingNamespace =
                        new String(reader.tokenBuffer, 6, reader.tokenBufferLength-6);
            }
            sawSpace = false;
            for (;;) {
                ch = reader.readUnicodeChar();
                if (ch < 0)
                    break;
                if (Character.isWhitespace(ch))
                    sawSpace = true;
                else
                    break;
            }
            Object attrExpr;
            if (ch != '=' && reader.tokenBufferLength == 0
                && (sawSpace || ch == '/' || ch == '>')) {
                // Attribute is an enclosed (attribute-valued) expression.
                reader.unread(ch);
                attrExpr = attrName;
            }
            else {
                if (ch != '=')
                    reader.error("missing '=' after attribute");
                ch = skipSpace(reader, ' ');
                PairWithPosition attrList
                    = PairWithPosition.make(xmlAttributeSymbol, LList.Empty,
                                            reader.getName(), startLine, startColumn);
                PairWithPosition attrPair
                    = PairWithPosition.make(attrName, LList.Empty,
                                            reader.getName(), startLine, startColumn);
                reader.setCdr(attrList, attrPair);
                Pair attrTail = attrPair;
                if (ch == '[' || ch == '(') {
                    ReadTable rtable = ReadTable.getCurrent();
                    attrTail = readEnclosed(reader, rtable, attrTail, ch,
                                            ch == '[' ? ']' : ')');
                } else if (ch == '"' || ch == '\'')
                    attrTail = readContent(reader, (char) ch, attrTail);
                else
                    reader.error("missing attribute value");
                attrExpr = attrList;
                if (definingNamespace != null) {
                    namespaceList = new PairWithPosition(attrPair,
                                                         Pair.make(Symbol.valueOf(definingNamespace), attrPair.getCdr()),
                                                         namespaceList);
                }
                sawSpace = false;
            }
            if (definingNamespace == null) {
                Pair pair = PairWithPosition.make(attrExpr,  reader.makeNil(),
                                                  null, -1, -1); // FIXME
                resultTail.setCdrBackdoor(pair);
                resultTail = pair;
            }
        }
        boolean empty = false;
        if (ch == '/') {
            ch = reader.read();
            if (ch == '>')
                empty = true;
            else
                reader.unread(ch);
        }
        if (! empty) {
            if (ch != '>') {
                reader.error("missing '>' after start element");
                return Boolean.FALSE;
            }
            resultTail = readContent(reader, '<', resultTail);
            ch = reader.readUnicodeChar();
            if (XName.isNameStart(ch)) {
                reader.tokenBufferLength = 0;
                for (;;) {
                    reader.tokenBufferAppend(ch);
                    ch = reader.readUnicodeChar();
                    if (! XName.isNamePart(ch) && ch != ':')
                        break;
                }
                String endTag = reader.tokenBufferString();
                if (startTag == null || ! endTag.equals(startTag))
                    reader.error('e', reader.getName(),
                                 reader.getLineNumber() + 1,
                                 reader.getColumnNumber() - reader.tokenBufferLength,
                                 startTag == null
                                 ? "computed start tag closed by '</"+endTag+">'"
                                 : "'<"+startTag+">' closed by '</"+endTag+">'");
                reader.tokenBufferLength = 0;
            }
            ch = skipSpace(reader, ch);
            if (ch != '>')
                reader.error("missing '>' after end element");
        }
        namespaceList = LList.reverseInPlace(namespaceList);
        return PairWithPosition.make(xmlElementSymbol,
                                     Pair.make(namespaceList, tagPair),
                                     reader.getName(),
                                     startLine, startColumn);
    }

    protected Object checkDelim(LispReader reader, int next, int delimiter)
        throws java.io.IOException, SyntaxException {
        if (delimiter == '<' && next == '<') {
            next = reader.read();
            if (next == '/')
                return Special.eof;
            else
                return readXMLConstructor(reader, next, true);
        }
        else if (next < 0 || (delimiter != '<' && next == delimiter))
            return Special.eof;
        else
            return null;
    }

    protected boolean isNestableStartDelim(int next) {
        return false;
    }

    protected boolean isNestableEndDelim(int next) {
        return false;
    }

  public  static int skipSpace (LispReader reader, int ch)
      throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
        if (ch < 0 || ! Character.isWhitespace(ch))
          return ch;
        ch = reader.readUnicodeChar();
      }
  }

    protected int enclosedExprDelim(int ch, LispReader reader) {
        if (ch == '[')
            return ']';
        if (ch == '{') {
            if (! reader.deprecatedXmlEnlosedReported) {
                reader.error('w', "use '[' instead of deprecated '{' for enclosed expression");
                reader.deprecatedXmlEnlosedReported = true;
            }
            return '}';
        }
        return -1;
    }
}
