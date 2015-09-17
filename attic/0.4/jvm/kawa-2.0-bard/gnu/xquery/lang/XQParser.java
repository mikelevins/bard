// Copyright (c) 2001, 2002, 2003, 2004, 2006  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.kawa.lispexpr.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.math.IntNum;
import java.util.Map;
import java.util.HashMap;
import java.util.Vector;
import java.util.Stack;
import gnu.kawa.xml.*;
import gnu.xml.*;
import gnu.bytecode.*;
import gnu.kawa.io.BinaryInPort;
import gnu.kawa.io.CharArrayInPort;
import gnu.kawa.io.FilePath;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.TtyInPort;
import gnu.kawa.io.URIPath;
import gnu.kawa.reflect.OccurrenceType;
import gnu.kawa.reflect.SingletonType;
import gnu.kawa.functions.Convert;
import gnu.lists.FString;
import gnu.lists.LList;
import gnu.text.Lexer;
import gnu.text.SourceError;
import gnu.text.SourceLocator;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.xquery.util.NamedCollator;
import gnu.xquery.util.CastableAs;
import gnu.xquery.util.QNameUtils;
import gnu.xquery.util.RelativeStep;
import gnu.xquery.util.ValuesFilter;
import kawa.standard.require;
import kawa.lang.Translator.FormStack;

/** A class to read xquery forms. */

public class XQParser extends Lexer
{
  int curToken;
  Object curValue;

  /** Normally null.
   * 'C' means parsing the type of a 'cast as' or 'castable as'. */
  int parseContext;
  /** True if we've seen a VarDecl, FunctionDecl, or OptionDecl. */
  boolean seenDeclaration;

  String libraryModuleNamespace;

    Map<String,SourceLocator> seenImports;

  /** Value of getLineNumber() at start of current token.
   * Sometimes set otherwise, to report errors. */
  int curLine;

  /** Value of getColumnNumber() at start of current token.
   * Sometimes set otherwise, to report errors. */
  int curColumn;

  XQuery interpreter;

  int seenPosition;
  int seenLast;

  public static boolean warnOldVersion = true;
  public static boolean warnHidePreviousDeclaration = false;

  /** The internal name of the variable containing '.', the context node. */
  static final Symbol DOT_VARNAME = Symbol.makeUninterned("$dot$");

  /** The pseduo-function position() is mapped to a reference. */
  static final Symbol POSITION_VARNAME = Symbol.makeUninterned("$position$");

  /** The pseduo-function last() is mapped to a reference to this variable. */
  static final Symbol LAST_VARNAME = Symbol.makeUninterned("$last$");

  public static final gnu.kawa.reflect.InstanceOf instanceOf
  = new gnu.kawa.reflect.InstanceOf(XQuery.getInstance(), "instance");
  public static final CastableAs castableAs = CastableAs.castableAs;
  public static final Convert treatAs = Convert.cast;

  NamedCollator defaultCollator = null;

  /** The default order for empty sequences.
   * Either <code>'L'</code> (for "least") or <code>'G'</code> (for "greatest").
   */
  char defaultEmptyOrder = 'L';
  boolean emptyOrderDeclarationSeen;

  Path baseURI = null;
  boolean baseURIDeclarationSeen;

  public void setStaticBaseUri (String uri)
  {
    try
      {
        baseURI = fixupStaticBaseUri(URIPath.valueOf(uri));
      }
    catch (Exception ex)
      {
        Throwable th = (ex instanceof WrappedException
                        ? ((WrappedException) ex).getCause()
                        : ex);
        error('e', "invalid URI: "+th.getMessage());
      }
  }

  static Path fixupStaticBaseUri (Path path)
  {
    path = path.getAbsolute();
    if (path instanceof FilePath)
      path = URIPath.valueOf(path.toURI());
    return path;
 }

  public String getStaticBaseUri ()
  {
    Path path = baseURI;
    if (path == null)
      {
        Environment env = Environment.getCurrent();
        Object value = env.get(Symbol.make("", "base-uri"), null, null);
        if (value != null)
          {
            if (value instanceof Path)
              path = (Path) path;
            else
              path = URIPath.valueOf(value.toString());
          }

        InPort port;
        if (path == null && (port = getPort()) != null)
          {
            path = port.getPath();
            if (path instanceof FilePath
                && (! path.exists()
                    || port instanceof TtyInPort
                    || port instanceof CharArrayInPort))
              path = null;
          }

        if (path == null)
          path = Path.currentPath();

        path = fixupStaticBaseUri(path);
        baseURI = path;
      }

    return path.toString();
  }

  public String resolveAgainstBaseUri (String uri)
  {
    if (Path.uriSchemeSpecified(uri))
      return uri;
    String base = getStaticBaseUri();
    Path basePath = Path.valueOf(base);
    return basePath.resolve(uri).toString();
  }

  boolean boundarySpacePreserve;
  boolean boundarySpaceDeclarationSeen;

  boolean orderingModeUnordered;
  boolean orderingModeSeen;

  /** True if we've seen a 'copy-namespaces' declaration'. */
  boolean copyNamespacesDeclarationSeen;
  int copyNamespacesMode
  = XMLFilter.COPY_NAMESPACES_PRESERVE|XMLFilter.COPY_NAMESPACES_INHERIT;

  /** The static construction mode. True if "strip"; false if "preserve". */
  boolean constructionModeStrip;
  /** True if a construction mode declaration has been seen. */
  boolean constructionModeDeclarationSeen;

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  /** Stack of currently active for/let Declarations. */
  Declaration[] flworDecls;
  /* Index in flworDecls of first Declaration in current FLWOR. */
  int flworDeclsFirst;
  /* Total number of currently active for/let Declarations. */
  int flworDeclsCount;

  int parseCount;
  int commentCount;
  /** An error message if comments are disallowed.  Normally null. */
  String errorIfComment;

  /** Skip whitespace.
   * Sets 'index' to the that of the next non-whitespace character,
   * and returns that.  If there are no more non-space characters,
   * returns ' '.  */
  final int skipSpace()
    throws java.io.IOException, SyntaxException
  {
    return skipSpace(true);
  }

  final int skipSpace(boolean verticalToo)
    throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
	int ch = read();
	if (ch == '(')
	  {
	    if (! checkNext(':'))
	      return '(';
	    skipComment();
	  }
	else if (ch == '{')
	  {
	    ch = read();
	     if (ch != '-')
	       {
		 unread(ch);
		 return '{';
	       }
	    ch = read();
	     if (ch != '-')
	       {
		 unread(ch);
		 unread('-');
		 return '{';
	       }
	     skipOldComment();
	  }
	else if (verticalToo
		 ? (ch < 0 || ! Character.isWhitespace((char) ch))
		 : (ch != ' ' && ch != '\t'))
	  return ch;
      }
  }

  final void skipToSemicolon ()
    throws java.io.IOException
  {
    for (;;)
      {
	int next = read();
	if (next < 0 || next == ';')
	  break;
      }
  }

  final void skipOldComment()
    throws java.io.IOException, SyntaxException
  {
    int seenDashes = 0;
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() - 2;
    warnOldVersion("use (: :) instead of old-style comment {-- --}");
    for (;;)
      {
	int ch = read();
	if (ch == '-')
	  seenDashes++;
	else if (ch == '}' && seenDashes >= 2)
	  return;
	else if (ch < 0)
	  {
	    curLine = startLine;
	    curColumn = startColumn;
	    eofError("non-terminated comment starting here");
	  }
	else
	  seenDashes = 0;
      }
  }

  final void skipComment()
    throws java.io.IOException, SyntaxException
  {
    commentCount++;
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() - 1;
    if (errorIfComment != null)
      {
        curLine = startLine;
        curColumn = startColumn;
        error('e', errorIfComment);
      }
    int prev = 0;
    int commentNesting = 0;
    char saveReadState = pushNesting(':');
    for (;;)
      {
	int ch = read();
	if (ch == ':')
	  {
	    if (prev == '(')
	      {
		commentNesting++;
		ch = 0;
	      }
	  }
	else if (ch == ')' && prev == ':')
	  {
	    if (commentNesting == 0)
	      {
		popNesting(saveReadState);
		return;
	      }
	    --commentNesting;
	  }
	else if (ch < 0)
	  {
	    curLine = startLine;
	    curColumn = startColumn;
	    eofError("non-terminated comment starting here");
	  }
	prev = ch;
      }
  }

  /** Do skipSpace followed by unread to find next non-space character. */
  final int peekNonSpace(String message)
    throws java.io.IOException, SyntaxException
  {
    int ch = skipSpace();
    if (ch < 0)
      eofError(message);
    unread(ch);
    return ch;
  }

  static final int EOF_TOKEN = -1;
  static final int EOL_TOKEN = '\n';
  static final char INTEGER_TOKEN = '0';
  static final char DECIMAL_TOKEN = '1';
  static final char DOUBLE_TOKEN = '2';
  static final int STRING_TOKEN = '"';
  static final int SLASHSLASH_TOKEN = 'D';
  static final int DOTDOT_TOKEN = '3';
  static final int COLON_EQUAL_TOKEN = 'L'; // ":="
  static final int COLON_COLON_TOKEN = 'X';

  /** A non-qualified (simple) name (NCName).
   * The tokenBuffer contains the name (which does not contain a ':'). */
  static final int NCNAME_TOKEN = 'A';

  /** A non-qualified (simple) name (NCName) followed by a colon.
   * The colon is not followed by another NCNAME (in which it would
   * be a QNAME_TOKEN instead).
   * The tokenBuffer contains the name (which does not contain the ':'). */
  static final int NCNAME_COLON_TOKEN = 'C';

  /** A Qualified name (QName).
   * The tokenBuffer contains the full name, which contains one ':'. */
  static final int QNAME_TOKEN = 'Q';

  static final int ARROW_TOKEN = 'R';

  /* FuncName including following '('). */
  static final int FNAME_TOKEN = 'F';

  static final int IMPORT_MODULE_TOKEN = 'I'; // <"import" "module">
  static final int IMPORT_SCHEMA_TOKEN = 'T'; // <"import" "schema">
  static final int MODULE_NAMESPACE_TOKEN = 'M'; // <"module" "namespace">
  static final int DECLARE_NAMESPACE_TOKEN = 'N'; // <"declare" "namespace">
  static final int DECLARE_BOUNDARY_SPACE_TOKEN = 'S'; // <"declare" "boundary-space">
  static final int DEFAULT_ELEMENT_TOKEN = 'E'; // <"declare" "default" "element">
  static final int DEFAULT_FUNCTION_TOKEN = 'O'; // <"declare" "default" "function">
  static final int DEFAULT_COLLATION_TOKEN = 'G';
  static final int DEFAULT_ORDER_TOKEN = 'H'; // <"declare" "default" "order">

  static final int DECLARE_FUNCTION_TOKEN = 'P'; // <"declare" "function">
  static final int DECLARE_VARIABLE_TOKEN = 'V'; // <"declare" "variable">
  static final int DECLARE_BASE_URI_TOKEN = 'B'; // <"declare" "base-uri">
  static final int DECLARE_ORDERING_TOKEN = 'U'; // <"declare" "ordering">
  static final int DECLARE_CONSTRUCTION_TOKEN = 'K'; // <"declare" "construction">
  static final int DECLARE_OPTION_TOKEN = 'o'; // <"declare" "option">
  static final int DECLARE_COPY_NAMESPACES_TOKEN = 'L'; // <"declare" "copy-namespaces">
  static final int DEFINE_QNAME_TOKEN = 'W'; // <"define" QName> - an error
  static final int XQUERY_VERSION_TOKEN = 'Y'; // <"xquery" "version">

  /* 'Q': QName (intern'ed name is curValue)
   * 'R': NCName ':' '*'
   * OP_AXIS_FIRST: 'ancestor' followed by '::'
   * ...
   * OP_AXIS_FIRST+AXIS_SELF: 'self' followed by '::'
   */

  static final int OP_AXIS_FIRST = 100;
  static final int COUNT_OP_AXIS = 13;
  static final int AXIS_ANCESTOR = 0;
  static final int AXIS_ANCESTOR_OR_SELF = 1;
  static final int AXIS_ATTRIBUTE = 2;
  static final int AXIS_CHILD = 3;
  static final int AXIS_DESCENDANT = 4;
  static final int AXIS_DESCENDANT_OR_SELF = 5;
  static final int AXIS_FOLLOWING = 6;
  static final int AXIS_FOLLOWING_SIBLING = 7;
  static final int AXIS_NAMESPACE = 8;
  static final int AXIS_PARENT = 9;
  static final int AXIS_PRECEDING = 10;
  static final int AXIS_PRECEDING_SIBLING = 11;
  static final int AXIS_SELF = 12;
  static final int OP_WHERE      = 196;
  static final int PRAGMA_START_TOKEN = 197; // '{#'
  // Token types for binary operators.
  static final int OP_BASE        = 400;
  static final int OP_OR         = OP_BASE;      // 'or'
  static final int OP_AND        = OP_BASE + 1;  // 'and'
  static final int OP_EQU        = OP_BASE + 2;  // ' ='
  static final int OP_NEQ        = OP_BASE + 3;  // '! ='
  static final int OP_LSS        = OP_BASE + 4;  // '<'
  static final int OP_GRT        = OP_BASE + 5;  // '>'
  static final int OP_LEQ        = OP_BASE + 6;  // '< ='
  static final int OP_GEQ        = OP_BASE + 7;  // '> ='
  static final int OP_IS         = OP_BASE + 8;  // 'is'
  static final int OP_ISNOT      = OP_BASE + 9;  // 'isnot'
  static final int OP_GRTGRT     = OP_BASE + 10; // '>>'
  static final int OP_LSSLSS     = OP_BASE + 11; // '<<'

  static final int OP_RANGE_TO   = OP_BASE + 12;  // 'to'

  static final int OP_ADD        = OP_BASE + 13;  // '+'
  static final int OP_SUB        = OP_BASE + 14;  // '-'

  static final int OP_MUL        = OP_BASE + 15;  // '*'
  static final int OP_DIV        = OP_BASE + 16;  // 'div'
  static final int OP_IDIV       = OP_BASE + 17;  // 'idiv'
  static final int OP_MOD        = OP_BASE + 18;  // 'mod'

  static final int OP_UNION      = OP_BASE + 19;  // 'union'

  static final int OP_INTERSECT  = OP_BASE + 20;  // 'intersect'
  static final int OP_EXCEPT     = OP_BASE + 21;  // 'except'

  static final int OP_INSTANCEOF = OP_BASE + 22;  // 'instance' 'of'
  static final int OP_TREAT_AS   = OP_BASE + 23;  // 'treat' 'as'
  static final int OP_CASTABLE_AS= OP_BASE + 24;  // 'castable' 'as'
  static final int OP_CAST_AS    = OP_BASE + 25;  // 'cast' 'as'

  static final int OP_EQ = OP_BASE + 26; // 'eq'
  static final int OP_NE = OP_BASE + 27; // 'ne'
  static final int OP_LT = OP_BASE + 28; // 'lt'
  static final int OP_LE = OP_BASE + 29; // 'le
  static final int OP_GT = OP_BASE + 30; // 'gt'
  static final int OP_GE = OP_BASE + 31; // 'ge'

  static final int OP_NODE = 230; // 'node' followed by '('
  static final int OP_TEXT = 231; // 'text' followed by '('
  static final int OP_COMMENT = 232; // 'comment' followed by '('
  static final int OP_PI = 233;   // 'processing-instruction' '('
  static final int OP_DOCUMENT = 234; // 'document-node' '('
  static final int OP_ELEMENT = 235; // 'element' '('
  static final int OP_ATTRIBUTE = 236; // 'attribute' '('
  static final int OP_ITEM = 237; // 'item' '('
  static final int OP_EMPTY_SEQUENCE = 238; // 'empty-sequence' '('
  static final int OP_SCHEMA_ATTRIBUTE = 239; // 'schema-attribute' '('
  static final int OP_SCHEMA_ELEMENT = 240; // 'schema-element' '('
  static final int IF_LPAREN_TOKEN = 241; // 'if' '('
  static final int TYPESWITCH_LPAREN_TOKEN = 242; // 'typeswitch' '('

  static final int FOR_DOLLAR_TOKEN = 243; // 'for' '$'
  static final int LET_DOLLAR_TOKEN = 244; // 'let' '$'
  static final int SOME_DOLLAR_TOKEN = 245; // 'some' '$'
  static final int EVERY_DOLLAR_TOKEN = 246; // 'every' '$'
  static final int CASE_DOLLAR_TOKEN = 247; // 'case' '$'
  static final int VALIDATE_LBRACE_TOKEN = 248; // 'validate' '{'
  static final int ORDERED_LBRACE_TOKEN = 249; // 'ordered' '{'
  static final int UNORDERED_LBRACE_TOKEN = 250; // 'unordered' '{'
  static final int ELEMENT_TOKEN = 251; // 'element' followed by '{' or alpha
  static final int ATTRIBUTE_TOKEN = 252;// 'attribute' followed by '{' or alpha
  static final int TEXT_TOKEN = 253; // 'text' followed by '{'
  static final int COMMENT_TOKEN = 254; // 'text' followed by '{'
  static final int PI_TOKEN = 255; // 'processing-instruction' followed by '{' or alpha
  static final int DOCUMENT_TOKEN = 256; // ;document' followed by '{'
  
  private int saveToken;
  private Object saveValue;

  public void mark ()
    throws java.io.IOException
  {
    super.mark();
    saveToken = curToken;
    saveValue = curValue;
  }

  public void reset()
    throws java.io.IOException
  {
    curToken = saveToken;
    curValue = saveValue;
    super.reset();
  }

  private int setToken (int token, int width)
  {
    curToken = token;
    curLine = port.getLineNumber() + 1;
    curColumn = port.getColumnNumber() + 1 - width;
    return token;
  }

  void checkSeparator (char ch)
  {
    if (XName.isNameStart(ch))
      error('e', "missing separator", "XPST0003");
  }

  int getRawToken()
      throws java.io.IOException, SyntaxException
  {
    int next;
    for (;;)
      {
	next = readUnicodeChar();
	if (next < 0)
	  return setToken(EOF_TOKEN, 0);
	if (next == '\n' || next == '\r')
	  {
	    if (nesting <= 0)
	      return setToken(EOL_TOKEN, 0);
	  }
	else if (next == '(')
	  {
	    if (checkNext(':'))
	      skipComment();
            else if (checkNext('#'))
              return setToken(PRAGMA_START_TOKEN, 2);
	    else
	      return setToken('(', 1);
	  }
	else if (next == '{')
	  {
	    if (! checkNext('-'))
	      return setToken('{', 1);
	    next = read();
	    if (next != '-')
	      {
		// FIXME backup 2 chars. Can fix using special token for '{-'.
		unread();
		unread();
		return setToken('{', 1);
	      }
	    skipOldComment();
	  }
	else if (next != ' ' && next != '\t')
	  break;
      }
    tokenBufferLength = 0;
    curLine = port.getLineNumber() + 1;
    curColumn = port.getColumnNumber();
    char ch = (char) next;
    switch (ch)
      {
      case ')':  case '[':  case ']':  case '}':
      case '$':  case '@':  case ',':  case '?':  case ';':
	break;
      case ':':
	if (checkNext('='))
	  ch = COLON_EQUAL_TOKEN;
	else if (checkNext(':'))
	  ch = COLON_COLON_TOKEN;
	break;
      case '|':
	ch = OP_UNION;
	break;
      case '*':
	ch = OP_MUL;
	break;
      case '+':
	ch = OP_ADD;
	break;
      case '-':
	ch = OP_SUB;
	break;
      case '!':
	if (checkNext('='))
	  ch = OP_NEQ;
	break;
      case '/':
	if (checkNext('/'))
	  ch = SLASHSLASH_TOKEN;
	break;
      case '=':
	if (checkNext('>'))
	  ch = ARROW_TOKEN;
	ch = OP_EQU;
	break;
      case '>':
	ch = checkNext('=') ? (char) OP_GEQ
	  : checkNext('>') ? (char) OP_GRTGRT : (char) OP_GRT;
	break;
      case '<':
	ch = checkNext('=') ? (char) OP_LEQ
	  : checkNext('<') ? (char) OP_LSSLSS : (char) OP_LSS;
	break;
      case '\'':  case '\"':
	char saveReadState = pushNesting ((char) next);
	for (;;)
	  {
	    next = readUnicodeChar();
	    if (next < 0)
	      eofError("unexpected end-of-file in string starting here");
	    if (next == '&')
	      {
		parseEntityOrCharRef();
		continue;
	      }
	    else if (ch == next)
	      {
		next = peek();
		if (ch != next)
                  break;
                next = read();
	      }
	    tokenBufferAppend(next);
	  }
	popNesting(saveReadState);
	ch = STRING_TOKEN;
	break;
      default:
	if (Character.isDigit(ch)
            || (ch == '.' && Character.isDigit((char) peek())))
	  {
	    boolean seenDot = ch == '.';
	    for (;; )
	      {
		tokenBufferAppend(ch);
		next = read();
		if (next < 0)
		  break;
		ch = (char) next;
		if (ch == '.')
		  {
		    if (seenDot)  break;
		    seenDot = true;
		  }
		else if (! Character.isDigit(ch))
		  break;
	      }
	    if (next == 'e' || next == 'E')
	      {
		tokenBufferAppend((char) next);
		next = read();
		if (next == '+' || next == '-')
		  {
		    tokenBufferAppend(next);
		    next = read();
		  }
		int expDigits = 0;
		for (;;)
		  {
		    if (next < 0)
		      break;
		    ch = (char) next;
		    if (! Character.isDigit(ch))
		      {
                        checkSeparator(ch);
			unread();
			break;
		      }
		    tokenBufferAppend(ch);
		    next = read();
		    expDigits++;
		  }
		if (expDigits == 0)
		  error('e', "no digits following exponent", "XPST0003");
		ch = DOUBLE_TOKEN;
	      }
	    else
	      {
		ch = seenDot ? DECIMAL_TOKEN : INTEGER_TOKEN;
		if (next >= 0)
                  {
                    checkSeparator((char) next);
                    unread(next);
                  }
	      }
	  }
        else if (ch == '.')
          {
            if (checkNext('.'))
              ch = DOTDOT_TOKEN;
	    break;
          }
	else if (XName.isNameStart(ch))
	  {
	    for (;;)
	      {
		tokenBufferAppend(ch);
		next = read();
		ch = (char) next;
		if (! XName.isNamePart(ch))
		  break;
	      }
	    if (next < 0)
	      ch = NCNAME_TOKEN;
	    else
	      {
		if (next != ':')
		    ch = NCNAME_TOKEN;
		else
		  {
		    next = read();
		    if (next < 0)
		      eofError("unexpected end-of-file after NAME ':'");
		    ch = (char) next;
		    if (XName.isNameStart(ch))
		      {
			tokenBufferAppend(':');
			for (;;)
			  {
			    tokenBufferAppend(ch);
			    next = read();
			    ch = (char) next;
			    if (! XName.isNamePart(ch))
			      break;
			  }
			ch = QNAME_TOKEN;
		      }
		    else if (ch == '=')
		      {
			unread(ch);
			ch = NCNAME_TOKEN;
		      }
		    else
		      ch = NCNAME_COLON_TOKEN;
		  }
		unread(next);
	      }
	  }
	else if (ch >= ' ' && ch < 127)
	  syntaxError("invalid character '"+ch+'\'');
	else
	  syntaxError("invalid character '\\u"+Integer.toHexString(ch)+'\'');
      }
    curToken = ch;
    return ch;
  }

  /** Scan until a given delimiter.
   * On success, text upto the delimiter is in then tokenBuffer (with
   * tokenBufferLength marking its length); the delimiter is not included.
   */
  public void getDelimited(String delimiter)
      throws java.io.IOException, SyntaxException
  {
    if (! readDelimited(delimiter))
      eofError("unexpected end-of-file looking for '"+delimiter+'\'');
  }

  public void appendNamedEntity(String name)
  {
    name = name.intern();
    char ch = '?';
    if (name == "lt")
      ch = '<';
    else if (name == "gt")
      ch = '>';
    else if (name == "amp")
      ch = '&';
    else if (name == "quot")
      ch = '"';
    else if (name == "apos")
      ch = '\'';
    else
      error("unknown enity reference: '"+name+"'");
    tokenBufferAppend(ch);
  }

  boolean match (String word1, String word2, boolean force)
      throws java.io.IOException, SyntaxException
  {
    if (match(word1))
      {
        mark();
        getRawToken();
        if (match(word2))
          {
            reset();
            getRawToken();
            return true;
          }
        reset();
        if (force)
          {
            error('e', "'"+word1+"' must be followed by '"+word2+"'",
                  "XPST0003");
            return true;
          }
      }
    return false;
  }

  /** Return the current token, assuming it is in operator context.
   * Resolve NCNAME_TOKEN (identifier) to 'and', 'or', 'div', etc.
   */
  int peekOperator()
      throws java.io.IOException, SyntaxException
  {
    while (curToken == EOL_TOKEN)
      {
	if (nesting == 0)
	  return EOL_TOKEN;
	getRawToken();
      }
    if (curToken == NCNAME_TOKEN)
      {
	int len = tokenBufferLength;
        char c1, c2, c3;
        switch (len)
          {
          case 2:
            c1 = tokenBuffer[0];
            c2 = tokenBuffer[1];
            if (c1 == 'o' && c2 == 'r')
              curToken = OP_OR;
            else if (c1 == 't' && c2 == 'o')
              curToken = OP_RANGE_TO;
            else if (c1 == 'i' && c2 == 's')
              curToken = OP_IS;
            else if (c1 == 'e' && c2 == 'q')
              curToken = OP_EQ;
            else if (c1 == 'n' && c2 == 'e')
              curToken = OP_NE;
            else if (c1 == 'g')
              {
                if (c2 == 'e')  curToken = OP_GE;
                else if (c2 == 't')  curToken = OP_GT;
              }
            else if (c1 == 'l')
              {
                if (c2 == 'e')  curToken = OP_LE;
                else if (c2 == 't')  curToken = OP_LT;
              }
            break;

          case 3:
            c1 = tokenBuffer[0];
            c2 = tokenBuffer[1];
            c3 = tokenBuffer[2];
            if (c1 == 'a')
              {
                if (c2 == 'n' && c3 == 'd')
                  curToken = OP_AND;
              }
            else if (c1 == 'm') {
              if (c2 == 'u' && c3 == 'l')
                curToken = OP_MUL;
              if (c2 == 'o' && c3 == 'd')
                curToken = OP_MOD;
            }
            else if (c1 == 'd') {
              if (c2 == 'i' && c3 == 'v')
                curToken = OP_DIV;
            }
            break;
          case 4:
            if (match("idiv"))
              curToken = OP_IDIV;
            else if (match("cast", "as", true))
              curToken = OP_CAST_AS;
            break;
          case 5:
            if (match("where"))
              curToken = OP_WHERE;
            else if (match("isnot"))
              curToken = OP_ISNOT;
            else if (match("union"))
              curToken = OP_UNION;
            else if (match("treat", "as", true))
              curToken = OP_TREAT_AS;
            break;
          case 6:
            if (match("except"))
              curToken = OP_EXCEPT;
            break;
          case 8:
            if (match("instance", "of", true))
              curToken = OP_INSTANCEOF;
            else if (match("castable", "as", true))
              curToken = OP_CASTABLE_AS;
            break;
          case 9:
            if (match("intersect"))
              curToken = OP_INTERSECT;
            break;
          case 10:
            if (match("instanceof")) // obsolete
              {
                warnOldVersion("use 'instanceof of' (two words) instead of 'instanceof'");
                curToken = OP_INSTANCEOF;
              }
            break;
          default:
            break;
          }
      }
    return curToken;
  }

  /**
   * Internal method to match against double-lexeme tokens.
   * @param word0 expected previous word
   * @param word1 expected next word
   */
  private boolean lookingAt (String word0, String word1)
      throws java.io.IOException, SyntaxException
  {
    if (! word0.equals(curValue))
      return false;
    int i = 0;
    int len = word1.length();
    for (;; )
      {
	int ch = read();
	if (i == len)
	  {
	    if (ch < 0)
	      return true;
	    if ( ! XName.isNamePart((char) ch))
	      {
		unread();
		return true;
	      }
	    i++;
	    break;
	  }
	if (ch < 0 || ch != word1.charAt(i++))
	  break;
      }
    port.skip(-i);
    return false;
  }

  int getAxis ()
  {
    // match axis name
    String name = new String(tokenBuffer, 0, tokenBufferLength).intern();
    int i;
    for (i = COUNT_OP_AXIS;  --i >= 0; )
      if (axisNames[i] == name)
	break;
    if (i < 0
        || i == AXIS_NAMESPACE) // The namespace-axis is XSLT/XPath-only.
      {
	error('e', "unknown axis name '" + name + '\'', "XPST0003");
	i = AXIS_CHILD;
      }
    return (char) (OP_AXIS_FIRST + i);
  }

  /** Process token, assuming we are in operand context.
   */

  int peekOperand()
      throws java.io.IOException, SyntaxException
  {
    while (curToken == EOL_TOKEN)
      getRawToken();
    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	int next = skipSpace(nesting != 0);
        switch (tokenBuffer[0])
          {
          case 'a':
            if (match("attribute"))
              {
                if (next == '(')
                  return curToken = OP_ATTRIBUTE;
                if (next == '{' || XName.isNameStart((char) next))
                  {
                    unread();
                    return curToken = ATTRIBUTE_TOKEN;
                  }
                break;
              }
            break;
          case 'c':
            if (match("comment"))
              {
                if (next == '(')
                  return curToken = OP_COMMENT;
                if (next == '{')
                  {
                    unread();
                    return curToken = COMMENT_TOKEN;
                  }
              }
            break;
          case 'd':
            if (next == '{' && match("document"))
              {
                unread();
                return curToken = DOCUMENT_TOKEN;
              }
            if (next == '(' && match("document-node"))
              return curToken = OP_DOCUMENT;
            break;
          case 'e':
            if (match("element"))
              {
                if (next == '(')
                  return curToken = OP_ELEMENT;
                if (next == '{' || XName.isNameStart((char) next))
                  {
                    unread();
                    return curToken = ELEMENT_TOKEN;
                  }
                break;
              }
            if (next == '(' && match("empty-sequence"))
              return curToken = OP_EMPTY_SEQUENCE;
            if (next == '$' && match("every"))
              return curToken = EVERY_DOLLAR_TOKEN;
            break;
          case 'f':
            if (next == '$' && match("for"))
              return curToken = FOR_DOLLAR_TOKEN;
            break;
          case 'i':
            if (next == '(' && match("if"))
              return curToken = IF_LPAREN_TOKEN;
            if (next == '(' && match("item"))
              return curToken = OP_ITEM;
            break;
          case 'l':
            if (next == '$' && match("let"))
              return curToken = LET_DOLLAR_TOKEN;
            break;
          case 'n':
            if (next == '(' && match("node"))
              return curToken = OP_NODE;
            break;
          case 'o':
            if (next == '{' && match("ordered"))
              return curToken = ORDERED_LBRACE_TOKEN;
            break; 
          case 'p':
            if (match("processing-instruction"))
              {
                if (next == '(')
                  return curToken = OP_PI;
                if (next == '{' || XName.isNameStart((char) next))
                  {
                    unread();
                    return curToken = PI_TOKEN;
                  }
                break;
              }
            break;
          case 's':
            if (next == '$' && match("some"))
              return curToken = SOME_DOLLAR_TOKEN;
            if (next == '(' && match("schema-attribute"))
              return curToken = OP_SCHEMA_ATTRIBUTE;
            if (next == '(' && match("schema-element"))
              return curToken = OP_SCHEMA_ELEMENT;
            break;
          case 't':
            if (match("text"))
              {
                if (next == '(')
                  return curToken = OP_TEXT;
                if (next == '{')
                  {
                    unread();
                    return curToken = TEXT_TOKEN;
                  }
              }
            if (next == '(' && match("typeswitch"))
              return curToken = TYPESWITCH_LPAREN_TOKEN;
            break;
          case 'u':
            if (next == '{' && match("unordered"))
              return curToken = UNORDERED_LBRACE_TOKEN;
            break; 
          case 'v':
            if (next == '{' && match("validate"))
              return curToken = VALIDATE_LBRACE_TOKEN;
            break;
          }
	if (next == '(' && peek() != ':')
	  {
	    return curToken = FNAME_TOKEN;
	  }
	if (next == ':' && peek() == ':')
	  return curToken = getAxis();
	String name = new String(tokenBuffer, 0, tokenBufferLength);
	curValue = name;
	switch (next)
	  {
          case 'b':
	    if (lookingAt("declare", /*"b"+*/ "ase-uri"))
              return curToken = DECLARE_BASE_URI_TOKEN;
	    if (lookingAt("declare", /*"b"+*/ "oundary-space"))
              return curToken = DECLARE_BOUNDARY_SPACE_TOKEN;
            break;
          case 'c':
	    if (lookingAt("declare", /*"c"+*/ "onstruction"))
              return curToken = DECLARE_CONSTRUCTION_TOKEN;
	    if (lookingAt("declare", /*"c"+*/ "opy-namespaces"))
              return curToken = DECLARE_COPY_NAMESPACES_TOKEN;
            break;
	  case 'd':
	    if (lookingAt("declare", /*"d"+*/ "efault"))
	      {
		getRawToken();
		if (match("function"))
		  return curToken = DEFAULT_FUNCTION_TOKEN;
		if (match("element"))
		  return curToken = DEFAULT_ELEMENT_TOKEN;
		if (match("collation"))
		  return curToken = DEFAULT_COLLATION_TOKEN;
		if (match("order"))
		  return curToken = DEFAULT_ORDER_TOKEN;
		error("unrecognized/unimplemented 'declare default'");
		skipToSemicolon();
		return peekOperand();
	      }
	  case 'e':
	    if (lookingAt("default", /*"e"+*/ "lement"))
	      {
		warnOldVersion("replace 'default element' by 'declare default element namespace'");
		return curToken = DEFAULT_ELEMENT_TOKEN;
	      }
	    break;
	  case 'f':
	    if (lookingAt("declare", /*"f"+*/ "unction"))
	      return curToken = DECLARE_FUNCTION_TOKEN;
	    if (lookingAt("define", /*"f"+*/ "unction"))
	      {
                warnOldVersion("replace 'define function' by 'declare function'");
		return curToken = DECLARE_FUNCTION_TOKEN;
	      }
	    if (lookingAt("default", /*"f"+*/ "unction"))
	      {
		warnOldVersion("replace 'default function' by 'declare default function namespace'");
		return curToken = DEFAULT_FUNCTION_TOKEN;
	      }
	    break;
	  case 'm':
	    if (lookingAt("import", /*"m"+*/ "odule"))
	      return curToken = IMPORT_MODULE_TOKEN;
	    break;
	  case 'n':
	    if (lookingAt("declare", /*"n"+*/ "amespace"))
	      return curToken = DECLARE_NAMESPACE_TOKEN;
	    if (lookingAt("default", /*"n"+*/ "amespace"))
	      {
		warnOldVersion("replace 'default namespace' by 'declare default element namespace'");
		return curToken = DEFAULT_ELEMENT_TOKEN;
	      }
	    if (lookingAt("module", /*"n"+*/ "amespace"))
	      return curToken = MODULE_NAMESPACE_TOKEN;
	    break;
	  case 'o':
	    if (lookingAt("declare", /*"o"+*/ "rdering"))
	      return curToken = DECLARE_ORDERING_TOKEN;
	    if (lookingAt("declare", /*"o"+*/ "ption"))
	      return curToken = DECLARE_OPTION_TOKEN;
	    break;
	  case 's':
	    if (lookingAt("import", /*"s"+*/ "chema"))
	      return curToken = IMPORT_SCHEMA_TOKEN;
	    break;
	  case 'v':
	    if (lookingAt("declare", /*"v"+*/ "ariable"))
	      return curToken = DECLARE_VARIABLE_TOKEN;
	    if (lookingAt("define", /*"v"+*/ "ariable"))
	      {
                warnOldVersion("replace 'define variable' by 'declare variable'");
		return curToken = DECLARE_VARIABLE_TOKEN;
	      }
            if (lookingAt("xquery", /*"v"+*/ "ersion"))
              return curToken = XQUERY_VERSION_TOKEN;
	    break;
	  case 'x':
	    if (lookingAt("declare", /*"x"+*/ "mlspace"))
              {
		warnOldVersion("replace 'define xmlspace' by 'declare boundary-space'");
                return curToken = DECLARE_BOUNDARY_SPACE_TOKEN;
              }
	    break;
	  }
	if (next >= 0)
	  {
	    unread();
	    if (XName.isNameStart((char) next) && curValue.equals("define"))
	      {
		getRawToken();
		curToken = DEFINE_QNAME_TOKEN;
	      }
	  }
	return curToken;
      }
    if (curToken == NCNAME_COLON_TOKEN)
      {
	int next = read();
	if (next == ':') // We've seen an Axis specifier.
	  curToken = getAxis();
	else
	  unread(next);
      }
    return curToken;
  }

  void checkAllowedNamespaceDeclaration (String prefix, String uri,
                                         boolean inConstructor)
  {
    boolean xmlPrefix = "xml".equals(prefix);
    if (NamespaceBinding.XML_NAMESPACE.equals(uri))
      {
        if (! xmlPrefix || ! inConstructor)
          error('e', "namespace uri cannot be the same as the prefined xml namespace",
                "XQST0070");
      }
    else if (xmlPrefix || "xmlns".equals(prefix))
      error('e', "namespace prefix cannot be 'xml' or 'xmlns'",
            "XQST0070");
  }

  void pushNamespace(String prefix, String uri)
  {
    if (uri.length() == 0)
      uri = null;
    prologNamespaces = new NamespaceBinding(prefix, uri, prologNamespaces);
  }

  public XQParser(InPort port, SourceMessages messages, XQuery interp)
  {
    super(port, messages);
    interpreter = interp;
    nesting = 1;

    // Push standard namespaces into lexical scope.
    NamespaceBinding ns = builtinNamespaces;
    prologNamespaces = ns;
  }

  public void setInteractive(boolean v)
  {
    if (interactive != v)
      if (v) nesting--; else nesting++;
    interactive = v;
  }
  
  private static final int priority(int opcode)
  {
    switch (opcode)
      {
      case OP_OR:
	return 1;
      case OP_AND:
        return 2;
      case OP_EQU:  case OP_NEQ:
      case OP_LSS:  case OP_GRT:  case OP_LEQ:  case OP_GEQ:
      case OP_EQ:  case OP_NE:
      case OP_LT:  case OP_GT:  case OP_LE:  case OP_GE:
      case OP_IS:  case OP_ISNOT:
      case OP_GRTGRT:  case OP_LSSLSS:
        return 3;
      case OP_RANGE_TO:
        return 4;
      case OP_ADD:  case OP_SUB:
	return 5;
      case OP_MUL: case OP_DIV:  case OP_IDIV: case OP_MOD:
        return 6;
      case OP_UNION:
        return 7;
      case OP_INTERSECT:  case OP_EXCEPT:
        return 8;
      case OP_INSTANCEOF:
	return 9;
      case OP_TREAT_AS:
        return 10;
      case OP_CASTABLE_AS:
        return 11;
      case OP_CAST_AS:
        return 12;
      default:
	return 0;
      }
  }

  static Expression makeBinary(Expression func,
			       Expression exp1, Expression exp2)
  {
    Expression[] args = new Expression[2];
    args[0] = exp1;
    args[1] = exp2;
    return new ApplyExp(func, args);
  }

  static Expression makeExprSequence(Expression exp1, Expression exp2)
  {
    return makeBinary(makeFunctionExp
		      ("gnu.kawa.functions.AppendValues", "appendValues"),
		      exp1, exp2);
  }

  Expression makeBinary(int op, Expression exp1, Expression exp2)
      throws java.io.IOException, SyntaxException
  {
    Expression func;
    switch (op)
      {
      case OP_ADD: 
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "add", "+");
	break;
      case OP_SUB:
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "sub", "-");
	break;
      case OP_MUL:
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "mul", "*");
	break;
      case OP_DIV:
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "div", "div");
	break;
      case OP_IDIV:
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "idiv", "idiv");
	break;
      case OP_MOD:
	func = makeFunctionExp("gnu.xquery.util.ArithOp", "mod", "mod");
	break;
      case OP_EQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valEq", "eq");
	break;
      case OP_NE:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valNe", "ne");
	break;
      case OP_LT:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valLt", "lt");
	break;
      case OP_LE:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valLe", "le");
	break;
      case OP_GT:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valGt", "gt");
	break;
      case OP_GE:
	func = makeFunctionExp("gnu.xquery.util.Compare", "valGe", "ge");
	break;
      case OP_EQU:
	func = makeFunctionExp("gnu.xquery.util.Compare", "=");
	break;
      case OP_NEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", "!=");
	break;
      case OP_LSS:
	func = makeFunctionExp("gnu.xquery.util.Compare", "<");
	break;
      case OP_LEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", "<=");
	break;
      case OP_GRT:
	func = makeFunctionExp("gnu.xquery.util.Compare", ">");
	break;
      case OP_GEQ:
	func = makeFunctionExp("gnu.xquery.util.Compare", ">=");
	break;
      case OP_IS:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Eq", "is");
	break;
      case OP_ISNOT:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Ne", "isnot");
	break;
      case OP_GRTGRT:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Gr", ">>");
	break;
      case OP_LSSLSS:
	func = makeFunctionExp("gnu.kawa.xml.NodeCompare", "$Ls", "<<");
	break;
      case OP_RANGE_TO:
	func = makeFunctionExp("gnu.xquery.util.Xutils", "integerRange");
	break;
      case OP_UNION:
	func = makeFunctionExp("gnu.kawa.xml.UnionNodes", "unionNodes");
	break;
      case OP_INTERSECT:
	func = makeFunctionExp("gnu.kawa.xml.IntersectNodes",
			       "intersectNodes");
	break;
      case OP_EXCEPT:
	func = makeFunctionExp("gnu.kawa.xml.IntersectNodes", "exceptNodes");
	break;
      default:
	return syntaxError("unimplemented binary op: "+op);
      }
    return makeBinary(func, exp1, exp2);
  }

  private void  parseSimpleKindType ()
    throws java.io.IOException, SyntaxException
  {
    getRawToken();
    if (curToken == ')')
      getRawToken();
    else
      error("expected ')'");
  }

  public Expression parseNamedNodeType (boolean attribute)
      throws java.io.IOException, SyntaxException
  {
    Expression qname, tname = null;
    getRawToken();
    if (curToken == ')')
      {
        qname = QuoteExp.getInstance(ElementType.MATCH_ANY_QNAME);
        getRawToken();
      }
    else
      {
        if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
          qname = parseNameTest(attribute);
        else
          {
            if (curToken != OP_MUL)
              syntaxError("expected QName or *");
            qname = QuoteExp.getInstance(ElementType.MATCH_ANY_QNAME);
          }

        getRawToken();
        if (curToken == ',')
          {
            getRawToken();
            if (curToken == QNAME_TOKEN || curToken == NCNAME_TOKEN)
              {
                tname = parseDataType();
              }
            else
               syntaxError("expected QName");
          }
        if (curToken == ')')
          getRawToken();
        else
          error("expected ')' after element");
      }
    return makeNamedNodeType(attribute, qname, tname);
  }

  static Expression makeNamedNodeType (boolean attribute,
                                       Expression qname, Expression tname)
  {
    Expression[] name = new Expression[2];
    ClassType nodeType = ClassType.make(attribute
                                        ? "gnu.kawa.xml.AttributeType"
                                        : "gnu.kawa.xml.ElementType");
    ApplyExp elt = new ApplyExp(nodeType.getDeclaredMethod("make", 1),
                                new Expression[] { qname });
    elt.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    if (tname == null)
      return elt;
    else
      {
        // For now "tname" is ignored - except we want to catch errors.
        return new BeginExp(tname, elt);
      }
  }

  private boolean warnedOldStyleKindTest;
  private void warnOldStyleKindTest()
  {
    if (warnedOldStyleKindTest)
      return;
    error('w', "old-style KindTest - first one here");
    warnedOldStyleKindTest = true;
  }

  /** Parse: ["as" SequenceType] */
  public Expression parseOptionalTypeDeclaration ()
      throws java.io.IOException, SyntaxException
  {
    if (! match("as"))
      return null;
    getRawToken();
    return parseDataType();
  }

  public Expression parseDataType()
      throws java.io.IOException, SyntaxException
  {
    Expression etype = parseItemType();
    int min, max;
    if (etype == null)
      {
        if (curToken != OP_EMPTY_SEQUENCE)
          return syntaxError("bad syntax - expected DataType");
        parseSimpleKindType();
        if (curToken == '?' || curToken == OP_ADD || curToken == OP_MUL)
          {
            getRawToken();
            return syntaxError("occurrence-indicator meaningless after empty-sequence()");
          }
        etype = QuoteExp.getInstance(OccurrenceType.emptySequenceType);
        min = 0;
        max = 0;
      }
    else if (curToken == '?')
      {
	min = 0;
	max = 1;
      }
    else if (curToken == OP_ADD)
      {
	min = 1;
	max = -1;
      }
    else if (curToken == OP_MUL)
      {
	min = 0;
	max = -1;
      }
    else
      {
	min = 1;
	max = 1;
      }
    if (parseContext == 'C')
      {
        if (max != 1)
          return syntaxError("type to 'cast as' or 'castable as' must be a 'SingleType'");
      }
    if (min != max)
      {
	getRawToken();
        Expression[] args = { etype,
                              QuoteExp.getInstance(gnu.math.IntNum.make(min)),
                              QuoteExp.getInstance(gnu.math.IntNum.make(max)) };
        ApplyExp otype
          = new ApplyExp(proc_OccurrenceType_getInstance,
                         args);
        otype.setFlag(ApplyExp.INLINE_IF_CONSTANT);
        return otype;
      }
    return etype;
  }

  static PrimProcedure proc_OccurrenceType_getInstance
  = new PrimProcedure(ClassType.make("gnu.kawa.reflect.OccurrenceType")
                      .getDeclaredMethod("getInstance", 3));

  public Expression parseMaybeKindTest ()
      throws java.io.IOException, SyntaxException
  {
    Type type;
    switch (curToken)
      {
      case OP_ATTRIBUTE:
      case OP_ELEMENT:
        return parseNamedNodeType(curToken == OP_ATTRIBUTE);

      case OP_TEXT:
        parseSimpleKindType();
        type = NodeType.textNodeTest;
        break;

      case OP_COMMENT:
        parseSimpleKindType();
        type = NodeType.commentNodeTest;
        break;

      case OP_DOCUMENT:
        parseSimpleKindType();
        type = NodeType.documentNodeTest;
        break;

      case OP_NODE:
        parseSimpleKindType();
        type = NodeType.anyNodeTest;
        break;

      case OP_PI:
        getRawToken();
        String piTarget = null;
        if (curToken == NCNAME_TOKEN || curToken == STRING_TOKEN)
          {
            piTarget = new String(tokenBuffer, 0, tokenBufferLength);
            getRawToken();
          }
        if (curToken == ')')
          getRawToken();
        else
          error("expected ')'");
        type = ProcessingInstructionType.getInstance(piTarget);
        break;

      default:
        return null;
      }
    return QuoteExp.getInstance(type);
  }

  public Expression parseItemType()
      throws java.io.IOException, SyntaxException
  {
    peekOperand();
    Expression etype = parseMaybeKindTest();
    Type type;
    if (etype != null)
      {
        if (parseContext == 'C')
          // Kludge to force error later.
          type = XDataType.anyAtomicType;
        else
          return etype;
      }
    else if (curToken == OP_ITEM)
      {
        parseSimpleKindType();
        type = SingletonType.getInstance();
      }
    else if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
      {
	String tname = new String(tokenBuffer, 0, tokenBufferLength);
        ReferenceExp rexp = new ReferenceExp(tname);
        rexp.setFlag(ReferenceExp.TYPE_NAME);
        maybeSetLine(rexp, curLine, curColumn);
	getRawToken();
        return rexp;
      }
    else
      return null;
    return QuoteExp.getInstance(type);
  }

  /** Parse a <code>URILiteral</code>..
   * @return either a String (on success),
   * or an ErrorExp (after emitting an error).
   */
  Object parseURILiteral ()
      throws java.io.IOException, SyntaxException
  {
    getRawToken();
    if (curToken != STRING_TOKEN)
      return declError("expected a URILiteral");
    String str = new String(tokenBuffer, 0, tokenBufferLength);
    str = TextUtils.replaceWhitespace(str, true);
    // FUTURE: An implementation MAY raise a static error if the value
    // of a URILiteral is of nonzero length and is not in the lexical
    // space of xs:anyURI, or if it is a string that represents a
    // relative URI as defined in [RFC2396].  err:XQST0046
    return str;
  }

  Expression parseExpr()
      throws java.io.IOException, SyntaxException
  {
    return parseExprSingle();
  }

  final Expression parseExprSingle ()
      throws java.io.IOException, SyntaxException
  {
    int startLine = curLine;
    int startColumn = curColumn;
    peekOperand();
    switch (curToken)
      {
        // FIXME old code tweaked line/column
        // as in:
        // maybeSetLine(exp, startLine, startColumn - 3);

      case IF_LPAREN_TOKEN:
        return parseIfExpr();
      case TYPESWITCH_LPAREN_TOKEN:
        return parseTypeSwitch();
      case FOR_DOLLAR_TOKEN:
        return parseFLWRExpression(true);
      case LET_DOLLAR_TOKEN:
        return parseFLWRExpression(false);
      case SOME_DOLLAR_TOKEN:
        return parseQuantifiedExpr(false);
      case EVERY_DOLLAR_TOKEN:
        return parseQuantifiedExpr(true);
      default:
        return parseBinaryExpr(priority(OP_OR));
      }
  }

  Expression parseBinaryExpr(int prio)
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseUnaryExpr();
    for (;;)
      {
	int token = peekOperator();
	if (token == EOL_TOKEN
	    // Following makes for better error handling.
	    || (token == OP_LSS && peek() == '/'))
	  return exp;  
	int tokPriority = priority(token);
	if (tokPriority < prio)
	  return exp;
	char saveReadState = pushNesting('%');
	getRawToken();
	popNesting(saveReadState);
        if (token >= OP_INSTANCEOF && token <= OP_CAST_AS)
          {
            if (token == OP_CAST_AS || token == OP_CASTABLE_AS)
              parseContext = 'C';
            Expression type = parseDataType();
            parseContext = '\0';
            Expression[] args = new Expression[2];
            Expression func;
            switch (token)
              {
              case OP_INSTANCEOF:
                args[0] = exp;
                args[1] = type;
                func = makeFunctionExp("gnu.xquery.lang.XQParser",
                                       "instanceOf");
                break;
              case OP_CASTABLE_AS:
                args[0] = exp;
                args[1] = type;
                func = new ReferenceExp(XQResolveNames.castableAsDecl);
                break;
              case OP_TREAT_AS:
                args[0] = type;
                args[1] = exp;
                func = makeFunctionExp("gnu.xquery.lang.XQParser",
                                       "treatAs");
                break;
              default: // i.e. case OP_CAST_AS:
                args[0] = type;
                args[1] = exp;
                func = new ReferenceExp(XQResolveNames.castAsDecl);
                break;
              }
            exp = new ApplyExp(func, args);
          }
	else if (token == OP_INSTANCEOF)
	  {
	    Expression[] args = { exp, parseDataType() };
	    exp = new ApplyExp(makeFunctionExp("gnu.xquery.lang.XQParser",
					       "instanceOf"),
			       args);
	  }
	else
	  {
	    Expression exp2 = parseBinaryExpr(tokPriority+1);
	    if (token == OP_AND)
	      exp = new IfExp(booleanValue(exp), booleanValue(exp2), XQuery.falseExp);
	    else if (token == OP_OR)
	      exp = new IfExp(booleanValue(exp), XQuery.trueExp, booleanValue(exp2));
	    else
	      exp = makeBinary(token, exp, exp2);
	  }
    }
  }

  Expression parseUnaryExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp;
    if (curToken == OP_SUB || curToken == OP_ADD)
      {
        int op = curToken;
        getRawToken();
        exp = parseUnaryExpr();
        Expression func
          = makeFunctionExp("gnu.xquery.util.ArithOp",
                            op == OP_ADD ? "plus" : "minus",
                            op == OP_ADD ? "+" : "-");
        exp = new ApplyExp(func, new Expression[] { exp });
      }
    else
      exp = parseUnionExpr();
    return exp;
  }

  Expression parseUnionExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseIntersectExceptExpr();
    for (;;)
      {
	int op = peekOperator();
	if (op != OP_UNION)
	  break;
	getRawToken();
	Expression exp2 = parseIntersectExceptExpr();
	exp = makeBinary(op, exp, exp2);
      }
    return exp;
  }

  Expression parseIntersectExceptExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parsePathExpr();
    for (;;)
      {
	int op = peekOperator();
	if (op != OP_INTERSECT && op != OP_EXCEPT)
	  break;
	getRawToken();
	Expression exp2 = parsePathExpr();
	exp = makeBinary(op, exp, exp2);
      }
    return exp;
  }

  Expression parsePathExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression step1;
    if (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	Declaration dotDecl = comp.lexical.lookup(DOT_VARNAME, false);
        Expression dot;
	if (dotDecl == null)
	  dot = syntaxError("context item is undefined", "XPDY0002");
        else
          dot = new ReferenceExp(DOT_VARNAME, dotDecl);
	step1 = new ApplyExp(ClassType.make("gnu.xquery.util.NodeUtils")
			     .getDeclaredMethod("rootDocument", 1),
			     new Expression[] { dot } );
	int next = skipSpace(nesting != 0);
        unread(next);
        if (next < 0 || next == ')' || next == '}')
          {
	    getRawToken();
            return step1;
          }
      }
    else
      step1 = parseStepExpr();
    return parseRelativePathExpr(step1);
  }

  /** Returns an expression that evaluates to a Symbol.
   * The expression will normally be constant
   * folded to a Symbol, but we cannot do that yet. */
  Expression parseNameTest (boolean attribute)
      throws java.io.IOException, SyntaxException
  {
    String local = null, prefix = null;
    if (curToken == QNAME_TOKEN)
      {
	int colon = tokenBufferLength;
	while (tokenBuffer[--colon] != ':') ;
	prefix = new String(tokenBuffer, 0, colon);
	colon++;
	local = new String(tokenBuffer, colon,
			   tokenBufferLength - colon);
      }
    else if (curToken == OP_MUL)
      {
 	int next = read();
        local = ElementType.MATCH_ANY_LOCALNAME;
	if (next != ':')
	  unread(next);
	else
	  {
	    next = read();
	    if (next < 0)
	      eofError("unexpected end-of-file after '*:'");
	    if (XName.isNameStart((char) next))
	      {
		unread();
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  syntaxError("invalid name test");
		else
		  local = new String(tokenBuffer, 0, tokenBufferLength)
                    .intern();
	      }
	    else if (next != '*')
	      syntaxError("missing local-name after '*:'");
	  }
        return QuoteExp.getInstance(Symbol.makeUninterned(local));
      }
    else if (curToken == NCNAME_TOKEN)
      {
	local = new String(tokenBuffer, 0, tokenBufferLength);
        if (attribute)
          return new QuoteExp(Namespace.EmptyNamespace.getSymbol(local.intern()));
        prefix = null;
      }
    else if (curToken == NCNAME_COLON_TOKEN)
      {
	prefix = new String(tokenBuffer, 0, tokenBufferLength);
	int next = read();
	if (next != '*')
	  syntaxError("invalid characters after 'NCName:'");
	local = ElementType.MATCH_ANY_LOCALNAME;
      }
    if (prefix != null)
      prefix = prefix.intern();
    Expression[] args = new Expression[3];
    args[0] = new ApplyExp(new ReferenceExp(XQResolveNames.resolvePrefixDecl),
                           new Expression[] { QuoteExp.getInstance(prefix) });
    args[1] = new QuoteExp(local == null ? "" : local);
    args[2] = new QuoteExp(prefix);
    ApplyExp make = new ApplyExp(Compilation.typeSymbol
                                 .getDeclaredMethod("make", 3),
                                 args);
    make.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return make;
  }

  Expression parseNodeTest(int axis)
      throws java.io.IOException, SyntaxException
  {
    int token = peekOperand();
    Expression[] args = new Expression[1];

    Expression etype = parseMaybeKindTest();

    if (etype != null)
      {	
	args[0] = etype;
      }
    else if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN 
	     || curToken == NCNAME_COLON_TOKEN || curToken == OP_MUL)
      {
        args[0] = makeNamedNodeType(axis == AXIS_ATTRIBUTE,
                                    parseNameTest(axis == AXIS_ATTRIBUTE),
                                    null);
      }
    else if (axis >= 0)
      return syntaxError("unsupported axis '"+axisNames[axis]+"::'");
    else
      return null;

    Declaration dotDecl = comp.lexical.lookup(DOT_VARNAME, false);
    Expression dot;
    if (dotDecl == null)
      dot = syntaxError("node test when context item is undefined", "XPDY0002");
    else
      dot = new ReferenceExp(DOT_VARNAME, dotDecl);
    if (etype == null)
      getRawToken();

    Expression makeAxisStep;
    if (axis == AXIS_CHILD || axis == -1)
      makeAxisStep = makeChildAxisStep;
    else if (axis == AXIS_DESCENDANT)
      makeAxisStep = makeDescendantAxisStep;
    else
      {
        String axisName;
        switch (axis)
          {
          case AXIS_DESCENDANT_OR_SELF: axisName = "DescendantOrSelf"; break;
          case AXIS_SELF:               axisName = "Self";             break;
          case AXIS_PARENT:             axisName = "Parent";           break;
          case AXIS_ANCESTOR:           axisName = "Ancestor";         break;
          case AXIS_ANCESTOR_OR_SELF:   axisName = "AncestorOrSelf";   break;
          case AXIS_FOLLOWING:          axisName = "Following";        break;
          case AXIS_FOLLOWING_SIBLING:  axisName = "FollowingSibling"; break;
          case AXIS_PRECEDING:          axisName = "Preceding";        break;
          case AXIS_PRECEDING_SIBLING:  axisName = "PrecedingSibling"; break;
          case AXIS_ATTRIBUTE:          axisName = "Attribute";        break;
          default: throw new Error();
          }
        makeAxisStep
          = QuoteExp.getInstance(new PrimProcedure
                                 ("gnu.kawa.xml."+axisName+"Axis",
                                  "make", 1));
      }
    ApplyExp mkAxis = new ApplyExp(makeAxisStep, args);
    mkAxis.setFlag(ApplyExp.INLINE_IF_CONSTANT);
    return new ApplyExp(mkAxis, new Expression[] { dot });
  }

  public static QuoteExp makeChildAxisStep
  = QuoteExp.getInstance(new PrimProcedure("gnu.kawa.xml.ChildAxis", "make", 1));
  public static QuoteExp makeDescendantAxisStep
  = QuoteExp.getInstance(new PrimProcedure("gnu.kawa.xml.DescendantAxis", "make", 1));

  Expression parseRelativePathExpr(Expression exp)
      throws java.io.IOException, SyntaxException
  {
    // If the previous operator was '//', then the corresponding E1.
    Expression beforeSlashSlash = null;

    while (curToken == '/' || curToken == SLASHSLASH_TOKEN)
      {
	boolean descendants = curToken == SLASHSLASH_TOKEN;

	LambdaExp lexp = new LambdaExp(3);
	Declaration dotDecl = lexp.addDeclaration(DOT_VARNAME);
	dotDecl.setFlag(Declaration.IS_SINGLE_VALUE);
        dotDecl.setType(NodeType.anyNodeTest);
	dotDecl.noteValueUnknown();
	lexp.addDeclaration(POSITION_VARNAME, LangPrimType.intType);
	lexp.addDeclaration(LAST_VARNAME, LangPrimType.intType);
	comp.push(lexp);
	if (descendants)
	  {
	    curToken = '/';
	    Expression dot = new ReferenceExp(DOT_VARNAME, dotDecl);
	    Expression[] args = { dot };
	    TreeScanner op = DescendantOrSelfAxis.anyNode;
	    lexp.body = new ApplyExp(op, args);
            beforeSlashSlash = exp;
	  }
	else
	  {
	    getRawToken();
            Expression exp2 = parseStepExpr();

            // Optimize: 'E1//child::TEST' to 'E1/descendant::TEST'
            Expression func;
            ApplyExp aexp;
            if (beforeSlashSlash != null
                && exp2 instanceof ApplyExp
                && (func = ((ApplyExp) exp2).getFunction()) instanceof ApplyExp
                && (aexp = (ApplyExp) func).getFunction() == makeChildAxisStep)
              {
                aexp.setFunction(makeDescendantAxisStep);
                exp = beforeSlashSlash;
              }

            lexp.body = exp2;
            beforeSlashSlash = null;
	  }
	comp.pop(lexp);

	/*
	if (lexp.body instanceof ApplyExp)
	  {
	    // Optimize the case of a simple name step.
	    ApplyExp aexp = (ApplyExp) lexp.body;
	    Expression func = aexp.getFunction();
	    Expression[] args = aexp.getArgs();
	    if (false
		&& func == funcNamedChildren && args.length==2
		&& args[0] instanceof ReferenceExp
		&& ((ReferenceExp) args[0]).getBinding() == decl)
	      {
		args[0] = exp;
		if (descendants)
		  func = funcNamedDescendants;
		exp = new ApplyExp (func, args);
		handled = true;
	      }
	    else if (func == funcForwardFilter && args.length==2
		     && args[0] instanceof ApplyExp
		     && descendants)
	      {
		ApplyExp xapp = (ApplyExp) args[0];
		Expression[] xargs = xapp.getArgs();
		if (xapp.getFunction() == funcNamedChildren
		    && xargs.length == 2
		    && ((ReferenceExp) xargs[0]).getBinding() == decl)
		  {
		    xapp.setFunction(funcNamedDescendants);
		  }
	      }
	  }
	*/

	Expression[] args = new Expression[] { exp, lexp };
	exp = new ApplyExp(RelativeStep.relativeStep, args);
      }
    return exp;
  }

  Expression parseStepExpr()
      throws java.io.IOException, SyntaxException
  {
    int axis;
    if (curToken == '.' || curToken == DOTDOT_TOKEN)
      {
	axis = curToken == '.' ? AXIS_SELF : AXIS_PARENT;
	getRawToken();
	Declaration dotDecl = comp.lexical.lookup(DOT_VARNAME, false);
        Expression exp;
	if (dotDecl == null)
	  exp = syntaxError("context item is undefined", "XPDY0002");
	else
          exp = new ReferenceExp(DOT_VARNAME, dotDecl);
	if (axis == AXIS_PARENT)
	  {
	    Expression[] args = { exp };
	    exp = new ApplyExp(ParentAxis.make(NodeType.anyNodeTest), args);
	  }
        // Note that '..' is an AbbrevReverseStep,
        // but '.' is a FilterExpr - and hence not a valid ForwardStep.
	return parseStepQualifiers(exp, axis == AXIS_SELF ? -1 : axis);
      }
    axis = peekOperand() - OP_AXIS_FIRST;
    Expression unqualifiedStep;
    if (axis >= 0 && axis < COUNT_OP_AXIS)
      {
	getRawToken();
	unqualifiedStep = parseNodeTest(axis);
      }
    else if (curToken == '@')
      {
	getRawToken();
	axis = AXIS_ATTRIBUTE;
	unqualifiedStep = parseNodeTest(axis);
      }
    else if (curToken == OP_ATTRIBUTE)
      {
	axis = AXIS_ATTRIBUTE;
	unqualifiedStep = parseNodeTest(axis);
      }
    else
      {
	unqualifiedStep = parseNodeTest(-1);
        if (unqualifiedStep != null)
          {
            axis = AXIS_CHILD;
          }
        else
          {
            axis = -1;
            unqualifiedStep = parsePrimaryExpr();
          }
      }
    return parseStepQualifiers(unqualifiedStep, axis);
  }

  Expression parseStepQualifiers(Expression exp, int axis)
    throws java.io.IOException, SyntaxException
  {
    for (;;)
      {
	if (curToken == '[')
	  {
	    int startLine = getLineNumber() + 1;
	    int startColumn = getColumnNumber() + 1;
	    int saveSeenPosition = seenPosition;
	    int saveSawLast = seenLast;
	    getRawToken();
	    LambdaExp lexp = new LambdaExp(3);
            maybeSetLine(lexp, startLine, startColumn);
	    Declaration dot = lexp.addDeclaration(DOT_VARNAME);
            if (axis >= 0)
              dot.setType(NodeType.anyNodeTest);
            else
              dot.setType(SingletonType.getInstance());
	    lexp.addDeclaration(POSITION_VARNAME, Type.intType);
	    lexp.addDeclaration(LAST_VARNAME, Type.intType);
	    comp.push(lexp);
	    dot.noteValueUnknown();
	    Expression cond = parseExprSequence(']', false);
            if (curToken == EOF_TOKEN)
              eofError("missing ']' - unexpected end-of-file");
	    char kind;
	    Procedure valuesFilter;
	    if (axis < 0)
	      {
		kind = 'P';
		valuesFilter = ValuesFilter.exprFilter;
	      }
	    else if (axis == AXIS_ANCESTOR || axis == AXIS_ANCESTOR_OR_SELF
		     || axis == AXIS_PARENT || axis == AXIS_PRECEDING
		     || axis == AXIS_PRECEDING_SIBLING)
	      {
		kind = 'R';
		valuesFilter = ValuesFilter.reverseFilter;
	      }
	    else
	      {
		kind = 'F';
		valuesFilter = ValuesFilter.forwardFilter;
	      }
	    /*)
	    boolean sawPosition = seenPosition > saveSeenPosition;
	    boolean sawLast = seenLast > saveSeenLast;
	    */
	    maybeSetLine(cond, startLine, startColumn);
	    comp.pop(lexp);
	    lexp.body = cond;
	    getRawToken();
	    Expression[] args = { exp, lexp };
	    exp = new ApplyExp(valuesFilter, args);
	  }
	/*
	else if (curToken == ARROW_TOKEN)
	  ...;
	*/
	else
	  {
	    return exp;
	  }
      }
  }

  /**
   * Parse a PrimaryExpr.
   * @return an Expression.
   */
  Expression parsePrimaryExpr()
      throws java.io.IOException, SyntaxException
  {
    Expression exp = parseMaybePrimaryExpr();
    if (exp == null)
      {
	exp = syntaxError("missing expression");
	if (curToken != EOF_TOKEN)
	  getRawToken();
	return exp;
      }
    return exp;
  }

  void parseEntityOrCharRef ()
      throws java.io.IOException, SyntaxException
  {
    int next = read();
    if (next == '#')
      {
	int base;
	next = read();
	if (next == 'x')
	  {
	    base = 16;
	    next = read();
	  }
	else
	  base = 10;
	int value = 0;
	while (next >= 0)
	  {
	    char ch = (char) next;
	    int digit = Character.digit((char) ch, base);
	    if (digit < 0)
	      break;
	    if (value >= 0x8000000)
	      break; // Overflow likely.
	    value = value * base;
	    value += digit;
	    next = read();
	  }
	if (next != ';')
	  {
	    unread();
	    error("invalid character reference");
	  }
        // See definition of 'Char' in XML 1.1 2nd ed Specification.
	else if ((value > 0 && value <= 0xD7FF)
                 || (value >= 0xE000 && value <= 0xFFFD)
                 || (value >= 0x10000 && value <= 0x10FFFF))
          tokenBufferAppend(value);
        else
          error('e', "invalid character value "+value, "XQST0090");
      }
    else
      {
	int saveLength = tokenBufferLength;
	while (next >= 0)
	  {
	    char ch = (char) next;
	    if (! XName.isNamePart(ch))
	      break;
	    tokenBufferAppend(ch);
	    next = read();
	  }
	if (next != ';')
	  {
	    unread();
	    error("invalid entity reference");
	    return;
	  }
	String ref = new String(tokenBuffer, saveLength,
				tokenBufferLength - saveLength);
	tokenBufferLength = saveLength;
	appendNamedEntity(ref);
      }
  }

  /** Count of enclosed expressions seen in element or attribute content. */
  int enclosedExpressionsSeen;

  public static Expression makeText =
    makeFunctionExp("gnu.kawa.xml.MakeText", "makeText");

  /** Parse ElementContent (delimiter == '<')  or AttributeContent (otherwise).
   * @param delimiter is '<' if parsing ElementContent, is either '\'' or
   *   '\"' if parsing AttributeContent depending on the starting quote
   * @param result a buffer to place the resulting Expressions.
   */
  void parseContent(char delimiter, Vector result)
      throws java.io.IOException, SyntaxException
  {
    tokenBufferLength = 0;
    int startSize = result.size();
    int prevEnclosed = startSize - 1;
    boolean skipBoundarySpace = ! boundarySpacePreserve && delimiter == '<';
    boolean skippable = skipBoundarySpace;
    for (;;)
      {
	int next = read();
        if (next == delimiter)
          {
            if (delimiter == '<')
              {
                next = read();
                Expression text = null;
                if (tokenBufferLength > 0)
                  {
                    // Literal text is represent by an FString, to distinguish
                    // from a string atomic value.
                    FString str = new FString(tokenBuffer, 0, tokenBufferLength);
                    text = new QuoteExp(str);
                  }
                tokenBufferLength = 0;
                if (next == '/')
                  {
                    if (text != null && ! skippable)
                      result.addElement(text);
                    break;
                  }
                Expression content = parseXMLConstructor(next, true);
                boolean isCDATA = false;
                boolean emptyCDATA = false;
                if (content instanceof ApplyExp)
                  {
                    ApplyExp aexp = (ApplyExp) content;
                    if (aexp.getFunction() == makeCDATA)
                      {
                        isCDATA = true;
                        String str = (String) aexp.getArg(0).valueIfConstant();
                        emptyCDATA = str != null && str.length() == 0;
                      }
                  }
                if (text != null && (! skippable || isCDATA))
                  result.addElement(text);
                if (isCDATA)
                  skippable = false;
                else
                  skippable = skipBoundarySpace;
                if (! emptyCDATA)
                  result.addElement(content);
                tokenBufferLength = 0;
                continue;
              }
            else if (checkNext(delimiter))
              {
                tokenBufferAppend(delimiter);
                continue;
              }
          }
	if (next == delimiter || next < 0 || next == '{')
	  {
            int postBrace = next == '{' ? read() : -1;
            if (postBrace == '{')
              {
                tokenBufferAppend('{');
                skippable = false;
                continue;
              }
          addText:
            {
              FString text;
              if (tokenBufferLength > 0 && ! skippable)
                text = new FString(tokenBuffer, 0, tokenBufferLength);
              else if (next == '{' && prevEnclosed == result.size())
                // Handle the <a>{E1}{E2}</a> case - we must insert a
                // joiner between E1 ad E2 to avoid a space being inserted.
                text = new FString();
              else
                break addText; // Don't need to add anything.
              result.addElement(new QuoteExp(text));
            }
	    tokenBufferLength = 0;
            if (next == delimiter)
              break;
            else if (next < 0)
              eofError("unexpected end-of-file");
            else // if (next == '{')
              {
                unread(postBrace);
                enclosedExpressionsSeen++;
                Expression exp = parseEnclosedExpr();
                result.addElement(exp);
                tokenBufferLength = 0;
                prevEnclosed = result.size();
                skippable = skipBoundarySpace;
              }
	  }
	else if (next == '}')
	  {
	    next = read();
	    if (next == '}')
	      {
		tokenBufferAppend('}');
		skippable = false;
	      }
	    else
	      {
		error("unexpected '}' in element content");
		unread(next);
	      }
	  }
	else if (next == '&')
	  {
	    parseEntityOrCharRef();
	    skippable = false;
	  }
	else
	  {
            if (delimiter != '<'
                && (next == '\t' || next == '\n' || next == '\r'))
              next = ' ';
            if (next == '<')
              error('e', "'<' must be quoted in a direct attribute value");
	    if (skippable)
	      skippable = Character.isWhitespace((char) next);
	    tokenBufferAppend((char) next);
	  }
      }
  }

  /** Parse an EnclosedExpr.
   * Assume the '{' has been read.
   */
  Expression parseEnclosedExpr()
      throws java.io.IOException, SyntaxException
  {
    String saveErrorIfComment = errorIfComment;
    errorIfComment = null;
    char saveReadState = pushNesting('{');
    peekNonSpace("unexpected end-of-file after '{'");
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() + 1;
    getRawToken();
    Expression exp = parseExpr();
    for (;;)
      {
	if (curToken == '}')
	  break;
	if (curToken == EOF_TOKEN || curToken == ')' || curToken == ']')
	  {
	    exp = syntaxError("missing '}'");
	    break;
	  }
	if (curToken != ',')
	  exp = syntaxError("missing '}' or ','");
	else
	  getRawToken();

	exp = makeExprSequence(exp, parseExpr());

      }
    maybeSetLine(exp, startLine, startColumn);
    popNesting(saveReadState);
    errorIfComment = saveErrorIfComment;
    return exp;
  }

  /** Coerce the value of an expresison to a boolean value. */
  public static Expression booleanValue(Expression exp)
  {
    Expression[] args = { exp };
    Expression string
      = makeFunctionExp("gnu.xquery.util.BooleanValue", "booleanValue");
    return new ApplyExp(string, args);
  }

  static final Expression makeCDATA =
    makeFunctionExp("gnu.kawa.xml.MakeCDATA", "makeCDATA");

  /** Parse an ElementConstructor or other constructs starting with '<'.
   * Assume initial '<' has been processed.
   * @param next next character (after '<').
   */
  Expression parseXMLConstructor (int next, boolean inElementContent)
      throws java.io.IOException, SyntaxException
  {
    Expression exp;
    if (next == '!')
      {
	next = read();
	if (next == '-' && peek() == '-')
	  {
	    skip();
	    getDelimited("-->");
            boolean bad = false;
            int i = tokenBufferLength;
            boolean sawHyphen = true;
            while (--i >= 0)
              {
                boolean curHyphen = tokenBuffer[i] == '-';
                if (sawHyphen && curHyphen)
                  {
                    bad = true;
                    break;
                  }
                sawHyphen = curHyphen;
              }
            if (bad)
              exp = syntaxError("consecutive or final hyphen in XML comment");
            else
              {
                Expression[] args =
                  { new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength)) };
                exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.CommentConstructor",
                                                   "commentConstructor"),
                                   args);
              }
	  }
	else if (next == '[' && read() == 'C' && read() == 'D'
		 && read() == 'A' && read() == 'T' && read() == 'A'
		 && read() == '[')
	  {
            if (! inElementContent)
              error('e', "CDATA section must be in element content");
	    getDelimited("]]>");
	    Expression[] args =
	      { new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength)) };
	    exp = new ApplyExp(makeCDATA, args);
	  }
	else
	  exp = syntaxError("'<!' must be followed by '--' or '[CDATA['");
      }
    else if (next == '?')
      {
	next = peek();
	if (next < 0 || ! XName.isNameStart((char) next)
	    || getRawToken() != NCNAME_TOKEN)
	  syntaxError("missing target after '<?'");
	String target = new String(tokenBuffer, 0, tokenBufferLength);
        int nspaces = 0;
        for (;;)
          {
            int ch = read();
            if (ch < 0)
              break;
            if (! Character.isWhitespace((char) ch))
              {
                unread();
                break;
              }
            nspaces++;
          }
	getDelimited("?>");
        if (nspaces == 0 && tokenBufferLength > 0)
          syntaxError("target must be followed by space or '?>'");
	String content = new String(tokenBuffer, 0, tokenBufferLength);
	Expression[] args = { new QuoteExp(target), new QuoteExp(content) };
	exp = new ApplyExp(makeFunctionExp("gnu.kawa.xml.MakeProcInst",
					   "makeProcInst"),
			   args);
      }
    else if (next < 0 || ! XName.isNameStart((char) next))
      exp = syntaxError("expected QName after '<'");
    else
      {
	unread(next);
	getRawToken();
	char saveReadState = pushNesting('<');
	exp = parseElementConstructor();
        if (! inElementContent)
          exp = wrapWithBaseUri(exp);
	popNesting(saveReadState);
      }
    return exp;
  }

  /** Generate code to cast argument to a QName
   * (which is implemented using <code>Symbol</code>). */
  static ApplyExp castQName (Expression value, boolean element)
  {
    Declaration fdecl = (element ? XQResolveNames.xsQNameDecl
                         : XQResolveNames.xsQNameIgnoreDefaultDecl);
    return new ApplyExp(new ReferenceExp(fdecl),
			new Expression[] { value });
  }

  /** Parse ElementConstructor.
   * Assume initial {@code '<'} has been processed,
   * and we're looking at the next token..
   * Reads through end of the end tag.  FIXME
   */
  Expression parseElementConstructor()
      throws java.io.IOException, SyntaxException
  {
    // Note that we cannot do namespace resolution at parse time,
    // because of constructs like this:  <a x="{$x:v}" xmlns:x="xx"/>
    // Instead we defer namespaced lookup until XQResolveNames.  (Mostly -
    // some places still incorrectly do premature namespace resolution.)
    String startTag = new String(tokenBuffer, 0, tokenBufferLength);
    Vector<Expression> vec = new Vector<Expression>();
    Expression[] args;
    vec.add(castQName(new QuoteExp(startTag), true));
    errorIfComment = "comment not allowed in element start tag";
    NamespaceBinding nsBindings = null;
    int ch;
    for (;;)
      {
        boolean sawSpace = false;
        ch = read();
        while (ch >= 0 && Character.isWhitespace((char) ch))
          {
            ch = read();
            sawSpace = true;
          }
	if (ch < 0 || ch == '>' || ch == '/')
	  break;
        if (! sawSpace)
          syntaxError("missing space before attribute");
	unread(ch);
	getRawToken();
	int vecSize = vec.size();
	if (curToken != NCNAME_TOKEN && curToken != QNAME_TOKEN)
	  break;
	String attrName = new String(tokenBuffer, 0, tokenBufferLength);
	int startLine = getLineNumber() + 1;
	int startColumn = getColumnNumber() + 1 - tokenBufferLength;
	String definingNamespace = null;
	if (curToken == NCNAME_TOKEN)
	  {
	    if (attrName.equals("xmlns"))
	      definingNamespace = "";
	  }
	else
	  {
	    if (attrName.startsWith("xmlns:"))
	      definingNamespace = attrName.substring(6).intern();
	  }
	Expression makeAttr
	  = definingNamespace != null ? null
	  : MakeAttribute.makeAttributeExp;
	vec.addElement(castQName(new QuoteExp(attrName), false));
	ch = skipSpace();
	if (ch != '=')
          {
            errorIfComment = null;
            return syntaxError("missing '=' after attribute");
          }
	ch = skipSpace();
        int enclosedExpressionsStart = enclosedExpressionsSeen;
	if (ch == '{')
	  {
            warnOldVersion("enclosed attribute value expression should be quoted");
	    vec.addElement(parseEnclosedExpr());
	  }
	else
	  parseContent((char) ch, vec);
	int n = vec.size() - vecSize;
	if (definingNamespace != null)
	  {
	    String ns = "";
	    if (n == 1)
	      ns = "";
	    else if (enclosedExpressionsSeen > enclosedExpressionsStart)
	      syntaxError("enclosed expression not allowed in namespace declaration");
	    else
              {
                ns = vec.get(vecSize+1)
                  .valueIfConstant().toString().intern();
              }
	    vec.setSize(vecSize);
            checkAllowedNamespaceDeclaration(definingNamespace, ns, true);
	    if (definingNamespace == "")
	      definingNamespace = null;
            for (NamespaceBinding nsb = nsBindings;
                 nsb != null;  nsb = nsb.getNext())
              {
                if (nsb.getPrefix() == definingNamespace)
                  {
                    error('e',
                          definingNamespace == null
                          ? "duplicate default namespace declaration"
                          : "duplicate namespace prefix '"+definingNamespace+'\'',
                          "XQST0071");
                    break;
                  }
              }
	    nsBindings
	      = new NamespaceBinding(definingNamespace,
				     ns == "" ? null : ns,
				     nsBindings);
	  }
	else
	  {
	    args = new Expression[n];
	    for (int i = n;  --i >= 0; )
	      args[i] = (Expression) vec.elementAt(vecSize + i);
	    vec.setSize(vecSize);
	    ApplyExp aexp = new ApplyExp(makeAttr, args);
	    maybeSetLine(aexp, startLine, startColumn);
	    vec.addElement(aexp);
	  }
      }
    errorIfComment = null;
    boolean empty = false;
    if (ch == '/')
      {
	ch = read();
	if (ch == '>')
	  empty = true;
	else
	  unread(ch);
      }
    if (! empty)
      {
	if (ch != '>')
	  return syntaxError("missing '>' after start element");
	parseContent('<', vec);
	ch = peek();
        if (ch >= 0)
	  {
            if (! XName.isNameStart((char) ch))
	      return syntaxError("invalid tag syntax after '</'");
	    getRawToken();
	    String tag = new String(tokenBuffer, 0, tokenBufferLength);
	    if (! (tag.equals(startTag)))
	      return syntaxError("'<"+startTag+">' closed by '</"+tag+">'");
            errorIfComment = "comment not allowed in element end tag";
	    ch = skipSpace();
            errorIfComment = null;
	  }
	if (ch != '>')
	  return syntaxError("missing '>' after end element");
      }
    args = new Expression[vec.size()];
    vec.copyInto(args);
    MakeElement mkElement = new MakeElement();
    mkElement.copyNamespacesMode = copyNamespacesMode;
    // Ths is just the chain of NamespaceBindings for namespace declaration
    // attributes from this immediate constructor.  At resolve time we chain
    // this list onto the list from outer element constructors.
    mkElement.setNamespaceNodes(nsBindings);
    Expression result = new ApplyExp(new QuoteExp(mkElement), args);
    return result;
  }

  Expression wrapWithBaseUri (Expression exp)
  {
    if (getStaticBaseUri() == null)
      return exp;
    return new ApplyExp(MakeWithBaseUri.makeWithBaseUri,
                        new Expression[] { 
                          new ApplyExp(new ReferenceExp(XQResolveNames.staticBaseUriDecl), Expression.noExpressions),
                          exp })
      .setLine(exp);
  }

  /** Parse ParenthesizedExpr.
   *.When called, curToken should be pointing at a '(',
   * or a token which ends if a '(', such as IF_LPAREN_TOKEN.
   */
  Expression parseParenExpr ()
      throws java.io.IOException, SyntaxException
  {
    getRawToken();
    char saveReadState = pushNesting('(');
    Expression exp = parseExprSequence(')', true);
    popNesting(saveReadState);
    if (curToken == EOF_TOKEN)
      eofError("missing ')' - unexpected end-of-file");
    return exp;
  }

  Expression parseExprSequence(int rightToken, boolean optional)
      throws java.io.IOException, SyntaxException
  {
    if (curToken == rightToken || curToken == EOF_TOKEN)
      {
        if (! optional)
          syntaxError("missing expression");
        return QuoteExp.voidExp;
      }
    Expression exp = null;
    for (;;)
      {
	Expression exp1 = parseExprSingle();

	exp = exp == null ? exp1 : makeExprSequence(exp, exp1);
	if (curToken == rightToken || curToken == EOF_TOKEN)
	  break;
	if (nesting == 0 && curToken == EOL_TOKEN)
	  return exp;
	if (curToken != ',')
	  return syntaxError (rightToken == ')' ? "expected ')'"
			       : "confused by syntax error");
	getRawToken();
      }
    return exp;
  }

  Expression parseTypeSwitch()
    throws java.io.IOException, SyntaxException
  {
    char save = pushNesting('t');
    Expression selector = parseParenExpr();
    getRawToken();
    Object varName = null;
    Declaration decl;
    Vector vec = new Vector();
    vec.addElement(selector);
    while (match("case"))
      {
	pushNesting('c');
	getRawToken();
	if (curToken == '$')
	  {
	    decl = parseVariableDeclaration();
	    if (decl == null)
	      return syntaxError("missing Variable after '$'");
	    getRawToken();
	    if (match("as"))
	      getRawToken();
	    else
	      error('e', "missing 'as'");
	  }
	else
	  decl = new Declaration("(arg)");
	decl.setTypeExp(parseDataType());
	popNesting('t');
	LambdaExp lexp = new LambdaExp(1);
	lexp.addDeclaration(decl);
	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'case'");
	comp.push(lexp);
	pushNesting('r');
	Expression caseExpr = parseExpr();
	lexp.body = caseExpr;
	popNesting('t');
	comp.pop(lexp);
	vec.addElement(lexp);
      }

    if (match("default"))
      {
        LambdaExp lexp = new LambdaExp(1);
	getRawToken();

        if (curToken == '$')
          {
            decl = parseVariableDeclaration();
            if (decl == null)
              return syntaxError("missing Variable after '$'");
            getRawToken();
          }
        else
          decl = new Declaration("(arg)");
        lexp.addDeclaration(decl);

	if (match("return"))
	  getRawToken();
	else
	  error("missing 'return' after 'default'");
	comp.push(lexp);
	Expression defaultExpr = parseExpr();
	lexp.body = defaultExpr;
	comp.pop(lexp);
        vec.addElement(lexp);
      }
    else
      {
        error(comp.isPedantic() ? 'e' : 'w',
               "no 'default' clause in 'typeswitch'",
               "XPST0003");
      }
    popNesting(save);
    Expression[] args = new Expression[vec.size()];
    vec.copyInto(args);
    return new ApplyExp(makeFunctionExp("gnu.kawa.reflect.TypeSwitch",
					"typeSwitch"),
			args);
  }

  /**
   * Try to parse a PrimaryExpr.
   * @return an Expression, or null if no PrimaryExpr was seen.
   */
  Expression parseMaybePrimaryExpr()
      throws java.io.IOException, SyntaxException
  {
    int startLine = curLine;
    int startColumn = curColumn;
    int token = peekOperand();
    Expression exp;
    int c1, c2, c3;
    Vector vec;
    Expression[] args;
    switch (token)
      {
      case '(':
        exp = parseParenExpr();
        break;

      case PRAGMA_START_TOKEN:
        Stack extArgs = new Stack();
        for (;;)
          {
            getRawToken();
            Expression qname;
            if (curToken != QNAME_TOKEN && curToken != NCNAME_TOKEN)
              qname = syntaxError("missing pragma name");
            else
              qname = QuoteExp.getInstance(new String(tokenBuffer, 0, tokenBufferLength));
            extArgs.push(qname);
            StringBuffer sbuf = new StringBuffer();
            int ch;
            int spaces = -1;
            do { ch = read(); spaces++; }
            while (ch >= 0 && Character.isWhitespace((char) ch));
            while (ch != '#' || peek() != ')')
              {
                if (ch < 0)
                  eofError("pragma ended by end-of-file");
                if (spaces == 0)
                  error("missing space between pragma and extension content");
                spaces = 1;
                sbuf.append((char) ch);
                ch = read();
              }
            read(); // skip ')'
            extArgs.push(QuoteExp.getInstance(sbuf.toString()));
            getRawToken();
            if (curToken != PRAGMA_START_TOKEN)
              break;
          }
        if (curToken == '{')
          {
            getRawToken();
            if (curToken != '}')
              {
                char saveReadState = pushNesting('{');
                extArgs.push(parseExprSequence('}', false));
                popNesting(saveReadState);
                if (curToken == EOF_TOKEN)
                  eofError("missing '}' - unexpected end-of-file");
              }
            args = new Expression[extArgs.size()];
            extArgs.copyInto(args);
            exp = new ApplyExp(new ReferenceExp(XQResolveNames.handleExtensionDecl), args);
          }
        else
          exp = syntaxError("missing '{' after pragma");
        break;

      case '{':
	exp = syntaxError("saw unexpected '{' - assume you meant '('");
	parseEnclosedExpr();
        break;

      case OP_LSS:
	int next = read();
	if (next == '/')
	  {
	    getRawToken();
	    String msg;
	    if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN
		|| curToken == NCNAME_COLON_TOKEN)
	      msg = "saw end tag '</" + new String(tokenBuffer, 0, tokenBufferLength) + ">' not in an element constructor";
	    else
	      msg = "saw end tag '</' not in an element constructor";
	    curLine = startLine;
	    curColumn = startColumn;
	    exp = syntaxError(msg);
	    while (curToken != OP_GRT && curToken != EOF_TOKEN && curToken != EOL_TOKEN)
	      getRawToken();
	    return exp;
	  }
	else 
	  {
	    exp = parseXMLConstructor(next, false);
            maybeSetLine(exp, startLine, startColumn);
	  }
        break;

      case STRING_TOKEN:
	exp = new QuoteExp(new String(tokenBuffer, 0, tokenBufferLength).intern());
        break;

      case INTEGER_TOKEN:
	exp = new QuoteExp(IntNum.valueOf(tokenBuffer, 0, tokenBufferLength,
                                          10, false));
        break;

      case DECIMAL_TOKEN:
      case DOUBLE_TOKEN:
        String str = new String(tokenBuffer, 0, tokenBufferLength);
        try
          {
            Object val;
            if (token == DECIMAL_TOKEN)
              val = new java.math.BigDecimal(str);
            else
              val = new java.lang.Double(str);
            exp = new QuoteExp(val);
          }
        catch (Exception ex)
          {
            exp = syntaxError("invalid decimal literal: '"+str+"'");
          }
        break;
      case '$':
	Object name = parseVariable();
	if (name == null)
	  return syntaxError("missing Variable");
	exp = new ReferenceExp(name);
        maybeSetLine(exp, curLine, curColumn);
        break;
      case FNAME_TOKEN:
	name = new String(tokenBuffer, 0, tokenBufferLength);
	char save = pushNesting('(');
	getRawToken();
        vec = new Vector(10);
	if (curToken != ')')
	  {
	    for (;;)
	      {
		Expression arg = parseExpr();
		vec.addElement(arg);
		if (curToken == ')')
		  break;
		if (curToken != ',')
		  return syntaxError("missing ')' after function call");
		getRawToken();
	      }
	  }
	args = new Expression[vec.size()];

	vec.copyInto(args);
	ReferenceExp rexp = new ReferenceExp(name, null);
	rexp.setProcedureName(true);
	exp = new ApplyExp(rexp, args);
        maybeSetLine(exp, startLine, startColumn);
	popNesting(save);
        break;

      case ELEMENT_TOKEN:
      case ATTRIBUTE_TOKEN:
      case COMMENT_TOKEN:
      case DOCUMENT_TOKEN:
      case TEXT_TOKEN:
      case PI_TOKEN:
        getRawToken();  // Skip 'element'.
        vec = new Vector();
        Expression func;

        if (token == ELEMENT_TOKEN || token == ATTRIBUTE_TOKEN)
          {
            Expression element;
            if (curToken == NCNAME_TOKEN || curToken == QNAME_TOKEN)
              element = parseNameTest(token != ELEMENT_TOKEN);
            else if (curToken == '{')
              element = parseEnclosedExpr();
            else
              return syntaxError("missing element/attribute name");
            vec.addElement(castQName(element, token == ELEMENT_TOKEN));
            if (token == ELEMENT_TOKEN)
              {
                MakeElement mk = new MakeElement();
                mk.copyNamespacesMode = copyNamespacesMode;
                func = new QuoteExp(mk);
              }
            else
              func = MakeAttribute.makeAttributeExp;
            getRawToken();
          }
        else if (token == DOCUMENT_TOKEN)
          func = makeFunctionExp("gnu.kawa.xml.DocumentConstructor",
                                 "documentConstructor");
        else if (token == COMMENT_TOKEN)
          func = makeFunctionExp("gnu.kawa.xml.CommentConstructor",
                                 "commentConstructor");
        else if (token == PI_TOKEN)
          {
            Expression target;
            if (curToken == NCNAME_TOKEN)
              target = new QuoteExp(new String(tokenBuffer, 0,
                                               tokenBufferLength).intern());
            else if (curToken == '{')
              {
                target = parseEnclosedExpr();
              }
            else
              {
                target = syntaxError("expected NCName or '{' after 'processing-instruction'");
                if (curToken != QNAME_TOKEN)
                  return target;
              }
            vec.addElement(target);
            func = makeFunctionExp("gnu.kawa.xml.MakeProcInst",
                                 "makeProcInst");
            getRawToken();
          }
        else /* token == TEXT_TOKEN */
          func = makeFunctionExp("gnu.kawa.xml.MakeText",
                                 "makeText");
        char saveReadState = pushNesting('{');
        peekNonSpace("unexpected end-of-file after '{'");
        if (curToken != '{')
          return syntaxError("missing '{'");
        getRawToken();
        if (token == TEXT_TOKEN || token == COMMENT_TOKEN
            || token == PI_TOKEN)
          vec.addElement(parseExprSequence('}', token == PI_TOKEN));
        else if (curToken != '}')
          {
            vec.addElement(parseExpr());
            while (curToken == ',')
              {
                getRawToken();
                vec.addElement(parseExpr());
              }
          }
        popNesting(saveReadState);
        if (curToken != '}')
          return syntaxError("missing '}'");
        args = new Expression[vec.size()];
        vec.copyInto(args);
        exp = new ApplyExp(func, args);
        maybeSetLine(exp, startLine, startColumn);
        if (token == DOCUMENT_TOKEN || token == ELEMENT_TOKEN)
          exp = wrapWithBaseUri(exp);
        break;

      case ORDERED_LBRACE_TOKEN:
      case UNORDERED_LBRACE_TOKEN:
        getRawToken();
        exp = parseExprSequence('}', false);
        break;

      default:
        return null;
      }
    /*
    if (nesting == 0)
      {
	int ch = skipSpace(false);
	if (ch < 0 || ch == '\n' || ch == '\r')
	  return exp;
	unread(ch);
      }
    */
    getRawToken();
    return exp;
  }

  public Expression parseIfExpr()
      throws java.io.IOException, SyntaxException
  {
    char saveReadState1 = pushNesting('i');
    getRawToken();
    char saveReadState2 = pushNesting('(');
    Expression cond = parseExprSequence(')', false);
    popNesting(saveReadState2);
    if (curToken == EOF_TOKEN)
      eofError("missing ')' - unexpected end-of-file");
    getRawToken();
    if (! match("then"))
      syntaxError("missing 'then'");
    else
      getRawToken();
    Expression thenPart = parseExpr();
    if (! match("else"))
      syntaxError("missing 'else'");
    else
      getRawToken();
    popNesting(saveReadState1);
    Expression elsePart = parseExpr();
    return new IfExp(booleanValue(cond), thenPart, elsePart);
  }

  public boolean match(String word)
  {
    if (curToken != NCNAME_TOKEN)
      return false;
    int len = word.length();
    if (tokenBufferLength != len)
      return false;
    for (int i = len;  --i >= 0; )
      {
	char cs = word.charAt(i);
	char cb = tokenBuffer[i];
	if (cs != cb)
	  return false;
      }
    return true;
  }

  /** Parse a Variable. */
  public Object parseVariable ()
    throws java.io.IOException, SyntaxException
  {
    if (curToken == '$')
      getRawToken();
    else
      syntaxError("missing '$' before variable name");
    String str = new String(tokenBuffer, 0, tokenBufferLength);
    // Note we cannot do namespace resolution here - see comment in
    // parseElementConstructor.
    if (curToken == QNAME_TOKEN)
      return str;
    else if (curToken == NCNAME_TOKEN)
      return Namespace.EmptyNamespace.getSymbol(str.intern());
    else
      return null;
  }

  public Declaration parseVariableDeclaration ()
      throws java.io.IOException, SyntaxException
  {
    Object name = parseVariable();
    if (name == null)
      return null;
    Declaration decl = new Declaration(name);
    maybeSetLine(decl, getLineNumber() + 1,
		 getColumnNumber() + 1 - tokenBufferLength);
    return decl;
  }

  public Expression parseFLWRExpression (boolean isFor)
      throws java.io.IOException, SyntaxException
  {
    int flworDeclsSave = flworDeclsFirst;
    flworDeclsFirst = flworDeclsCount;
    Expression exp = parseFLWRInner(isFor);

    if (match ("order"))
      {
        char saveNesting = pushNesting(isFor ? 'f' : 'l');
        getRawToken();
        if (match ("by"))
          getRawToken();
        else
          error("missing 'by' following 'order'");
        Stack specs = new Stack();
        for (;;)
          {
            boolean descending = false;
            char emptyOrder = defaultEmptyOrder;

            LambdaExp lexp = new LambdaExp(flworDeclsCount-flworDeclsFirst);
            for (int i = flworDeclsFirst;  i < flworDeclsCount;  i++)
              lexp.addDeclaration(flworDecls[i].getSymbol());
            comp.push(lexp);
            lexp.body = parseExprSingle();
            comp.pop(lexp);
            specs.push(lexp);

            if (match("ascending"))
              getRawToken();
            else if (match("descending"))
              {
                getRawToken();
                descending = true;
              }
            if (match("empty"))
              {
                getRawToken();
                if (match("greatest"))
                  {
                    getRawToken();
                    emptyOrder = 'G';
                  }
                else if (match("least"))
                  {
                    getRawToken();
                    emptyOrder = 'L';
                  }
                else
                  error
                    ("'empty' sequence order must be 'greatest' or 'least'");
              }
            specs.push(new QuoteExp((descending ? "D" : "A") + emptyOrder));
            Object collation = defaultCollator;
            if (match("collation"))
              {
                Object uri = parseURILiteral();
                if (uri instanceof String)
                  {
                    try
                      {
                        String uriString = resolveAgainstBaseUri((String) uri);
                        collation = NamedCollator.make(uriString);
                      }
                    catch (Exception name)
                      {
                        error('e', "unknown collation '"+uri+"'", "XQST0076");
                      }
                  }
                getRawToken();
              }
            specs.push(new QuoteExp(collation));
            if (curToken != ',')
              break;
            getRawToken();
          }
        if (! match("return"))
          return syntaxError("expected 'return' clause");
        getRawToken();

        LambdaExp lexp = new LambdaExp(flworDeclsCount-flworDeclsFirst);
        //maybeSetLine(lexp, declLine, declColumn);
        for (int i = flworDeclsFirst;  i < flworDeclsCount;  i++)
          lexp.addDeclaration(flworDecls[i].getSymbol());
        popNesting(saveNesting);
        comp.push(lexp);
	lexp.body = parseExprSingle();
        comp.pop(lexp);
        int nspecs = specs.size();
        Expression[] args = new Expression[2 + nspecs];
        args[0] = exp;
        args[1] = lexp;
        for (int i = 0;  i < nspecs;  i++)
          args[2+i] = (Expression) specs.elementAt(i);
	return new ApplyExp(makeFunctionExp("gnu.xquery.util.OrderedMap",
					    "orderedMap"),
			    args);

      }
    flworDeclsCount = flworDeclsFirst;
    flworDeclsFirst = flworDeclsSave;
    return exp;
  }

  /** Parse a let- or a for-expression.
   * Assume the 'let'/'for'-token has been seen, and we've read '$'.
   *
   * If we see the 'order' keyword of an 'order by' clause then we stop
   * parsing, and return a result as if we instead saw a
   * 'return make-tuple($x, ...)'.  The 'order by' clause will get
   * parsed by the outer-most 'for' or 'let'.
   */
  public Expression parseFLWRInner (boolean isFor)
      throws java.io.IOException, SyntaxException
  {
    char saveNesting = pushNesting(isFor ? 'f' : 'l');
    curToken = '$';
    Declaration decl = parseVariableDeclaration();
    if (decl == null)
      return syntaxError("missing Variable - saw "+tokenString());
    if (flworDecls == null)
      flworDecls = new Declaration[8];
    else if (flworDeclsCount >= flworDecls.length)
      {
        Declaration[] tmp = new Declaration[2 * flworDeclsCount];
        System.arraycopy(flworDecls, 0, tmp, 0, flworDeclsCount);
        flworDecls = tmp;
      }
    flworDecls[flworDeclsCount++] = decl;
    getRawToken();

    Expression type = parseOptionalTypeDeclaration();
    LambdaExp lexp = null;
    Declaration posDecl = null;
    if (isFor)
      {
	boolean sawAt = match("at");
	lexp = new LambdaExp(sawAt ? 2 : 1);
	if (sawAt)
	  {
	    getRawToken();
	    if (curToken == '$')
	      {
		posDecl = parseVariableDeclaration();
		getRawToken();
	      }
	    if (posDecl == null)
	      syntaxError("missing Variable after 'at'");
	  }
	if (match("in"))
	  getRawToken();
	else
	  {	
	    if (curToken == COLON_EQUAL_TOKEN)
	      getRawToken();
	    syntaxError("missing 'in' in 'for' clause");
	  }
        comp.push(lexp);
      }
    else
      {
	if (curToken == COLON_EQUAL_TOKEN)
	  getRawToken();
	else
	  {	
	    if (match("in"))
	      getRawToken();
	    syntaxError("missing ':=' in 'let' clause");
	  }
        comp.letStart();
      }
    Expression init = parseExprSingle();
    if (isFor)
      {
        lexp.addDeclaration(decl);
	decl.noteValueUnknown();
	decl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    else
      {
        if (type != null) // FIXME - for now
          init = Compilation.makeCoercion(init, type);
        comp.letVariable(decl, init);
        comp.letEnter();
      }
    if (type != null)
      decl.setTypeExp(type);
    popNesting(saveNesting);
    if (posDecl != null)
      {
	lexp.addDeclaration(posDecl);
	posDecl.setType(LangPrimType.intType);
	posDecl.noteValueUnknown();
	posDecl.setFlag(Declaration.IS_SINGLE_VALUE);
      }
    Expression body;
    if (curToken == ',')
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after ','");
	body = parseFLWRInner(isFor);
      }
    else if (match("for"))
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after 'for'");
	body = parseFLWRInner(true);
      }
    else if (match("let"))
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after 'let'");
	body = parseFLWRInner(false);
      }
    else
      {
	Expression cond;
	char save = pushNesting('w');
	if (curToken == OP_WHERE)
	  {
	    getRawToken();
	    cond = parseExprSingle();
	  }
	else if (match("where"))
	  {
	    cond = parseExprSingle();
	  }
	else
	  cond = null;
	popNesting(save);
	boolean sawStable = match("stable");
	if (sawStable)
	  getRawToken();
	boolean sawReturn = match("return");
	boolean sawOrder = match("order");
	if (! sawReturn && ! sawOrder && ! match("let") && ! match("for"))
	  return syntaxError("missing 'return' clause");
        if (! sawOrder)
          peekNonSpace("unexpected eof-of-file after 'return'");
	int bodyLine = getLineNumber() + 1;
	int bodyColumn = getColumnNumber() + 1;
	if (sawReturn)
	  getRawToken();
        if (sawOrder)
          {
            int ndecls = flworDeclsCount - flworDeclsFirst;
            Expression[] args = new Expression[ndecls];
            for (int i = 0;  i < ndecls;  i++)
              args[i] = new ReferenceExp(flworDecls[flworDeclsFirst+i]);
            body = new ApplyExp(new PrimProcedure("gnu.xquery.util.OrderedMap",
                                                  "makeTuple$V", 1),
                                args);
          }
        else
          body = parseExprSingle();
	if (cond != null)
          body = new IfExp(booleanValue(cond), body, QuoteExp.voidExp);
	maybeSetLine(body, bodyLine, bodyColumn);
      }
    if (isFor)
      {
        comp.pop(lexp);
	lexp.body = body;
	Expression[] args = { lexp, init};  // SIC
	return new ApplyExp(makeFunctionExp("gnu.kawa.functions.ValuesMap",
					    lexp.min_args == 1 ? "valuesMap"
					    : "valuesMapWithPos"),
			    args);
      }
    else
      return comp.letDone(body);
  }

  /** Parse a some- or an every-expression.
   * Assume the 'some'/'every'-token has been seen, and we've read '$'. */
  public Expression parseQuantifiedExpr (boolean isEvery)
      throws java.io.IOException, SyntaxException
  {
    char saveNesting = pushNesting(isEvery ? 'e' : 's');
    curToken = '$';
    Declaration decl = parseVariableDeclaration();
    if (decl == null)
      return syntaxError("missing Variable token:"+curToken);
    getRawToken();
    
    LambdaExp lexp = new LambdaExp(1);
    lexp.addDeclaration(decl);
    decl.noteValueUnknown();
    decl.setFlag(Declaration.IS_SINGLE_VALUE);
    decl.setTypeExp(parseOptionalTypeDeclaration());

    if (match("in"))
      getRawToken();
    else
      {	
	if (curToken == COLON_EQUAL_TOKEN)
	  getRawToken();
	syntaxError("missing 'in' in QuantifiedExpr");
      }
    Expression[] inits = { parseExprSingle() };
    popNesting(saveNesting);
    comp.push(lexp);
    Expression body;
    if (curToken == ',')
      {
	getRawToken();
	if (curToken != '$')
	  return syntaxError("missing $NAME after ','");
	body = parseQuantifiedExpr(isEvery);
      }
    else
      {
	boolean sawSatisfies = match("satisfies");
	if (! sawSatisfies && ! match("every") && ! match("some"))
	  return syntaxError("missing 'satisfies' clause");
	peekNonSpace("unexpected eof-of-file after 'satisfies'");
	int bodyLine = getLineNumber() + 1;
	int bodyColumn = getColumnNumber() + 1;
	if (sawSatisfies)
	  getRawToken();
	body = parseExprSingle();
	maybeSetLine(body, bodyLine, bodyColumn);
      }
    comp.pop(lexp);
    lexp.body = body;
    Expression[] args = { lexp, inits[0]};  // SIC
    return new ApplyExp(makeFunctionExp("gnu.xquery.util.Xutils",
					isEvery ? "every" : "some"),
			args);
  }

  public Expression parseFunctionDefinition(int declLine, int declColumn)
      throws java.io.IOException, SyntaxException
  {
    if (curToken != QNAME_TOKEN && curToken != NCNAME_TOKEN)
      return syntaxError("missing function name");
    String name = new String(tokenBuffer, 0, tokenBufferLength);
    getMessages().setLine(port.getName(), curLine, curColumn);
    Symbol sym = namespaceResolve(name, true);
    String uri = sym.getNamespaceURI();
    if (uri == NamespaceBinding.XML_NAMESPACE
        || uri == XQuery.SCHEMA_NAMESPACE
        || uri == XQuery.SCHEMA_INSTANCE_NAMESPACE
        || uri == XQuery.XQUERY_FUNCTION_NAMESPACE)
      {
        error('e', 
              "cannot declare function in standard namespace '"+uri+'\'',
              "XQST0045");
      }
    else if (uri == "")
      {
        error(comp.isPedantic() ? 'e' : 'w',
              "cannot declare function in empty namespace",
              "XQST0060");
      }
    else if (libraryModuleNamespace != null && uri != libraryModuleNamespace
             && (! XQuery.LOCAL_NAMESPACE.equals(uri) || comp.isPedantic()))
      {
        error('e', "function not in namespace of library module", "XQST0048");
      }
    getRawToken();
    if (curToken != '(')
      return syntaxError("missing parameter list:"+curToken);
    getRawToken();
    LambdaExp lexp = new LambdaExp();
    maybeSetLine(lexp, declLine, declColumn);
    lexp.setName(name);
    Declaration decl = comp.currentScope().addDeclaration(sym);
    if (comp.isStatic())
      decl.setFlag(Declaration.STATIC_SPECIFIED);
    lexp.setFlag(LambdaExp.OVERLOADABLE_FIELD);
    decl.setCanRead(true);
    decl.setProcedureDecl(true);
    maybeSetLine(decl, declLine, declColumn);
    comp.push(lexp);
    if (curToken != ')')
      {
      paramLoop:
	for (;;)
	  {
	    Declaration param = parseVariableDeclaration();
	    if (param == null)
	      error("missing parameter name");
	    else
	      {
		lexp.addDeclaration(param);
		getRawToken();
		lexp.min_args++;
		lexp.max_args++;
		param.setTypeExp(parseOptionalTypeDeclaration());
	      }
	    if (curToken == ')')
	      break;
	    if (curToken != ',')
              {
                Expression err = syntaxError("missing ',' in parameter list");
                for (;;)
                  {
                    getRawToken();
                    if (curToken < 0 || curToken == ';' || curToken == ';')
                      return err;
                    if (curToken == ')')
                      break paramLoop;
                    if (curToken == ',')
                      break;
                  }
              }
            else
              getRawToken();
	  }
      }
    getRawToken();
    Expression retType = parseOptionalTypeDeclaration ();
    lexp.body = parseEnclosedExpr();
    comp.pop(lexp);
    if (retType != null)
      lexp.setCoercedReturnValue(retType, interpreter);
    SetExp sexp = new SetExp(decl, lexp);
    sexp.setDefining (true);
    decl.noteValue(lexp);
    return sexp;
  }

  public Object readObject ()
      throws java.io.IOException, SyntaxException
  {
    return parse(null);
  }

  Compilation comp;

  String defaultElementNamespace = "";

  /** Chain of namespace bindings from namespace declaration attributes
   * in outer direct element constructors.  This is only non-empty during
   * resolve time, but it is declared here so namespaceResolve can use it. */
  NamespaceBinding constructorNamespaces = NamespaceBinding.predefinedXML;

  /** Chain of namespace bindings from declarations in prolog,
   *  followed by the builtinNamespaces. */
  NamespaceBinding prologNamespaces;

  static NamespaceBinding builtinNamespaces;
  static {
    NamespaceBinding ns = NamespaceBinding.predefinedXML;
    ns = new NamespaceBinding("xml", NamespaceBinding.XML_NAMESPACE, ns);
    ns = new NamespaceBinding("xs", XQuery.SCHEMA_NAMESPACE, ns);
    ns = new NamespaceBinding("xsi", XQuery.SCHEMA_INSTANCE_NAMESPACE, ns);
    ns = new NamespaceBinding("fn", XQuery.XQUERY_FUNCTION_NAMESPACE, ns);
    ns = new NamespaceBinding("html", XQuery.XHTML_NAMESPACE, ns);
    ns = new NamespaceBinding("kawa", XQuery.KAWA_FUNCTION_NAMESPACE, ns);
    ns = new NamespaceBinding("qexo", XQuery.QEXO_FUNCTION_NAMESPACE, ns);
    ns = new NamespaceBinding("local", XQuery.LOCAL_NAMESPACE, ns);
    builtinNamespaces = ns;
  }

  protected Symbol namespaceResolve (String name, boolean function)
  {
    int colon = name.indexOf(':');
    String prefix = colon >= 0 ? name.substring(0, colon).intern()
      : function ? XQuery.DEFAULT_FUNCTION_PREFIX
      : XQuery.DEFAULT_ELEMENT_PREFIX;
    String uri = QNameUtils.lookupPrefix(prefix, constructorNamespaces,
                                         prologNamespaces);

    if (uri == null)
      {
	if (colon < 0)
	  uri = "";
	else if (! comp.isPedantic())
	  {
	    try
	      {
		Class cl = Class.forName(prefix);
		uri = "class:" + prefix;
	      }
	    catch (Exception ex)
	      {
                uri = null;
              }
	  }
        if (uri == null)
          {
            getMessages().error('e',
                  "unknown namespace prefix '" + prefix + "'",
                  "XPST0081");
            uri = "(unknown namespace)";
          }
      }
    String local = colon < 0 ? name : name.substring(colon+1);
    return Symbol.make((String) uri, local, prefix);
  }

  void parseSeparator ()
    throws java.io.IOException, SyntaxException
  {
    int startLine = port.getLineNumber() + 1;
    int startColumn = port.getColumnNumber() + 1;
    int next = skipSpace(nesting != 0);
    if (next == ';')
      return;
    if (warnOldVersion && next != '\n')
      {
	curLine = startLine;
	curColumn = startColumn;
	error('w', "missing ';' after declaration");
      }
    if (next >= 0)
      unread(next);
  }

  public static final QuoteExp getExternalFunction =
    QuoteExp.getInstance(new PrimProcedure("gnu.xquery.lang.XQuery",
                                           "getExternal", 2));

  /** Parse an expression.
   * Return null on EOF. */
  public Expression parse(Compilation comp)
      throws java.io.IOException, SyntaxException
  {
    this.comp = comp;
    int ch = skipSpace();
    if (ch < 0)
      return null;
    parseCount++;
    unread(ch);
    int startLine = getLineNumber() + 1;
    int startColumn = getColumnNumber() + 1;

    // Handle Unix #!PROGRAM convention. */
    if (ch == '#' && startLine == 1 && startColumn == 1)
      {
	read();
	if ((ch = read()) != '!' || (ch = read()) != '/')
	  error("'#' is only allowed in initial '#!/PROGRAM'");
	while (ch != '\r' && ch != '\n' && ch >= 0)
	  ch = read();
      }

    if (getRawToken() == EOF_TOKEN)
      return null;
    peekOperand();

    if (curToken == NCNAME_TOKEN
	&& "namespace".equals((String) curValue))
      {
	if (warnOldVersion)
	  error('w', "use 'declare namespace' instead of 'namespace'");
	curToken = DECLARE_NAMESPACE_TOKEN;
      }

    int declLine, declColumn, next;
    Declaration decl;
    String prefix, uri;
    Object val;
    Expression exp;
    switch (curToken)
      {
      case DEFINE_QNAME_TOKEN:
	declLine = getLineNumber() + 1;
	declColumn = getColumnNumber() + 1;
	next = peekNonSpace("unexpected end-of-file after 'define QName'");
	if (next == '(')
	  {
	    syntaxError("'missing 'function' after 'define'");
	    curToken = NCNAME_TOKEN;
	    return parseFunctionDefinition(declLine, declColumn);
	  }
	else
	  return syntaxError("missing keyword after 'define'");

      case DECLARE_FUNCTION_TOKEN:
	declLine = getLineNumber() + 1;
	declColumn = getColumnNumber() + 1;
	getRawToken();
	peekNonSpace("unexpected end-of-file after 'define function'");
	char save = pushNesting('d');
	exp = parseFunctionDefinition(declLine, declColumn);
	popNesting(save);
	parseSeparator();
        maybeSetLine(exp, startLine, startColumn);
        seenDeclaration = true;
	return exp;

      case DECLARE_VARIABLE_TOKEN:
	getRawToken();
	decl = parseVariableDeclaration();
	if (decl == null)
	  return syntaxError("missing Variable");
        Object name = decl.getSymbol();
        if (name instanceof String)
          {
            getMessages().setLine(port.getName(), curLine, curColumn);
            decl.setSymbol(namespaceResolve((String) name, false));
          }
        if (libraryModuleNamespace != null)
          {
            uri = ((Symbol) decl.getSymbol()).getNamespaceURI();
            if (uri != libraryModuleNamespace
                && (! XQuery.LOCAL_NAMESPACE.equals(uri) || comp.isPedantic()))
              error('e', "variable not in namespace of library module"
                    , "XQST0048");
          }
	comp.currentScope().addDeclaration(decl);
	getRawToken();
	Expression type = parseOptionalTypeDeclaration();
	decl.setCanRead(true);
	//decl.setFlag(Declaration.NONSTATIC_SPECIFIED);
	decl.setFlag(Declaration.IS_CONSTANT);
	Expression init = null;
	boolean sawEq = false;
	if (curToken == OP_EQU || curToken == COLON_EQUAL_TOKEN)
	  {
	    if (curToken==OP_EQU)
	      error("declare variable contains '=' instead of ':='");
	    getRawToken();
	    sawEq = true;
	  }
	if (curToken == '{')
	  {
	    warnOldVersion("obsolete '{' in variable declaration");
	    init = parseEnclosedExpr();
	    parseSeparator();
	  }
	else if (match("external"))
	  {
            Expression[] args =
              {
                castQName(new QuoteExp(decl.getSymbol()), false),
                type==null ? QuoteExp.nullExp : type
              };
            init = new ApplyExp(getExternalFunction, args);
            maybeSetLine(init, curLine, curColumn);
            getRawToken();
	  }
	else
	  {
	    init = parseExpr();
	    Expression err = null;
	    if (! sawEq || init == null)
	      err = syntaxError("expected ':= init' or 'external'");
	    if (init == null)
	      init = err;
	  }
        if (type != null)
          init = Compilation.makeCoercion(init, type);
        decl.noteValue(init);
	exp = SetExp.makeDefinition(decl, init);
	maybeSetLine(exp, startLine, startColumn);
        seenDeclaration = true;
	return exp;

      case DECLARE_NAMESPACE_TOKEN:
      case MODULE_NAMESPACE_TOKEN:
	int command = curToken;
        if (command == MODULE_NAMESPACE_TOKEN
            && libraryModuleNamespace != null )
          error('e', "duplicate module declaration");
        else if (seenDeclaration && ! interactive)
          error('e', "namespace declared after function/variable/option");
	next = skipSpace(nesting != 0);
	if (next >= 0)
	  {
	    unread();
	    if (XName.isNameStart((char) next))
	      {
		getRawToken();
		if (curToken != NCNAME_TOKEN)
		  return syntaxError("missing namespace prefix");
		prefix = new String(tokenBuffer, 0, tokenBufferLength);
		getRawToken();
		if (curToken != OP_EQU)
		  return syntaxError("missing '=' in namespace declaration");
		getRawToken();
		if (curToken != STRING_TOKEN)
		  return syntaxError("missing uri in namespace declaration");
		uri = new String(tokenBuffer, 0, tokenBufferLength).intern();
                prefix = prefix.intern();
                for (NamespaceBinding ns = prologNamespaces;
                     ns != builtinNamespaces;
                     ns = ns.getNext())
                  {
                    if (ns.getPrefix() == prefix)
                      {
                        error('e',
                              "duplicate declarations for the same namespace prefix '"
                              +prefix+"'",
                              "XQST0033");
                        break;
                      }
                  }
		pushNamespace(prefix, uri);
                checkAllowedNamespaceDeclaration(prefix, uri, false);
		parseSeparator();
		if (command == MODULE_NAMESPACE_TOKEN)
                  {
                    ModuleExp module = comp.getModule();
                    String className = Compilation.mangleURI(uri)
                      + '.' + XQuery.makeClassName(module.getFileName());
                    module.setName(className);
                    comp.mainClass = new ClassType(className);
                    module.setType(comp.mainClass);
                    ModuleManager manager = ModuleManager.getInstance();
                    ModuleInfo info = manager.find(comp);
                    info.setNamespaceUri(uri);
                    module.setType(comp.mainClass);
                    if (uri.length() == 0)
                      return syntaxError("zero-length module namespace", "XQST0088");
                    libraryModuleNamespace = uri;
                  }
                return QuoteExp.voidExp;
	      }
	  }

      case IMPORT_SCHEMA_TOKEN:
        fatal("'import schema' not implemented", "XQST0009");

      case IMPORT_MODULE_TOKEN:
	getRawToken();
	prefix = null;
	if (match("namespace"))
	  {
	    getRawToken();
	    if (curToken != NCNAME_TOKEN)
	      return syntaxError("missing namespace prefix");
	    prefix = new String(tokenBuffer, 0, tokenBufferLength);
	    getRawToken();
	    if (curToken != OP_EQU)
	      return syntaxError("missing '=' in namespace declaration");
	    getRawToken();
	  }
	if (curToken != STRING_TOKEN)
	  return syntaxError("missing uri in namespace declaration");
        if (tokenBufferLength == 0)
          return syntaxError("zero-length target namespace", "XQST0088");
	uri = new String(tokenBuffer, 0, tokenBufferLength).intern();
	if (prefix != null)
          {
            checkAllowedNamespaceDeclaration(prefix, uri, false);
            pushNamespace(prefix.intern(), uri);
          }
	getRawToken();
        // Make sure we have a ModuleInfo before we call importDefinitions.
        ModuleManager.getInstance().find(comp);

        if (seenImports == null)
            seenImports = new HashMap<String, SourceLocator>();
        SourceLocator oldImport = seenImports.get(uri);
        Declaration loc = new Declaration(uri);
        maybeSetLine(loc, startLine, startColumn);
        if (oldImport != null) {
            comp.error('e', "duplicate import of '"+uri+"'",
                       "XQST0047", loc);
            comp.error('e', "(this is the previous import)", null, oldImport);
            return QuoteExp.voidExp;
        }
        seenImports.put(uri, loc);

        String at;
 	ModuleExp module = comp.getModule();
	FormStack formStack = new FormStack(comp);
        String packageName = Compilation.mangleURI(uri);
        comp.setLine(port.getName(), startLine, startColumn);
	if (match("at"))
	  {
            for (;;)
              {
                getRawToken();
                if (curToken != STRING_TOKEN)
                  return syntaxError("missing module location");
                at = new String(tokenBuffer, 0, tokenBufferLength);
                String className = Compilation.mangleURI(uri)
                  + '.' + XQuery.makeClassName(at);

                ModuleInfo info = require.lookupModuleFromSourcePath(at, module);
                if (info == null)
                  comp.error('e', "malformed URL: "+at);
                require.importDefinitions(className, info,
                                          null, formStack, module, comp);
                next = skipSpace(nesting != 0);
                if (next != ',')
                  {
                    unread(next);
                    break;
                  }
              }
	    parseSeparator();
	  }
	else
          {
            ModuleManager manager = ModuleManager.getInstance();
            int n = 0;
            try
              {
                manager.loadPackageInfo(packageName);
              }
            catch (ClassNotFoundException ex)
              {
                // Do nothing.  If there is no such module,
                // that will be reported below.
              }
            catch (Exception ex)
              {
                error('e', "error loading map for "+uri+" - "+ex);
              }
            for (int i = 0; ; i++)
              {
                ModuleInfo info = manager.getModule(i);
                if (info == null)
                  break;
                if (! uri.equals(info.getNamespaceUri()))
                  continue;
                n++;
                require.importDefinitions(info.getClassName(), info, null, formStack, module, comp);
              }
            if (n == 0)
              error('e', "no module found for "+uri);
            at = null;
            if (curToken != ';')
              parseSeparator();
          }
        if (comp.pendingImports != null && comp.pendingImports.size() > 0)
          {
            error('e', "module import forms a cycle", "XQST0073");
          }
        LList forms = (LList) formStack.getFirst();
	Expression[] inits = new Expression[forms.size()];
	forms.toArray(inits);
	return BeginExp.canonicalize(inits);

      case DEFAULT_COLLATION_TOKEN:
	if (defaultCollator != null && ! interactive)
          error('e', "duplicate default collation declaration", "XQST0038");
        val = parseURILiteral();
        if (val instanceof Expression) // an ErrorExp
          return (Expression) val;
	String collation = (String) val;
	try
	  {
            collation = resolveAgainstBaseUri(collation);
	    defaultCollator = NamedCollator.make(collation);
	  }
	catch (Exception ex)
	  {
	    defaultCollator = NamedCollator.codepointCollation;
	    error('e', "unknown collation '"+collation+"'", "XQST0038");
	  }
	parseSeparator();
	return QuoteExp.voidExp;

      case DEFAULT_ELEMENT_TOKEN:
      case DEFAULT_FUNCTION_TOKEN:
	boolean forFunctions = curToken == DEFAULT_FUNCTION_TOKEN;
        prefix = forFunctions ? XQuery.DEFAULT_FUNCTION_PREFIX
          : XQuery.DEFAULT_ELEMENT_PREFIX;
        if (prologNamespaces.resolve(prefix, builtinNamespaces) != null)
          error('e',
                "duplicate default namespace declaration",
                "XQST0066");
        else if (seenDeclaration && ! interactive)
          error('e', "default namespace declared after function/variable/option");
	getRawToken();
	if (match("namespace"))
	  getRawToken();
	else
	  {
	    String msg = "expected 'namespace' keyword";
	    if (curToken != STRING_TOKEN && curToken != OP_EQU)
	      return declError(msg);
	    else
	      warnOldVersion(msg);
	  }
	if (curToken == OP_EQU || curToken == COLON_EQUAL_TOKEN)
	  {
	    warnOldVersion("extra '=' in default namespace declaration");
	    getRawToken();
	  }
	if (curToken != STRING_TOKEN)
	  return declError("missing namespace uri");
	uri = new String(tokenBuffer, 0, tokenBufferLength).intern();
	if (forFunctions)
	  {
	    functionNamespacePath = new Namespace[1];
	    functionNamespacePath[0] = Namespace.valueOf(uri);
	  }
	else
	  {
	    defaultElementNamespace = uri;
	  }
	pushNamespace(prefix, uri);
        checkAllowedNamespaceDeclaration(prefix, uri, false);
	parseSeparator();
	return QuoteExp.voidExp;

      case DECLARE_BOUNDARY_SPACE_TOKEN:
	getRawToken();
	if (curToken == OP_EQU)
	  {
	    warnOldVersion("obsolate '=' in boundary-space declaration");
	    getRawToken();
	  }
        if (boundarySpaceDeclarationSeen && ! interactive)
          syntaxError("duplicate 'declare boundary-space' seen", "XQST0068");
        boundarySpaceDeclarationSeen = true;
	if (match("preserve"))
	  boundarySpacePreserve = true;
	else if (match("strip"))
	  boundarySpacePreserve = false;
	else if (match("skip"))
          {
	    warnOldVersion("update: declare boundary-space skip -> strip");
            boundarySpacePreserve = false;
          }
	else
	  return syntaxError("boundary-space declaration must be preserve or strip");
	parseSeparator();
	return QuoteExp.voidExp;

      case DECLARE_CONSTRUCTION_TOKEN:
	getRawToken();
        if (constructionModeDeclarationSeen && ! interactive)
          syntaxError("duplicate 'declare construction' seen", "XQST0067");
        constructionModeDeclarationSeen = true;
	if (match("strip"))
          constructionModeStrip = true;
	else if (match("preserve"))
          constructionModeStrip = false;
	else
	  return syntaxError("construction declaration must be strip or preserve");
	parseSeparator();
	return QuoteExp.voidExp;

      case DECLARE_COPY_NAMESPACES_TOKEN:
	getRawToken();
        if (copyNamespacesDeclarationSeen && ! interactive)
          syntaxError("duplicate 'declare copy-namespaces' seen", "XQST0055");
        copyNamespacesDeclarationSeen = true;
	if (match("preserve"))
          copyNamespacesMode |= XMLFilter.COPY_NAMESPACES_PRESERVE;
	else if (match("no-preserve"))
          copyNamespacesMode &= ~XMLFilter.COPY_NAMESPACES_PRESERVE;
	else
	  return syntaxError("expected 'preserve' or 'no-preserve' after 'declare copy-namespaces'");
        getRawToken();
        if (curToken != ',')
          return syntaxError("missing ',' in copy-namespaces declaration");
        getRawToken();
	if (match("inherit"))
          copyNamespacesMode |= XMLFilter.COPY_NAMESPACES_INHERIT;
	else if (match("no-inherit"))
          copyNamespacesMode &= ~XMLFilter.COPY_NAMESPACES_INHERIT;
	else
	  return syntaxError("expected 'inherit' or 'no-inherit' in copy-namespaces declaration");
	parseSeparator();
	return QuoteExp.voidExp;

      case DEFAULT_ORDER_TOKEN:
        getRawToken();
        boolean sawEmpty = match("empty");
        if (emptyOrderDeclarationSeen && ! interactive)
          syntaxError("duplicate 'declare default empty order' seen", "XQST0069");
        emptyOrderDeclarationSeen = true;
        if (sawEmpty)
          getRawToken();
        else
          syntaxError("expected 'empty greatest' or 'empty least'");
        if (match("greatest"))
          defaultEmptyOrder = 'G';
        else if (match("least"))
          defaultEmptyOrder = 'L';
        else
          return syntaxError("expected 'empty greatest' or 'empty least'");
        parseSeparator();
	return QuoteExp.voidExp;

      case DECLARE_OPTION_TOKEN:
	getRawToken();
        if (curToken != QNAME_TOKEN)
          syntaxError("expected QName after 'declare option'");
        else
          {
            String str = new String(tokenBuffer, 0, tokenBufferLength);
            getMessages().setLine(port.getName(), curLine, curColumn);
            Symbol sym = namespaceResolve(str, false);
            getRawToken();
            if (curToken != STRING_TOKEN)
              syntaxError("expected string literal after 'declare option "
                          +str+'\'');
            else
              handleOption(sym, new String(tokenBuffer, 0, tokenBufferLength));
          }
        parseSeparator();
        seenDeclaration = true;
	return QuoteExp.voidExp;

      case DECLARE_ORDERING_TOKEN:
        if (orderingModeSeen && ! interactive)
          syntaxError("duplicate 'declare ordering' seen", "XQST0065");
        orderingModeSeen = true;
	getRawToken();
	if (match("ordered"))
          orderingModeUnordered = false;
	else if (match("unordered"))
          orderingModeUnordered = true;
	else
	  return syntaxError("ordering declaration must be ordered or unordered");
	parseSeparator();
	return QuoteExp.voidExp;

      case XQUERY_VERSION_TOKEN:
        if (parseCount != 1)
          error('e', "'xquery version' does not start module");
        else if (commentCount > 0)
          error('w', "comments should not precede 'xquery version'");
        getRawToken();
        if (curToken == STRING_TOKEN)
          {
            String version = new String(tokenBuffer, 0, tokenBufferLength);
            if (! version.equals("1.0"))
              error('e', "unrecognized xquery version "+version, "XQST0031");
            getRawToken();
          }
        else
          return syntaxError("missing version string after 'xquery version'");
        if (match("encoding"))
          {
            getRawToken();
            if (curToken != STRING_TOKEN)
              return syntaxError("invalid encoding specification");
            else
              {
                String encoding = new String(tokenBuffer, 0, tokenBufferLength);
                int i = tokenBufferLength;
                boolean bad = i == 0;
                while (--i >= 0 && ! bad)
                  {
                    ch = tokenBuffer[i];
                    if ((ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))
                      continue;
                    if (i == 0
                        || ! ((ch >= '0' && ch <= '9')
                              || ch == '.' || ch == '_' || ch == '-'))
                      bad = true;
                  }
                if (bad)
                  error('e', "invalid encoding name syntax", "XQST0087");
                InPort port = getPort();
                if (port instanceof BinaryInPort) {
                    try {
                        ((BinaryInPort) port).setCharset(encoding);
                    } catch (java.nio.charset.UnsupportedCharsetException ex) {
                        error('e', "unrecognized encoding name");
                    } catch (Exception ex) {
                        error('e', "cannot set encoding name here");
                    }
                }
                // ignore encoding specification.
                getRawToken();
              }
          }
        if (curToken != ';')
          syntaxError("missing ';'");
        return QuoteExp.voidExp;

      case DECLARE_BASE_URI_TOKEN:
        if (baseURIDeclarationSeen && ! interactive)
          syntaxError("duplicate 'declare base-uri' seen", "XQST0032");
        baseURIDeclarationSeen = true;
        val = parseURILiteral();
        if (val instanceof Expression) // an ErrorExp
          return (Expression) val;
	parseSeparator();
        setStaticBaseUri((String) val);
	return QuoteExp.voidExp;
      }
    exp = parseExprSequence(EOF_TOKEN, true);
    if (curToken == EOL_TOKEN)
      unread('\n');
    maybeSetLine(exp, startLine, startColumn);
    if (libraryModuleNamespace != null)
      error('e', "query body in a library module", "XPST0003");
    return exp;
  }

  public void handleOption (Symbol name, String value)
  {
    // Nothing, for now.
  }

  public final static String[] axisNames = new String[COUNT_OP_AXIS];
  static
  {
    axisNames[AXIS_ANCESTOR] = "ancestor";
    axisNames[AXIS_ANCESTOR_OR_SELF] = "ancestor-or-self";
    axisNames[AXIS_ATTRIBUTE] = "attribute";
    axisNames[AXIS_CHILD] = "child";
    axisNames[AXIS_DESCENDANT] = "descendant";
    axisNames[AXIS_DESCENDANT_OR_SELF] = "descendant-or-self";
    axisNames[AXIS_FOLLOWING] = "following";
    axisNames[AXIS_FOLLOWING_SIBLING] = "following-sibling";
    axisNames[AXIS_NAMESPACE] = "namespace";
    axisNames[AXIS_PARENT] = "parent";
    axisNames[AXIS_PRECEDING] = "preceding";
    axisNames[AXIS_PRECEDING_SIBLING] = "preceding-sibling";
    axisNames[AXIS_SELF] = "self";
  }
    
  public static Expression makeFunctionExp(String className, String name)
  {
    return makeFunctionExp(className,
			   Compilation.mangleNameIfNeeded(name),
			   name);
  }

  public static Expression makeFunctionExp(String className,
					   String fieldName, String name)
  {
    return new ReferenceExp(name,
                            Declaration.getDeclarationValueFromStatic
                            (className, fieldName, name));
  }

  /** Helper method for debugging. */
  String tokenString()
  {
    switch (curToken)
      {
      case STRING_TOKEN:
	StringBuffer sbuf = new StringBuffer();
	sbuf.append('"');
	for (int i = 0;  i < tokenBufferLength;  i++)
	  {
	    char ch = tokenBuffer[i];
	    if (ch == '"')
	      sbuf.append('"');
	    sbuf.append(ch);
	  }
	sbuf.append('"');
	return sbuf.toString();
      case FNAME_TOKEN:
	return new String(tokenBuffer, 0, tokenBufferLength) + " + '('";
      case NCNAME_TOKEN:
      case QNAME_TOKEN:
	return new String(tokenBuffer, 0, tokenBufferLength);
      case EOF_TOKEN:
	return "<EOF>";
      default:
        if (curToken >= OP_AXIS_FIRST
            && curToken - OP_AXIS_FIRST < COUNT_OP_AXIS)
          return axisNames[curToken - OP_AXIS_FIRST]+"::-axis("+curToken+")";
        if (curToken >= ' ' && curToken < 127)
          return Integer.toString(curToken)+"='"+((char) curToken)+"'";
	return Integer.toString(curToken);
      }
  }

  public void error(char severity, String message, String code)
  {
    SourceMessages messages = getMessages();
    SourceError err
      = new SourceError(severity, port.getName(), curLine, curColumn, message);
    err.code = code;
    messages.error(err);
  }

  public void error(char severity, String message)
  {
    error(severity, message, null);
  }

  public Expression declError (String message)
    throws java.io.IOException, SyntaxException
  {
    if (interactive)
      return syntaxError(message);
    error(message);
    for (;;)
      {
	if (curToken==';' || curToken == EOF_TOKEN)
	  break;
	getRawToken();
      }
    return new ErrorExp (message);
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public Expression syntaxError (String message, String code)
    throws java.io.IOException, SyntaxException
  {
    error('e', message, code);
    if (interactive)
      {
	curToken = 0;
	curValue = null;
	nesting = 0;
	((InPort) getPort()).readState = '\n';
	for (;;)
	  {
	    int ch = read();
	    if (ch < 0)
	      break;
	    if (ch == '\r' || ch == '\n')
	      {
		unread(ch);
		break;
	      }
	  }
	throw new SyntaxException(getMessages());
      }
    return new ErrorExp (message);
  }

  public Expression syntaxError (String message)
    throws java.io.IOException, SyntaxException
  {
    return syntaxError(message, "XPST0003");
  }

  public void eofError(String msg) throws SyntaxException
  {
    fatal(msg, "XPST0003");
  }

  public void fatal(String msg, String code) throws SyntaxException
  {
    SourceMessages messages = getMessages();
    SourceError err
      = new SourceError('f', port.getName(), curLine, curColumn, msg);
    err.code = code;
    messages.error(err);
    throw new SyntaxException(messages);
  }

  void warnOldVersion (String message)
  {
    if (warnOldVersion || comp.isPedantic())
      error(comp.isPedantic() ? 'e' : 'w', message);
  }

  public void maybeSetLine (Expression exp, int line, int column)
  {
    String file = getName();
    if (file != null && exp.getFileName() == null
        && ! (exp instanceof QuoteExp))
      {
        exp.setFile(file);
	exp.setLine(line, column);
      }
  }

  public void maybeSetLine (Declaration decl, int line, int column)
  {
    String file = getName();
    if (file != null)
      {
        decl.setFile(file);
	decl.setLine(line, column);
      }
  }
}
