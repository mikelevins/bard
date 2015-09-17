package gnu.ecmascript;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.mapping.*;
import gnu.lists.Sequence;
import gnu.text.Char;

/**
  * Reads EcmaScript token from a InPort.
  */

public class Lexer extends gnu.text.Lexer
{
  private boolean prevWasCR = false;

  public Lexer (InPort port)
  {
    super(port);
  }

  public final static Char lparenToken = Char.make('(');
  public final static Char rparenToken = Char.make(')');
  public final static Char lbraceToken = Char.make('{');
  public final static Char rbraceToken = Char.make('}');
  public final static Char lbracketToken = Char.make('[');
  public final static Char rbracketToken = Char.make(']');
  public final static Char dotToken = Char.make('.');
  public final static Char condToken = Char.make('?');
  public final static Char commaToken = Char.make(',');
  public final static Char colonToken = Char.make(':');
  public final static Char equalToken = Char.make('=');
  public final static Char tildeToken = Char.make('~');
  public final static Char notToken = Char.make('!');
  public final static Char semicolonToken = Char.make(';');
  public final static Object eolToken = Char.make('\n');
  public final static Object eofToken = Sequence.eofValue;
  public final static Reserved elseToken
  = new Reserved("else", Reserved.ELSE_TOKEN);
  public final static Reserved newToken
  = new Reserved("new", Reserved.NEW_TOKEN);

  static java.util.Hashtable reserved;
  static synchronized void initReserved()
  {
    if (reserved == null)
      {
	reserved = new java.util.Hashtable (20);
	reserved.put("null", new QuoteExp(null));
	reserved.put("true", new QuoteExp(java.lang.Boolean.TRUE));
	reserved.put("false", new QuoteExp(java.lang.Boolean.FALSE));

	reserved.put("var", new Reserved("var", Reserved.VAR_TOKEN));
	reserved.put("if", new Reserved("if", Reserved.IF_TOKEN));
	reserved.put("while", new Reserved("while", Reserved.WHILE_TOKEN));
	reserved.put("for", new Reserved("for", Reserved.FOR_TOKEN));
	reserved.put("continue",
		     new Reserved("continue", Reserved.CONTINUE_TOKEN));
	reserved.put("break", new Reserved("break", Reserved.BREAK_TOKEN));
	reserved.put("return", new Reserved("return", Reserved.RETURN_TOKEN));
	reserved.put("with", new Reserved("with", Reserved.WITH_TOKEN));
	reserved.put("function",
		     new Reserved("function", Reserved.FUNCTION_TOKEN));
	reserved.put("this", new Reserved("this", Reserved.THIS_TOKEN));
	reserved.put("else", elseToken);
	reserved.put("new", newToken);
      }
  }
  public static Object checkReserved(String name)
  {
    if (reserved == null)
      initReserved();
    return reserved.get(name);
  }

  public Double getNumericLiteral (int c)
    throws java.io.IOException
  {
    int radix = 10;
    if (c == '0')
      {
	c = read();
	if (c == 'x' || c == 'X')
	  {
	    radix = 16;
	    c = read();
	  }
	else if (c == '.' || c == 'e' || c == 'E') ;
	else
	  radix = 8;
      }
    int i = port.pos;
    if (c >= 0)
      i--;   // Reset to position before current char c.
    port.pos = i;
    long ival = Lexer.readDigitsInBuffer(port, radix);
    boolean digit_seen = port.pos > i;
    if (digit_seen && port.pos < port.limit)
      {
	c = port.buffer[port.pos];
	if (! Character.isLetterOrDigit((char) c) && c != '.')
	  {
	    double dval;
	    if (ival >= 0)
	      dval = (double) ival;
	    else // FIXME - do we want to use gnu.math??
	      dval = gnu.math.IntNum.valueOf(port.buffer, i, port.pos - i,
					     radix, false).doubleValue();
	    return new Double(dval);
	  }
      }
    if (radix != 10)
      error("invalid character in non-decimal number");
    StringBuffer str = new StringBuffer (20);
    if (digit_seen)
      str.append(port.buffer, i, port.pos - i);

   /* location of decimal point in str.  */
    int point_loc = -1;
    int exp = 0;
    boolean exp_seen = false;
    for (;;)
      {
	c = port.read ();
	if (Character.digit ((char)c, radix) >= 0)
	  {
	    digit_seen = true;
	    str.append ((char) c);
	    continue;
	  }
	switch (c)
	  {
	  case '.':
	    if (point_loc >= 0)
	      error("duplicate '.' in number");
	    else
	      {
		point_loc = str.length ();
		str.append ('.');
	      }
	    continue;
	  case 'e':  case 'E':
	    int next;
	    if (radix != 10 || !((next = port.peek ()) == '+' || next == '-'
				 || Character.digit ((char)next, 10) >= 0))
	      break;
	    if (!digit_seen)
	      error("mantissa with no digits");
	    exp = readOptionalExponent();
	    exp_seen = true;
	    c = read();
	    break;
	  }
	break;
      }

    if (c >= 0)
      port.unread();

    if (exp != 0)
      {
	str.append('e');
	str.append(exp);
      }
    return new Double(str.toString ());
  }

  public String getStringLiteral (char quote)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    int i = port.pos;
    int start = i;
    int limit = port.limit;
    char[] buffer = port.buffer;
    char c;
    for ( ; i < limit; i++)
      {
	c = buffer[i];
	if (c == quote)
	  {
	    port.pos = i+1;
	    return new String(buffer, start, i - start);
	  }
	if (c == '\\' || c == '\n' || c == '\r')
	  break;
      }
    port.pos = i;
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(buffer, start, i - start);
    for (;;)
      {
	int ch = port.read();
	if (ch == quote)
	  return sbuf.toString();
	if (ch < 0)
	  eofError("unterminated string literal");
	if (ch == '\n' || ch == '\r')
	  fatal("string literal not terminated before end of line");
	if (ch == '\\')
	  {
	    ch = port.read();
	    int val;
	    switch (ch)
	      {
	      case -1:
		eofError("eof following '\\' in string");
	      case '\n': case '\r':
		fatal("line terminator following '\\' in string");
	      case '\'':  case '\"':  case '\\':
		break;
	      case 'b':  ch = '\b';  break;
	      case 't':  ch = '\t';  break;
	      case 'n':  ch = '\n';  break;
	      case 'f':  ch = '\f';  break;
	      case 'r':  ch = '\r';  break;
	      case 'x':  case 'u':
		val = 0;
		for (i = ch == 'x' ? 2 : 4;  --i >= 0; )
		  {
		    int d = port.read();
		    if (d < 0)
		      eofError("eof following '\\"
			       +((char)ch)+"' in string");
		    d = Character.forDigit((char) d, 16);
		    if (d < 0)
		      {
			error("invalid char following '\\"
			      +((char)ch)+"' in string");
			val = '?';
			break;		      }
		    val = 16 * val + d;
		  }
		ch = val;
		break;
	      default:
		if (ch < '0' || ch > '7')
		  break;
		val = 0;
		for (i = 3;  --i >= 0; )
		  {
		    int d = port.read();
		    if (d < 0)
		      eofError("eof in octal escape in string literal");
		    d = Character.forDigit((char) d, 8);
		    if (d < 0)
		      {
			port.unread_quick();
			break;
		      }
		    val = 8 * val + d;
		  }
		ch = val;
		break;
		
	      }
	  }
	sbuf.append((char) ch);
      }
  }

  public String getIdentifier (int ch) 
    throws java.io.IOException
  {
    int i = port.pos;
    int start = i - 1;
    int limit = port.limit;
    char[] buffer = port.buffer;
    while (i < limit && Character.isJavaIdentifierPart(buffer[i]))
      i++;
    port.pos = i;
    if (i < limit)
      return new String(buffer, start, i - start);
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(buffer, start, i - start);
    for (;;)
      {
	ch = port.read();
	if (ch < 0)
	  break;
	if (Character.isJavaIdentifierPart((char) ch))
	  sbuf.append((char) ch);
	else
	  {
	    port.unread_quick();
	    break;
	  }
      }
    return sbuf.toString();
  }


  public Object maybeAssignment(Object token)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    int ch = read();
    if (ch == '=')
      {
	error("assignment operation not implemented");
	// return makeAssignmentOp(token);
      }
    if (ch >= 0)
      port.unread_quick();
    return token;
  }

  /**
    * Returns the next token.
    * Returns: <dl>
    * <dt>end-of-file<dd>Sequence.eofValue
    * <dt>end-of-line>dd>eolToken
    * <dt>reserved word<dd> ???
    * <dt>identifier><dd>a java.lang.String
    * <dt>punctuator<dd> ???
    * </dl>
    * Literals are returned a QuoteExp objects,  Specifically:
    * <dl>
    * <dt>numeric literal<dd>a QuoteExp of a java.lang.Double value
    * <dt>boolean literal<dd>a QuoteExp of java.lang.Boolean.TRUE or FALSE
    * <dt>null literal<dd>a QuoteExp whose value is null
    * <dt>string literal<dd>a QuoteExp whose value is a String
    * </dl>
    */

  public Object getToken()
    throws java.io.IOException, gnu.text.SyntaxException
  {
    int ch = read();
    for (;;)
      {
	if (ch < 0)
	  return eofToken;
	if (! Character.isWhitespace((char) ch))
	  break;
	if (ch == '\r')
	  {
	    prevWasCR = true;
	    return eolToken;
	  }
	if (ch == '\n' && ! prevWasCR)
	  return eolToken;
	prevWasCR = false;
	ch = read();
      }

    switch (ch)
      {
      case '.':
	ch = port.peek();
	if (ch >= '0' && ch  <= '9')
	  return new QuoteExp(getNumericLiteral('.'));
	return dotToken;
      case '0':  case '1':  case '2':  case '3':  case '4':
      case '5':  case '6':  case '7':  case '8':  case '9':
	return new QuoteExp(getNumericLiteral(ch));
      case '\'':  case '\"':
	return new QuoteExp(getStringLiteral((char) ch));
      case '(':  return lparenToken;
      case ')':  return rparenToken;
      case '[':  return lbracketToken;
      case ']':  return rbracketToken;
      case '{':  return lbraceToken;
      case '}':  return rbraceToken;
      case '?':  return condToken;
      case ':':  return colonToken;
      case ';':  return semicolonToken;
      case ',':  return commaToken;
      case '=':
	if (port.peek() == '=')
	  {
	    port.skip_quick();
	    return Reserved.opEqual;
	  }
	return equalToken;
      case '!':
	if (port.peek() == '=')
	  {
	    port.skip_quick();
	    return Reserved.opNotEqual;
	  }
	return notToken;
      case '~':
	return tildeToken;
      case '*':	return maybeAssignment(Reserved.opTimes);
      case '/':	return maybeAssignment(Reserved.opDivide);
      case '^':	return maybeAssignment(Reserved.opBitXor);
      case '%':	return maybeAssignment(Reserved.opRemainder);
      case '+':
	if (port.peek() == '+')
	  {
	    port.skip_quick(); 
            return maybeAssignment(Reserved.opPlusPlus); 
	  }
	return maybeAssignment(Reserved.opPlus);
      case '-':
	if (port.peek() == '-')
	  {
	    port.skip_quick(); 
            return maybeAssignment(Reserved.opMinusMinus); 
	  }
	return maybeAssignment(Reserved.opMinus);
      case '&':
	if (port.peek() == '&')
	  {
	    port.skip_quick(); 
            return maybeAssignment(Reserved.opBoolAnd); 
	  }
	return maybeAssignment(Reserved.opBitAnd);
      case '|':
	if (port.peek() == '|')
	  {
	    port.skip_quick(); 
            return maybeAssignment(Reserved.opBoolOr); 
	  }
	return maybeAssignment(Reserved.opBitOr);
      case '>':
	ch = port.peek();
	switch (ch)
	  {
	  case '>':
	    port.skip_quick();
	    if (port.peek() == '>')
	      {
		port.skip_quick();
		return maybeAssignment(Reserved.opRshiftUnsigned);
	      }
	    return maybeAssignment(Reserved.opRshiftSigned);
	  case '=':
	    port.skip_quick();
	    return Reserved.opGreaterEqual;
	  default:
	    return Reserved.opGreater;
	  }
      case '<':
	ch = port.peek();
	switch (ch)
	  {
	  case '<':
	    port.skip_quick();
	    return maybeAssignment(Reserved.opLshift);
	  case '=':
	    port.skip_quick();
	    return Reserved.opLessEqual;
	  default:
	    return Reserved.opLess;
	  }
      }
    if (Character.isJavaIdentifierStart((char) ch))
      {
	String word = getIdentifier(ch).intern();
	Object token = checkReserved(word);
	if (token != null)
	  return token;
	return word;
      }
    return Char.make((char) ch);
  }

  public static Object getToken(InPort inp)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return new Lexer(inp).getToken();
  }

  public static void main(String[] args)
  {
    InPort inp = InPort.inDefault();
    Lexer reader = new Lexer(inp);
    for (;;)
      {
	try
	  {
	    Object token = reader.getToken();
	    OutPort out = OutPort.outDefault();
	    out.print("token:");
	    out.print(token);
	    out.println(" [class:"+token.getClass()+"]");
	    if (token == Sequence.eofValue)
	      break;
	  }
	catch (Exception ex)
	  {
	    System.err.println("caught exception:"+ex);
	    return;
	  }
      }
  }
}
