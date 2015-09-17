package gnu.ecmascript;
import java.util.Vector;
import gnu.kawa.io.InPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.TtyInPort;
import gnu.text.SyntaxException;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.Sequence;

public class Parser
{
  InPort port;
  Lexer lexer;

  Object previous_token;
  Object token;

  public static Expression eofExpr = new QuoteExp(Sequence.eofValue);

  public Parser (InPort port)
  {
    this.port = port;
    this.lexer = new Lexer(port);
  }

  public Expression parseConditionalExpression()
    throws java.io.IOException, SyntaxException
  {
    Expression exp1 = parseBinaryExpression(1);
    Object result = peekToken();
    if (result != Lexer.condToken)
      return exp1;
    skipToken();
    Expression exp2 = parseAssignmentExpression();
    if (getToken() != Lexer.colonToken)
      return syntaxError("expected ':' in conditional expression");
    Expression exp3 = parseAssignmentExpression();
    return new IfExp(exp1, exp2, exp3);
  }

  public Expression parseAssignmentExpression()
    throws java.io.IOException, SyntaxException
  {
    Expression exp1 = parseConditionalExpression();
    Object token = peekToken();
    if (token == Lexer.equalToken)
      {
	skipToken();
	Expression exp2 = parseAssignmentExpression();
	if (exp1 instanceof ReferenceExp)
	  {
	    SetExp sex = new SetExp(((ReferenceExp) exp1).getName(), exp2);
	    sex.setDefining(true);
	    return sex;
	  }
	return syntaxError("unmplemented non-symbol ihs in assignment");
      }
    else
      {
	if (! (token instanceof Reserved))
	  return exp1;
	Reserved op = (Reserved) token;
	if (!op.isAssignmentOp())
	  return exp1;
	skipToken();
	Expression exp2 = parseAssignmentExpression();
	Expression[] args = { exp1, exp2 };
	return new ApplyExp(new QuoteExp(op.proc), args);
      }
  }

  public Expression parseExpression()
    throws java.io.IOException, SyntaxException
  {
    Expression[] exps = null;
    int nExps = 0;
    for (;;)
      {
	Expression exp1 = parseAssignmentExpression();
	boolean last = peekToken() != Lexer.commaToken;
	if (exps == null)
	  {
	    if (last)
	      return exp1;
	    exps = new Expression[2];
	  }
	else if (last ? exps.length != nExps + 1 : exps.length <= nExps)
	  { // need to resize
	    int newsize = last ? nExps + 1 : 2 * exps.length;
	    Expression[] new_exps = new Expression[newsize];
	    System.arraycopy(exps, 0, new_exps, 0, nExps);
	    exps = new_exps;
	  }
	exps[nExps++] = exp1;
	if (last)
	  return new BeginExp(exps);
	skipToken();
      }
  }

  /** Return the next token from the lexer.
   * A LineTerminator is considered a token.
   */
  public Object peekTokenOrLine()
    throws java.io.IOException, SyntaxException
  {
    if (token == null)
      token = lexer.getToken();
    return token;
  }

  /** Return the next non-whitespace token from the lexer.
   * LineTerminators are skipped until a non-eolToken is found.
   */
  public Object peekToken()
    throws java.io.IOException, SyntaxException
  {
    if (token == null)
      token = lexer.getToken();
    while (token == Lexer.eolToken)
      {
	skipToken();
	token = lexer.getToken();
      }
    return token;
  }

  public Object getToken()
    throws java.io.IOException, SyntaxException
  {
    Object result = peekToken();
    skipToken();
    return result;
  }

  public final void skipToken()
  {
    if (token != Lexer.eofToken)
      {
	previous_token = token;
	token = null;
      }
  }

  /** Skip an explicit or implicit semicolon. */
  public void getSemicolon()
    throws java.io.IOException, SyntaxException
  {
    token = peekToken();
    if (token == Lexer.semicolonToken)
      skipToken();
    else if (token == Lexer.rbraceToken
	     || token == Lexer.eofToken
	     || previous_token == Lexer.eolToken)
	; // implicit ("inserted") semicolon
    else
      syntaxError("missing ';' after expression");
  }


  public Expression parsePrimaryExpression()
    throws java.io.IOException, SyntaxException
  {
    Object result = getToken();
    if (result instanceof QuoteExp)
      return (QuoteExp) result;
    if (result instanceof String)
      return new ReferenceExp((String) result);
    if (result == Lexer.lparenToken)
      {
	Expression expr = parseExpression();
	Object token = getToken();
	if (token != Lexer.rparenToken)
	  return syntaxError("expected ')' - got:"+token);
	return expr;
      }
    return syntaxError("unexpected token: "+result);
  }

  public Expression makePropertyAccessor (Expression exp, Expression prop)
  {
    return null;  // FIXME
  }

  public final static Expression[] emptyArgs = { };

  public Expression[] parseArguments()
    throws java.io.IOException, SyntaxException
  {
    skipToken();
    Object token = peekToken();
    if (token == Lexer.rparenToken)
      {
	skipToken();
	return emptyArgs;
      }
    Vector args = new Vector(10);
    for (;;)
      {
	Expression arg = parseAssignmentExpression();
	args.addElement(arg);
	token = getToken();
	if (token == Lexer.rparenToken)
	  break;
	if (token != Lexer.commaToken)
	  syntaxError("invalid token '"+token+"' in argument list");
      }
    Expression[] exps = new Expression[args.size()];
    args.copyInto(exps);
    return exps;
  }

  public Expression makeNewExpression(Expression exp, Expression[] args)
  {
    if (args == null)
      args = emptyArgs;
    exp = null;  // FIXME
    return new ApplyExp(exp, args);
  }

  public Expression makeCallExpression(Expression exp, Expression[] args)
  {
    return new ApplyExp(exp, args); // FIXME
  }

  public String getIdentifier()
    throws java.io.IOException, SyntaxException
  {
    Object token = getToken();
    if (token instanceof String)
      return (String) token;
    syntaxError("missing identifier");
    return "??";
  }

  public Expression parseLeftHandSideExpression ()
    throws java.io.IOException, SyntaxException
  {
    int newCount = 0;
    while (peekToken() == Lexer.newToken)
      {
	newCount++;
	skipToken();
      }
    Expression exp = parsePrimaryExpression();
    for (;;)
      {
	Object token = peekToken();
	if (token == Lexer.dotToken)
	  {
	    skipToken();
	    String name = getIdentifier();
	    exp = makePropertyAccessor(exp, new QuoteExp(name));
	  }
	else if (token == Lexer.lbracketToken)
	  {
	    skipToken();
	    Expression prop = parseExpression();
	    token = getToken();
	    if (token != Lexer.rbracketToken)
	      return syntaxError("expected ']' - got:"+token);
	    exp = makePropertyAccessor(exp, prop);
	  }
	else if (token == Lexer.lparenToken)
	  {
	    Expression[] args = parseArguments();
System.err.println("after parseArgs:"+peekToken());
	    if (newCount > 0)
	      {
		exp = makeNewExpression(exp, args);
		newCount--;
	      }
	    else
	      exp = makeCallExpression(exp, args);
	  }
	else
	  break;
      }
    for (; newCount > 0;  newCount--)
      {
	exp = makeNewExpression(exp, null);
      }
    return exp;
  }

  public Expression parsePostfixExpression ()
    throws java.io.IOException, SyntaxException
  {
    Expression exp = parseLeftHandSideExpression();
    Object op = peekTokenOrLine();
    if (op != Reserved.opPlusPlus && op != Reserved.opMinusMinus)
      return exp;
    skipToken();
    Expression[] args = { exp };
    return new ApplyExp(new QuoteExp(((Reserved)op).proc), args);
  }


  public Expression parseUnaryExpression ()
    throws java.io.IOException, SyntaxException
  {
    //Object op = peekTokenOrLine();
    // FIXME
    return parsePostfixExpression();
  }

  public int errors;

  public Expression syntaxError(String message)
  {
    // same as Translator.syntaxError.  FIXME
    errors++;
    OutPort err = OutPort.errDefault();
    String current_filename = port.getName();
    int current_line = port.getLineNumber()+1;
    int current_column = port.getColumnNumber()+1;
    if (current_line > 0)
      {
	if (current_filename != null)
	  err.print (current_filename);
	err.print (':');
	err.print (current_line);
	if (current_column > 1)
	  {
	    err.print (':');
	    err.print (current_column);
	  }
	err.print (": ");
      }
    err.println (message);
    return new ErrorExp (message);
  }

  public Expression parseBinaryExpression(int prio)
    throws java.io.IOException, SyntaxException
  {
    Expression exp1 = parseUnaryExpression();
    for (;;)
      {
	token = peekToken();
	if (! (token instanceof Reserved))
	  return exp1;
	Reserved op = (Reserved) token;
	if (op.prio < prio)
	  return exp1;
	getToken();
	Expression exp2 = parseBinaryExpression(op.prio+1);
	Expression[] args = { exp1, exp2 };
	exp1 = new ApplyExp(new QuoteExp(op.proc), args);
      }
  }

  static Expression emptyStatement = new QuoteExp(Values.empty);

  public Expression parseIfStatement()
    throws java.io.IOException, SyntaxException
  {
    skipToken();
    Object token = getToken();
    if (token != Lexer.lparenToken)
      return syntaxError("expected '(' - got:"+token);
    Expression test_part = parseExpression();
    token = getToken();
    if (token != Lexer.rparenToken)
      return syntaxError("expected ')' - got:"+token);
    Expression then_part = parseStatement();
    token = peekToken();
    Expression else_part;
    if (token == Lexer.elseToken)
      {
	skipToken();
	else_part = parseStatement();
      }
    else
      else_part = null;
    return new IfExp(test_part, then_part, else_part);
  }

  public Expression buildLoop (Expression init, Expression test,
			       Expression incr, Expression body)
  {
    if (init != null)
      {
	Expression[] pair = new Expression[2];
	pair[0] = init;
	pair[1] = buildLoop (null, test, incr, body);
	return new BeginExp(pair);
      }
    throw new Error("not implemented - buildLoop");
  }

  public Expression parseWhileStatement()
    throws java.io.IOException, SyntaxException
  {
    skipToken();  // Skip 'while'.
    Object token = getToken();
    if (token != Lexer.lparenToken)
      return syntaxError("expected '(' - got:"+token);
    Expression test_part = parseExpression();
    token = getToken();
    if (token != Lexer.rparenToken)
      return syntaxError("expected ')' - got:"+token);
    Expression body = parseStatement();
    return buildLoop (null, test_part, null, body);
  }

  public Expression parseFunctionDefinition() 
    throws java.io.IOException, SyntaxException
 {
    skipToken();
    String name = getIdentifier();
    Object token = getToken();
    if (token != Lexer.lparenToken)
      return syntaxError("expected '(' - got:"+token);
    Vector args = new Vector(10);
    if (peekToken() == Lexer.rparenToken)
      {
	skipToken();
      }
    else
      {
	for (;;)
	  {
	    String arg = getIdentifier();
	    args.addElement(arg);
	    token = getToken();
	    if (token == Lexer.rparenToken)
	      break;
	    if (token != Lexer.commaToken)
	      syntaxError("invalid token '"+token+"' in argument list");
	  }
      }
    Expression body = parseBlock();
    LambdaExp lexp = new LambdaExp(body);
    lexp.setName(name);
    SetExp sexp = new SetExp(name, lexp);
    sexp.setDefining(true);
    return sexp;
  }

  public Expression parseBlock()
    throws java.io.IOException, SyntaxException
  {
    Expression[] exps = null;
    if (getToken() != Lexer.lbraceToken)
      return syntaxError("extened '{'");
    int nExps = 0;
    for (;;)
      {
	token = peekToken();
	boolean last;
	if (token == Lexer.rbraceToken)
	  {
	    skipToken();
	    if (exps == null)
	      return emptyStatement;
	    last = true;
	  }
	else
	  last = false;
	if (exps == null)
	  exps = new Expression[2];
	else if (last ? exps.length !=nExps : exps.length <= nExps) 
	  { // need to resize 
	    int newsize = last ? nExps : 2 * exps.length; 
	    Expression[] new_exps = new Expression[newsize]; 
	    System.arraycopy(exps, 0, new_exps, 0, nExps); 
	    exps = new_exps; 
	  } 
	if (last)
	  return new BeginExp(exps);
	exps[nExps++] = parseStatement();
      }
  }

  public Expression parseStatement()
    throws java.io.IOException, SyntaxException
  {
    Object token = peekToken();
    if (token instanceof Reserved)
      {
	switch (((Reserved) token).prio)
	  {
	  case Reserved.IF_TOKEN:  return parseIfStatement();
	  case Reserved.WHILE_TOKEN:  return parseWhileStatement();
	  case Reserved.FUNCTION_TOKEN:  return parseFunctionDefinition();
	  }
      }
    if (token == Lexer.eofToken)
      return eofExpr;
    if (token == Lexer.semicolonToken)
      {
	skipToken();
	return emptyStatement;
      }
    if (token == Lexer.lbraceToken)
      return parseBlock();
    
    Expression exp = parseExpression();
    getSemicolon();
    return exp;
  }

  public static void main (String[] args)
  {
    Language language = new kawa.standard.Scheme();  // FIXME

    InPort inp = InPort.inDefault ();
    if (inp instanceof TtyInPort)
      {
	Object prompter = new Prompter();
	((TtyInPort)inp).setPrompter((Procedure) prompter);
      }

    Parser parser = new Parser(inp);
    OutPort out = OutPort.outDefault();
    for (;;)
      {
	try
	  {
	    /*
	    Object token = parser.peekToken();
	    if (token == Lexer.eofToken)
	      break;
	    if (token == Lexer.eolToken)
	      {
		parser.getToken();
		continue;
	      }
	    Expression expr = parser.parseExpression();
	    */
	    Expression expr = parser.parseStatement();
	    if (expr == eofExpr)
	      break;
	    out.print("[Expression: ");
	    expr.print(out);
	    out.println("]");
	    Object result = expr.eval(Environment.user());
	    out.print("result: ");
	    out.print(result);
	    out.println();
	  }
	catch (Throwable ex)
	  {
	    System.err.println("caught exception:"+ex);
	    ex.printStackTrace(System.err);
	    return;
	  }
      }
  }
}
