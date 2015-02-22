package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.math.IntNum;
import gnu.bytecode.*;

public class syntax_case extends Syntax
{
  public static final syntax_case syntax_case = new syntax_case();
  static { syntax_case.setName("syntax-case"); }

  PrimProcedure call_error;

  Expression rewriteClauses (Object clauses, syntax_case_work work,
                             Translator tr)
  {
    Language language = tr.getLanguage();
    if (clauses == LList.Empty)
      {
        /*
        // FIXME - throw exception instead??  perhaps SyntaxException?
        return new QuoteExp(new Pair("quote",
                                     new Pair("((no match in syntax-case))",
                                     LList.Empty)));
        */
        Expression[] args = new Expression[2];
        args[0] = new QuoteExp("syntax-case");
        args[1] = new ReferenceExp(work.inputExpression);
        if (call_error == null)
          {
            ClassType clas = ClassType.make("kawa.standard.syntax_case");
            Type[] argtypes = new Type[2];
            argtypes[0] = Compilation.javaStringType;
            argtypes[1] = Type.objectType;
            Method method = clas.addMethod("error", argtypes,
                                           Type.objectType,
                                           Access.PUBLIC|Access.STATIC);
            call_error = new PrimProcedure(method, language);
          }
        return new ApplyExp(call_error, args);
      }
    Object savePos = tr.pushPositionOf(clauses);
    Object clause;
    try
      {
	if (! (clauses instanceof Pair)
	    || ! ((clause = ((Pair) clauses).getCar()) instanceof Pair))
	  return tr.syntaxError("syntax-case:  bad clause list");
	Pair pair = (Pair) clause;
	PatternScope clauseScope = PatternScope.push(tr);
	clauseScope.matchArray = tr.matchArray;
	tr.push(clauseScope);

        SyntaxForm syntax = null;
        Object tail = pair.getCdr();
        while (tail instanceof SyntaxForm)
          {
            syntax = (SyntaxForm) tail;
            tail = syntax.getDatum();
          }
        // Check for nonsense before bothering to analyze the pattern.
        if (! (tail instanceof Pair))
          return tr.syntaxError("missing syntax-case output expression");

	int outerVarCount = clauseScope.pattern_names.size();
	SyntaxPattern pattern
	  = new SyntaxPattern(pair.getCar(), work.literal_identifiers, tr);
	int varCount = pattern.varCount();
	if (varCount > work.maxVars)
	  work.maxVars = varCount;

	BlockExp block = new BlockExp();
	Expression[] args = new Expression[4];
	args[0] = new QuoteExp(pattern);
	args[1] = new ReferenceExp(work.inputExpression);
	args[2] = new ReferenceExp(tr.matchArray);
	args[3] = new QuoteExp(IntNum.zero());
	Expression tryMatch
	  = new ApplyExp(new PrimProcedure(Pattern.matchPatternMethod, language), args);

	Expression output;
	pair = (Pair) tail;
	if (pair.getCdr() == LList.Empty)
	  output = tr.rewrite_car(pair, syntax);
	else
	  {
	    Expression fender = tr.rewrite_car(pair, syntax);
	    if (! (pair.getCdr() instanceof Pair
		   && (pair = (Pair) pair.getCdr()).getCdr() == LList.Empty))
	      return tr.syntaxError("syntax-case:  bad clause");
	    output = new IfExp(fender, tr.rewrite_car(pair, syntax),
                               new ExitExp(block));
	  }
	clauseScope.setBody(output);

	tr.pop(clauseScope);
	PatternScope.pop(tr);
	block.setBody(new IfExp(tryMatch, clauseScope, new ExitExp(block)),
		      rewriteClauses(((Pair) clauses).getCdr(), work, tr));
	return block;
      }
    finally
      {
	tr.popPositionOf(savePos);
      }
  }

  private static final Method allocVars =
    ClassType.make("kawa.lang.SyntaxPattern")
    .getDeclaredMethod("allocVars", 2);

  public Expression rewriteForm (Pair form, Translator tr)
  {
    syntax_case_work work = new syntax_case_work();

    Object obj = form.getCdr();
    if (obj instanceof Pair && ((Pair)obj).getCdr() instanceof Pair)
      {
        tr.letStart();
        form = (Pair) obj;
        work.inputExpression = tr.letVariable(null, null, tr.rewrite(form.getCar()));
	work.inputExpression.setCanRead(true);

        tr.letEnter();
	LetExp let2 = new LetExp();
	Declaration matchArrayOuter = tr.matchArray;
	Declaration matchArray = let2.addDeclaration((String) null);
	matchArray.setType(Compilation.objArrayType);
	matchArray.setCanRead(true);
	tr.matchArray = matchArray;

        obj = form.getCdr();

	form = (Pair) obj;
	work.literal_identifiers
	    = SyntaxPattern.getLiteralsList(form.getCar(), null, tr);
	obj = form.getCdr();
        try
          {
            tr.push(let2);
            let2.setBody(rewriteClauses(obj, work, tr));
            Expression[] args = new Expression[] {
              new QuoteExp(IntNum.make(work.maxVars)),
              (matchArrayOuter == null ? QuoteExp.nullExp
               : new ReferenceExp(matchArrayOuter))};
            matchArray.setInitValue(new ApplyExp(allocVars, args));
            matchArray.noteValueUnknown();
            tr.pop(let2);
            return tr.letDone(let2);
          }
        finally
          {
            tr.matchArray = matchArrayOuter;
          }
      }
    return tr.syntaxError("insufficiant arguments to syntax-case");
  }

  /** Called (at run-time) if syntax-case has no match. */
  public static Object error(String kind, Object arg)
  {
    Translator tr = (Translator) Compilation.getCurrent();
    if (tr == null)
      throw new RuntimeException("no match in syntax-case");
    Syntax syntax = tr.getCurrentSyntax();
    String name = syntax == null ? "some syntax" : syntax.getName();
    String msg = "no matching case while expanding " + name;
    return tr.syntaxError(msg);
  }
}

class syntax_case_work
{
  LetExp let;
  Object[] literal_identifiers;

  /** A temporary to hold the value of the input expression. */
  Declaration inputExpression;

  /** The maximum of the varCount() for the patterns seen so far. */
  int maxVars;
}
