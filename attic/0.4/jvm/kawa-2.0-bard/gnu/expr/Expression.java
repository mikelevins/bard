package gnu.expr;
import gnu.bytecode.Type;
import gnu.mapping.*;
import gnu.text.Printable;
import gnu.text.SourceLocator;
import gnu.lists.Consumer;
import java.io.PrintWriter;
import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.OutPort;
import gnu.kawa.util.IdentityHashTable;
import gnu.kawa.reflect.OccurrenceType;

/**
 * Abstract class for syntactic forms that evaluate to a value.
 * Scheme S-expressions get re-written to these before evaluation.
 * @author	Per Bothner
 */

public abstract class Expression extends Procedure0
  implements Printable, SourceLocator
{
  public final Object eval (CallContext ctx) throws Throwable
  {
    int start = ctx.startFromContext();
    try
      {
	match0(ctx);
	return ctx.getFromContext(start);
      }
    catch (Throwable ex)
      {
	ctx.cleanupFromContext(start);
	throw ex;
      }
  }

  public final Object eval (Environment env) throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    Environment saveEnv = Environment.setSaveCurrent(env);
    try
      {
        return eval(ctx);
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  protected abstract boolean mustCompile ();

  public final int match0 (CallContext ctx)
  {
    ctx.proc = this;
    ctx.pc = 0;
    return 0;
  }

  public final Object apply0 () throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    check0(ctx);
    return ctx.runUntilValue();
  }

  /** Evaluate the expression.
   * This is named apply rather than eval so it is compatible with the
   * full-tail-call calling convention, and we can stash an Expression in
   * CallContext's proc field.  FIXME - are we making use of this?
   */
  public void apply (CallContext ctx) throws Throwable
  {
    throw new RuntimeException ("internal error - "
			        + getClass() + ".eval called");
  }

  public final void print (Consumer out)
  {
    if (out instanceof OutPort)
      print((OutPort) out);
    else if (out instanceof PrintWriter)
      {
	OutPort port = new OutPort((PrintWriter) out);
	print(port);
	port.close();
      }
    else
      {
	CharArrayOutPort port = new CharArrayOutPort();
	print(port);
	port.close();
        port.writeTo(out);
      }
  }

  public abstract void print (OutPort ps);

  /**
   * Print line and column number if specified.
   * This is a helper routineintended for use by print(OutPort).
   */
  public void printLineColumn(OutPort out)
  {
    int line = getLineNumber();
    if (line > 0)
      {
	out.print("line:");
	out.print(line);
	int column = getColumnNumber();
	if (column > 0)
	  {
	    out.print(':');
	    out.print(column);
	  }
	out.writeSpaceFill();
      }
  }

  public abstract void compile (Compilation comp, Target target);

  /** Same as compile, but emit line number beforehard. */
  public final void compileWithPosition(Compilation comp, Target target)
  {
    int line = getLineNumber ();
    if (line > 0)
      {
        comp.getCode().putLineNumber(getFileName(), line);
        compileNotePosition(comp, target, this);
      }
    else
      compile(comp, target);
  }

  /** Same as 2-argument compileWithPosition,
   * but use some other Expression's line number. */
  public final void compileWithPosition(Compilation comp, Target target,
					Expression position)
  {
    int line = position.getLineNumber ();
    if (line > 0)
      {
        comp.getCode().putLineNumber(position.getFileName(), line);
        compileNotePosition(comp, target, position);
      }
    else
      compile(comp, target);
  }

  /** Compile, but take note of line number. */
  public final void compileNotePosition(Compilation comp, Target target,
					Expression position)
  {
    String saveFilename = comp.getFileName();
    int saveLine = comp.getLineNumber();
    int saveColumn = comp.getColumnNumber();
    comp.setLine(position);
    compile(comp, target);
    // This might logically belong in a `finally' clause.
    // It is intentionally not so, so if there is an internal error causing
    // an exception, we get the line number where the exception was thrown.
    comp.setLine(saveFilename, saveLine, saveColumn);
  }

  public final void compile (Compilation comp, Type type)
  {
    // Should we use Target.pushValue instead?  FIXME.
    compile (comp, StackTarget.getInstance(type));
  }

  /** Compile an expression with checking suitable for a known Declaration.
   * Leaves the result on the stack (i.e. does not assign to the lhs).
   * It does coerce the value to a suitable type for the lhs, and
   * throw a hopefully-informative WrongType exception on failure.
   */
  public final void compile (Compilation comp, Declaration lhs)
  {
    compile (comp, CheckedTarget.getInstance(lhs));
  }

  /** Compile all but the first sub-"statement".
   * A kludge used for constructor methods, since if the first "statement"
   * is a super-constructor we need to inject initializer expressions. */
  public static void compileButFirst (Expression exp, Compilation comp)
  {
    if (exp instanceof BeginExp)
      {
        BeginExp bexp = (BeginExp) exp;
 	int n = bexp.length;
        if (n == 0)
          return;
        Expression[] exps = bexp.exps;
        compileButFirst(exps[0], comp);
	for (int i = 1; i < n; i++)
	  exps[i].compileWithPosition(comp, Target.Ignore);       
      }
  }

  /** Make a deep copy of this expression, if possible.
   * @param mapper used to lookup parts (expressions, declarations)
   *   that have been translated in the parent.
   *   Needed for copied Declarations and BlockExps.
   * @return a copy, or null if we can't copy.
   */
  public static Expression deepCopy (Expression exp, IdentityHashTable mapper)
  {
    if (exp == null)
      return null;
    Object tr = mapper.get(exp);
    if (tr != null) return (Expression) tr;
    Expression copy = exp.deepCopy(mapper);
    mapper.put(exp, copy);
    return copy;
  }

  public static Expression[] deepCopy (Expression[] exps,
                                       IdentityHashTable mapper)
  {
    if (exps == null)
      return null;
    int nargs = exps.length;
    Expression[] a = new Expression[nargs];
    for (int i = 0; i < nargs;  i++)
      {
        Expression ei = exps[i];
        Expression ai = deepCopy(ei, mapper);
        if (ai == null && ei != null)
          return null;
        a[i] = ai;
      }
    return a;
  }

  protected static Expression deepCopy (Expression exp)
  {
    return deepCopy(exp, new IdentityHashTable());
  }

  protected Expression deepCopy (IdentityHashTable mapper)
  {
    return null;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitExpression(this, d);
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d) { }

  /** Apply inlining transformations on a given ApplyExp.
   * Assumes the ApplyExp's function is this expression,
   * or can be optimized to this expression.
   * @param exp an application whose function expression can be simplified
   *  to this expression.
   * @param visitor the context for the current inlining pass
   * @param decl if non-null, a Declaration bound to this expression.
   * @return an Expression equivalent to the passed-in exp.
   */
  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    exp.args = visitor.visitExps(exp.args, null);
    return exp;
  }

  String filename;
  int position;

  public static final Expression[] noExpressions = new Expression[0];

  /** Helper method to create a `while' statement. */
  public static Expression makeWhile(Object cond, Object body, Compilation parser)
  {
    parser.loopStart();
    parser.loopEnter();
    parser.loopCond(parser.parse(cond));
    parser.loopBody(parser.parse(body));
    return parser.loopRepeatDone();
  }
  
  /** Copies the current location. */
  public final void setLocation (SourceLocator location)
  {
    this.filename = location.getFileName();
    setLine(location.getLineNumber(), location.getColumnNumber());
  }

  public final Expression setLine(Expression old)
  {
    setLocation(old);
    return this;
  }

    public final Expression maybeSetLine(Expression old) {
        if (position == 0 && old != null && old.position != 0)
            setLocation(old);
        return this;
    }

  public final void setFile (String filename)
  {
    this.filename = filename;
  }

  public final void setLine (int lineno, int colno)
  {
    if (lineno < 0)
      lineno = 0;
    if (colno < 0)
      colno = 0;
    position = (lineno << 12) + colno;
  }

  public final void setLine (int lineno)
  {
    setLine (lineno, 0);
  }

  public final String getFileName ()
  {
    return filename;
  }

  /** Set line number from current position in <code>Compilation</code>. */
  public void setLine (Compilation comp)
  {
    int line = comp.getLineNumber();
    if (line > 0)
      {
	setFile(comp.getFileName());
	setLine(line, comp.getColumnNumber());
      }
  }

  public String getPublicId ()
  {
    return null;
  }

  public String getSystemId ()
  {
    return filename;
  }

  /** Get the line number of (the start of) this Expression.
    * The "first" line is line 1; unknown is -1. */
  public final int getLineNumber()
  {
    int line = position >> 12;
    return line == 0 ? -1 : line;
  }

  public final int getColumnNumber()
  {
    int column = position & ((1 << 12) - 1);
    return column == 0 ? -1 : column;
  }

  public boolean isStableSourceLocation() { return true; }

  protected Type type;

  /** Return the Type used to represent the values of this Expression. */
  public final Type getType()
  {
    if (type == null)
      {
        type = Type.objectType; // to guard against cycles
        type = calculateType();
      }
    return type;
  }

  protected Type calculateType ()
  {
    return Type.pointer_type;
  }

    public final Type getTypeRaw() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    /** True if the expression provably never returns.
     * Currently, this is very limited, not handling infinite recursion
     * (tail- or otherwise), but better analysis is planned.
     */
    public boolean neverReturns() {
        return getType() == Type.neverReturnsType;
    }

    /** Is this a keyword argument?
     * In the future this will return non-null only for explicit
     * literal non-quoted Keywords.
     */
    public Keyword checkLiteralKeyword() {
        if (this instanceof QuoteExp) {
            Object val = ((QuoteExp) this).getValue();
            if (val instanceof Keyword)
                return (Keyword) val;
        }
        return null;
    }

  public boolean isSingleValue()
  {
    return OccurrenceType.itemCountIsOne(getType());
  }

  /** Return value if it is constant, or null if non-constant or unknown. */
  public Object valueIfConstant ()
  {
    return null;
  }

  protected int flags;
  public static final int VALIDATED = 1;
  protected static final int NEXT_AVAIL_FLAG = 2;

  public void setFlag (boolean setting, int flag)
  {
    if (setting) flags |= flag;
    else flags &= ~flag;
  }

  public void setFlag (int flag)
  {
    flags |= flag;
  }

  public int getFlags()
  {
    return flags;
  }

  public boolean getFlag (int flag)
  {
    return (flags & flag) != 0;
  }

  /** True if evaluating may have side-effects. */
  public boolean side_effects () { return true; }

  public String toString ()
  {
    String tname = getClass().getName();
    if (tname.startsWith("gnu.expr."))
      tname = tname.substring(9);
    return tname+"@"+Integer.toHexString(hashCode());
  }
}
