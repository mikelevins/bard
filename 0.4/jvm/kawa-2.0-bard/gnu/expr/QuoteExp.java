package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.text.SourceLocator;
import gnu.kawa.io.OutPort;
import gnu.kawa.reflect.MakeAnnotation;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  public final Object valueIfConstant() { return value; }

  public static final int EXPLICITLY_TYPED = Expression.NEXT_AVAIL_FLAG;
  public static final int SHARED_CONSTANT = EXPLICITLY_TYPED << 1;
  public static final int IS_KEYWORD = EXPLICITLY_TYPED << 2;

  public final gnu.bytecode.Type getRawType() { return type; }
  protected final gnu.bytecode.Type calculateType()
  {
    if (value == Values.empty)
      return Type.voidType;
    else if (value == null)
      return Type.nullType;
    else if (value instanceof Class)
      return new ParameterizedType(Type.javalangClassType, Type.make((Class) value));
    else if (this == undefined_exp)
      return Type.pointer_type;
    else
      return Type.make(value.getClass());
  }

    @Override
    public void setType(Type type) {
        super.setType(type);
        setFlag(EXPLICITLY_TYPED);
    }

  public boolean isExplicitlyTyped ()
  {
    return getFlag(EXPLICITLY_TYPED);
  }

  public boolean isSharedConstant ()
  {
    return getFlag(SHARED_CONSTANT);
  }

  static public QuoteExp undefined_exp = makeShared(Special.undefined);
  static public QuoteExp abstractExp = makeShared(Special.abstractSpecial);
  static public QuoteExp nativeExp = makeShared(Special.nativeSpecial);
  static public QuoteExp voidExp = makeShared(Values.empty, Type.voidType);
    /** Same value as voidExp, but different type, to suppress diagnostics. */
  static public QuoteExp voidObjectExp = makeShared(Values.empty, Type.objectType);
    static public QuoteExp trueExp = makeShared(Boolean.TRUE, Type.booleanType);
  static public QuoteExp falseExp = makeShared(Boolean.FALSE, Type.booleanType);
    static public QuoteExp trueObjExp = makeShared(Boolean.TRUE);
  static public QuoteExp falseObjExp = makeShared(Boolean.FALSE);
  static public QuoteExp nullExp = makeShared(null, Type.nullType);
  public static final QuoteExp classObjectExp = makeShared(Type.objectType);

  public static QuoteExp getInstance (Object value)
  {
    return getInstance(value, null);
  }

  public static QuoteExp getInstance (Object value, SourceLocator position)
  {
    if (value == null)
      return nullExp;
    if (value == Type.pointer_type)
      return classObjectExp;
    if (value == Special.undefined)
      return undefined_exp;
    if (value == Values.empty)
      return voidExp;
    // Note we deliberately don't map abstractSpecial to abstractExp.
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue() ? trueObjExp : falseObjExp;
    QuoteExp q = new QuoteExp(value);
    if (position != null)
      q.setLocation(position);
    return q;
  }

  static QuoteExp makeShared (Object value)
  {
    QuoteExp exp = new QuoteExp(value);
    exp.setFlag(SHARED_CONSTANT);
    return exp;
  }

  static QuoteExp makeShared (Object value, Type type)
  {
    QuoteExp exp = new QuoteExp(value, type);
    exp.setFlag(SHARED_CONSTANT);
    return exp;
  }

  public QuoteExp (Object val) { value = val; }

  public QuoteExp (Object val, Type type) { value = val; setType(type); }
  
  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx)
  {
    ctx.writeValue(value);
  }

    public void compile (Compilation comp, Target target) {
        Type targetType;
        if (type == null || type == Type.pointer_type
            || target instanceof IgnoreTarget
            || (type instanceof ObjectType
                && type.isInstance(value))
            || (type instanceof PrimType
                // In this case the value is assumed to be an instance
                // of ((PrimType) type).boxedType().
                && ((targetType = target.getType()) == Type.objectType
                    || targetType == ((PrimType) type).boxedType()))) {
            comp.compileConstant(value, target);
        } else {
            Type vtype = type.isVoid() ? Type.objectType : type;
            comp.compileConstant(value, StackTarget.getInstance(vtype));
            target.compileFromStack(comp, vtype);
        }
    }
 
  public Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    return this;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitQuoteExp(this, d);
  }

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    if (this == QuoteExp.undefined_exp)
      return exp;
    Object fval = getValue();
    if (! (fval instanceof Procedure))
      return visitor.noteError(decl == null || fval == null ? "called value is not a procedure"
			      : ("calling " + decl.getName()
				 + " which is a "+fval.getClass().getName()));
    Procedure proc = (Procedure) fval;
    int nargs = exp.getArgCount();
    int spliceCount = exp.spliceCount();
    String msg = WrongArguments.checkArgCount(proc, nargs-spliceCount,
                                              spliceCount>0);
    if (msg != null)
      return visitor.noteError(msg);
    Expression inlined = visitor.maybeInline(exp, required, proc);
    if (inlined != null)
      return inlined;
    Expression[] args = exp.args;
    MethodProc asMProc = proc instanceof MethodProc ? (MethodProc) proc : null;
    for (int i = 0;  i < nargs;  i++)
      {
        Type ptype = asMProc != null ? asMProc.getParameterType(i) : null;
        // The final varargs parameter T[] can match T or T[].
        if (i == nargs - 1 && ptype != null
            && asMProc.maxArgs() < 0 && i == asMProc.minArgs())
          ptype = null;
        args[i] = visitor.visit(args[i],
                                InlineCalls.ValueNeededType.make(ptype));
      }
    Compilation comp = visitor.getCompilation();
    if (exp.getFlag(ApplyExp.INLINE_IF_CONSTANT))
      {
	Expression e = exp.inlineIfConstant(proc, visitor);
	if (e != exp)
	  return visitor.visit(e, required);
        if (proc == MakeAnnotation.makeMethodProc && nargs == 1 && visitor.processingAnnotations())
          {
            Object name = null;
            if (args[0] instanceof ReferenceExp)
              name = ((ReferenceExp) args[0]).getName();
            msg = "unknown annotation type";
            if (name != null)
              msg = msg + " '" + name + '\'';
            comp.error('e', msg, args[0]);
          }
      }
    if (comp.inlineOk(proc) && exp.isSimple()
        && ! ApplyExp.isInlineable(proc))
      {
	PrimProcedure mproc
	  = PrimProcedure.getMethodFor(proc, decl, exp.args,
				       comp.getLanguage());
	if (mproc != null)
	  {
	    ApplyExp nexp;
	    if (mproc.getStaticFlag() || decl == null)
	      nexp = new ApplyExp(mproc, exp.args);
	    else if (decl.base == null)
	      return exp;
	    else
	      {
		Expression[] margs = new Expression[1 + nargs];
		System.arraycopy(exp.getArgs(), 0, margs, 1, nargs);
		margs[0] = new ReferenceExp(decl.base);
		nexp = new ApplyExp(mproc, margs);
	      }
	    return nexp.setLine(exp);
	  }
      }
    return exp;
  }

  public boolean side_effects () { return false; }

  public String toString ()
  {
    return "QuoteExp["+value+"]";
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Quote", ")", 2);
    out.writeSpaceLinear();
    Object val = this.value;
    if (val instanceof Expression)
      val = val.toString(); // To avoid cycles.
    gnu.lists.AbstractFormat saveFormat = out.objectFormat;
    try
      {
	out.objectFormat = Language.getDefaultLanguage().getFormat(true);
	out.print(val);
        if (type != null)
          {
            out.print(" ::");
            out.print(type.getName());
          }
        /*
        if (value != null)
          {
            out.print(" ::");
            out.print(value.getClass().getName());
          }
        */
      }
    finally
      {
	out.objectFormat = saveFormat;
      }
    out.endLogicalBlock(")");
  }
}
