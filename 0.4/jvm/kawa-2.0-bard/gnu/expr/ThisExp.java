package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

/** Evaluates to the "this" implicit variable.
 * This is currently neither robust nor general.  FIXME!
 */

public class ThisExp extends ReferenceExp
{
  /** Non-interned name for implicit 'this' variable. */
  public static final String THIS_NAME = new String("$this$");

  /** When evaluating, return the context.
   * This is used for the "context" of a Macro.
   */
  static int EVAL_TO_CONTEXT = NEXT_AVAIL_FLAG;

  /** The class which this refers to. */
  ScopeExp context;

  /** If this is being used to pass the context instance to a Macro. */
  public final boolean isForContext ()
  {
    return (flags & EVAL_TO_CONTEXT) != 0;
  }

  public void apply (CallContext ctx)
    throws Throwable
  {
    if (isForContext())
      ctx.writeValue(context);
    else
      super.apply(ctx);
  }

  public ScopeExp getContextScope () { return context; }

  public ThisExp ()
  {
    super(THIS_NAME);
  }

  public ThisExp(ScopeExp context)
  {
    super(THIS_NAME);
    this.context = context;
  }

  public ThisExp (Declaration binding)
  {
    super(THIS_NAME, binding);
  }

  public ThisExp (ClassType type)
  {
    this(new Declaration(THIS_NAME, type));
  }

  public static ThisExp makeGivingContext (ScopeExp context)
  {
    ThisExp exp = new ThisExp(context);
    exp.flags |= EVAL_TO_CONTEXT;
    return exp;
  }

  public void compile (Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (isForContext())
      {
        // This is an extension used by define_syntax.
        CodeAttr code = comp.getCode();
        if (comp.method.getStaticFlag())
          code.emitGetStatic(comp.moduleInstanceMainField);
        else
          code.emitPushThis();
        target.compileFromStack(comp, getType());
      }
    else
      {
        super.compile(comp, target);
      }
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitThisExp(this, d);
  }

  protected final gnu.bytecode.Type calculateType()
  {
    if (binding != null)
      return binding.getType();
    if (context instanceof ClassExp || context instanceof ModuleExp)
      return context.getType();
    return Type.pointer_type;
  }
}
