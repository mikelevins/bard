// Copyright (c) 1999, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.io.OutPort;
import gnu.mapping.CallContext;
import gnu.mapping.Environment;
import gnu.mapping.EnvironmentKey;
import gnu.mapping.Procedure;
import gnu.mapping.Symbol;
import gnu.mapping.UnboundLocationException;
import gnu.math.IntNum;

/**
 * This class represents a variable reference (an identifier).
 * @author	Per Bothner
 */

public class ReferenceExp extends AccessExp
{
  static int counter;
  /** Unique id number, to ease print-outs and debugging. */
  int id = ++counter;

  public static final int DONT_DEREFERENCE = AccessExp.NEXT_AVAIL_FLAG;
  public static final int PROCEDURE_NAME = AccessExp.NEXT_AVAIL_FLAG<<1;
  /** Flag indicates a reference to a type name. */
  public static final int TYPE_NAME = AccessExp.NEXT_AVAIL_FLAG<<2;
  public static final int ALLOCATE_ON_STACK_LAST = AccessExp.NEXT_AVAIL_FLAG<<3;

  /** Links in list headed by {@see LambdaExp#siblingReferences}. */
  ReferenceExp siblingReferencesNext;

  /* If true, must have binding.isIndirectBinding().  Don't dereference it. */
  public final boolean getDontDereference()
  {
    return (flags & DONT_DEREFERENCE) != 0;
  }

  public final void setDontDereference(boolean setting)
  { setFlag(setting, DONT_DEREFERENCE); }

  public final boolean isUnknown ()
  {
    return Declaration.isUnknown(binding);
  }

  /** True if this identifier appears in "function call position".
   * If so, it should be interpreted as a function name, which makes a
   * difference for languages (like Common Lisp) that have two name spaces. */
  public final boolean isProcedureName()
  {
    return (flags & PROCEDURE_NAME) != 0;
  }

  /** Note if this identifier appears in "function call position". */
  public final void setProcedureName(boolean setting)
  {
    setFlag(setting, PROCEDURE_NAME);
  }

  public ReferenceExp (Object symbol)
  {
    this.symbol = symbol;
  }

  public ReferenceExp (Object symbol, Declaration binding)
  {
    this.symbol = symbol;
    this.binding = binding;
  }

  public ReferenceExp (Declaration binding) 
  {
    this(binding.getSymbol(), binding);
  }

  protected boolean mustCompile () { return false; }

  public final Object valueIfConstant()
  {
    if (binding != null)
      {
        Expression dvalue = binding.getValue();
        if (dvalue != null)
          return dvalue.valueIfConstant();
      }
    return null;
  }

  public void apply (CallContext ctx)
    throws Throwable
  {
    Object value;
    Expression dvalue;
    if (binding != null && binding.isAlias() && ! getDontDereference()
        && (dvalue = binding.getValueRaw()) instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) dvalue;
        if (rexp.getDontDereference() && rexp.binding != null)
          {
            Expression v = rexp.binding.getValue();
            if (v instanceof QuoteExp || v instanceof ReferenceExp
                || v instanceof LambdaExp)
              {
                v.apply(ctx);
                return;
              }
          }
        value = dvalue.eval(ctx);
      }
    else if (binding != null && binding.field != null
             && binding.field.getDeclaringClass().isExisting()
             && (! getDontDereference() || binding.isIndirectBinding()))
      {
        try
          {
            Object instance = binding.field.getStaticFlag() ? null
              : contextDecl().getValue().eval(ctx);
            value = binding.field.getReflectField().get(instance);
          }
        catch (Exception ex)
          {
            String msg = "exception evaluating "+symbol
              +" from "+binding.field+" - "+ex;
            // We abuse msg as a UnboundLocationException name.
            throw new UnboundLocationException(msg, this);
          }
      }
    // This isn't just an optimization - it's needed for evaluating procedural
    // macros (e.g. syntax-case) defined in a not-yet-compiled module.
    else if (binding != null
        && ((dvalue = binding.getValue()) instanceof QuoteExp
            || dvalue instanceof LambdaExp)
        && dvalue != QuoteExp.undefined_exp
        && (! getDontDereference() || binding.isIndirectBinding()))
      {
        value = dvalue.eval(ctx);
      }
    else if (binding == null
             || (binding.context instanceof ModuleExp
                && ! binding.isPrivate()))
      {
        Environment env = Environment.getCurrent();
        Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
          : env.getSymbol(symbol.toString());
        Object property = getFlag(PREFER_BINDING2) && isProcedureName()
          ? EnvironmentKey.FUNCTION
          : null;
        if (getDontDereference())
          value = env.getLocation(sym, property);
        else
          {
            Object unb = gnu.mapping.Location.UNBOUND;
            value = env.get(sym, property, unb);
            if (value == unb)
              throw new UnboundLocationException(sym, this);
          }
        ctx.writeValue(value);
        return;
      }
    else
      value = ctx.evalFrames[ScopeExp.nesting(binding.context)][binding.evalIndex];
    if (! getDontDereference() && binding.isIndirectBinding())
      value = ((gnu.mapping.Location) value).get();
    ctx.writeValue(value);
  }

  public void compile (Compilation comp, Target target)
  {
    if (! (target instanceof ConsumerTarget)
        || binding.getFlag(Declaration.ALLOCATE_ON_STACK)
        || ! ((ConsumerTarget) target).compileWrite(this, comp))
      binding.load(this, flags, comp, target);
  }

  protected Expression deepCopy (gnu.kawa.util.IdentityHashTable mapper)
  {
    Declaration d = (Declaration) mapper.get(binding, binding);
    Object s = mapper.get(symbol, symbol);
    ReferenceExp copy = new ReferenceExp(s, d);
    copy.flags = getFlags();
    return copy;
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitReferenceExp(this, d);
  }

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    decl = this.binding; // We don't use the passed-in Declaration.
    if (decl != null && ! decl.getFlag(Declaration.IS_UNKNOWN))
      {
        decl = Declaration.followAliases(decl);
        if (! (decl.isIndirectBinding()))
          {
            Expression dval = decl.getValue();
            if (dval != null)
              return dval.validateApply(exp, visitor, required, decl);
            Type dtype = decl.type;
            if (dtype instanceof ClassType
                && ((ClassType) dtype).isSubclass("kawa.lang.Continuation"))
                exp.setType(Type.neverReturnsType);
          }
      }
    else if (getSymbol() instanceof Symbol)
      {
        Symbol symbol = (Symbol) getSymbol();
        Object fval = Environment.getCurrent().getFunction(symbol, null);
        if (fval instanceof Procedure)
          return new QuoteExp(fval).validateApply(exp, visitor, required, null);
      }
    exp.visitArgs(visitor);
    return exp;
  }

  public void print (OutPort ps)
  {
    ps.print("(Ref/");
    ps.print(id);
    if (getDontDereference())
      ps.print(",dont-deref");
    if (symbol != null
	&& (binding == null || symbol.toString() != binding.getName()))
      {
	ps.print('/');
	ps.print(symbol);
      }
    if (binding != null)
      {
	ps.print('/');
	ps.print(binding);
      }
    ps.print(")");
  }

  protected gnu.bytecode.Type calculateType()
  {
    Declaration decl = binding;
    if (decl == null || decl.isFluid())
      return Type.pointer_type;
    if (getDontDereference())
      {
        if (decl.field != null && ! decl.isIndirectBinding())
          return decl.field.getStaticFlag()
            ? Compilation.typeStaticFieldLocation
            : Compilation.typeFieldLocation;
        return Compilation.typeLocation;
      }
    decl = Declaration.followAliases(decl);
    Type type = decl.isAlias() && decl.isIndirectBinding()
        ? Type.objectType // FIXME - should use a parameterized Location type
        : decl.getType();
    if (type == Type.toStringType)
      type = Type.javalangStringType;
    return type;
  }

  public boolean isSingleValue()
  {
    if (binding != null && binding.getFlag(Declaration.IS_SINGLE_VALUE))
      return true;
    return super.isSingleValue();
  }

  public boolean side_effects ()
  {
    return binding == null || ! binding.isLexical();
  }

  public String toString()
  {
    return "RefExp/"+symbol+'/'+id+'/';
  }
}
