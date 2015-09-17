// Copyright (c) 1999, 2001, 2004, 200, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.bytecode.*;
import gnu.kawa.functions.AddOp;
import gnu.kawa.io.OutPort;
import gnu.math.IntNum;

/** An Expression to set (bind) or define a new value to a named variable.
 * @author	Per Bothner
 */

public class SetExp extends AccessExp
{
  /** The new value to assign to the variable. */
  Expression new_value;

  /** Index into {@code values} array of binding. */
  int valueIndex;

  public SetExp (Object symbol, Expression val)
  { this.symbol = symbol;  new_value = val; }

  public SetExp (Declaration decl, Expression val)
  {
    this.binding = decl;
    symbol = decl.getSymbol();
    new_value = val;
  }

  public static SetExp makeDefinition (Object symbol, Expression val)
  {
    SetExp sexp = new SetExp(symbol, val);
    sexp.setDefining(true);
    return sexp;
  }

  public static SetExp makeDefinition (Declaration decl, Expression val)
  {
    SetExp sexp = new SetExp(decl, val);
    sexp.setDefining(true);
    return sexp;
  }

  /** Get the Expression for calculating the new ("right-hand") value. */
  public final Expression getNewValue() { return new_value; }
  public void setNewValue(Expression newValue) { this.new_value = newValue; }

  public static final int DEFINING_FLAG = AccessExp.NEXT_AVAIL_FLAG;
  public static final int GLOBAL_FLAG = AccessExp.NEXT_AVAIL_FLAG << 1;
  public static final int PROCEDURE = AccessExp.NEXT_AVAIL_FLAG << 2;
  public static final int SET_IF_UNBOUND = AccessExp.NEXT_AVAIL_FLAG << 3;
  public static final int HAS_VALUE = AccessExp.NEXT_AVAIL_FLAG << 4;

  public final boolean isDefining ()
  {
    return (flags & DEFINING_FLAG) != 0;
  }

  public final void setDefining (boolean value)
  {
    if (value) flags |= DEFINING_FLAG; else flags &= ~DEFINING_FLAG;
  }

  /** True if evaluating the SetExp yields the value of the RHS. */
  public final boolean getHasValue()
  { return (flags & HAS_VALUE) != 0; }

  public final void setHasValue (boolean value)
  { if (value) flags |= HAS_VALUE; else flags &= ~HAS_VALUE; }

  /** True if this is a functon definition ("defun"). */
  public final boolean isFuncDef()
  { return (flags & PROCEDURE) != 0; }

  public final void setFuncDef (boolean value)
  { if (value) flags |= PROCEDURE; else flags &= ~PROCEDURE; }

  public final boolean isSetIfUnbound()
  { return (flags & SET_IF_UNBOUND) != 0; }

  public final void setSetIfUnbound (boolean value)
  { if (value) flags |= SET_IF_UNBOUND; else flags &= ~SET_IF_UNBOUND; }

  protected boolean mustCompile () { return false; }

  public void apply (CallContext ctx) throws Throwable
  {
    Environment env = Environment.getCurrent();
    Symbol sym = symbol instanceof Symbol ? (Symbol) symbol
      : env.getSymbol(symbol.toString());
    Object property = null;
    Language language = Language.getDefaultLanguage();
    if (isFuncDef() && language.hasSeparateFunctionNamespace())
      property = EnvironmentKey.FUNCTION;
    if (isSetIfUnbound())
      {
	Location loc = env.getLocation(sym, property);
	if (! loc.isBound())
	  loc.set(new_value.eval (env));
	if (getHasValue())
	   ctx.writeValue(loc);
        return;
      }

    Object new_val = new_value.eval (env);
    if (binding != null && ! (binding.context instanceof ModuleExp))
      {
        Object[] evalFrame = ctx.evalFrames[ScopeExp.nesting(binding.context)];
        if (binding.isIndirectBinding())
          {
            Location loc;
            if (isDefining())
              evalFrame[binding.evalIndex] =  Location.make(sym);
            loc = (Location) evalFrame[binding.evalIndex];
            loc.set(new_value);
          }
        else
          evalFrame[binding.evalIndex] = new_val;
      }
    else if (isDefining ())
      {
	/*
	if (binding != null && binding.isAlias())
	  env.addLocation(sym, null, (gnu.mapping.Location) new_val);
	else
	*/
	env.define(sym, property, new_val);
      }
    else
      {
	env.put(sym, property, new_val);
      }
    if (getHasValue())
      ctx.writeValue(new_val);
  }

  public void compile (Compilation comp, Target target)
  {
    if (new_value instanceof LambdaExp
	&& target instanceof IgnoreTarget
	&& ((LambdaExp) new_value).getInlineOnly())
      return;
    Type type;
    gnu.bytecode.CodeAttr code = comp.getCode();
    // FIXME - handle isSetIfUnbound
    boolean needValue = getHasValue() && ! (target instanceof IgnoreTarget);

    // set the following to true if the value has been pushed.
    // this is used to detect not implemented cases.
    // when all cases are implemented, remove this.
    boolean valuePushed = false;

    // This code is kind of kludgy, because it handles a number of
    // different cases:  assignments and definitions to both local and
    // global variables.  Some of the complication is because we want
    // to generate fields for module-level definitions;  this is how
    // bindings are exported from modules.

    Declaration decl = binding;
    Expression declValue = decl.getValue();
    if (declValue instanceof LambdaExp
	&& decl.context instanceof ModuleExp
        && ! decl.ignorable()
	&& ((LambdaExp) declValue).getName() != null // FIXME
	&& declValue == new_value)
      {
	((LambdaExp) new_value).compileSetField(comp);
      }
    else if ((decl.shouldEarlyInit() || decl.isAlias())
        && decl.context instanceof ModuleExp
	&& isDefining() && ! decl.ignorable())
      {
        if (decl.shouldEarlyInit()
            && decl.field != null && ! decl.field.hasConstantValueAttr()) 
          BindingInitializer.create(decl, new_value, comp);
        if (needValue)
          {
            decl.load(this, 0, comp, Target.pushObject);
	    valuePushed = true;
	  }
      }
    else
      {
        AccessExp access = this;
        Declaration owner = contextDecl();
	if (! isDefining())
          {
            while (decl != null && decl.isAlias())
              {
                declValue = decl.getValue();
                if (! (declValue instanceof ReferenceExp))
                  break;
                ReferenceExp rexp = (ReferenceExp) declValue;
                Declaration orig = rexp.binding;
                if (orig == null)
                  break;
                if (owner != null && orig.needsContext())
                  break;
                owner = rexp.contextDecl();
                access = rexp;
                decl = orig;
              }
	  }
	if (decl.ignorable())
	  new_value.compile (comp, Target.Ignore);
	else if (decl.isAlias() && isDefining())
	  {
            decl.load(this, ReferenceExp.DONT_DEREFERENCE,
                      comp, Target.pushObject);
	    ClassType locType
	      = ClassType.make("gnu.mapping.IndirectableLocation");
	    code.emitCheckcast(locType);
	    new_value.compile(comp, Target.pushObject);
	    Method meth = locType.getDeclaredMethod("setAlias", 1);
	    code.emitInvokeVirtual(meth);
	  }
	else if (decl.isIndirectBinding())
	  {
            decl.load(access, ReferenceExp.DONT_DEREFERENCE,
                      comp, Target.pushObject);
	    if (isSetIfUnbound())
	      {
		if (needValue)
		  {
		    code.emitDup();
		    valuePushed = true;
		  }
		code.pushScope();
		code.emitDup();
		Variable symLoc = code.addLocal(Compilation.typeLocation);
		code.emitStore(symLoc);
		code.emitInvokeVirtual(Compilation.typeLocation
				       .getDeclaredMethod("isBound", 0));
		code.emitIfIntEqZero();
		code.emitLoad(symLoc);
	      }
	    new_value.compile (comp, Target.pushObject);
	    if (needValue && ! isSetIfUnbound())
	      {
		code.emitDupX();
		valuePushed = true;
	      }
	    String setterName = "set";
	    code.emitInvokeVirtual(Compilation.typeLocation
				   .getDeclaredMethod(setterName, 1));
	    if (isSetIfUnbound())
	      {
		code.emitFi();
		code.popScope();
	      }
	  }
	else if (decl.isSimple ())
	  {
            type = decl.getType();
	    Variable var = decl.getVariable();
	    if (var == null)
                var = decl.allocateVariable(code, true);
            int delta = canUseInc(new_value, decl);
            if (delta != BAD_SHORT)
              {
                comp.getCode().emitInc(var, (short) delta);
                if (needValue)
                  {
                    code.emitLoad(var);
                    valuePushed = true;
                  }
              }
            else
              {
                new_value.compile(comp, decl);
                if (! checkReachable(comp))
                  return;
                if (needValue)
                  {
                    code.emitDup(type);  // dup or dup2
                    valuePushed = true;
                  }
                code.emitStore(var);
              }
	  }
        else
	  {
	    Field field = decl.field;
            boolean isStatic = field != null && field.getStaticFlag();
            Method setter;
            if (field == null)
              {
                String setName = ClassExp.slotToMethodName("set",
                                                           decl.getName());
                ClassExp cl = (ClassExp) decl.context;
                setter = cl.compiledType.getDeclaredMethod(setName, 1);
                comp.usedClass(setter.getDeclaringClass());
              }
            else
              {
                setter = null;
                comp.usedClass(field.getDeclaringClass());
              }
            if (! isStatic)
              decl.loadOwningObject(owner, comp);
	    new_value.compile(comp, decl);
            if (! checkReachable(comp))
              return;
            if (isStatic)
              {
                if (needValue)
                  {
                    code.emitDup();
                    valuePushed = true;
                  }
                code.emitPutStatic(field);
              }
            else
              { 
                if (needValue)
                  {
                    code.emitDupX();
                    valuePushed = true;
                  }
                if (field != null)
                    code.emitPutField(field);
                else
                    code.emitInvoke(setter); 
              }
	  }
      }

    if (needValue && ! valuePushed)
      throw new Error("SetExp.compile: not implemented - return value");

    if (needValue)
      target.compileFromStack(comp, getType());
    else
      comp.compileConstant(Values.empty, target);
  }

  boolean checkReachable (Compilation comp)
  {
    boolean reachable = comp.getCode().reachableHere();
    if (! reachable)
      comp.error('w', "'"+getSymbol()+"' can never be set because expression never finishes", new_value);
    return reachable;
  }

  /** "Failure" return value of canUseInc. */
  public static final int BAD_SHORT = 0x10000;

  /* Check if we can use the 'iinc' instruction.
   * Also, if the {@code rhs} is the same as (i.e. a reference to)
   * {@code target}, return zero, to indicate a possible no-op.
   * I.e. we treat {@code target=target} as if it were
   * {@code target=target+0} - even is {@code target} is non-numeric.
   * @return return increment, or {@code BAD_SHORT}.
   */
  public static int canUseInc (Expression rhs, Declaration target)
  {
    ApplyExp aexp;
    Variable var = target.getVariable();
    if (target.isSimple()
        && rhs instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) rhs;
        if (rexp.binding == target && ! rexp.getDontDereference())
          return 0;
      }
  body:
    if (target.isSimple()
        && var.getType().getImplementationType().promote() == Type.intType
        && rhs instanceof ApplyExp
        && (aexp = (ApplyExp) rhs).getArgCount() == 2)
      {
        Expression funcExp = aexp.getFunction();
        Object func = funcExp.valueIfConstant();
        int sign;
        if (func == AddOp.$Pl)
          sign = 1;
        else if (func == AddOp.$Mn)
          sign = -1;
        else
          break body;
        Expression arg0 = aexp.getArg(0);
        Expression arg1 = aexp.getArg(1);
        if (arg0 instanceof QuoteExp && sign > 0)
          {
            Expression tmp = arg1;
            arg1 = arg0;
            arg0 = tmp;
          }
        if (arg0 instanceof ReferenceExp)
          {
            ReferenceExp ref0 = (ReferenceExp) arg0;
            if (ref0.getBinding() != target || ref0.getDontDereference())
              break body;
            Object value1 = arg1.valueIfConstant();
            int val1;
            if (value1 instanceof Integer)
              {
                val1 = ((Integer) value1).intValue();
                if (sign < 0)
                  val1 = - val1;
                if (((short) val1) == val1)
                  return val1;
              }
            else if (value1 instanceof IntNum)
              {
                IntNum int1 = (IntNum) value1;
                int hi = 0x7FFF;
                int lo = -hi;
                if (sign > 0)
                  lo--;
                else
                  hi++;
                if (IntNum.compare(int1, lo) >= 0
                    && IntNum.compare(int1, hi) <= 0)
                  return sign * int1.intValue();
              }
          }
      }
    return BAD_SHORT;
  }

  protected final gnu.bytecode.Type calculateType ()
  {
    return ! getHasValue() ? Type.voidType
      : binding == null ? Type.pointer_type : binding.getType();
  }

  protected <R,D> R visit (ExpVisitor<R,D> visitor, D d)
  {
    return visitor.visitSetExp(this, d);
  }

  protected <R,D> void visitChildren (ExpVisitor<R,D> visitor, D d)
  {
    new_value = visitor.visitAndUpdate(new_value, d);
  }
  public void print (OutPort out)
  {
    out.startLogicalBlock(isDefining () ? "(Define" : "(Set", ")", 2);
    out.writeSpaceFill();
    printLineColumn(out);
    if (symbol != null
        && (binding == null || symbol.toString() != binding.getName()))
      {
	out.print('/');
	out.print(symbol);
      }
    if (binding != null)
      {
	out.print('/');
	out.print(binding);
      }
    out.writeSpaceLinear();
    new_value.print(out);
    out.endLogicalBlock(")");
  }

  public String toString()
  {
    return "SetExp["+symbol+":="+new_value+']';
  }
}
