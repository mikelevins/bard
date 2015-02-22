package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.text.*;

public class BindingInitializer extends Initializer
{
  Declaration decl;
  Expression value;

  /** Create a BindingInitializer and link it into the correct
   * initializer chain. */
  public static void create (Declaration decl, Expression value,
                             Compilation comp)
  {
    BindingInitializer init = new BindingInitializer(decl, value);
    if (decl.field != null ? decl.field.getStaticFlag() : decl.isStatic())
      {
        init.next = comp.clinitChain;
        comp.clinitChain = init;
      }
    else
      {
        ModuleExp mod = comp.getModule();
        init.next = mod.initChain; // FIXME why mainLambda?
        mod.initChain = init;
      }
  }

  public BindingInitializer(Declaration decl, Expression value)
  {
    this.decl = decl;
    this.value = value;
    this.field = decl.field;
  }

  public void emit(Compilation comp)
  {
    if (decl.ignorable())
      {
        if (value != null)
          value.compile(comp, Target.Ignore);
        return;
      }
    CodeAttr code = comp.getCode();

    if (value instanceof QuoteExp)
      {
	Object val = ((QuoteExp) value).getValue();
	if (val != null && ! (val instanceof String))
	  {
	    Literal lit = comp.litTable.findLiteral(val);
	    if (lit.field == this.field)
	      return;
	  }
      }

    int line = decl.getLineNumber();
    SourceMessages messages = comp.getMessages();
    SourceLocator saveLoc = messages.swapSourceLocator(decl);
    if (line > 0)
      code.putLineNumber(decl.getFileName(), line);
    if (field != null && ! field.getStaticFlag())
      code.emitPushThis();

    if (value == null)
      {
	boolean func = comp.getLanguage().hasSeparateFunctionNamespace();
	Object property
	  = func && decl.isProcedureDecl() ? EnvironmentKey.FUNCTION : null;

	Object name = decl.getSymbol();

	if (decl.getFlag(Declaration.IS_UNKNOWN|Declaration.IS_DYNAMIC))
          {
            if (name instanceof String)
              name = Namespace.EmptyNamespace.getSymbol((String) name);
            comp.compileConstant(name, Target.pushObject);
            if (property == null)
              code.emitPushNull();
            else
              comp.compileConstant(property, Target.pushObject);
            code.emitInvokeStatic(typeDynamicLocation.getDeclaredMethod("getInstance", 2));
          }
	else if (decl.isFluid())
          {
            // This is basically an optimization, since if we don't initialize
            // to an anonymous ThreadLocation, then a fluid-let will lazily
            // do it - in NamedLocation.setWithSave.
            Type[] atypes = new Type[1];
            atypes[0] = name instanceof Symbol ? Compilation.typeSymbol
              : Type.toStringType;
            comp.compileConstant(name, Target.pushObject);
            Method m = typeThreadLocation
              .getDeclaredMethod("makeAnonymous", atypes);
            code.emitInvokeStatic(m);
          }
        else
          {
            if (name instanceof String)
              name = Namespace.EmptyNamespace.getSymbol(((String)name).intern());
            comp.compileConstant(name, Target.pushObject);
            code.emitInvokeStatic(Compilation.typeLocation.getDeclaredMethod("define", 1));
          }
      }
    else
      {
        Type type = field == null ? decl.getType() : field.getType();
        value.compileWithPosition(comp, StackTarget.getInstance(type));
      }

    // Optimization of Declaration.compileStore, to avoid swap.
    if (field == null)
      {
	Variable var = decl.getVariable();
	if (var == null)
            var = decl.allocateVariable(code, true);
	code.emitStore(var);
      }
    else if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
    messages.swapSourceLocator(saveLoc);
  }

    static final ClassType typeThreadLocation =
      ClassType.make("gnu.mapping.ThreadLocation");
    static final ClassType typeDynamicLocation =
      ClassType.make("gnu.mapping.DynamicLocation");

  public static Method makeLocationMethod (Object name)
  {
    Type[] atypes = new Type[1];
    if (name instanceof Symbol)
      atypes[0] = Compilation.typeSymbol;
    else
      atypes[0] = Type.javalangStringType;
    return Compilation.typeLocation.getDeclaredMethod("make", atypes);
  }
}
