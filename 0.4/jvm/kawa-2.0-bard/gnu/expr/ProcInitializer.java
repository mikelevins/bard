package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class ProcInitializer extends Initializer
{
  LambdaExp proc;

  public ProcInitializer(LambdaExp lexp, Compilation comp, Field field)
  {
    this.field = field;
    proc = lexp;
    LambdaExp heapLambda = field.getStaticFlag() ? comp.getModule()
      : lexp.getOwningLambda() ;
    if (heapLambda instanceof ModuleExp && comp.isStatic())
      {
	next = comp.clinitChain;
	comp.clinitChain = this;
      }
    else
      {
	next = heapLambda.initChain;
	heapLambda.initChain = this;
      }
  }

  /** Create and load a ModuleMethod for the given procedure. */
  public static void emitLoadModuleMethod(LambdaExp proc, Compilation comp)
  {
    Declaration pdecl = proc.nameDecl;
    Object pname = pdecl == null ? proc.getName() : pdecl.getSymbol();
    ModuleMethod oldproc = null;
    if (comp.immediate && pname != null
        && pdecl != null && pdecl.context instanceof ModuleExp)
      {
        // In interactive mode allow dynamic rebinding of procedures.
        // If there is an existing ModuleMethod binding, re-use it.
        Environment env = Environment.getCurrent();
        Symbol sym = pname instanceof Symbol ? (Symbol) pname
          : Symbol.make("", pname.toString().intern());
        Object property = comp.getLanguage().getEnvPropertyFor(proc.nameDecl);
        Object old = env.get(sym, property, null);
        if (old instanceof ModuleMethod) {
                String moduleName =
                    ((ModuleMethod) old).module.getClass().getName();
                if (moduleName.startsWith(ModuleManager.interactiveClassPrefix)
                    || moduleName.equals(comp.moduleClass.getName()))
                    oldproc = (ModuleMethod) old;
            }
      }
    CodeAttr code = comp.getCode();
    ClassType procClass = proc.usingCallContext()
        ? Compilation.typeModuleMethodWithContext
        : Compilation.typeModuleMethod;
    Method initModuleMethod;
    if (oldproc == null)
      {
        code.emitNew(procClass);
        code.emitDup(1);
        initModuleMethod = procClass.getDeclaredMethod("<init>", 4);
      }
    else
      {
        comp.compileConstant(oldproc, Target.pushValue(procClass));
        initModuleMethod = Compilation.typeModuleMethod.getDeclaredMethod("init", 4);
      }
    LambdaExp owning = proc.getNeedsClosureEnv() ? proc.getOwningLambda()
      : comp.getModule();
    if (owning instanceof ClassExp && owning.staticLinkField != null)
      code.emitLoad(code.getCurrentScope().getVariable(1));
    else if (! (owning instanceof ModuleExp)
	|| (comp.moduleClass == comp.mainClass
	    && ! comp.method.getStaticFlag()))
      code.emitPushThis();
    else
      {
        if (comp.moduleInstanceVar == null || comp.moduleInstanceVar.dead())
	  {
	    comp.moduleInstanceVar
	      = code.locals.current_scope.addVariable(code,
						      comp.moduleClass,
						      "$instance");
	    if (comp.moduleClass != comp.mainClass && ! comp.isStatic())
	      {
                code.emitNew(comp.moduleClass);
                code.emitDup(comp.moduleClass);
                code.emitInvokeSpecial(comp.moduleClass.constructor);
		comp.moduleInstanceMainField = 
		  comp.moduleClass.addField("$main", comp.mainClass, 0);
		code.emitDup(comp.moduleClass);
		code.emitPushThis();
		code.emitPutField(comp.moduleInstanceMainField);
	      }
            else
              code.emitGetStatic(comp.moduleInstanceMainField);
	    code.emitStore(comp.moduleInstanceVar);
	  }
	code.emitLoad(comp.moduleInstanceVar);
      }
    code.emitPushInt(proc.getSelectorValue(comp));
    comp.compileConstant(pname, Target.pushObject);
    // If there are keyword arguments, we treat that as "unlimited" maxArgs,
    // so that ModuleBody.matchX methods call matchN.  A kludge, I guess.
    code.emitPushInt(proc.min_args
                     | ((proc.keywords == null ? proc.max_args : -1) << 12));
    code.emitInvoke(initModuleMethod);

    if (proc.properties != null)
      {
	int len = proc.properties.length;
	for (int i = 0;  i < len;  i += 2)
	  {
	    Object key = proc.properties[i];
	    // Skip "name" property since we've taken care of that specially.
	    if (key != null && key != PropertySet.nameKey)
	      {
		Object val = proc.properties[i+1];
		code.emitDup(1);
		comp.compileConstant(key);
                Target target = Target.pushObject;
                if (val instanceof Expression)
                  ((Expression) val).compile(comp, target);
                else
                  comp.compileConstant(val, target);
		Method m = (ClassType.make("gnu.mapping.PropertySet")
			    .getDeclaredMethod("setProperty", 2));
		code.emitInvokeVirtual(m);
	      }
	  }
      }
  }

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();

    emitLoadModuleMethod(proc, comp);

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }

  public void reportError (String message, Compilation comp)
  {
    String saveFile = comp.getFileName();
    int saveLine = comp.getLineNumber();
    int saveColumn = comp.getColumnNumber();
    comp.setLocation(proc);
    String name = proc.getName();
    StringBuffer sbuf = new StringBuffer(message);
    if (name == null)
      sbuf.append("unnamed procedure");
    else
      {
        sbuf.append("procedure ");
        sbuf.append(name);
      }
    comp.error('e', sbuf.toString());
    comp.setLine(saveFile, saveLine, saveColumn);
  }
}
