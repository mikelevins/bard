// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import java.lang.reflect.*;

/** Call a specified method in in a ModuleBody.
 * We use an extra level of indirection, but we save by having
 * to create fewer classes than in the one-class-per-procedure
 * scheme, without having to use (slow) reflection.
 */

public class ModuleMethod extends MethodProc
{
  public ModuleBody module;
  public int selector;
  protected int numArgs;

  public ModuleMethod(ModuleBody module, int selector,
                      Object name, int numArgs)
  {
    init(module, selector, name, numArgs);
  }

  public ModuleMethod (ModuleBody module, int selector,
		       Object name, int numArgs, Object argTypes)
  {
    init(module, selector, name, numArgs);
    this.argTypes = argTypes;
  }

  public ModuleMethod init(ModuleBody module, int selector,
                           Object name, int numArgs)
  {
    this.module = module;
    this.selector = selector;
    this.numArgs = numArgs;
    if (name != null)
      setSymbol(name);
    return this;
  }

  /** Figure out parameter types.
   * Uses reflection to get method parameter types.
   * INCOMPLETE - does not handle procedures with optional or rest args. */
  protected void resolveParameterTypes()
  {
    Method method = null;
    String name = getName();
    if (name != null)
      {
        try
          {
            Class moduleClass = module.getClass();
            Method[] methods = moduleClass.getDeclaredMethods();
            String mangledName = Compilation.mangleNameIfNeeded(name);
            for (int i = methods.length;  --i >= 0; )
              {
                if (methods[i].getName().equals(mangledName))
                  {
                    if (method != null)
                      {
                        method = null;
                        break;
                      }
                    method = methods[i];
                  }
              }
            if (method != null)
              {
                Language lang = Language.getDefaultLanguage();
                if (lang != null)
                  {
                    Class[] parameterClasses = method.getParameterTypes();
                    int numParamTypes = parameterClasses.length;
                    gnu.bytecode.Type[] atypes = new gnu.bytecode.Type[numParamTypes];
                    for (int i = numParamTypes;  --i >= 0; )
                      {
                        atypes[i] = lang.getTypeFor(parameterClasses[i]);
                      }
                    this.argTypes = atypes;
                  }
              }
          }
        catch (Exception ex)
          {
          }
      }
    if (argTypes == null)
      super.resolveParameterTypes();
  }

  public int numArgs() { return numArgs; }

  public int match0 (CallContext ctx)
  {
    ctx.count = 0;
    ctx.where = 0;
    return module.match0(this, ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    ctx.count = 1;
    ctx.where = CallContext.ARG_IN_VALUE1;
    return module.match1(this, arg1, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    ctx.count = 2;
    ctx.where = CallContext.ARG_IN_VALUE1
      |(CallContext.ARG_IN_VALUE2<<4);
    return module.match2(this, arg1, arg2, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    ctx.count = 3;
    ctx.where = CallContext.ARG_IN_VALUE1
      |(CallContext.ARG_IN_VALUE2<<4)
      |(CallContext.ARG_IN_VALUE3<<8);
    return module.match3(this, arg1, arg2, arg3, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    ctx.count = 4;
    ctx.where = (CallContext.ARG_IN_VALUE1
		 |(CallContext.ARG_IN_VALUE2<<4)
		 |(CallContext.ARG_IN_VALUE3<<8)
		 |(CallContext.ARG_IN_VALUE4<<12));
    return module.match4(this, arg1, arg2, arg3, arg4, ctx);
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    ctx.count = args.length;
    ctx.where = 0;
    return module.matchN(this, args, ctx);
  }

  public void apply (CallContext ctx)
    throws Throwable
  {
    // This method does not get called for methods compiled with
    // --full-tailcalls, since their match methods set ctx.proc to the
    // ModuleWithContext, rather than this ModuleMethod.
    // We get here for methods compiled with the --no-full-tailcalls option.
    // In that case the compiler-generated matchX methods set ctx.pc
    // to 0...5 to indicate where the arguments were saved.
    // It would be more consistent to set ctx.where and ctx.count, though
    // a simple switch of ctx.pc is faster.  But how robust is this?
    Object result;
    switch (ctx.pc)
      {
      case 0:
	result = apply0();
	break;
      case 1:
	result = apply1(ctx.value1);
	break;
      case 2:
	result = apply2(ctx.value1, ctx.value2);
	break;
      case 3:
	result = apply3(ctx.value1, ctx.value2, ctx.value3);
	break;
      case 4:
	result = apply4(ctx.value1, ctx.value2, ctx.value3, ctx.value4);
	break;
      case 5:
	result = applyN(ctx.values);
	break;
      default:
	throw new Error("internal error - apply "+this);
      }
    ctx.writeValue(result);
  }

  public Object apply0()
    throws Throwable
  {
    return module.apply0(this);
  }

  public Object apply1(Object arg1)
    throws Throwable
  {
    return module.apply1(this, arg1);
  }

  public Object apply2(Object arg1, Object arg2)
    throws Throwable
  {
    return module.apply2(this, arg1, arg2);
  }

  public Object apply3(Object arg1, Object arg2, Object arg3)
    throws Throwable
  {
    return module.apply3(this, arg1, arg2, arg3);
  }

  public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4)
    throws Throwable
  {
    return module.apply4(this, arg1, arg2, arg3, arg4);
  }

  public Object applyN(Object[] args)
    throws Throwable
  {
    return module.applyN(this, args);
  }

  public static Object apply0Default(ModuleMethod method)
    throws Throwable
  {
    return method.module.applyN(method, Values.noArgs);
  }

  public static Object apply1Default(ModuleMethod method, Object arg1)
    throws Throwable
  {
    Object[] args = new Object[1];
    args[0] = arg1;
    return method.module.applyN(method, args);
  }

  public static Object apply2Default(ModuleMethod method, Object arg1, Object arg2)
    throws Throwable
  {
    Object[] args = new Object[2];
    args[0] = arg1;
    args[1] = arg2;
    return method.module.applyN(method, args);
  }

  public static Object apply3Default(ModuleMethod method,
                       Object arg1, Object arg2, Object arg3)
    throws Throwable
  {
    Object[] args = new Object[3];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    return method.module.applyN(method, args);
  }

  public static Object apply4Default(ModuleMethod method,
                       Object arg1, Object arg2, Object arg3, Object arg4)
    throws Throwable
  {
    Object[] args = new Object[4];
    args[0] = arg1;
    args[1] = arg2;
    args[2] = arg3;
    args[3] = arg4;
    return method.module.applyN(method, args);
  }

  public static Object applyNDefault(ModuleMethod method, Object[] args)
    throws Throwable
  {
    int count = args.length;
    int num = method.numArgs();
    ModuleBody module = method.module;
    if (count >= (num & 0xFFF)
	&& (num < 0 || count <= (num >> 12)))
      {
        switch (count)
          {
          case 0:
            return module.apply0(method);
          case 1:
            return module.apply1(method, args[0]);
          case 2:
            return module.apply2(method, args[0], args[1]);
          case 3:
            return module.apply3(method, args[0], args[1], args[2]);
          case 4:
            return module.apply4(method, args[0], args[1], args[2], args[3]);
          }
      }
    throw new WrongArguments(method, count);
  }

  /** Helper methods for default ModuleBody actions. */

  public static void applyError()
  {
    throw new Error("internal error - bad selector");
  }
}
