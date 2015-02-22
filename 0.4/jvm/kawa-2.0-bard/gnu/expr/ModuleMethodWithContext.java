// Copyright (c) 2011 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;

/** A ModuleMethod compiled to pass in the CallContext.
 * I.e. with getCallConvention() returning CALL_WITH_CONSUMER or greater.
 */

public class ModuleMethodWithContext extends ModuleMethod {

    public ModuleMethodWithContext(ModuleBody module, int selector,
                                   Object name, int numArgs) {
        super(module, selector, name, numArgs);
    }

    public ModuleMethodWithContext(ModuleBody module, int selector,
                                   Object name, int numArgs, Object argTypes) {
        super(module, selector, name, numArgs, argTypes);
    }

    public void apply (CallContext ctx) throws Throwable {
 	ctx.pc = selector;
        module.apply(ctx);
    }

    public Object apply0() throws Throwable {
        CallContext ctx = CallContext.getInstance();
        check0(ctx);
        return ctx.runUntilValue();
    }

    public Object apply1(Object arg1) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        check1(arg1, ctx);
        return ctx.runUntilValue();
    }

    public Object apply2(Object arg1, Object arg2) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        check2(arg1, arg2, ctx);
        return ctx.runUntilValue();
    }

    public Object apply3(Object arg1, Object arg2, Object arg3)
        throws Throwable {
        CallContext ctx = CallContext.getInstance();
        check3(arg1, arg2, arg3, ctx);
        return ctx.runUntilValue();
    }

    public Object apply4(Object arg1, Object arg2, Object arg3, Object arg4)
        throws Throwable {
        CallContext ctx = CallContext.getInstance();
        check4(arg1, arg2, arg3, arg4, ctx);
        return ctx.runUntilValue();
    }

    public Object applyN(Object[] args) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        checkN(args, ctx);
        return ctx.runUntilValue();
    }
}
