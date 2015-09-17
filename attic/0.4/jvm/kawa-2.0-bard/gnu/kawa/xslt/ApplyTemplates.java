// Copyright (c) 2013  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xslt;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.ClassType;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.lists.Consumer;
import gnu.lists.TreeList;
import gnu.mapping.CallContext;
import gnu.mapping.Symbol;

/** Implements the XSLT apply-templates command. */

public class ApplyTemplates extends NodeConstructor {

    public static final ApplyTemplates applyTemplatesProc
        = new ApplyTemplates();

    public int numArgs() { return 0x2002; }

    public static void applyTemplates$C(String select, Symbol mode,
                                        Consumer out) throws Throwable {
        CallContext ctx = CallContext.getInstance();
        Consumer save = ctx.consumer;
        ctx.consumer = out;
        try {
            applyTemplates$X(select, mode, ctx);
        } finally {
            ctx.consumer = save;
        }
    }

    public static void applyTemplates$X(String select, Symbol mode, CallContext ctx)
        throws Throwable {
        if (mode == null)
            mode = XSLT.nullMode;
        TemplateTable table = TemplateTable.getTemplateTable(mode);
        Focus pos = Focus.getCurrent();
        TreeList doc = (TreeList) pos.sequence;
        Object cur = doc.getPosNext(pos.ipos);
        pos.push(doc, doc.firstChildPos(pos.ipos));
        XSLT.process(doc, pos, ctx);
        pos.pop();
    }

    public void apply(CallContext ctx) throws Throwable {
        applyTemplates$X((String) ctx.getNextArg(null),
                         (Symbol) ctx.getNextArg(null),
                         ctx);
    }

    public void compileToNode(ApplyExp exp, Compilation comp,
                              ConsumerTarget target) {
        CodeAttr code = comp.getCode();
        Expression[] args = exp.getArgs();
        args[0].compile(comp, Target.pushObject);
        args[1].compile(comp, Target.pushObject);
        String mname;
        if (target.isContextTarget()) {
            comp.loadCallContext();
            mname = "applyTemplates$X";
        } else {
            code.emitLoad(target.getConsumerVariable());
            mname = "applyTemplates$C";
        }
        code.emitInvokeStatic(ClassType.make("gnu.kawa.xslt.ApplyTemplates")
                              .getDeclaredMethod(mname, 3));
    }

}
