// Copyright (c) 2001, 2003  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.xml.*;
import gnu.lists.*;
import java.util.List;

public abstract class NodeConstructor extends MethodProc
implements Inlineable
{
  public abstract void compileToNode (ApplyExp exp, Compilation comp,
				      ConsumerTarget target);

    /** If true, top-level strings are treated as text nodes.
     * This means don't separate them with spaces when printing as XML.
     */
    public void setStringIsText(boolean stringIsText) {
        this.stringIsText = stringIsText;
    }

    protected boolean stringIsText;

  public static XMLFilter pushNodeConsumer (Consumer out)
  {
    if (out instanceof XMLFilter)
      return (XMLFilter) out;
    else
      return new XMLFilter(new NodeTree());
  }

  public static void popNodeConsumer (Consumer saved, Consumer current)
  {
    if (saved != current)
      saved.writeObject(current instanceof XMLFilter
                        ? (Object) KNode.make((NodeTree) ((XMLFilter) current).out)
                        : (Object) current);
  }

  public static XMLFilter pushNodeContext (CallContext ctx)
  {
    Consumer out = ctx.consumer;
    if (out instanceof XMLFilter)
      return (XMLFilter) out;
    else
      {
        // FIXME: It would be more efficinet to just do:
        // filter = new XMLFilter(out);
        // There is at least one problem "JOINER" isn't handled properly;
        // it's not obvious how to ensure we get the right whitespace.
	XMLFilter filter = new XMLFilter(new NodeTree());
	ctx.consumer = filter;
	return filter;
      }
  }

  public static void popNodeContext (Consumer saved, CallContext ctx)
  {
    Object current = ctx.consumer;
    if (saved != current)
      {
	if (current instanceof XMLFilter)
	  current = KNode.make((NodeTree) ((XMLFilter) current).out);
	saved.writeObject(current);
	ctx.consumer = saved;
      }
  }

    public static void compileChild (Expression arg, boolean stringIsText,
                                     Compilation comp, ConsumerTarget target) {
        if (arg instanceof ApplyExp) {
            ApplyExp app = (ApplyExp) arg;
            Expression func = app.getFunction();
            Object proc = func.valueIfConstant();
            // Don't call compileToNode if child is a MakeText, because
            // XQuery's rules for space-separating computed text nodes are
            // non-trivial and context-dependent.
            if (proc instanceof NodeConstructor
                && ! (proc instanceof MakeText)) {
                ((NodeConstructor) proc).compileToNode(app, comp, target);
                return;
            }
        }
        CodeAttr code = comp.getCode();
        if (arg instanceof QuoteExp) {
            Object value = ((QuoteExp) arg).getValue();
            if (value instanceof FString) {
                code.emitLoad(target.getConsumerVariable());
                code.emitPushString(value.toString());
                code.emitInvoke(Compilation.typeConsumer
                                .getDeclaredMethod("write",
                                    new Type[] { Type.javalangStringType }));
                return;
          }
        }
        arg.compileWithPosition(comp, Target.pushObject);
        code.emitLoad(target.getConsumerVariable());
        code.emitInvokeStatic(ClassType.make("gnu.kawa.xml.NodeConstructor")
                              .getDeclaredMethod(stringIsText ? "writeContentS"
                                                 : "writeContent", 2));
    }

  /** Compile an expression using a fresh NodeTree.
   * Compare with ConsumerTarget.compileUsingConsumer, but creates a NodeTree.
   */
  public static void compileUsingNodeTree(Expression exp,
					  Compilation comp, Target target)
  {
    Method makeMethod = typeNodeConstructor.getDeclaredMethod("makeNode", 0);
    Method makeKNodeMethod = typeNodeConstructor.getDeclaredMethod("finishNode", 1);
    ConsumerTarget.compileUsingConsumer(exp, comp, target,
					makeMethod, makeKNodeMethod);
  }

  public static XMLFilter makeNode ()
  {
    return new XMLFilter(new NodeTree());
  }

  public static KNode finishNode (XMLFilter filter)
  {
    return KNode.make((NodeTree) filter.out);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    if (target instanceof IgnoreTarget)
      ApplyExp.compile(exp, comp, target);
    else if (! (target instanceof ConsumerTarget))
      compileUsingNodeTree(exp, comp, target);
    else
      {
	ConsumerTarget ctarget = (ConsumerTarget) target;
	Variable cvar = ctarget.getConsumerVariable();
	Type ctype = cvar.getType();
	if (ctype.isSubtype(typeXMLFilter))
	  compileToNode(exp, comp, ctarget);
	else
	  {
	    Expression[] args = exp.getArgs();
	    int nargs = args.length;
	    CodeAttr code = comp.getCode();
	    Scope scope = code.pushScope();
            Variable xvar
	      = scope.addVariable(code, typeXMLFilter, null);
	    if (ctarget.isContextTarget())
	      {
		comp.loadCallContext();
		code.emitInvokeStatic(pushNodeContextMethod);
	      }
	    else
	      {
		code.emitLoad(cvar);
		code.emitInvokeStatic(pushNodeConsumerMethod);
	      }
	    code.emitStore(xvar);
	    code.emitTryStart(true, Type.void_type);
            ConsumerTarget xtarget = new ConsumerTarget(xvar);
	    compileToNode(exp, comp, xtarget);
	    code.emitTryEnd();
	    code.emitFinallyStart();
	    code.emitLoad(cvar);
	    if (ctarget.isContextTarget())
	      {
		comp.loadCallContext();
		code.emitInvokeStatic(popNodeContextMethod);
	      }
	    else
	      {
		code.emitLoad(xvar);
		code.emitInvokeStatic(popNodeConsumerMethod);
	      }
	    code.emitFinallyEnd();
	    code.emitTryCatchEnd();
	    code.popScope();
	  }
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

    public static void writeContentS(Object arg, Consumer out) {
          if (arg instanceof CharSequence && ! (arg instanceof UnescapedData)) {
                CharSequence carg = (CharSequence) arg;
                out.write(carg, 0, carg.length());
            }
          else
              writeContent(arg, out);
    }
    public static void writeContent(Object arg, Consumer out) {
        if (arg instanceof List  && ! (arg instanceof CharSequence)) {
            for (Object e : (List) arg) {
                writeContent1(e, out);
            }
        }
        else
            writeContent1(arg, out);
    }

    protected static void writeContent1(Object arg, Consumer out) {
        if (arg instanceof Consumable)
            ((Consumable) arg).consume(out);
        else
            Values.writeValues(arg, out);
    }

  static final ClassType typeXMLFilter
    = ClassType.make("gnu.xml.XMLFilter");
  static final ClassType typeKNode
    = ClassType.make("gnu.kawa.xml.KNode");
  static final ClassType typeNodeConstructor
    = ClassType.make("gnu.kawa.xml.NodeConstructor");
  static final Method pushNodeContextMethod
    = typeNodeConstructor.getDeclaredMethod("pushNodeContext", 1);
  static final Method popNodeContextMethod
    = typeNodeConstructor.getDeclaredMethod("popNodeContext", 2);
  static final Method pushNodeConsumerMethod
    = typeNodeConstructor.getDeclaredMethod("pushNodeConsumer", 1);
  static final Method popNodeConsumerMethod
    = typeNodeConstructor.getDeclaredMethod("popNodeConsumer", 2);
}
