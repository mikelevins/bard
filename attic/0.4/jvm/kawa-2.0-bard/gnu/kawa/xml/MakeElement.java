// Copyright (c) 2001, 2003, 2004  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.xml.*;

public class MakeElement extends NodeConstructor {
    public static final MakeElement makeElementS = new MakeElement();
    static { makeElementS.setStringIsText(true); }

  public int numArgs() { return tag == null ? 0xFFFFF001 : 0xFFFFF000; }

  /** Optional tag.  If non-null, the element tag is this value,
   * rather than the first parameter. */
  public Symbol tag;

  public String toString() { return "makeElement["+tag+"]"; }

  public int copyNamespacesMode = XMLFilter.COPY_NAMESPACES_PRESERVE;

  private boolean handlingKeywordParameters;

  /** Should {@code KEYWORD: EXPRESSION} be mapped to an
   * attribute constructor? */
  public boolean isHandlingKeywordParameters ()
  {
    return handlingKeywordParameters;
  }

  public void setHandlingKeywordParameters (boolean value)
  {
    handlingKeywordParameters = value;
  }

  NamespaceBinding namespaceNodes;

  public NamespaceBinding getNamespaceNodes ()
  {
    return namespaceNodes;
  }

  public void setNamespaceNodes (NamespaceBinding bindings)
  {
    namespaceNodes = bindings;
  }

  public static Symbol getTagName (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length > 0)
      {
	Expression arg0 = args[0];
	if (arg0 instanceof QuoteExp)
	  {
	    Object val = ((QuoteExp) arg0).getValue();
	    if (val instanceof Symbol)
	      return (Symbol) val;
	  }
      }
    return null;
  }

  public static void startElement (Consumer out, Symbol qname,
                                   int copyNamespacesMode,
                                   NamespaceBinding namespaceNodes)
  {
    XName type = new XName((Symbol) qname, namespaceNodes);
    if (out instanceof XMLFilter)
      ((XMLFilter) out).copyNamespacesMode = copyNamespacesMode;
    out.startElement(type);
  }

  public static void startElement (Consumer out, Symbol qname,
                                   int copyNamespacesMode)
  {
    if (out instanceof XMLFilter)
      ((XMLFilter) out).copyNamespacesMode = copyNamespacesMode;
    out.startElement(qname);
  }

  public static void endElement (Consumer out, Object type/*FIXME:unused*/)
  {
    out.endElement();
  }

  public void apply (CallContext ctx)
  {
    Consumer saved = ctx.consumer;
    Consumer out = pushNodeContext(ctx);
    try
      {
        Symbol type = tag != null ? tag : (Symbol) ctx.getNextArg();
	if (namespaceNodes != null)
	  startElement(out, type, copyNamespacesMode, namespaceNodes);
	else
	  startElement(out, type, copyNamespacesMode);
	Object endMarker = Special.dfault;
	for (;;)
	  {
	    Object arg = ctx.getNextArg(endMarker);
	    if (arg == endMarker)
	      break;
            if (stringIsText)
                writeContentS(arg, out);
            else
                writeContent(arg, out);
            // Handling Keyword values is actually done by the Consumer.
            if (isHandlingKeywordParameters())
              out.endAttribute();
	  }
	endElement(out, type);
      }
    finally
      {
	popNodeContext(saved, ctx);
      }
  }

  public void compileToNode (ApplyExp exp, Compilation comp,
			     ConsumerTarget target)
  {
    Variable consumer = target.getConsumerVariable();
    Expression[] args = exp.getArgs();
    int nargs = args.length;
    CodeAttr code = comp.getCode();
    code.emitLoad(consumer);
    code.emitDup();
    int i;
    Target tagTarget = CheckedTarget.getInstance(Compilation.typeSymbol);
    if (tag == null)
      {
        args[0].compile(comp, tagTarget);
        i = 1;
      }
    else
      {
        comp.compileConstant(tag, tagTarget);
        i = 0;
      }
    code.emitDup(1, 1); // dup_x1
    // Stack:  consumer, tagtype, consumer, tagtype
    code.emitPushInt(copyNamespacesMode);
    if (namespaceNodes != null)
      {
	comp.compileConstant(namespaceNodes, Target.pushObject);
	code.emitInvokeStatic(startElementMethod4);
      }
    else
      code.emitInvokeStatic(startElementMethod3);
    for (;  i < nargs;  i++)
      {
        compileChild(args[i], stringIsText, comp, target);
        if (isHandlingKeywordParameters())
          {
            code.emitLoad(consumer);
            code.emitInvokeInterface(MakeAttribute.endAttributeMethod);
          }
      }
    code.emitInvokeStatic(endElementMethod);
  }

  public Type getReturnType (Expression[] args)
  {
    return Compilation.typeObject;
  }

  static final ClassType typeMakeElement
    = ClassType.make("gnu.kawa.xml.MakeElement");
  static final Method startElementMethod3
    = typeMakeElement.getDeclaredMethod("startElement", 3);
  static final Method startElementMethod4
    = typeMakeElement.getDeclaredMethod("startElement", 4);
  static final Method endElementMethod
    = typeMakeElement.getDeclaredMethod("endElement", 2);
}
