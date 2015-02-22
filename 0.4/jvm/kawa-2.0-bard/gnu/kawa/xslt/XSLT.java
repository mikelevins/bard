// Copyright (c) 2002, 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xslt;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.xquery.lang.*;
import gnu.kawa.io.InPort;
import gnu.kawa.xml.*;

/** New Kawa language XSLT (XML Stylesheet Language Tranformations). */

public class XSLT extends XQuery
{
  // This field need to be public so that the findLiteral method in
  // gnu.expr.LitTable can find it.
  public static XSLT instance;

  public String getName()
  {
    return "XSLT";
  }

  public XSLT ()
  {
    instance = this;
    ModuleBody.setMainPrintValues(true);
  }

  public static XSLT getXsltInstance()
  {
    if (instance == null)
      new XSLT ();
    return instance;
  }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    return new XslTranslator(inp, messages, this);
  }

  public boolean parse (Compilation comp, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    ((XslTranslator) comp.lexer).parse(comp);
    comp.setState(Compilation.BODY_PARSED);

    XQParser xqparser = new XQParser(null, comp.getMessages(), this);
    XQResolveNames resolver = new XQResolveNames(comp);
    resolver.functionNamespacePath = xqparser.functionNamespacePath;
    resolver.parser = xqparser;
    resolver.resolveModule(comp.mainLambda);

    return true;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(new XSLT());
  }

  public static void defineCallTemplate(Symbol name,
					double priority,
					Procedure template)
  {
  }

  public static Symbol nullMode = Symbol.make(null, "");

  public static void defineApplyTemplate(String pattern,
					 double priority,
					 Symbol mode,
					 Procedure template)
  {
    if (mode == null)
      mode = nullMode;
    TemplateTable table = TemplateTable.getTemplateTable(mode);
    table.enter(pattern, priority, template);
  }

  public static void defineTemplate(Symbol name, String pattern,
				    double priority, Symbol mode,
				    Procedure template)
  {
    if (name != null)
      defineCallTemplate(name, priority, template);
    if (pattern != null)
      defineApplyTemplate(pattern, priority, mode, template);
  }

  public static void process(TreeList doc, Focus pos, CallContext ctx)
    throws Throwable
  {
    Consumer out = ctx.consumer;
    for (;;)
      {
	int ipos = pos.ipos;
	int kind = doc.getNextKind(ipos);
        Object type;
        String name;
        Procedure proc;
	switch (kind)
	  {
	  case Sequence.DOCUMENT_VALUE:
	    ipos = doc.firstChildPos(ipos);
	    break;
	  case Sequence.ELEMENT_VALUE:
	    type = pos.getNextTypeObject();
	    name = pos.getNextTypeName();
	    proc = TemplateTable.nullModeTable.find(name);
	    if (proc != null)
	      {
		proc.check0(ctx);
		ctx.runUntilDone();
	      }
	    else
	      {
		out.startElement(type);
                int child = doc.firstAttributePos(ipos);
                if (child == 0)
                  child = doc.firstChildPos(ipos);
		pos.push(doc, child);
		process(doc, pos, ctx);
		pos.pop();
		out.endElement();
	      }
	    ipos = doc.nextDataIndex(ipos >>> 1) << 1;
	    pos.gotoNext();
	    break;
	  case Sequence.ATTRIBUTE_VALUE:
	    type = pos.getNextTypeObject();
	    name = pos.getNextTypeName();
	    proc = TemplateTable.nullModeTable.find("@"+name);
            if (proc != null)
	      {
		proc.check0(ctx);
		ctx.runUntilDone();
                break;
	      }
            // else fall through ...
	  case Sequence.CHAR_VALUE:
	    int ichild = ipos >>> 1;
	    int next = doc.nextNodeIndex(ichild, -1 >>> 1);
	    if (ichild == next)
	      next = doc.nextDataIndex(ichild);
	    doc.consumeIRange(ichild, next, out);
	    ipos = next << 1;
	    break;
          case Sequence.COMMENT_VALUE:
          case Sequence.PROCESSING_INSTRUCTION_VALUE:
	    ipos = doc.nextDataIndex(ipos >>> 1) << 1;
            break;
	  case Sequence.TEXT_BYTE_VALUE: // FIXME
	  case Sequence.OBJECT_VALUE: // FIXME
	  default:
	    return;
	  }
	pos.ipos = ipos;
      }
  }

  public static void runStylesheet()
    throws Throwable
  {
    CallContext ctx = CallContext.getInstance();
    ApplicationMainSupport.processSetProperties();
    String[] args = ApplicationMainSupport.commandLineArgArray;
    for (int i = 0;  i < args.length;  i++)
      {
	String arg = args[i];
	KDocument doc = Document.parse(arg);
	Focus pos = Focus.getCurrent();
	pos.push(doc.sequence, doc.ipos);
	process((TreeList) doc.sequence, pos, ctx);
      }
  }
}
