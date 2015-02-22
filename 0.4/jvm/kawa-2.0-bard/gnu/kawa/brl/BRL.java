package gnu.kawa.brl;
import gnu.mapping.*;
import kawa.standard.Scheme;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.kawa.lispexpr.ReadTable;

public class BRL extends Scheme
{
  // The following two fields need to be public so that the findLiteral
  // method in gnu.expr.LitTable can find them.
  /** Language instance for KRL dialect. */
  public static final BRL krl_instance;
  /** Language instance for BRL dialect. */
  public static final BRL brl_instance;
  static final Object emptyForm = new FString();

  protected static final SimpleEnvironment brlEnvironment
    = Environment.make("brl-environment", Scheme.kawaEnvironment);

  static BRLReaderString brlReader =  new BRLReaderString();

  static
  {
    krl_instance = new BRL(brlEnvironment);
    brl_instance = new BRL(brlEnvironment);
    brl_instance.setBrlCompatible(true);
    Environment saveEnv = Environment.setSaveCurrent(brlEnvironment);
    try
      {
        krl_instance.initBRL();
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  protected BRL (Environment env)
  {
    super(env);
  }

  void initBRL ()
  {
    ModuleBody.setMainPrintValues(true);
    try
      {
	loadClass("gnu.brl.stringfun");
	loadClass("gnu.kawa.brl.progfun");
	loadClass("gnu.kawa.servlet.HTTP");
      }
    catch (Exception ex)
      {
	System.err.println("caught "+ex);
      }
  }

  public static Language getInstance(boolean brlCompatible)
  {
    return brlCompatible ? getBrlInstance() : getBrlInstance();
  }

  public static BRL getKrlInstance()
  {
    return krl_instance;
  }

  public static BRL getBrlInstance()
  {
    return brl_instance;
  }

  boolean brlCompatible = false;

  public boolean isBrlCompatible() { return brlCompatible; }
  public void setBrlCompatible(boolean compat) {  brlCompatible = compat; }

  public boolean appendBodyValues () { return ! isBrlCompatible(); }

  public gnu.kawa.lispexpr.LispReader getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    BRLRead lexer = new BRLRead(inp, messages);
    lexer.setBrlCompatible(isBrlCompatible());
    return lexer;
  }

  public Consumer getOutputConsumer(java.io.Writer out)
  {
    if (isBrlCompatible())
      return super.getOutputConsumer(out);
    return new XMLPrinter(out, false);
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(getKrlInstance());
  }

  public ReadTable createReadTable ()
  {
    ReadTable rt = super.createReadTable();
    if (isBrlCompatible())
      rt.setInitialColonIsKeyword(true);
    rt.setBracketMode(1);
    rt.set(']', brlReader);
    return rt;
  }

  public Procedure getPrompter()
  {
    return new Prompter();
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    if (state == ']')
      return "<!--BRL:"+line+"-->";
    else
      {
	if (state == '\n')
	  state = '-';
	return "#|--BRL:"+line+state+"|#";
      }
  }
}
