package gnu.q2.lang;
import gnu.mapping.*;
import kawa.standard.Scheme;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import gnu.text.*;
import gnu.kawa.io.InPort;
import gnu.kawa.lispexpr.*;

/** Support for the experimental Q2 language.
 * See the <a href="http://www.gnu.org/software/kawa/q2/">web site</a>
 * for information.
 */

public class Q2 extends Scheme
{
  static Q2 instance;
  static final Object emptyForm = new FString();

  public Q2 ()
  {
    environ = q2Environment;
    ModuleBody.setMainPrintValues(true);
  }

  protected Q2 (Environment env)
  {
    super(env);
  }

  protected static final SimpleEnvironment q2Environment
  = Environment.make("q2-environment", Scheme.kawaEnvironment);

  static
  {
    instance = new Q2();
    Environment saveEnv = Environment.setSaveCurrent(q2Environment);
    try
      {
	instance.initQ2();
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  public static Q2 getQ2Instance()
  {
    if (instance == null)
      new Q2 ();
    return instance;    
  }

  public void initQ2 ()
  {
    defSntxStFld(";", "gnu.q2.lang.Operator", "SEMI");
    defSntxStFld("+", "gnu.q2.lang.Operator", "PLUS");
    defSntxStFld("-", "gnu.q2.lang.Operator", "MINUS");
    defSntxStFld("*", "gnu.q2.lang.Operator", "STAR");
    defSntxStFld("/", "gnu.q2.lang.Operator", "SLASH");
    defSntxStFld("<", "gnu.q2.lang.Operator", "LT");
    defSntxStFld(">", "gnu.q2.lang.Operator", "GT");
    defSntxStFld("==", "gnu.q2.lang.Operator", "EQ");
    defSntxStFld("=<", "gnu.q2.lang.Operator", "LE");
    defSntxStFld(">=", "gnu.q2.lang.Operator", "GE");
    defSntxStFld(":=", "gnu.q2.lang.Operator", "ASSIGN");
  }

  public String getName()
  {
    return "Q2";
  }

  public LispReader getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    Q2Read lexer = new Q2Read(inp, messages);
    return lexer;
  }

  public String getCompilationClass () { return "gnu.q2.lang.Q2Translator"; }

  /*
  public Consumer getOutputConsumer(java.io.Writer out)
  {
    return new XMLPrinter(out, false);
  }
  */

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(new Q2());
  }

  public boolean appendBodyValues () { return true; }

  public Procedure getPrompter()
  {
    return new Prompter();
  }

  public ReadTable createReadTable ()
  {
    ReadTable rt = ReadTable.createInitial();
    rt.set('(', new Q2Read.ReadTableEntry());
    rt.set(';', new Q2Read.ReadTableEntry());
    ReaderDispatch rdispatch = ReaderDispatch.create(rt, false);
    rt.set('#', rdispatch);
    rdispatch.set(' ', ReaderIgnoreRestOfLine.getInstance());
    //For now, allow #t #f #!eof etc.
    //Also want to allow #!/ at start of file as comment start.
    //rdispatch.set('!', ReaderIgnoreRestOfLine.getInstance());
    rt.setFinalColonIsKeyword(true);
    return rt;
  }

  /** Compare two indentation amounts.
   * An indentation is @{code (numberOfTabs<<16)+numberOfSpaces}.
   * A comparison is indeterminate if it depends on tab-width - e.g.
   * the number of tabs in indentation1 is less than indentation2,
   * but it's the reverse when it comes to spaces.
   * @return Integer.MIN_VALUE if the comparison is indeterminate;
   *   otherwise returns the "difference" between the two (where
   *   tabs count as 8 spaces).
   */
  public static int compareIndentation (int indentation1, int indentation2)
  {
    int numTabs1 = indentation1 >>> 16;
    int numTabs2 = indentation1 >>> 16;
    int numSpaces1 = indentation1 & 0xFF;
    int numSpaces2 = indentation2 & 0xFF;
    if (numTabs1 == numTabs2)
      return numSpaces1 - numSpaces2;
    else if ((numTabs1 < numTabs2 && numSpaces1 <= numSpaces2)
             || (numTabs1 > numTabs2 && numSpaces1 >= numSpaces2))
      return 8 * (numTabs1 - numTabs2);
    else
      return Integer.MIN_VALUE;
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    return "#|Q2"+state+line+"|# ";
  }
}
