package gnu.jemacs.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.*;
import kawa.standard.Scheme;
import gnu.bytecode.Type;
import gnu.commonlisp.lang.*;
import gnu.kawa.lispexpr.*;
import gnu.kawa.functions.Arithmetic;

public class ELisp extends Lisp2
{
  static boolean charIsInt = false;

  /** Get a ELisp character object. */
  public static Object getCharacter(int c)
  {
    if (charIsInt)
      return gnu.math.IntNum.make(c);
    else
      return Char.make((char)c);
  }

  public static gnu.math.Numeric asNumber(Object arg)
  {
    if (arg instanceof Char)
      return gnu.math.IntNum.make(((Char) arg).intValue());
    if (arg instanceof javax.swing.text.Position)
      return gnu.math.IntNum.make(1 + ((javax.swing.text.Position) arg).getOffset());
    return Arithmetic.asNumeric(arg);
  }

  public static int asChar (Object x)
  {
    if (x instanceof Char)
      return ((Char) x).intValue();
    int i;
    if (x instanceof gnu.math.Numeric)
      i = ((gnu.math.Numeric) x).intValue();
    else if (x instanceof javax.swing.text.Position)
      i = ((javax.swing.text.Position) x).getOffset() + 1;
    else
      i = -1;
    if (i < 0 || i >= (1<<20))
      throw new gnu.jemacs.buffer.Signal("error", "not a character value");
    return i;
  }

  public String getName()
  {
    return "Emacs-Lisp";
  }

  static final ELisp instance;

  public static final Environment elispEnvironment
    = Environment.make("elisp-environment");

  static
  {
    instance = new ELisp();

    instance.define("t", TRUE);
    instance.define("nil", FALSE);
    Environment saveEnv = Environment.setSaveCurrent(elispEnvironment);
    try
      {
        instance.initELisp();
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  ELisp ()
  {
    environ = elispEnvironment;
  }

  private void initELisp ()
  {
    try
      {
	// Force it to be loaded now, so we can over-ride let* length etc.
	loadClass("gnu.commonlisp.lisp.PrimOps");
	loadClass("gnu.jemacs.lang.NumberOps");
	loadClass("gnu.jemacs.lang.MiscOps");

        defProcStFld("emacs", "gnu.jemacs.buffer.emacs");
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }

    defSntxStFld("if", "gnu.jemacs.lang.MiscOps", "if");
    defProcStFld("invoke", "gnu.kawa.reflect.Invoke", "invoke");

    defProcStFld("+", "gnu.jemacs.lang.AddOp", "$Pl");
    defProcStFld("-", "gnu.jemacs.lang.AddOp", "$Mn");
    defProcStFld("/", "gnu.jemacs.lang.DivideOp", "$Sl");
    defProcStFld("=", "gnu.jemacs.lang.NumberCompare", "$Eq");
    defProcStFld("<", "gnu.jemacs.lang.NumberCompare", "$Ls");
    defProcStFld(">", "gnu.jemacs.lang.NumberCompare", "$Gr");
    defProcStFld("<=", "gnu.jemacs.lang.NumberCompare", "$Ls$Eq");
    defProcStFld(">=", "gnu.jemacs.lang.NumberCompare", "$Gr$Eq");

    defun("self-insert-command", new gnu.jemacs.buffer.SelfInsertCommand());

    lambda lambda = new gnu.jemacs.lang.lambda();
    lambda.setKeywords(getSymbol("&optional"),
		       getSymbol("&rest"),
		       getSymbol("&key"));
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new gnu.commonlisp.lang.defun(lambda));
    defun("function", new gnu.commonlisp.lang.function(lambda));

    defun(gnu.kawa.lispexpr.LispLanguage.quote_str,
	  kawa.lang.Quote.plainQuote);
    defun("defgroup", new defgroup());
    defun("defcustom", new defcustom());
    defun("defvar", new gnu.commonlisp.lang.defvar(false));
    defun("defconst", new gnu.commonlisp.lang.defvar(true));
    defun("defsubst", new gnu.commonlisp.lang.defun(lambda));
    defun("setq", new gnu.commonlisp.lang.setq());
    defun("prog1", gnu.commonlisp.lang.prog1.prog1);
    defun("prog2", gnu.commonlisp.lang.prog1.prog2);
    defun("progn", new kawa.standard.begin());
    defun("while", new gnu.jemacs.lang.While());
    defun("unwind-protect", new gnu.commonlisp.lang.UnwindProtect());
    defun("save-excursion", new gnu.jemacs.lang.SaveExcursion(false));
    defun("save-current-buffer", new gnu.jemacs.lang.SaveExcursion(true));
    defun("let", new kawa.standard.fluid_let(false, false, nilExpr));
    defun("%let", kawa.standard.let.let);
    defun("let*", new kawa.standard.fluid_let(true, false, nilExpr));
    defProcStFld("concat", "kawa.lib.strings", "string$Mnappend");
    Procedure not = new gnu.kawa.functions.Not(this);
    defun("not", not);
    defun("null", not);
    defun("eq", new gnu.kawa.functions.IsEq(this, "eq"));
    defun("equal", new gnu.kawa.functions.IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
    defProcStFld("princ", "gnu.jemacs.lang.MiscOps");
    defProcStFld("prin1", "gnu.jemacs.lang.MiscOps");
    LocationEnumeration e = Scheme.builtin().enumerateAllLocations();
    while (e.hasMoreElements())
      {
        importLocation(e.nextLocation());
      }
    try
      {
	loadClass("gnu.jemacs.lisp.primitives");
	loadClass("gnu.jemacs.buffer.emacs");
	loadClass("gnu.jemacs.lisp.simple");
	loadClass("gnu.jemacs.lisp.autoloads");
	loadClass("gnu.jemacs.lisp.keymap");
	loadClass("gnu.jemacs.lisp.editfns");
	loadClass("gnu.jemacs.lisp.keydefs");
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	// Ignore - happens while building this directory.
      }
  }

  public static ELisp getInstance()
  {
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(instance);
  }

    public static final AbstractFormat writeFormat = new Print(true);
    public static final AbstractFormat displayFormat = new Print(false);

  public AbstractFormat getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  LangPrimType booleanType;

  public Type getTypeFor(String name)
  {
    if (name == "t")
      name = "java.lang.Object";
    else if (name == "marker")
      name = "gnu.jemacs.buffer.Marker";
    else if (name == "buffer")
      name = "gnu.jemacs.buffer.Buffer";
    else if (name == "window")
      name = "gnu.jemacs.buffer.Window";
    return super.getTypeFor(name);
  }

  public Type getTypeFor (Class clas)
  {
    if (clas.isPrimitive())
      {
	String name = clas.getName();
	if (name.equals("boolean"))
	  {
	    if (booleanType == null)
	      booleanType = new LangPrimType(Type.booleanType, this);
	    return booleanType;
	  }
	return getNamedType(name);
      }
    return Type.make(clas);
  }

  public ReadTable createReadTable ()
  {
    ReadTable rt = super.createReadTable();
    rt.set('[', new ReaderVector(']'));
    rt.remove(']');
    rt.set('?', new ELispReadTableEntry('?'));
    return rt;
  }

  public static void readableChar(char ch, StringBuffer buf, boolean quote)
  {
    if (quote && (ch == '\\' || ch == '\'' || ch == '\"'))
      {
        buf.append('\\');
        buf.append(ch);
      }
    else if (ch > 127)
      {
        buf.append("\\u");
        String hex = Integer.toHexString(ch);
        for (int i = hex.length();  i < 4;  i++)  buf.append('0');
        buf.append(hex);
      }
    else if (ch >= ' ')
      buf.append(ch);
    else if (ch == '\t')  buf.append("\\t");
    else if (ch == '\r')  buf.append("\\r");
    else if (ch == '\n')  buf.append("\\n");
    else
      {
        buf.append("\\0");
        buf.append((ch >> 3) & 7);
        buf.append(ch & 7);
      }
  }

  /**
   * Call toString, quoting characters that are not ascii graphic chars.
   * This method will probably be moved somewhere more appropriate.
   */
  public static String readableString(Object obj)
  {
    String str = obj.toString();
    StringBuffer buf = new StringBuffer(200);
    for (int i = 0;  i < str.length();  i++)
      readableChar(str.charAt(i), buf, false);
    return buf.toString();
  }

  public static void main(String[] args)
  {
    kawa.repl.processArgs(new String[] { "--elisp" }, 0, 1);
    if (args.length == 0)
      args = new String[] { "-e", "(emacs)", "--" };
    kawa.repl.main(args);
  }
}

class ELispReadTableEntry extends ReaderDispatchMisc
{
  public ELispReadTableEntry(int code)
  {
    super(code);
  }

  public Object read (Lexer in, int ch, int count)
    throws java.io.IOException, SyntaxException
  {
    LispReader reader = (LispReader) in;
    if (code >= 0)
      ch = code;
    switch (ch)
      {
      case '?':
	ch = reader.read();
	if (ch == '\\')
	  {
	    ch = reader.read();
	    if (ch != ' ' && ch >= 0)
	      ch = reader.readEscape(ch);
	  }
	if (ch < 0)
	  {
	    reader.error("unexpected EOF in character literal");
	    ch = '?';
	  }
	return ELisp.getCharacter(ch);
      }
    reader.error("unexpected dispatch character");
    return null;
  }
}

