// Copyright (c) 2001, 2004, 2005  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.commonlisp.lang;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.*;
import gnu.text.Char;
import kawa.standard.Scheme;
import gnu.kawa.functions.*;
import gnu.bytecode.Type;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.functions.DisplayFormat;
import gnu.kawa.functions.NumberCompare;

public class CommonLisp extends Lisp2
{
  static boolean charIsInt = false;

  /** Get a CommonLisp character object. */
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
    return (gnu.math.Numeric) arg;
  }

  public static char asChar(Object x)
  {
    if (x instanceof Char)
      return ((Char) x).charValue();
    int i;
    if (x instanceof gnu.math.Numeric)
      i = ((gnu.math.Numeric) x).intValue();
    else
      i = -1;
    if (i < 0 || i > 0xffff)
      throw new ClassCastException("not a character value");
    return (char) i;
  }

  public String getName()
  {
    return "CommonLisp";
  }

  // This field need to be public so that the findLiteral method in
  // gnu.expr.LitTable can find it.
  public static final CommonLisp instance;

  public static final Environment clispEnvironment
    = Environment.make("clisp-environment");

  public static final NumberCompare numEqu;
  public static final NumberCompare numGrt;
  public static final NumberCompare numGEq;
  public static final NumberCompare numLss;
  public static final NumberCompare numLEq;

  public static final Not not;
  public static final IsEq isEq;
  public static final IsEqv isEqv;
  
  /** Package location symbols. */
  public static final Symbol internalKeyword = Keyword.make("INTERNAL");
  public static final Symbol inheritedKeyword = Keyword.make("INHERITED");
  public static final Symbol externalKeyword = Keyword.make("EXTERNAL");

  static
  {
    instance = new CommonLisp();

    instance.define("t", TRUE);
    instance.define("nil", FALSE);
    not = new Not(instance, "not");
    numEqu = NumberCompare.make(instance, "=",
                                NumberCompare.TRUE_IF_EQU);
    numGrt = NumberCompare.make(instance, ">",
                                NumberCompare.TRUE_IF_GRT);
    numGEq = NumberCompare.make(instance, ">=",
                                NumberCompare.TRUE_IF_GRT|NumberCompare.TRUE_IF_EQU);
    numLss = NumberCompare.make(instance, "<",
                                NumberCompare.TRUE_IF_LSS);
    numLEq = NumberCompare.make(instance, "<=",
                                NumberCompare.TRUE_IF_LSS|NumberCompare.TRUE_IF_EQU);
    isEq = new IsEq(instance, "eq?");
    isEqv = new IsEqv(instance, "eqv?", isEq);
    
    Environment saveEnv = Environment.setSaveCurrent(clispEnvironment);
    try
      {
        instance.initLisp();
      }
    finally
      {
        Environment.restoreCurrent(saveEnv);
      }
  }

  public CommonLisp()
  {
    environ = clispEnvironment;
  }

  void initLisp()
  {
    LocationEnumeration e = Scheme.builtin().enumerateAllLocations();
    while (e.hasMoreElements())
      {
        importLocation(e.nextLocation());
      }

    try
      {
        // Force it to be loaded now, so we can over-ride let* length etc.
        loadClass("kawa.lib.prim_syntax");
        loadClass("kawa.lib.std_syntax");
        loadClass("kawa.lib.lists");
        loadClass("kawa.lib.strings");
        loadClass("gnu.commonlisp.lisp.PrimOps");
      }
    catch (java.lang.ClassNotFoundException ex)
      {
        // Ignore - happens while building this directory.
      }

    OrdinaryLambda lambda = new OrdinaryLambda();
    lambda.setKeywords(asSymbol("&optional"),
                       asSymbol("&rest"),
                       asSymbol("&key"),
                       asSymbol("&allow-other-keys"),
                       asSymbol("&aux"),
                       asSymbol("&body"));
    lambda.defaultDefault = nilExpr;
    defun("lambda", lambda);
    defun("defun", new defun(lambda));

    defun("defvar", new defvar(false));
    defun("defconst", new defvar(true));
    defun("defsubst", new defun(lambda));
    defun("function", new function(lambda));
    defun("setq", new setq());
    defun("prog1", new prog1("prog1", 1));
    defun("prog2", prog1.prog2);
    defun("progn", new kawa.standard.begin());
    defun("unwind-protect", new gnu.commonlisp.lang.UnwindProtect());
    defun("null", not);
    defun("eq", new IsEq(this, "eq"));
    defun("equal", new IsEqual(this, "equal"));
    defun("typep", new gnu.kawa.reflect.InstanceOf(this));
    defProcStFld("the", "gnu.kawa.functions.Convert", "as");
    defun("%flet", new kawa.standard.let("flet", true));
    defProcStFld("princ", "gnu.commonlisp.lisp.PrimOps");
    defProcStFld("prin1", "gnu.commonlisp.lisp.PrimOps");

    defAliasStFld("*package*", "gnu.kawa.lispexpr.LispPackage", "currentPackage");
    defProcStFld("=", "gnu.commonlisp.lang.CommonLisp", "numEqu");
    defProcStFld("<", "gnu.commonlisp.lang.CommonLisp", "numLss");
    defProcStFld(">", "gnu.commonlisp.lang.CommonLisp", "numGrt");
    defProcStFld("<=", "gnu.commonlisp.lang.CommonLisp", "numLEq");
    defProcStFld(">=", "gnu.commonlisp.lang.CommonLisp", "numGEq");
    defProcStFld("not", "gnu.commonlisp.lang.CommonLisp");
    defProcStFld("eq?", "gnu.commonlisp.lang.CommonLisp", "isEq");
    defProcStFld("eqv?", "gnu.commonlisp.lang.CommonLisp", "isEqv");
    defProcStFld("functionp", "gnu.commonlisp.lisp.PrimOps");
    defProcStFld("car", "gnu.commonlisp.lisp.primitives");
    defProcStFld("first", "gnu.commonlisp.lisp.primitives");
    defProcStFld("cdr", "gnu.commonlisp.lisp.primitives");
    defProcStFld("caar", "kawa.lib.lists");
    defProcStFld("cadr", "kawa.lib.lists");
    defProcStFld("cdar", "kawa.lib.lists");
    defProcStFld("cddr", "kawa.lib.lists");
    defProcStFld("caaar", "kawa.lib.lists");
    defProcStFld("caadr", "kawa.lib.lists");
    defProcStFld("cadar", "kawa.lib.lists");
    defProcStFld("caddr", "kawa.lib.lists");
    defProcStFld("cdaar", "kawa.lib.lists");
    defProcStFld("cdadr", "kawa.lib.lists");
    defProcStFld("cddar", "kawa.lib.lists");
    defProcStFld("cdddr", "kawa.lib.lists");
    defProcStFld("caaaar", "kawa.lib.lists");
    defProcStFld("caaadr", "kawa.lib.lists");
    defProcStFld("caadar", "kawa.lib.lists");
    defProcStFld("caaddr", "kawa.lib.lists");
    defProcStFld("cadaar", "kawa.lib.lists");
    defProcStFld("cadadr", "kawa.lib.lists");
    defProcStFld("caddar", "kawa.lib.lists");
    defProcStFld("cadddr", "kawa.lib.lists");
    defProcStFld("cdaaar", "kawa.lib.lists");
    defProcStFld("cdaadr", "kawa.lib.lists");
    defProcStFld("cdadar", "kawa.lib.lists");
    defProcStFld("cdaddr", "kawa.lib.lists");
    defProcStFld("cddaar", "kawa.lib.lists");
    defProcStFld("cddadr", "kawa.lib.lists");
    defProcStFld("cdddar", "kawa.lib.lists");
    defProcStFld("cddddr", "kawa.lib.lists");
    defProcStFld("rest", "gnu.commonlisp.lisp.primitives");
    defProcStFld("second", "gnu.commonlisp.lisp.primitives");
    defProcStFld("third", "gnu.commonlisp.lisp.primitives");
    defProcStFld("nthcdr", "gnu.commonlisp.lisp.primitives");
    defProcStFld("nth", "gnu.commonlisp.lisp.primitives");
    defProcStFld("1-", "gnu.commonlisp.lisp.primitives");
    defProcStFld("1+", "gnu.commonlisp.lisp.primitives");
    defProcStFld("acons", "gnu.commonlisp.lisp.primitives");
    defProcStFld("listp", "gnu.commonlisp.lisp.primitives");
    defProcStFld("numberp", "gnu.commonlisp.lisp.primitives");
    defProcStFldAs("zerop", "kawa.lib.numbers", "zero?");
    defProcStFldAs("consp", "kawa.lib.lists", "pair?");
    defProcStFld("atom", "gnu.commonlisp.lisp.primitives");
    defProcStFld("eql", "gnu.commonlisp.lisp.primitives");
    defProcStFld("member", "gnu.commonlisp.lisp.primitives");
    defProcStFld("complement", "gnu.commonlisp.lisp.primitives");
    defProcStFld("apply", "gnu.commonlisp.lisp.primitives");
    defProcStFld("funcall", "gnu.commonlisp.lisp.primitives");
    defProcStFld("minusp", "gnu.commonlisp.lisp.primitives");
    defProcStFld("plusp", "gnu.commonlisp.lisp.primitives");
    defProcStFld("flet", "gnu.commonlisp.lisp.primitives");
    defProcStFld("labels", "gnu.commonlisp.lisp.primitives");
    defProcStFld("multiple-value-bind", "gnu.commonlisp.lisp.primitives");
    defProcStFld("floor", "gnu.commonlisp.lisp.primitives");
  }

  public static CommonLisp getInstance()
  {
    return instance;
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(instance);
  }

    public static final AbstractFormat writeFormat
        = new DisplayFormat(true, 'C');
    public static final AbstractFormat displayFormat
        = new DisplayFormat(false, 'C');

  public AbstractFormat getFormat(boolean readable)
  {
    return readable ? writeFormat : displayFormat;
  }

  LangPrimType booleanType;

    @Override
    public Type getTypeFor(String name) {
        if (name == "t")
            name = "java.lang.Object";
        return super.getTypeFor(name);
    }

    @Override
    public Type getTypeFor(Class clas) {
        if (clas.isPrimitive())
            return getNamedType(clas.getName());
        return Type.make(clas);
    }

    public Type getNamedType (String name) {
        if (name.equals("boolean")) {
            if (booleanType == null)
                booleanType = new LangPrimType(Type.booleanType, this);
            return booleanType;
        }
        return super.getNamedType(name);
    }
}
