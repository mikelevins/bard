// Copyright (c) 1999, 2000-2005, 2006, 2010 Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.util.*;
import java.io.*;
import kawa.Shell;
import java.util.zip.*;
import java.util.Stack;
import gnu.kawa.functions.Convert;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.kawa.reflect.LazyType;
import gnu.lists.Pair;
import gnu.text.Char;
import gnu.text.Lexer;
import gnu.text.Options;
import gnu.text.SourceLocator;
import gnu.text.SourceMessages;
import kawa.lang.Translator.FormStack;

/** State for a single expression or module.
 * For each top-level thing (expression or file) we compile or evaluate
 * we create a new Compilation.
 */

public class Compilation implements SourceLocator
{
  /** True if the form is too complex to evaluate,and we must compile it.
   * This is because it contains a construct we know how to compile, but not
   * evaluate, and it it outside a function (which we always compile).
   * This can be a let scope, or primitive procedure. */
  public boolean mustCompile = ModuleExp.alwaysCompile;

  /** Used by LambdaExp.getSelectorValue if need to allocate new selector. */
  int maxSelectorValue;

  public ClassType curClass;
  public ClassType mainClass;
  /** Generated class that extends ModuleBody.  Normally same as mainClass. */
  public ClassType moduleClass;

  public LambdaExp curLambda;
  public ModuleExp mainLambda;
  public Variable thisDecl;

  /** Contains "$instance" if the module is static; otherwise null. */
  Variable moduleInstanceVar;

  /** A code, one of the following constants, indicating how far along
   * we are in the parsing/compilation process.
   * These codes are even integers for completed stages and odd integers
   * for begun but not completed stages. */
  private int state;
  /** Returns a code indicating how far along
   * we are in the parsing/compilation process. */
  public int getState () { return state; }
  public void setState (int state) { this.state = state; }
  /** State code for initial pre-parse looking for module name. */
  public static final int PROLOG_PARSING = 1;
  /** We have determined the module name and class, but not finished parsing. */
  public static final int PROLOG_PARSED = 2;
  /** State code indicating the entire module has been parsed. */
  public static final int BODY_PARSED = 4;
  /** State code for lexical bindings having been resolved. */ 
  public static final int RESOLVED = 6;
  /** State code when initial tree-walking (PushApply) are done. */
  public static final int PRE_WALKED = 8;
  /** State code when various inlining and optimization passes are done. */
  public static final int WALKED = 10;
  /** State code that various compile-only data has been determined. */
  public static final int COMPILE_SETUP = 12;
  /** State code indicating the bytecode has been generated. */
  public static final int COMPILED = 14;
  /** State code indicating that bytecode has been written to its target. */
  public static final int CLASS_WRITTEN = 16;
  public static final int ERROR_SEEN = 100;

  public Lexer lexer;

    private boolean pedantic;
    public boolean isPedantic() { return pedantic; }
    public void setPedantic(boolean value) { pedantic = value; }

  /** Used to access the "main" instance.
   * This is used for two different purposes, which may be confusing:
   * <ul>
   * <li>
   * If we're compiling a static module, then {@code moduleInstanceMainField}
   * is a field in {@code mainClass} named {@code "$instance"} that
   * points to the single instance of the module.</li>
   * <li>
   * If {@code moduleClass!=mainClass} (typically because we've specified
   * {@code module-extends}) <em>and</em> the module is non-static then
   * {@code moduleInstanceMainField} is a field in {@code moduleClass}
   * named {@code "$main"} that points back to {@code mainClass}.</li></ul>
   */
  Field moduleInstanceMainField;

  /** Stack of quads of (ModuleInfo, ScopeExp, position, formSize). */
  public java.util.Stack<Object> pendingImports;

  public void pushPendingImport(ModuleInfo info, ScopeExp defs, FormStack forms)
  {
    if (pendingImports == null)
      pendingImports = new java.util.Stack<Object>();
    pendingImports.push(info);
    pendingImports.push(defs);
    Expression posExp = new ReferenceExp((Object) null);
    posExp.setLine(this);
    pendingImports.push(posExp);
    pendingImports.push(forms.lastPair());
  }

    public Map<String,ModuleInfo> subModuleMap;

    /* Write out implicitly-compiled classes.
     * Compare javac's -implicit:class flag.  Current not set.
     */
    public static boolean writeImplicitClasses = false;
    
  /** If true, print out expressions after parsing and before optimizations. */
  public static boolean debugPrintExpr = false;

  /** If true, print out final expressions after optimizations etc. */
  public static boolean debugPrintFinalExpr;

  public static Options options = new Options();
  public static Options.OptionInfo fullTailCallsVariable =
    options.add("full-tailcalls",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"support full tailcalls");
  public static Options.OptionInfo mainMethodVariable =
    options.add("main",
                Options.BOOLEAN_OPTION, Boolean.FALSE,
                "generate an application, with a main method");
  public static Options.OptionInfo warnUnreachable =
    options.add("warn-unreachable",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if this code can never be executed");
  public static Options.OptionInfo warnVoidUsed =
    options.add("warn-void-used",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if void-valued expression is used");
  public static Options.OptionInfo warnUndefinedVariable =
    options.add("warn-undefined-variable",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if no compiler-visible binding for a variable");
  public static Options.OptionInfo warnUnknownMember =
    options.add("warn-unknown-member",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
		"warn if referencing an unknown method or field");
  public static Options.OptionInfo warnInvokeUnknownMethod =
    options.add("warn-invoke-unknown-method",
                Options.BOOLEAN_OPTION, warnUnknownMember,
		"warn if invoke calls an unknown method (subsumed by warn-unknown-member)");
  public static Options.OptionInfo warnUnused =
    options.add("warn-unused",
                Options.BOOLEAN_OPTION, Boolean.TRUE,
                "warn if a variable is usused or code never executed");
  public static Options.OptionInfo warnAsError =
    options.add("warn-as-error", Options.BOOLEAN_OPTION, Boolean.FALSE,
		"Make all warnings into errors");

  public Options currentOptions = new Options(options);

  public boolean generateMainMethod ()
  {
    return currentOptions.getBoolean(mainMethodVariable);
  }
  
  public boolean warnUnreachable ()
  {
    return currentOptions.getBoolean(warnUnreachable);
  }
  public boolean warnUndefinedVariable ()
  {
    return currentOptions.getBoolean(warnUndefinedVariable);
  }
  public boolean warnUnknownMember ()
  {
    return currentOptions.getBoolean(warnUnknownMember);
  }
  public boolean warnInvokeUnknownMethod ()
  {
    return currentOptions.getBoolean(warnInvokeUnknownMethod);
  }
  public boolean warnUnused()
  {
    return currentOptions.getBoolean(warnUnused);
  }
  public boolean warnVoidUsed()
  {
    return currentOptions.getBoolean(warnVoidUsed);
  }
  public boolean warnAsError ()
  {
    return currentOptions.getBoolean(warnAsError);
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key, boolean defaultValue)
  {
    return currentOptions.getBoolean(key, defaultValue);
  }

  /** Get a named boolean option. */
  public final boolean getBooleanOption (String key)
  {
    return currentOptions.getBoolean(key);
  }

  public static int defaultClassFileVersion =
    /* #ifdef JAVA8 */
    ClassType.JDK_1_8_VERSION
    /* #else */
    /* #ifdef JAVA7 */
    // ClassType.JDK_1_7_VERSION
    /* #else */
    /* #ifdef JAVA6 */
    // ClassType.JDK_1_6_VERSION
    /* #else */
    /* #ifdef JAVA5 */
    // ClassType.JDK_1_5_VERSION
    /* #else */
    // ClassType.JDK_1_1_VERSION
    /* #endif */
    /* #endif */
    /* #endif */
    /* #endif */
    ;

  /** The default calling convention.
   * One of the following CALL_WITH_xxx values. */
  public static int defaultCallConvention;
  public static final int CALL_WITH_UNSPECIFIED = 0;
  /** Plain calling convention, using regular Java parameters and returns. */
  public static final int CALL_WITH_RETURN = 1;
  /** Function results are written to the current CallContext's Consumer. */
  public static final int CALL_WITH_CONSUMER = 2;
  /** Like CALL_WITH_CONSUMER, but handle full on-stack-growing tail-calls. */
  public static final int CALL_WITH_TAILCALLS = 3;
  /** Support for full continuations.  Not implemented. */
  public static final int CALL_WITH_CONTINUATIONS = 4;

    public int currentCallConvention() {
        Object ft = currentOptions.getLocal("full-tailcalls");
        if (ft instanceof Boolean)
            return ((Boolean) ft).booleanValue()
                ? Compilation.CALL_WITH_TAILCALLS
                : Compilation.CALL_WITH_RETURN;
        return defaultCallConvention;
    }

  public boolean usingCPStyle()
  { return currentCallConvention() == CALL_WITH_CONTINUATIONS; }
  public boolean usingTailCalls()
  { return currentCallConvention() >= CALL_WITH_TAILCALLS; }
  public boolean usingCallContext()
  { return currentCallConvention() >= CALL_WITH_CONSUMER; }

  public static final int MODULE_NONSTATIC = -1;
  public static final int MODULE_STATIC_DEFAULT = 0;
  public static final int MODULE_STATIC = 1;
  public static final int MODULE_STATIC_RUN = 2;
  /** If moduleStatic > 0, (module-static #t) is implied by default.
   * If moduleStatic == 2, <clinit> calls run.
   * If moduleStatic < 0, (module-static #f) is implied by default. */
  public static int moduleStatic = MODULE_STATIC_DEFAULT;

  ClassType[] classes;
  int numClasses;

  /** When immediate, the ClassLoader we will load the compiled
   * classes from. */
  ArrayClassLoader loader;

  /** True if the compiled result will be immediately loaded. */ 
  public boolean immediate;

  /** Compilation was explicitly requested, rather than being a dependency. */
  public boolean explicit;

  /** The current method. */
  public Method method;

  Method clinitMethod;

  public final CodeAttr getCode() { return method.getCode(); }

  int method_counter;

  /* When multiple procedures are compiled into a single method,
     we use a switch to jump to the correct part of the method. */
  SwitchState fswitch;

  Field fswitchIndex;

  // Various standard classes
  static public ClassType typeObject = Type.objectType;
  static public ClassType scmBooleanType = ClassType.make("java.lang.Boolean");
  static public ClassType typeString = ClassType.make("java.lang.String");
  static public ClassType javaStringType = typeString;
  static public ClassType scmKeywordType = ClassType.make("gnu.expr.Keyword");
  static public ClassType scmSequenceType = ClassType.make("gnu.lists.Sequence");
  public static final ClassType typeList = ClassType.make("java.util.List");
  static public ClassType scmListType = ClassType.make("gnu.lists.LList");
  static public ClassType typePair = ClassType.make("gnu.lists.Pair");
  public static final ClassType typeConstVector = ClassType.make("gnu.lists.ConstVector");
  public static final ArrayType objArrayType = ArrayType.make(typeObject);
  static public ClassType typeRunnable = ClassType.make("java.lang.Runnable");
  static public ClassType typeRunnableModule = ClassType.make("gnu.expr.RunnableModule");
  public static ClassType typeType = ClassType.make("gnu.bytecode.Type");
  public static ClassType typeObjectType
    = ClassType.make("gnu.bytecode.ObjectType");
  public static ClassType typeClass = Type.javalangClassType;
  static public ClassType typeClassType = ClassType.make("gnu.bytecode.ClassType");
  static public ClassType typeProcedure
    = ClassType.make("gnu.mapping.Procedure");
  static public ClassType typeLanguage
    = ClassType.make("gnu.expr.Language");
  static public ClassType typeEnvironment
    = ClassType.make("gnu.mapping.Environment");
  static public ClassType typeLocation
    = ClassType.make("gnu.mapping.Location");
  static public ClassType typeFieldLocation
    = ClassType.make("gnu.kawa.reflect.FieldLocation");
  static public ClassType typeStaticFieldLocation
    = ClassType.make("gnu.kawa.reflect.StaticFieldLocation");
  static public ClassType typeSymbol
    = ClassType.make("gnu.mapping.Symbol");
  static public final Field trueConstant
    = scmBooleanType.getDeclaredField("TRUE"); 
  static public final Field falseConstant
    = scmBooleanType.getDeclaredField("FALSE");

  static Method makeListMethod;
  
  public static final Type[] int1Args = { Type.intType  };
  public static final Type[] string1Arg = { javaStringType };
  public static final Type[] sym1Arg = string1Arg;

  static {
    Type[] makeListArgs = { objArrayType, Type.intType  };
    makeListMethod = scmListType.addMethod ("makeList",
					     makeListArgs, scmListType,
					     Access.PUBLIC|Access.STATIC);
  }

  public static Method getCurrentEnvironmentMethod
    = typeEnvironment.addMethod("getCurrent", Type.typeArray0,
				typeEnvironment,Access.PUBLIC|Access.STATIC);

  public static Type[] apply0args = Type.typeArray0;
  public static Type[] apply1args = { typeObject };
  public static Type[] apply2args = { typeObject, typeObject };
  public static Type[] applyNargs = { objArrayType };

  static Method checkArgCountMethod;

  public static Method apply0method = typeProcedure.addMethod
  ("apply0", apply0args, typeObject, Access.PUBLIC|Access.FINAL);

  public static Method apply1method;
  public static Method apply2method;
  public static Method apply3method;
  public static Method apply4method;
  public static Method applyNmethod;

  static
  {
    apply1method = typeProcedure.addMethod ("apply1", apply1args,
						typeObject, Access.PUBLIC);
    apply2method = typeProcedure.addMethod ("apply2", apply2args,
						typeObject, Access.PUBLIC);
    Type[] apply3args = { typeObject, typeObject, typeObject };
    apply3method = typeProcedure.addMethod ("apply3", apply3args,
						typeObject, Access.PUBLIC);
    Type[] apply4args = { typeObject , typeObject, typeObject, typeObject};
    apply4method = typeProcedure.addMethod ("apply4", apply4args,
						typeObject, Access.PUBLIC);
    applyNmethod = typeProcedure.addMethod ("applyN", applyNargs,
						typeObject, Access.PUBLIC);
    Type[] args = new Type[2];
    args[0] = typeProcedure;
    args[1] = Type.intType;
    checkArgCountMethod
      = typeProcedure.addMethod("checkArgCount", args, Type.voidType,
				   Access.PUBLIC|Access.STATIC);
  }

  public static Method[] applymethods = {
    apply0method, apply1method, apply2method, apply3method,
    apply4method, applyNmethod };

  public static ClassType typeProcedure0
    = ClassType.make("gnu.mapping.Procedure0");
  public static ClassType typeProcedure1
    = ClassType.make("gnu.mapping.Procedure1");
  public static ClassType typeProcedure2
    = ClassType.make("gnu.mapping.Procedure2");
  public static ClassType typeProcedure3
    = ClassType.make("gnu.mapping.Procedure3");
  public static ClassType typeProcedure4
    = ClassType.make("gnu.mapping.Procedure4");
  public static ClassType typeProcedureN
    = ClassType.make("gnu.mapping.ProcedureN");
  public static ClassType typeModuleBody
    = ClassType.make("gnu.expr.ModuleBody");
  public static ClassType typeApplet = ClassType.make("java.applet.Applet");
  public static ClassType typeServlet = ClassType.make("gnu.kawa.servlet.KawaServlet");

  /* Classes, fields, and methods used wgen usingCPStyle". */
  public static ClassType typeCallContext
    = ClassType.make("gnu.mapping.CallContext");
  public static final ClassType typeConsumer
    = ClassType.make("gnu.lists.Consumer");
  public static Method getCallContextInstanceMethod
    = typeCallContext.getDeclaredMethod("getInstance", 0);
  public static ClassType typeValues
    = ClassType.make("gnu.mapping.Values");
  public static Field noArgsField
    = typeValues.getDeclaredField("noArgs");
  public static Field pcCallContextField
    = typeCallContext.getDeclaredField("pc");
  public static ClassType typeMethodProc
  = ClassType.make("gnu.mapping.MethodProc");
  public static ClassType typeModuleMethod
  = ClassType.make("gnu.expr.ModuleMethod");
  public static ClassType typeModuleMethodWithContext
  = ClassType.make("gnu.expr.ModuleMethodWithContext");
  //  public static Field numArgsCallFrameField = typeCallFrame.getDeclaredField("numArgs");
  public static Field argsCallContextField
    = typeCallContext.getDeclaredField("values");
  public static Field procCallContextField
    = typeCallContext.getDeclaredField("proc");
  private static Type[] applyCpsArgs = { typeCallContext};
  public static Method applyCpsMethod
    = typeProcedure.addMethod("apply", applyCpsArgs, Type.voidType,
				 Access.PUBLIC);

  public static ClassType[] typeProcedureArray = {
    typeProcedure0, typeProcedure1, typeProcedure2, typeProcedure3,
    typeProcedure4 };

  /** Rembembers stuff to do in <clinit> of main class. */
  Initializer clinitChain;

  LitTable litTable;

  int langOptions;
  
  /** True if we should generate an Applet. */
  public boolean generatingApplet ()
  {
    return (langOptions & Language.PARSE_FOR_APPLET) != 0;
  }

  /** True if we should generate a Servlet. */
  public boolean generatingServlet ()
  {
    return (langOptions & Language.PARSE_FOR_SERVLET) != 0;
  }

  public boolean sharedModuleDefs ()
  {
    return (langOptions & Language.PARSE_CURRENT_NAMES) != 0;
  }

  public void setSharedModuleDefs (boolean shared)
  {
    if (shared)
      langOptions |= Language.PARSE_CURRENT_NAMES;
    else
      langOptions &= ~Language.PARSE_CURRENT_NAMES;
  }

  public final ClassType getModuleType()
  {
    return typeModuleBody;
  }

    /** Emit code to "evaluate" a compile-time constant.
     * This is the normal external interface.
     * @param value the value to be compiled
     */
    public void compileConstant (Object value) {
        gnu.bytecode.CodeAttr code = getCode();
        if (value == null)
            code.emitPushNull();
        else if (value instanceof String && ! immediate)
            code.emitPushString((String) value);
        else {
            Literal literal = litTable.findLiteral(value);
            // if (immediate) maybe we want to use a differnet approach.
            if (literal.field == null)
                literal.assign(litTable);
            code.emitGetStatic(literal.field);
        }
    }

  public static boolean inlineOk = true;

  public boolean inlineOk (Expression proc)
  {
    if (proc instanceof LambdaExp)
      {
        LambdaExp lproc = (LambdaExp) proc;
        Declaration nameDecl = lproc.nameDecl;
	// The compiler gets confused if we turn off inlining for nested
	// procedures - and they can't be rebound anyway.
        if (nameDecl == null || nameDecl.getSymbol() == null
            || ! (nameDecl.context instanceof ModuleExp))
          return true;
        if (immediate
            && nameDecl.isPublic()
            && ! lproc.getFlag(LambdaExp.OVERLOADABLE_FIELD)
            && (curLambda == null || lproc.topLevel() != curLambda.topLevel()))
          return false;
      }
    return inlineOk;
  }

  public boolean inlineOk (Procedure proc)
  {
    if (immediate && proc instanceof ModuleMethod
        && ((ModuleMethod) proc).module.getClass().getClassLoader() instanceof ArrayClassLoader)
      return false;
    return inlineOk;
  }

    /** Should this inlineable method by inlined?
     * Usually it's best to not inline a module-level function, since that
     * makes stack traces less helpful, and increases the risk of
     * methods getting too big.
     */
    static boolean avoidInline(LambdaExp proc) {
        return proc.getOuter() instanceof ModuleExp && proc.nameDecl != null;
    }

  public boolean isApplyFunction (Expression exp)
  {
    return false;
  }

  /** A simple apply function maps actual arguments to formals directly.
   * E.g. no distribution of multiple values.
   */
  public boolean isSimpleApplyFunction (Expression exp)
  {
    return false;
  }

  public void compileConstant (Object value, Target target)
  {
    if (target instanceof IgnoreTarget)
      return;
    if (value instanceof Values && target instanceof ConsumerTarget)
      {
        Object[] values = ((Values) value).getValues();
        int len = values.length;
        for (int i = 0;  i < len;  i++)
          {
            compileConstant(values[i],
                            ((ConsumerTarget) target).getSingleTarget());
          } 
        return;
      }
    if (target instanceof ConditionalTarget)
      {
	ConditionalTarget ctarg = (ConditionalTarget) target;
	getCode().emitGoto(getLanguage().isTrue(value) ? ctarg.ifTrue
			   : ctarg.ifFalse);
	return;
      }
    if (target instanceof StackTarget)
      {
	Type type = ((StackTarget) target).getType();
	if (type instanceof LazyType)
	  type = ((LazyType) type).getValueType();
	if (type instanceof PrimType)
	  {
	    try
	      {
		String signature = type.getSignature();
		CodeAttr code = getCode();
		char sig1 = (signature == null || signature.length() != 1) ? ' '
		  : signature.charAt(0);
		if (value instanceof Number)
		  {
		    Number num = (Number) value;
		    switch (sig1)
		      {
                      case 'C':
		      case 'I':
			code.emitPushInt(num.intValue());
			return;
		      case 'S':
			code.emitPushInt(num.shortValue());
			return;
		      case 'B':
			code.emitPushInt(num.byteValue());
			return;
		      case 'J':
			code.emitPushLong(num.longValue());
			return;
		      case 'F':
			code.emitPushFloat(num.floatValue());
			return;
		      case 'D':
			code.emitPushDouble(num.doubleValue());
			return;
		      }
		  }
                // FIXME we should move this into a new method
                // PrimType#pushValue(Object value), with LangPrimType override.
                if (type == LangPrimType.characterType
                    || type == LangPrimType.characterOrEofType)
                  {
                    if (value instanceof Char)
                      {
                        code.emitPushInt(((Char) value).intValue());
                        return;
                      }
                    if (value instanceof Character)
                      {
                        code.emitPushInt(((Character) value).charValue());
                        return;
                      }
                    if (value == gnu.lists.Sequence.eofValue
                        && type == LangPrimType.characterOrEofType)
                      {
                        code.emitPushInt(-1);
                        return;
                     }
                  }
		if (sig1 == 'C')
		  {
		    code.emitPushInt((int) ((PrimType) type).charValue(value));
		    return;
		  }
		if (sig1 == 'Z')
		  {
		    boolean val = getLanguage().isTrue(value);
		    code.emitPushInt(val ? 1 : 0);
		    return;
		  }
	      }
	    catch (ClassCastException ex)
	      {
		// should print an ERROR.
	      }
	  }
        if (type == Compilation.typeClass && value instanceof ClassType)
          {
            loadClassRef((ClassType) value);
            return;
          }
        try
          {
            value = type.coerceFromObject(value);
          }
        catch (Exception ex)
          {
	    StringBuffer sbuf = new StringBuffer();
	    if (value == Values.empty)
	      sbuf.append("cannot convert void to ");
	    else
	      {
		sbuf.append("cannot convert literal (of type ");
                if (value == null)
                  sbuf.append("<null>");
                else
                  sbuf.append(value.getClass().getName());
		sbuf.append(") to ");
	      }
	    sbuf.append(type);
            error('w', sbuf.toString());
         }
      }
    compileConstant(value);
    target.compileFromStack(this,
                            value == null ? target.getType()
                            : Type.make(value.getClass()));
  }

  public void emitPushBoolean(boolean value)
  {
    getCode().emitGetStatic(value ? Compilation.trueConstant
                            : Compilation.falseConstant);
  }

  /** Generate code to test if an object is considered true.
   * Assume the object has been pushed on the JVM stack.
   * Generate code to push true or false as appropriate. */
  public void emitCoerceToBoolean()
  {
    CodeAttr code = getCode();
    emitPushBoolean(false);
    code.emitIfNEq();
    code.emitPushInt(1);
    code.emitElse();
    code.emitPushInt(0);
    code.emitFi();
  }

    boolean dumpingInitializers;

    private void dumpInitializers(Initializer inits) {
        dumpingInitializers = true;
        for (Initializer init = Initializer.reverse(inits);
             init != null;  init = init.next)
            init.emit(this);
        dumpingInitializers = false;
    }

  /** Search this Compilation for a ClassType with a given name.
   * @param name the name of the class desired
   * @return the matching ClassType, or null if none is found */
  public ClassType findNamedClass (String name)
  {
    for (int i = 0;  i < numClasses; i++)
      {
	if (name.equals (classes[i].getName ()))
	  return classes[i];
      }
    return null;
  }

  public static String classPrefixDefault = "";
  /** If non-null: a prefix for generateClassName to prepend to names. */
  public String classPrefix = classPrefixDefault;

  /** Recusive helper function to reverse order of words in hostname. */
  private static void putURLWords(String name, StringBuffer sbuf)
  {
    int dot = name.indexOf('.');
    if (dot > 0)
      {
	putURLWords(name.substring(dot+1), sbuf);
	sbuf.append('.');
	name = name.substring(0, dot);
      }
    sbuf.append(name);
  }

  /** Map a URI to a package/class name.
   * Similar to the JAXB mangling, and that in the Java language spec.
   */
  public static String mangleURI (String name)
  {
    boolean hasSlash = name.indexOf('/') >= 0;
    int len = name.length();
    if (len > 6 && name.startsWith("class:"))
      return name.substring(6);
    // Remove "http:" or "urn:".
    if (len > 5 && name.charAt(4) == ':'
	&& name.substring(0, 4).equalsIgnoreCase("http"))
      {
	name = name.substring(5);
	len -= 5;
	hasSlash = true;
      }
    else if (len > 4 && name.charAt(3) == ':'
	     && name.substring(0, 3).equalsIgnoreCase("uri"))
      {
	name = name.substring(4);
	len -= 4;
      }
    int start = 0;
    StringBuffer sbuf = new StringBuffer();
    for (;;)
      {
	int slash = name.indexOf('/', start);
	int end = slash < 0 ? len : slash;
	boolean first = sbuf.length() == 0;
	if (first && hasSlash)
	  {
	    // Remove initial "www.".
	    String host = name.substring(start, end);
	    if (end - start > 4 && host.startsWith("www."))
	      host = host.substring(4);
	    // Reverse order of words in "host" part.
	    putURLWords(host, sbuf);
	  }
	else if (start != end)
	  {
	    if (! first)
	      sbuf.append('.');
	    if (end == len)
	      {
		int dot = name.lastIndexOf('.', len);
		if (dot > start + 1 && ! first)
		  {
		    // Remove file extension:
		    int extLen = len - dot;
		    if (extLen <= 4
			|| (extLen == 5 && name.endsWith("html")))
		      {
			len -= extLen;
			end = len;
			name = name.substring(0, len);
		      }
		  }
	      }
	    sbuf.append(name.substring(start, end));
	  }
	if (slash < 0)
	  break;
	start = slash + 1;
      }
    return sbuf.toString();
  }

  public static String mangleName (String name)
  {
    return Language.mangleName(name, -1);
  }

  /** Convert a string to a safe Java identifier.
   * @param reversible if we should use an invertible mapping.
   */
  public static String mangleName (String name, boolean reversible)
  {
    return Language.mangleName(name, reversible ? 1 : -1);
  }

  public static String mangleNameIfNeeded (String name)
  {
    return Language.mangleNameIfNeeded(name);
  }

  /** Demangle a three-character mangling starting with '$'.
   * UNFINISHED!
   */
  public static char demangle2(char char1, char char2)
  {
    switch (char1 << 16 | char2)
      {
      case 'A' << 16 | 'm':  return '&';
      case 'A' << 16 | 't':  return '@';
      case 'C' << 16 | 'l':  return ':';
      case 'C' << 16 | 'm':  return ',';
      case 'D' << 16 | 'q':  return '\"';
      case 'D' << 16 | 't':  return '.';
      case 'E' << 16 | 'q':  return '=';
      case 'E' << 16 | 'x':  return '!';
      case 'G' << 16 | 'r':  return '>';
      case 'L' << 16 | 'B':  return '[';
      case 'L' << 16 | 'C':  return '{';
      case 'L' << 16 | 'P':  return '(';
      case 'L' << 16 | 's':  return '<';
      case 'M' << 16 | 'c':  return '%';
      case 'M' << 16 | 'n':  return '-';
      case 'N' << 16 | 'm':  return '#';
      case 'P' << 16 | 'c':  return '%';
      case 'P' << 16 | 'l':  return '+';
      case 'Q' << 16 | 'u':  return '?';
      case 'R' << 16 | 'B':  return ']';
      case 'R' << 16 | 'C':  return '}';
      case 'R' << 16 | 'P':  return ')';
      case 'S' << 16 | 'C':  return ';';
      case 'S' << 16 | 'l':  return '/';
      case 'S' << 16 | 'q':  return '\\';
      case 'S' << 16 | 't':  return '*';
      case 'T' << 16 | 'l':  return '~';
      case 'U' << 16 | 'p':  return '^';
      case 'V' << 16 | 'B':  return '|';
      }
    return (char) (-1);
  }

  public static String demangleName(String name)
  {
    return demangleName(name, false);
  }

  public static String demangleName(String name, boolean reversible)
  {
    StringBuffer sbuf = new StringBuffer();
    int len = name.length();
    boolean mangled = false;
    boolean predicate = false;
    boolean downCaseNext = false;
    for (int i = 0;  i < len;  i++)
      {
	char ch = name.charAt(i);
        if (i == 0 && ch == '$' && len >= 3 && name.charAt(1) == 'N')
          {
            i = 1;
            mangled = true;
            continue;
          }
	if (downCaseNext && ! reversible)
	  {
	    ch = Character.toLowerCase(ch);
	    downCaseNext = false;
	  }
	char d;
	if (!reversible
	    && ch == 'i' && i == 0 && len > 2 && name.charAt(i+1) == 's'
	    && ! Character.isLowerCase(d = name.charAt(i+2)))
	  {
	    mangled = true;
	    predicate = true;
	    i++;
	    if (Character.isUpperCase(d) || Character.isTitleCase(d))
	      {
		sbuf.append(Character.toLowerCase(d));
		i++;
		continue;
	      }
	    continue;
	  }
	else if (ch == '$' && i + 2 < len)
	  {
	    char c1 = name.charAt(i+1);
	    char c2 = name.charAt(i+2);
	    d = Compilation.demangle2(c1, c2);
	    if (d != (char)(-1))
	      {
		sbuf.append(d);
		i += 2;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	    else if (c1 == 'T' && c2 == 'o' && i + 3 < len
		     && name.charAt(i+3) == '$')
	      {
		sbuf.append("->");
		i += 3;
		mangled = true;
		downCaseNext = true;
		continue;
	      }
	  }
	else if (! reversible && i > 1
		 && (Character.isUpperCase(ch) || Character.isTitleCase(ch))
		 && (Character.isLowerCase(name.charAt(i-1))))
	  {
	    sbuf.append('-');
	    mangled = true;
	    ch = Character.toLowerCase(ch);
	  }
	sbuf.append(ch);
      }
    if (predicate)
      sbuf.append('?');
    return mangled ? sbuf.toString() : name;
  }

  /** Generate an unused class name.
   * @param hint the requested name (or prefix)
   * @return a unique class name.
   */
  public String generateClassName (String hint)
  {
    hint = mangleName(hint, true);
    if (mainClass != null)
      hint = mainClass.getName() + '$' + hint;
    else if (classPrefix != null)
      hint = classPrefix + hint;
    if (findNamedClass (hint) == null)
      return hint;
    for (int i = 0;  ; i++)
      {
	String new_hint = hint + i;
	if (findNamedClass (new_hint) == null)
	  return new_hint;
      }
  }

  public Compilation (Language language, SourceMessages messages,
		      NameLookup lexical)
  {
    this.language = language;
    this.messages = messages;
    this.lexical = lexical;
  }

  public void outputClass (String directory) throws IOException
  {
    char dirSep = File.separatorChar;
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	String out_name
	  = (directory + clas.getName().replace('.', dirSep)
	     + ".class");
	String parent = new File(out_name).getParent();
	if (parent != null)
	  new File(parent).mkdirs();
	clas.writeToFile(out_name);
      }
    getMinfo().cleanupAfterCompilation();
  }

  public void cleanupAfterCompilation ()
  {
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      classes[iClass].cleanupAfterCompilation();
    classes = null;
    ModuleInfo minfo = getMinfo();
    minfo.className = mainClass.getName(); // In case it hasn't been set yet.
    minfo.setCompilation(null);
    // We don't clear minfo.exp itself, since it might be re-required.
    if (minfo.exp != null)
      minfo.exp.body = null;
    mainLambda.body = null;
    mainLambda = null;
    if (! immediate)
      litTable = null;
  }

  public void compileToArchive (ModuleExp mexp, String fname)
    throws java.io.IOException
  {
    boolean makeJar = false;
    if (fname.endsWith(".zip"))
      makeJar = false;
    else if (fname.endsWith(".jar"))
      makeJar = true;
    else
      {
	fname = fname + ".zip";
	makeJar = false;
      }

    process(COMPILED);

    File zar_file = new File (fname);
    if (zar_file.exists ())
      zar_file.delete ();
    ZipOutputStream zout;
    /* #ifdef JAVA2 */
    if (makeJar)
      zout = new java.util.jar.JarOutputStream(new FileOutputStream(zar_file));
    else
    /* #endif */
      zout = new ZipOutputStream(new FileOutputStream(zar_file));

    byte[][] classBytes = new byte[numClasses][];
    CRC32 zcrc = new CRC32();
    for (int iClass = 0;  iClass < numClasses;  iClass++)
      {
	ClassType clas = classes[iClass];
	classBytes[iClass] = clas.writeToArray ();
	ZipEntry zent = new ZipEntry(clas.getName ().replace ('.', '/')
				     + ".class");

	zent.setSize(classBytes[iClass].length);
	zcrc.reset();
	zcrc.update(classBytes[iClass], 0, classBytes[iClass].length);
	zent.setCrc(zcrc.getValue());

	zout.putNextEntry (zent);
	zout.write (classBytes[iClass]);
      }
    zout.close ();
  }

  // FIXME - make this settable, as it does make .class files bigger.
  public static boolean emitSourceDebugExtAttr = true;

  private void registerClass (ClassType new_class)
  {
    if (classes == null)
      classes = new ClassType[20];
    else if (numClasses >= classes.length)
      {
	ClassType[] new_classes = new ClassType[2 * classes.length];
	System.arraycopy (classes, 0, new_classes, 0, numClasses);
	classes = new_classes;
      }
    new_class.addModifiers(new_class.isInterface() ? Access.PUBLIC
                           : Access.PUBLIC|Access.SUPER);
    if (new_class == mainClass && numClasses > 0)
      {
        // Ensure mainClass is written first when writing an archive.
        new_class = classes[0];
        classes[0] = mainClass;
      }
    classes[numClasses++] = new_class;
  }

  public void addClass (ClassType new_class)
  {
    String fname = getModule().filename;
    if (fname != null)
      {
	if (emitSourceDebugExtAttr)
	  new_class.setStratum(getLanguage().getName());
	new_class.setSourceFile(fname);
      }
    registerClass(new_class);
    new_class.setClassfileVersion(defaultClassFileVersion);
  }

  public boolean makeRunnable ()
  {
    return ! generatingServlet() && ! generatingApplet()
      && ! getModule().staticInitRun()
      && ! getModule().getFlag(ModuleExp.USE_DEFINED_CLASS);
  }

  public void addMainClass (ModuleExp module)
  {
    mainClass = module.classFor(this);
    ClassType type = mainClass;
    ClassType[] interfaces = module.getInterfaces();
    if (interfaces != null)
      type.setInterfaces(interfaces);
    ClassType sup = module.getSuperType();
    if (sup == null)
      {
        if (generatingApplet())
	  sup = typeApplet;
	else if (generatingServlet())
	  sup = typeServlet;
        else if (module.getFlag(ModuleExp.USE_DEFINED_CLASS))
          sup = Type.objectType;
	else
	  sup = getModuleType();
      }
    if (makeRunnable())
      type.addInterface(typeRunnable);
    type.addInterface(typeRunnableModule);
    type.setSuper(sup);

    module.compiledType = type;
    addClass(type);
  }

  public final Method getConstructor (LambdaExp lexp)
  {
    return getConstructor(lexp.getHeapFrameType(), lexp);
  }

  public static final Method getConstructor (ClassType clas, LambdaExp lexp)
  {
    Method meth = clas.getDeclaredMethod("<init>", 0);
    if (meth != null)
      return meth;
    Type[] args;
    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	args = new Type[1];
	args[0] = lexp.staticLinkField.getType();
      }
    else
      args = apply0args;
    return clas.addMethod("<init>", Access.PUBLIC, args, Type.voidType);
  }

  public final void generateConstructor (LambdaExp lexp)
  {
    generateConstructor (lexp.getHeapFrameType(), lexp);
  }

  public final void generateConstructor (ClassType clas, LambdaExp lexp)
  {
    Method save_method = method;
    Variable callContextSave = callContextVar;
    callContextVar = null;
    ClassType save_class = curClass;
    curClass = clas;
    Method constructor_method = getConstructor(clas, lexp);
    clas.constructor = constructor_method;
    method = constructor_method;
    CodeAttr code = constructor_method.startCode();

    if (lexp instanceof ClassExp && lexp.staticLinkField != null)
      {
	code.emitPushThis();
	code.emitLoad(code.getCurrentScope().getVariable(1));
	code.emitPutField(lexp.staticLinkField);
      }
    ClassType superClass = clas.getSuperclass();
    ClassExp.invokeDefaultSuperConstructor(superClass, this, lexp);

    if (curClass == mainClass
        // Optimization: No point in calling ModuleInfo.register if we aren't
        // compiling a named module.
        && getMinfo() != null && getMinfo().sourcePath != null
        && ! getModule().getFlag(ModuleExp.USE_DEFINED_CLASS))
      {
	code.emitPushThis();
	code.emitInvokeStatic(ClassType.make("gnu.expr.ModuleInfo")
                              .getDeclaredMethod("register", 1));
      }

    if (lexp != null && lexp.initChain != null)
      {
	// Create dummy lambda, for its closureEnv.  This may be needed
	// if init.value contains a reference that uses our heap frame.
	LambdaExp save = curLambda;
	curLambda = new LambdaExp();
	curLambda.closureEnv = code.getArg(0);
	curLambda.setOuter(save);
        Initializer init;
	while ((init = lexp.initChain) != null)
	  {
	    lexp.initChain = null;
	    dumpInitializers(init);
	  }
	curLambda = save;
      }

    if (lexp instanceof ClassExp)
      {
	ClassExp cexp = (ClassExp) lexp;
	callInitMethods(cexp.getCompiledClassType(this),
                        new ArrayList<ClassType>(10));
      }

    code.emitReturn();
    method = save_method;
    curClass = save_class;
    callContextVar = callContextSave;
  }

  /** In an <init> for a generated ClassExp, emit $finit$ calls.
   * This recursively traverses superclasses, and also calls their $finit$.
   * @param clas Class to search for $finit$, and to search supertypes.
   * @param seen array of seen classes, to avoid duplicate $finit$ calls.
   */
  void callInitMethods (ClassType clas, ArrayList<ClassType> seen)
  {
    if (clas == null)
      return;

    String name = clas.getName();
    if ("java.lang.Object".equals(name))
      return;
    // Check for duplicates.
    for (int i = seen.size();  --i >= 0; )
      if (seen.get(i).getName() == name)
	return;
    seen.add(clas);

    // Recusive call to emit $finit$ of super-types.  However, don't do that
    // for clas.getSuperclass(), because our <init> will automatically call
    // the super-class's <init>, which will call its $finit$.
    ClassType[] interfaces = clas.getInterfaces();
    if (interfaces != null)
      {
	int n = interfaces.length;
	for (int i = 0;  i < n;  i++)
	  callInitMethods(interfaces[i], seen);
      }

    int clEnvArgs = 1;
    if (clas.isInterface())
      {	
        if (clas instanceof PairClassType)
          clas = ((PairClassType) clas).instanceType;
        else
          {
            try
              {
                clas = ((ClassType)
                        Type.make(Class.forName(clas.getName() + "$class")));
              }
            catch (Exception ex)
              {
                return;
              }
          }
      }
    else
      clEnvArgs = 0;
    Method meth = clas.getDeclaredMethod("$finit$", clEnvArgs);
    if (meth != null)
      {
	CodeAttr code = getCode();
	code.emitPushThis();
	code.emitInvoke(meth);
      }
  }

  public void generateMatchMethods(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    Method save_method = method;
    ClassType save_class = curClass;
    ClassType procType = typeModuleMethod;
    curClass = lexp.getHeapFrameType();
    if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
      curClass = moduleClass;
    CodeAttr code = null;
    for (int i = 0;  i <= 5; i++)
      {
	boolean needThisMatch = false;
	SwitchState aswitch = null;
	String mname = null;
	Type[] matchArgs = null;
	for (int j = numApplyMethods;  --j >= 0; )
	  {
	    LambdaExp source = lexp.applyMethods.get(j);
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    Method[] primMethods = source.primMethods;
	    int numMethods = primMethods.length;
	    boolean varArgs = source.max_args < 0
	      || source.max_args >= source.min_args + numMethods;
	    int methodIndex;
	    if (i < 5) // Handling match0 .. match4
	      {
		methodIndex = i - source.min_args;
		if (methodIndex < 0 || methodIndex >= numMethods
		    || (methodIndex == numMethods - 1 && varArgs))
		  continue;
		numMethods = 1;
		varArgs = false;
	      }
	    else // Handling matchN
	      {
		methodIndex = 5 - source.min_args;
		if (methodIndex > 0 && numMethods <= methodIndex && ! varArgs)
		  continue;
		methodIndex = numMethods-1;
	      }
	    if (! needThisMatch)
	      {
		// First LambdaExp we seen suitable for this i.
		if (i < 5)
		  {
		    mname = "match"+i;
		    matchArgs = new Type[i + 2];
		    for (int k = i;  k >= 0;  k--)
		      matchArgs[k+1] = typeObject;
		    matchArgs[i+1] = typeCallContext;
		  }
		else
		  {
		    mname = "matchN";
		    matchArgs = new Type[3];
		    matchArgs[1] = objArrayType;
		    matchArgs[2] = typeCallContext;
		  }
		matchArgs[0] = procType;
		method = curClass.addMethod (mname, matchArgs, Type.intType,
					     Access.PUBLIC);
		code = method.startCode();

		code.emitLoad(code.getArg(1)); // method
		code.emitGetField(procType.getField("selector"));
		aswitch = code.startSwitch();

		needThisMatch = true;
	      }

	    aswitch.addCase(source.getSelectorValue(this), code);

	    int line = source.getLineNumber();
	    if (line > 0)
	      code.putLineNumber(source.getFileName(), line);

	    Variable ctxVar = code.getArg(i == 5 ? 3 : i+2);

	    if (i < 5)
	      {
		Declaration var = source.firstDecl();
		for (int k = 1;  k <= i;  k++)
		  {
		    code.emitLoad(ctxVar);
		    code.emitLoad(code.getArg(k+1));
		    Type ptype = var.getType();
		    if (ptype != Type.objectType)
		      {
                        StackTarget.forceLazyIfNeeded(this, Type.objectType, ptype);
			if (ptype instanceof TypeValue)
			  {
			    Label trueLabel = new Label(code),
			      falseLabel = new Label(code);
			    ConditionalTarget ctarget =
			      new ConditionalTarget(trueLabel, falseLabel,
						    getLanguage());
			    code.emitDup();
			    ((TypeValue) ptype).emitIsInstance(null, this,
							       ctarget);
			    falseLabel.define(code);
			    code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|k);
			    code.emitReturn();
			    trueLabel.define(code);
			  }
			else if (ptype instanceof ClassType
			    && ptype != Type.objectType
			    && ptype != Type.toStringType)  // FIXME
			  {
			    code.emitDup();
			    ptype.emitIsInstance(code);
			    code.emitIfIntEqZero();
			    code.emitPushInt(MethodProc.NO_MATCH_BAD_TYPE|k);
			    code.emitReturn();
			    code.emitFi();
			  }
		      }
		    code.emitPutField(typeCallContext.getField("value"+k));
		    var = var.nextDecl();
		  }
	      }
	    else
	      {
		// FIXME - need to check
		code.emitLoad(ctxVar);
		code.emitLoad(code.getArg(2));
		code.emitPutField(typeCallContext.getField("values"));
	      }
	    code.emitLoad(ctxVar);
            boolean usingCallContext = usingCallContext();
            if (usingCallContext)
	      code.emitLoad(code.getArg(0)); // this (module)
	    else
	      code.emitLoad(code.getArg(1)); // proc
	    code.emitPutField(procCallContextField);
	    code.emitLoad(ctxVar);
	    if (usingCallContext)
	      code.emitPushInt(source.getSelectorValue(this)+methodIndex);
	    else
	      code.emitPushInt(i);
	    code.emitPutField(pcCallContextField);
	    code.emitPushInt(0);
	    code.emitReturn();
          }
	if (needThisMatch)
	  {
	    aswitch.addDefault(code);
	    int nargs = i > 4 ? 2 : i + 1;
	    nargs++;
	    for (int k = 0;  k <= nargs;  k++)
	      code.emitLoad(code.getArg(k));
	    Method defMethod = (typeModuleBody
				.getDeclaredMethod(mname, matchArgs.length));
	    code.emitInvokeSpecial(defMethod);
	    code.emitReturn();
	    aswitch.finish(code);
	  }
      }
    method = save_method;
    curClass = save_class;
  }

  /** Generate ModuleBody's <tt>apply(CallContext)</tt> method
   * Use the <tt>applyMethods</tt> vector, which contains methods that
   * implement the (public, readable) methods of the current module. */
  public void generateApplyMethodsWithContext(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    ClassType save_class = curClass;
    curClass = lexp.getHeapFrameType();
    if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
      curClass = moduleClass;
    ClassType procType = typeModuleMethod;
    Method save_method = method;
    CodeAttr code = null;
    Type[] applyArgs = { typeCallContext };

    // First LambdaExp we seen suitable for this i.
    method = curClass.addMethod ("apply", applyArgs,
				 (Type) Type.voidType,
				 Access.PUBLIC);
    code = method.startCode();
    Variable ctxVar = code.getArg(1);

    code.emitLoad(ctxVar);
    code.emitGetField(pcCallContextField);
    SwitchState aswitch = code.startSwitch();

    for (int j = 0;  j < numApplyMethods;  ++j)
      {
	LambdaExp source = lexp.applyMethods.get(j);
        if (! source.usingCallContext())
          continue;
	Method[] primMethods = source.primMethods;
	int numMethods = primMethods.length;

	for (int i = 0; i < numMethods; i++)
	  {
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    boolean varArgs
	      = (i == numMethods - 1
		 && (source.max_args < 0
		     || source.max_args >= source.min_args + numMethods));
	    int methodIndex = i;

	    aswitch.addCase(source.getSelectorValue(this) + i, code);

            SourceLocator saveLoc1 = messages.swapSourceLocator(source);
	    int line = source.getLineNumber();
	    if (line > 0)
	      code.putLineNumber(source.getFileName(), line);

	    Method primMethod = primMethods[methodIndex];
	    Type[] primArgTypes = primMethod.getParameterTypes();
	    int singleArgs = source.min_args+methodIndex;
	    Variable counter = null;
	    int pendingIfEnds = 0;

	    if (i > 4 && numMethods > 1) // FIXME
	      {
		counter = code.addLocal(Type.intType);
		code.emitLoad(ctxVar);
                code.emitInvoke(typeCallContext.getDeclaredMethod("getArgCount", 0));
		if (source.min_args != 0)
		  {
		    code.emitPushInt(source.min_args);
		    code.emitSub(Type.intType);
		  }
		code.emitStore(counter);
	      }

	    int needsThis = primMethod.getStaticFlag() ? 0 : 1;
            int explicitFrameArg
              = singleArgs + (varArgs ? 2 : 1) < primArgTypes.length ? 1 : 0;
	    if (needsThis + explicitFrameArg > 0)
	      {
		code.emitPushThis();
		if (curClass == moduleClass && mainClass != moduleClass)
		  code.emitGetField(moduleInstanceMainField);
	      }

	    Declaration var = source.firstDecl();
            if (var != null && var.isThisParameter())
              var = var.nextDecl();
	    for (int k = 0; k < singleArgs;  k++)
	      {
		if (counter != null && k >= source.min_args)
		  {
		    code.emitLoad(counter);
		    code.emitIfIntLEqZero();
		    code.emitLoad(ctxVar);
		    code.emitInvoke(primMethods[k - source.min_args]);
		    code.emitElse();
		    pendingIfEnds++;
		    code.emitInc(counter, (short) (-1));
		  }

		code.emitLoad(ctxVar);
		if (k <= 4 && ! varArgs && source.max_args <= 4)
		  code.emitGetField(typeCallContext
				    .getDeclaredField("value"+(k+1)));
		else
		  {
		    code.emitGetField(typeCallContext
				      .getDeclaredField("values"));
		    code.emitPushInt(k);
		    code.emitArrayLoad(Type.objectType);
		  }
		Type ptype = var.getType();
		if (ptype != Type.objectType)
                  {
                    SourceLocator saveLoc2 = messages.swapSourceLocator(var);
                    CheckedTarget.emitCheckedCoerce(this, source, k+1,
                                                    Type.objectType, ptype,
                                                    null);
                    messages.swapSourceLocator(saveLoc2);
                  }
		var = var.nextDecl();
	      }

	    if (varArgs)
	      {
		Type lastArgType = primArgTypes[explicitFrameArg+singleArgs];
		if (lastArgType instanceof ArrayType)
                  varArgsToArray(source, singleArgs, counter, lastArgType, ctxVar);
		else if ("gnu.lists.LList".equals
			 (lastArgType.getName()))
		  {	
		    code.emitLoad(ctxVar);
		    code.emitPushInt(singleArgs);
		    code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsList", 1));
		  }
		else if (lastArgType == typeCallContext)
		  code.emitLoad(ctxVar);
		else
		  throw new RuntimeException("unsupported #!rest type:"+lastArgType);
              }
	    code.emitLoad(ctxVar); // get $ctx
	    code.emitInvoke(primMethod);
	    while (--pendingIfEnds >= 0)
	      code.emitFi();
	    if (! usingCallContext())
	      Target.pushObject.compileFromStack(this,
						 source.getReturnType());
            messages.swapSourceLocator(saveLoc1);
	    code.emitReturn();
          }
      }
    aswitch.addDefault(code);
    Method errMethod = typeModuleMethod.getDeclaredMethod("applyError", 0);
    code.emitInvokeStatic(errMethod);
    code.emitReturn();
    aswitch.finish(code);
    method = save_method;
    curClass = save_class;
  }

  /** Generate ModuleBody's <tt>apply0</tt>...<tt>applyN</tt> methods.
   * Use the <tt>applyMethods</tt> vector, which contains methods that
   * implement the (public, readable) methods of the current module.
   */
  public void generateApplyMethodsWithoutContext(LambdaExp lexp)
  {
    int numApplyMethods
      = lexp.applyMethods == null ? 0 : lexp.applyMethods.size();
    if (numApplyMethods == 0)
      return;
    ClassType save_class = curClass;
    curClass = lexp.getHeapFrameType();
    ClassType procType = typeModuleMethod;
    if (! (curClass.getSuperclass().isSubtype(typeModuleBody)))
      curClass = moduleClass;
    Method save_method = method;
    CodeAttr code = null;
    for (int i = usingCallContext() ? 5 : 0; i < 6; i++)
      {
	// If i < 5, generate the method named ("apply"+i);
	// else generate "applyN".
	boolean needThisApply = false;
	SwitchState aswitch = null;
	String mname = null;
	Type[] applyArgs = null;

	for (int j = 0;  j < numApplyMethods;  j++)
	  {
	    LambdaExp source = lexp.applyMethods.get(j);
            if (source.usingCallContext())
              continue;
	    // Select the subset of source.primMethods[*] that are suitable
	    // for the current apply method.
	    Method[] primMethods = source.primMethods;
	    int numMethods = primMethods.length;
	    boolean varArgs = source.max_args < 0
	      || source.max_args >= source.min_args + numMethods;
	    int methodIndex;
	    boolean skipThisProc = false;
	    if (i < 5) // Handling apply0 .. apply4
	      {
		methodIndex = i - source.min_args;
		if (methodIndex < 0 || methodIndex >= numMethods
		    || (methodIndex == numMethods - 1 && varArgs))
		  skipThisProc = true;
		numMethods = 1;
		varArgs = false;
	      }
	    else // Handling applyN
	      {
		methodIndex = 5 - source.min_args;
		if (methodIndex > 0 && numMethods <= methodIndex && ! varArgs)
		  skipThisProc = true;
		methodIndex = numMethods-1;
	      }
	    if (skipThisProc)
	      continue;
	    if (! needThisApply)
	      {
		// First LambdaExp we seen suitable for this i.
		if (i < 5)
		  {
		    mname =  "apply"+i;
		    applyArgs = new Type[i + 1];
		    for (int k = i;  k > 0;  k--)
		      applyArgs[k] = typeObject;
		  }
		else
		  {
		    mname = "applyN";
		    applyArgs = new Type[2];
		    applyArgs[1] = objArrayType;
		  }
		applyArgs[0] = procType;
		method = curClass.addMethod (mname, applyArgs,
					     usingCallContext() ? (Type) Type.voidType : (Type) Type.objectType,
					     Access.PUBLIC);
		code = method.startCode();

		code.emitLoad(code.getArg(1)); // method
		code.emitGetField(procType.getField("selector"));
		aswitch = code.startSwitch();

		needThisApply = true;
	      }

	    aswitch.addCase(source.getSelectorValue(this), code);

            SourceLocator saveLoc1 = messages.swapSourceLocator(source);
	    int line = source.getLineNumber();
	    if (line > 0)
	      code.putLineNumber(source.getFileName(), line);

	    Method primMethod = primMethods[methodIndex];
	    Type[] primArgTypes = primMethod.getParameterTypes();
	    int singleArgs = source.min_args+methodIndex;
	    Variable counter = null;
	    int pendingIfEnds = 0;

	    if (i > 4 && numMethods > 1)
	      {
		counter = code.addLocal(Type.intType);
		code.emitLoad(code.getArg(2));
		code.emitArrayLength();
		if (source.min_args != 0)
		  {
		    code.emitPushInt(source.min_args);
		    code.emitSub(Type.intType);
		  }
		code.emitStore(counter);
	      }

	    int needsThis = primMethod.getStaticFlag() ? 0 : 1;
            int explicitFrameArg
              = singleArgs + (varArgs ? 1 : 0) < primArgTypes.length ? 1 : 0;
	    if (needsThis + explicitFrameArg > 0)
	      {
		code.emitPushThis();
		if (curClass == moduleClass && mainClass != moduleClass)
		  code.emitGetField(moduleInstanceMainField);
	      }

	    Declaration var = source.firstDecl();
            if (var != null && var.isThisParameter())
              var = var.nextDecl();
	    for (int k = 0; k < singleArgs;  k++)
	      {
		if (counter != null && k >= source.min_args)
		  {
		    code.emitLoad(counter);
		    code.emitIfIntLEqZero();
		    code.emitInvoke(primMethods[k - source.min_args]);
		    code.emitElse();
		    pendingIfEnds++;
		    code.emitInc(counter, (short) (-1));
		  }

		Variable pvar = null;
		if (i <= 4) // apply'i method
		  {
		    pvar = code.getArg(k + 2);
		    code.emitLoad(pvar);
		  }
		else // applyN method
		  {
		    // Load Object[]args value:
		    code.emitLoad(code.getArg(2));
		    code.emitPushInt(k);
		    code.emitArrayLoad(Type.objectType);
		  }
		Type ptype = var.getType();
		if (ptype != Type.objectType)
                  {
                    SourceLocator saveLoc2 = messages.swapSourceLocator(var);
                    CheckedTarget.emitCheckedCoerce(this, source, k+1,
                                                    Type.objectType, ptype,
                                                    pvar);
                    messages.swapSourceLocator(saveLoc2);
                  }
		var = var.nextDecl();
	      }

	    if (varArgs)
	      {
		Type lastArgType = primArgTypes[explicitFrameArg+singleArgs];
		if (lastArgType instanceof ArrayType)
                  varArgsToArray(source, singleArgs, counter, lastArgType, null);
		else if ("gnu.lists.LList".equals
			 (lastArgType.getName()))
		  {	
		    code.emitLoad(code.getArg(2)); // load args array.
		    code.emitPushInt(singleArgs);
		    code.emitInvokeStatic(Compilation.makeListMethod);
		  }
		else if (lastArgType == typeCallContext)
		  code.emitLoad(code.getArg(2));
		else
		  throw new RuntimeException("unsupported #!rest type:"+lastArgType);
              }
	    code.emitInvoke(primMethod);
	    while (--pendingIfEnds >= 0)
	      code.emitFi();
	    if (! usingCallContext())
	      Target.pushObject.compileFromStack(this,
						 source.getReturnType());
            messages.swapSourceLocator(saveLoc1);
	    code.emitReturn();
          }
	if (needThisApply)
	  {
	    aswitch.addDefault(code);
	    if (usingCallContext())
	      {
		Method errMethod
		  = typeModuleMethod.getDeclaredMethod("applyError", 0);
		code.emitInvokeStatic(errMethod);
	      }
	    else
	      {
		int nargs = i > 4 ? 2 : i + 1;
		nargs++;
		for (int k = 0; k < nargs;  k++)
		  code.emitLoad(code.getArg(k));
		code.emitInvokeSpecial(typeModuleBody.getDeclaredMethod(mname, applyArgs));
	      }
	    code.emitReturn();
	    aswitch.finish(code);
	  }
      }
    method = save_method;
    curClass = save_class;
  }

  /** Copy incoming arguments to varargs/#!rest array.
   */
  private void varArgsToArray (LambdaExp source, int singleArgs,
                               Variable counter, Type lastArgType,
                               Variable ctxVar)
  {
    CodeAttr code = getCode();
    Type elType = ((ArrayType) lastArgType).getComponentType();
    boolean mustConvert = ! "java.lang.Object".equals(elType.getName());
    if (ctxVar != null && ! mustConvert)
      {
        code.emitLoad(ctxVar);
        code.emitPushInt(singleArgs);
        code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getRestArgsArray", 1));
      }
    else if (singleArgs == 0 && ! mustConvert)
      code.emitLoad(code.getArg(2)); // load args array.
    else
      {
        code.pushScope();
        if (counter == null)
          {
            counter = code.addLocal(Type.intType);
            if (ctxVar != null)
              {
                code.emitLoad(ctxVar);
                code.emitInvoke(typeCallContext.getDeclaredMethod("getArgCount", 0));
              }
            else
              {
                code.emitLoad(code.getArg(2));
                code.emitArrayLength();
              }
            if (singleArgs != 0)
              {
                code.emitPushInt(singleArgs);
                code.emitSub(Type.intType);
              }
            code.emitStore(counter);
          }
        code.emitLoad(counter);
        code.emitNewArray(elType.getImplementationType());
        Label testLabel = new Label(code);
        Label loopTopLabel = new Label(code);
        loopTopLabel.setTypes(code);
        code.emitGoto(testLabel);
        loopTopLabel.define(code);

        code.emitDup(1); // new array
        code.emitLoad(counter);
        if (ctxVar != null)
          code.emitLoad(ctxVar);
        else
          code.emitLoad(code.getArg(2));
        code.emitLoad(counter);
        if (singleArgs != 0)
          {
            code.emitPushInt(singleArgs);
            code.emitAdd(Type.intType);
          }
        if (ctxVar != null)
          code.emitInvokeVirtual(typeCallContext.getDeclaredMethod("getArgAsObject", 1));
        else
          code.emitArrayLoad(Type.objectType);
        if (mustConvert)
          {
            CheckedTarget.emitCheckedCoerce
              (this, source, source.getName(),
               0, elType, null);
          }
        code.emitArrayStore(elType);
        testLabel.define(code);
        code.emitInc(counter, (short) (-1));
        code.emitLoad(counter);
        code.emitGotoIfIntGeZero(loopTopLabel);
        code.popScope();	
      }
  }

  private Method startClassInit ()
  {
    method = curClass.addMethod ("<clinit>", apply0args, Type.voidType,
				 Access.PUBLIC|Access.STATIC);

    CodeAttr code = method.startCode();

    if (generateMainMethod() || generatingApplet() || generatingServlet())
      {
	ClassType languageType
	  = (ClassType) Type.make(getLanguage().getClass());
	Method registerMethod
	  = languageType.getDeclaredMethod("registerEnvironment", 0);
	if (registerMethod != null)
	  code.emitInvokeStatic(registerMethod);
      }
    return method;
  }

  /** Parse/visit/compile this module as needed and requested.
   * This method does not process any dependent modules (expect indirectly,
   * such as may be done by a require form).
   * @param wantedState the desired value of getState().
   */
  public void process (int wantedState)
  {
    Compilation saveCompilation = Compilation.setSaveCurrent(this);
    try
      {
        ModuleExp mexp = getModule();
        if (wantedState >= BODY_PARSED && getState() < BODY_PARSED-1)
          {
            setState(BODY_PARSED-1);
            language.parse(this, 0);
            mexp.classFor(this);
            if (lexer != null)
                lexer.close();
            lexer = null;
            setState(messages.seenErrors() ? ERROR_SEEN : BODY_PARSED);
            if (pendingImports != null)
              return;
          }
        if (wantedState >= RESOLVED && getState() < RESOLVED)
          {
            language.resolve(this);
            // Doing addMainClass is a bit flakey in the case that
            // ModuleExp.alwaysCompile is false.  We don't want to
            // call addMainClass *unless* we're compiling, but when
            // dealing with eval, mutually recursive modules, etc
            // it doesn't quite work.
            addMainClass(mexp);
            setState(messages.seenErrors() ? ERROR_SEEN : RESOLVED);
          }

        // Avoid writing class needlessly.
        if (! explicit && ! immediate
            && getMinfo().checkCurrent(ModuleManager.getInstance(), System.currentTimeMillis()))
          {
            getMinfo().cleanupAfterCompilation();
            setState(CLASS_WRITTEN);
          }

        if (wantedState >= PRE_WALKED && getState() < PRE_WALKED)
          {
            if (debugPrintExpr) {
                OutPort dout = OutPort.errDefault();
                dout.println("[Module:" + mexp.getName());
                mexp.print(dout);
                dout.println(']');
                dout.flush();
            }
            PushApply.pushApply(mexp, this);  
            setState(messages.seenErrors() ? ERROR_SEEN : PRE_WALKED);
          }

        if (wantedState >= WALKED && getState() < WALKED)
          {
            InlineCalls.inlineCalls(mexp, this);
            ChainLambdas.chainLambdas(mexp, this);
            FindTailCalls.findTailCalls(mexp, this);
            setState(messages.seenErrors() ? ERROR_SEEN : WALKED);
          }

        if (wantedState >= COMPILE_SETUP && getState() < COMPILE_SETUP)
          {
            litTable = new LitTable(this);
            mexp.setCanRead(true);
            FindCapturedVars.findCapturedVars(mexp, this);
            mexp.allocFields(this);
            mexp.allocChildMethods(this);
            setState(messages.seenErrors() ? ERROR_SEEN : COMPILE_SETUP);
          }
        if (wantedState >= COMPILED && getState() < COMPILED)
          {
            if (immediate)
              {
                ClassLoader parentLoader = ObjectType.getContextClassLoader();
                loader = new ArrayClassLoader(parentLoader);
              }
            generateBytecode();
            setState(messages.seenErrors() ? ERROR_SEEN : COMPILED);
          }
        if (wantedState >= CLASS_WRITTEN && getState() < CLASS_WRITTEN)
          {
            if (! (mexp.getFlag(ModuleExp.HAS_SUB_MODULE)
                   && mexp.body == QuoteExp.voidExp && mexp.firstDecl() == null))
                outputClass(ModuleManager.getInstance().getCompilationDirectory());
            setState(CLASS_WRITTEN);
          }
      }
    catch (gnu.text.SyntaxException ex)
      {
        setState(ERROR_SEEN);
        if (ex.getMessages() != getMessages())
          throw new RuntimeException ("confussing syntax error: "+ex);
        // otherwise ignore it - it's already been recorded in messages.
      }
    catch (java.io.IOException ex)
      {
        ex.printStackTrace();
        error('f', "caught "+ex);
        setState(ERROR_SEEN);
      }
    finally
      {
        Compilation.restoreCurrent(saveCompilation);
      }
  }

  /** The guts of compiling a module to one or more classes.
   * Assumes walkModule has been done.
   */
  void generateBytecode ()
  {
    ModuleExp module = getModule();
    if (debugPrintFinalExpr)
      {
	OutPort dout = OutPort.errDefault();
	dout.println ("[Compiling final "+module.getName()
                     + " to " + mainClass.getName() + ":");
	module.print(dout);
	dout.println(']');
	dout.flush();
      }

    ClassType neededSuper = getModuleType();
    if (mainClass.getSuperclass().isSubtype(neededSuper)
        && ! module.getFlag(ModuleExp.USE_DEFINED_CLASS))
      moduleClass = mainClass;
    else
      {
	moduleClass = new ClassType(generateClassName("frame"));
	moduleClass.setSuper(neededSuper);
	addClass(moduleClass);
	generateConstructor(moduleClass, null);
      }

    curClass = module.compiledType;
    int arg_count;
    LambdaExp saveLambda = curLambda;
    curLambda = module;
    Type[] arg_types;
    if (module.isHandlingTailCalls()) // Is this ever false?
      {
	arg_count = 1;
	arg_types = new Type[1];
	arg_types[0] = typeCallContext;
      }
    else if (module.min_args != module.max_args || module.min_args > 4)
      { // Likely dead code.
	arg_count = 1;
	arg_types = new Type[1];
	arg_types[0] = new ArrayType (typeObject);
      }
    else
      { // Likely dead code.
	arg_count = module.min_args;
	arg_types = new Type[arg_count];
	for (int i = arg_count;  --i >= 0; )
	  arg_types[i] = typeObject;
      }

    CodeAttr code;
    Variable heapFrame = module.heapFrame;
    boolean staticModule = module.isStatic();
    Method apply_method;
    
    apply_method = curClass.addMethod ("run", arg_types, Type.voidType,
				       Access.PUBLIC+Access.FINAL);
    method = apply_method;
    // For each parameter, assign it to its proper slot.
    // If a parameter !isSimple(), we cannot assign it to a local slot,
    // so instead create an artificial Variable for the incoming argument.
    // Below, we assign the value to the slot.
    method.initCode();
    code = getCode();
    // if (usingCPStyle())   code.addParamLocals();

    thisDecl = method.getStaticFlag() ? null : module.declareThis(module.compiledType);
    module.closureEnv = module.thisVariable;
    module.heapFrame = module.isStatic() ? null : module.thisVariable;
    module.allocChildClasses(this);

    if (module.isHandlingTailCalls() || usingCPStyle())
      {
	callContextVar = new Variable ("$ctx", typeCallContext);
	module.getVarScope().addVariableAfter(thisDecl, callContextVar);
	callContextVar.setParameter(true);
      }

    int line = module.getLineNumber();
    if (line > 0)
      code.putLineNumber(module.getFileName(), line);

    module.allocParameters(this);
    module.enterFunction(this);
    if (usingCPStyle())
      {
	loadCallContext();
        code.emitGetField(pcCallContextField);
        fswitch = code.startSwitch();
	fswitch.addCase(0, code);
      }

    module.compileBody(this);
    module.compileEnd(this);

    Label startLiterals = null;
    Label afterLiterals = null;
    Method initMethod = null;

    if (curClass == mainClass) // redundant - never false.
      {
	Method save_method = method;
        Variable callContextSave = callContextVar;
        callContextVar = null;

	initMethod = startClassInit();
        clinitMethod = initMethod;
	code = getCode();

        startLiterals = new Label(code);
        afterLiterals = new Label(code);
        code.fixupChain(afterLiterals, startLiterals);

	if (staticModule)
	  {
            if (! module.getFlag(ModuleExp.USE_DEFINED_CLASS))
              generateConstructor(module);

	    code.emitNew(moduleClass);
	    code.emitDup(moduleClass);
	    code.emitInvokeSpecial(moduleClass.constructor);
            // The $instance field needs to be public so
            // ModuleContext.findInstance can find it.
            // It needs to be non-final in case moduleClass!=mainClass.
            // (The latter should probably be fixed by moving this code
            // to moduleClass's <clinit>.)
	    moduleInstanceMainField
	      = moduleClass.addField("$instance", moduleClass,
				     Access.STATIC|Access.PUBLIC);
	    code.emitPutStatic(moduleInstanceMainField);
	  }
        Initializer init;
        while ((init = clinitChain) != null)
          {
            clinitChain = null;
            dumpInitializers(init);
          }

	if (module.staticInitRun())
	  {
	    code.emitGetStatic(moduleInstanceMainField);
	    code.emitInvoke(typeModuleBody.getDeclaredMethod("run", 0));
	  }
	code.emitReturn();

	if (moduleClass != mainClass && ! staticModule
            && curClass.getSuperclass().getDeclaredMethod("run", 0) == null)
	  {
	    // Compare the run methods in ModuleBody.
	    method = curClass.addMethod("run", Access.PUBLIC,
					Type.typeArray0, Type.voidType);
	    code = method.startCode();
	    Variable ctxVar = code.addLocal(typeCallContext);
	    Variable saveVar = code.addLocal(typeConsumer);
	    Variable exceptionVar = code.addLocal(Type.javalangThrowableType);
	    // ctx = CallContext.getInstance();
	    code.emitInvokeStatic(getCallContextInstanceMethod);
	    code.emitStore(ctxVar);
	    Field consumerFld = typeCallContext.getDeclaredField("consumer");
	    // save = ctx.consumer;
	    code.emitLoad(ctxVar);
	    code.emitGetField(consumerFld);
	    code.emitStore(saveVar);
	    // ctx.consumer = VoidConsumer.instance:
	    code.emitLoad(ctxVar);
	    code.emitGetStatic(ClassType.make("gnu.lists.VoidConsumer")
			       .getDeclaredField("instance"));
	    code.emitPutField(consumerFld);
	    // try {
	    code.emitTryStart(false, Type.voidType);
	    // this.apply(ctx):
	    code.emitPushThis();
	    code.emitLoad(ctxVar);
	    code.emitInvokeVirtual(save_method);
	    // exception = null
	    code.emitPushNull();
	    code.emitStore(exceptionVar);
	    // } catch (Throwable th) { exception = th; }
	    code.emitTryEnd();
	    code.emitCatchStart(exceptionVar);
	    code.emitCatchEnd();
	    code.emitTryCatchEnd();
	    // MooduleBody.runCleanup(ctx, ex, save);
	    code.emitLoad(ctxVar);
	    code.emitLoad(exceptionVar);
	    code.emitLoad(saveVar);
	    code.emitInvokeStatic(typeModuleBody
				  .getDeclaredMethod("runCleanup", 3));
	    code.emitReturn();
	  }

	method = save_method;
        callContextVar = callContextSave;
      }

    module.generateApplyMethods(this);

    curLambda = saveLambda;

    module.heapFrame = heapFrame;  // Restore heapFrame.
    if (usingCPStyle())
      {
	code = getCode();
	fswitch.finish(code);
      }

    if (startLiterals != null || callContextVar != null)
      {
	method = initMethod;
	code = getCode();

	Label endLiterals = new Label(code);
	code.fixupChain(startLiterals, endLiterals);

        if (callContextVarForInit != null)
          {
            code.emitInvokeStatic(getCallContextInstanceMethod);
            code.emitStore(callContextVarForInit);
          }

	try
	  {
            if (immediate)
              {
                code.emitPushInt(registerForImmediateLiterals(this));
                code.emitInvokeStatic(ClassType.make("gnu.expr.Compilation")
                                      .getDeclaredMethod("setupLiterals", 1));
              }
            else
              litTable.emit();
	  }
	catch (Exception ex)
	  {
	    error('e', "Literals: Internal error:" + ex);
	  }
	code.fixupChain(endLiterals, afterLiterals);
      }

    if (generateMainMethod() && curClass == mainClass)
      {
	Type[] args = { new ArrayType(javaStringType) };
	method = curClass.addMethod("main", Access.PUBLIC|Access.STATIC,
				    args, Type.voidType);
				    
	code = method.startCode();

	if (Shell.defaultFormatName != null)
	  {
	    code.emitPushString(Shell.defaultFormatName);
	    code.emitInvokeStatic(ClassType.make("kawa.Shell")
				  .getDeclaredMethod("setDefaultFormat", 1));
	  }
	code.emitLoad(code.getArg(0));
	code.emitInvokeStatic(ClassType.make("gnu.expr.ApplicationMainSupport")
                              .getDeclaredMethod("processArgs", 1));
	if (moduleInstanceMainField != null)
	  code.emitGetStatic(moduleInstanceMainField);
	else
	  {
	    code.emitNew(curClass);
	    code.emitDup(curClass);
	    code.emitInvokeSpecial(curClass.constructor);
	  }
        Method runAsMainMethod = null;
        ClassType superClass = curClass.getSuperclass();
        if (superClass != typeModuleBody)
           runAsMainMethod = superClass.getDeclaredMethod("runAsMain", 0);
        if (runAsMainMethod == null)
            runAsMainMethod = typeModuleBody.getDeclaredMethod("runAsMain", 1);
        code.emitInvoke(runAsMainMethod);
	code.emitReturn();
      }

    String uri;
    if (getMinfo() != null && (uri = getMinfo().getNamespaceUri()) != null)
      {
        // Need to generate a ModuleSet for this class, so XQuery can find
        // this module and other modules in the same namespace.
        ModuleManager manager = ModuleManager.getInstance();
        String mainPrefix = mainClass.getName();
        int dot = mainPrefix.lastIndexOf('.');
        if (dot < 0)
          {
            mainPrefix = "";
          }
        else
          {
            String mainPackage = mainPrefix.substring(0, dot);
            try
              {
                manager.loadPackageInfo(mainPackage);
              }
            catch (ClassNotFoundException ex)
              {
                // Do nothing.
              }
            catch (Exception ex)
              {
                error('e', "error loading map for "+mainPackage+" - "+ex);
              }
            mainPrefix = mainPrefix.substring(0, dot+1);
          }
        ClassType mapClass = new ClassType(mainPrefix + ModuleSet.MODULES_MAP);
        ClassType typeModuleSet = ClassType.make("gnu.expr.ModuleSet");
        mapClass.setSuper(typeModuleSet);
        registerClass(mapClass);

        method = mapClass.addMethod("<init>", Access.PUBLIC,
                                apply0args, Type.voidType);
        Method superConstructor
          = typeModuleSet.addMethod("<init>", Access.PUBLIC,
                                    apply0args, Type.voidType);
        code = method.startCode();
        code.emitPushThis();
        code.emitInvokeSpecial(superConstructor);
        code.emitReturn();

        ClassType typeModuleManager = ClassType.make("gnu.expr.ModuleManager");
        Type[] margs = { typeModuleManager };
        method = mapClass.addMethod("register", margs, Type.voidType,
                                    Access.PUBLIC);
        code = method.startCode();
        Method reg = typeModuleManager.getDeclaredMethod("register", 3);

        for (int i = manager.numModules;  --i >= 0; )
          {
            ModuleInfo mi = manager.modules[i];
            String miClassName = mi.getClassName();
            if (miClassName == null
                || ! miClassName.startsWith(mainPrefix))
              continue;
            String moduleSource = mi.sourcePath;
            String moduleUri = mi.getNamespaceUri();
            code.emitLoad(code.getArg(1));
            compileConstant(miClassName);
            if (! Path.valueOf(moduleSource).isAbsolute())
              try
                {
                  // If the source path was relative, emit it as relative.
                  // But make it relative to the compilation directory,
                  // to allow sources to be moved along with binaries.
                  String path = Path.toURL(manager.getCompilationDirectory())
                      + mainPrefix.replace('.', '/');
                  int plen = path.length();
                  if (plen > 0 && path.charAt(plen-1) != '/')
                    path = path + '/';
                  String sourcePath =
                      Path.toURL(mi.getSourceAbsPathname()).toString();
                  moduleSource = Path.relativize(sourcePath, path);
                }
              catch (Exception ex)
                {
                  throw new WrappedException("exception while fixing up '"
                                             +moduleSource+'\'',
                                             ex);
                }
            compileConstant(moduleSource);
            compileConstant(moduleUri);
            code.emitInvokeVirtual(reg);
          }
        code.emitReturn();
      }
  }

  int localFieldIndex; 
  public Field allocLocalField (Type type, String name)
  {
    if (name == null)
      name = "tmp_"+(++localFieldIndex);
    Field field = curClass.addField(name, type, 0);
    return field;
  }

  /** If non-null, contains the value of the current CallContext. */
  Variable callContextVar;
  Variable callContextVarForInit;

  /** Generate code to push the current CallContext on the JVM stack. */
  public final void loadCallContext()
  {
    CodeAttr code = getCode();
    if (callContextVar != null && ! callContextVar.dead())
      code.emitLoad(callContextVar);
    // We're cautious about re-using a previously extracted CallContext,
    // because it's tricky to manage the variables safely.
    // A possible solution is to inject a Variable into the current scope,
    // and making sure each separate straight-line block has its own scope.
    // (If the current scope is in the same "basic block" as an outer scope,
    // we can use that instead.)  FIXME
    else if (method == clinitMethod)
      {
        // The variable is initialized just after literals.
        callContextVar = new Variable("$ctx", typeCallContext);
        // To make sure it doesn't clash with variables that have already
        // allocated and freed for previous initialzier.
        callContextVar.reserveLocal(code.getMaxLocals(), code);
        code.emitLoad(callContextVar);
        callContextVarForInit = callContextVar;
      }
    else
      {
        code.emitInvokeStatic(getCallContextInstanceMethod);
        code.emitDup();
        callContextVar = new Variable("$ctx", typeCallContext);
        code.getCurrentScope().addVariable(code, callContextVar);
        code.emitStore(callContextVar);
      }
  }

  public void freeLocalField (Field field)
  {
    // FIXME
  }

  /** This may not make sense, except for Lisp-like languages.
   * For those, 'input' an s-expression  from the reader. */
  public Expression parse (Object input)
  {
    throw new Error("unimeplemented parse");
  }

  protected Language language;
  public Language getLanguage() { return language; }

  public LambdaExp currentLambda () { return current_scope.currentLambda (); }

  public final ModuleExp getModule() { return mainLambda; }
  public void setModule(ModuleExp mexp) { mainLambda = mexp; }

  public boolean isStatic() { return mainLambda.isStatic(); }

  /** The same as getModule, until we allow nested modules. */
  public ModuleExp currentModule() { return current_scope.currentModule(); }

  /** Note that we have seen a construct that must be compiled, not evaluated.
   * If we are not inside a lambda (which is always compiled), but
   * only inside the outer-most ModuleExp, note that it must be compiled.
   */
  public void mustCompileHere ()
  {
    if (! mustCompile && ! ModuleExp.compilerAvailable())
      error('e', "this expression must be compiled, but compiler is unavailable");
    mustCompile = true;
  }

  public ScopeExp currentScope() { return current_scope; }

  /** Set <code>currentScope()</code>.
   * Also update the <code>nesting</code> object.
   */
  public void setCurrentScope (ScopeExp scope)
  {
    int scope_nesting = ScopeExp.nesting(scope);
    int current_nesting = ScopeExp.nesting(current_scope);
    while (current_nesting > scope_nesting)
      {
	pop(current_scope);
	current_nesting--;
      }
    ScopeExp sc = scope;
    while (scope_nesting > current_nesting)
      {
	sc = sc.getOuter();
	scope_nesting--;
      }
    while (sc != current_scope)
      {
	pop(current_scope);
        sc = sc.getOuter();
      }
    pushChain(scope, sc);
  }

    public ScopeExp setPushCurrentScope (ScopeExp scope) {
        ScopeExp old = currentScope();
        lexical.pushSaveTopLevelRedefs();
        setCurrentScope(scope);
        return old;
    }

    public void setPopCurrentScope (ScopeExp old) {
        setCurrentScope(old);
        lexical.popSaveTopLevelRedefs();
    }

  void pushChain (ScopeExp scope, ScopeExp limit)
  {
    if (scope != limit)
      {
        pushChain(scope.getOuter(), limit);
        pushScope(scope);
        lexical.push(scope);
      }
  }

  public ModuleExp pushNewModule (Lexer lexer)
  {
    this.lexer = lexer;
    return pushNewModule(lexer.getName());
  }

  public ModuleExp pushNewModule (String filename)
  {
    ModuleExp module = new ModuleExp();
    if (filename != null)
      module.setFile(filename);
    if (generatingApplet() || generatingServlet())
      module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    mainLambda = module;
    if (immediate)
      {
        module.setFlag(ModuleExp.IMMEDIATE);
        new ModuleInfo().setCompilation(this);
      }
    push(module);
    return module;
  }

  public void push (ScopeExp scope)
  {
    pushScope(scope);
    lexical.push(scope);
  }

  public final void pushScope (ScopeExp scope)
  {
    if (! mustCompile
        && (scope.mustCompile()
            || (ModuleExp.compilerAvailable()
                // We set mustCompile if we see a LambdaExp - not because
                // we must but because it is usually desirable.
                && scope instanceof LambdaExp
                && ! (scope instanceof ModuleExp))))
      mustCompileHere();
    scope.setOuter(current_scope);
    current_scope = scope;
  }

  public void pop (ScopeExp scope)
  {
    lexical.pop(scope);
    current_scope = scope.getOuter();
  }

  public final void pop ()
  {
    pop(current_scope);
  }

  public void push (Declaration decl)
  {
    lexical.push(decl);
  }

  public Declaration lookup(Object name, int namespace)
  {
    return lexical.lookup(name, namespace);
  }

  /** Called for classes referenced in bytecode.
   * Since this only does something when immediate, we only care about
   * classes referenced in the bytecode when immediate.
   * It is used to ensure that we can inherit from classes defined when in
   * immediate mode (in Scheme using define-class or similar).
   */
    public void usedClass(Type type) {
        while (type instanceof ArrayType)
            type = ((ArrayType) type).getComponentType();
        if (immediate && type instanceof ClassType) {
            ClassType cl = (ClassType) type;
            for (;;) {
                loader.addClass(cl);
                ClassType enc = cl.getDeclaringClass();
                if (enc == null)
                    break;
                cl = enc;
            }
        }
    }

    /** Set module name - which sets name of generated class. */
    public void setModuleName(String name) {
        getModule().setName(name);
    }

    /** Generate and set unique module name suitable for an interactive session. */
    public void setInteractiveName() {
        setModuleName(ModuleManager.getInstance().getNewInteractiveName());
    }

    /** Generate and set unique module name suitable for a call to eval. */
    public void setEvalName() {
        setModuleName(ModuleManager.getInstance().getNewEvalName());
    }

  public SourceMessages getMessages() { return messages; }
  public void setMessages (SourceMessages messages)
  { this.messages = messages; }
 
  public void error(char severity, String message, SourceLocator location)
  {
    String file = location.getFileName();
    int line = location.getLineNumber();
    int column = location.getColumnNumber();
    if (file == null || line <= 0)
      {
        file = getFileName();
        line = getLineNumber();
        column = getColumnNumber();
      }

    if (severity == 'w' && warnAsError())
      severity = 'e';
    messages.error(severity, file, line, column, message);
  }

  public void error(char severity, String message)
  {
    if (severity == 'w' && warnAsError())
      severity = 'e';
    
    messages.error(severity, this, message);
  }

  public void error(char severity, Declaration decl, String msg1, String msg2)
  {
    error(severity, msg1 + decl.getName() + msg2, null, decl);
  }

  public void error(char severity, String message,
                    String code, SourceLocator decl)
  {
    if (severity == 'w' && warnAsError())
      severity = 'e';
    
    String filename = getFileName();
    int line = getLineNumber();
    int column = getColumnNumber();
    int decl_line = decl.getLineNumber();
    if (decl_line > 0)
      {
	filename = decl.getFileName();
	line = decl_line;
	column = decl.getColumnNumber();
      }
    messages.error(severity, filename, line, column, message, code);
  }

  /**
   * Handle syntax errors (at rewrite time).
   * @param message an error message to print out
   * @return an ErrorExp
   */
  public ErrorExp syntaxError (String message)
  {
    error('e', message);
    return new ErrorExp (message);
  }

  public final int getLineNumber()  { return messages.getLineNumber(); }
  public final int getColumnNumber() { return messages.getColumnNumber(); }
  public final String getFileName() { return messages.getFileName(); }
  public String getPublicId() { return messages.getPublicId(); }
  public String getSystemId() { return messages.getSystemId(); }
  public boolean isStableSourceLocation() { return false; }

  public void setFile(String filename) { messages.setFile(filename); }
  public void setLine(int line) { messages.setLine(line); }
  public void setColumn(int column) { messages.setColumn(column); }
  public final void setLine(Expression position)
  { messages.setLocation(position); }
  public void setLine (Object location)
  {
    if (location instanceof SourceLocator)
      messages.setLocation((SourceLocator) location);
  }
  public final void setLocation (SourceLocator position)
  { messages.setLocation(position); }

  public void setLine(String filename, int line, int column)
  {
    messages.setLine(filename, line, column);
  }

  /** A help vector for building expressions. */
  public Stack<Expression> exprStack;

  public void letStart ()
  {
    pushScope(new LetExp());
  }

  public Declaration letVariable (Object name, Type type, Expression init)
  {
    Declaration decl = new Declaration(name, type);
    letVariable(decl, init);
    return decl;
  }

  public void letVariable (Declaration decl, Expression init)
  {
    LetExp let = (LetExp) current_scope;
    let.add(decl);
    decl.setInitValue(init);
  }

  public void letEnter ()
  {
    LetExp let = (LetExp) current_scope;
    // Set a flag which letDone uses to check if letEnter has been called.
    let.setFlag(Expression.VALIDATED);
    for (Declaration decl = let.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
        Expression init = decl.getInitValue();
        if (init != QuoteExp.undefined_exp)
          decl.noteValueFromLet(let);
      }
    lexical.push(let);
  }

  public LetExp letDone (Expression body)
  {
    LetExp let = (LetExp) current_scope;
    // Check if letEnter has been called.
    if (! let.getFlag(Expression.VALIDATED))
      letEnter();
    let.setFlag(false, Expression.VALIDATED);
    let.body = body;
    pop(let);
    return let;
  }

    private void checkLoop() {
        if (((LambdaExp) current_scope).getName() != "%do%loop")
            throw new Error("internal error - bad loop state");
    }

    /** Start a new loop.
     * This provides the functionality of Scheme 'named let'. 
     */
    public LambdaExp loopStart() {
        if (exprStack == null)
            exprStack = new Stack<Expression>();
        LambdaExp loopLambda = new LambdaExp();
        LetExp let = new LetExp();
        String fname = "%do%loop";
        Declaration fdecl = let.addDeclaration(fname);
        fdecl.setInitValue(loopLambda);
        fdecl.noteValueFromLet(let);
        loopLambda.setName(fname);
        let.setOuter(current_scope);
        loopLambda.setOuter(let);
        current_scope = loopLambda;
        return loopLambda;
    }

    /** Add a new loop variable, with initializer. */
    public Declaration loopVariable(Object name, Type type, Expression init) {
        checkLoop();
        LambdaExp loopLambda = (LambdaExp) current_scope;
        Declaration decl = loopLambda.addDeclaration(name, type);
        exprStack.push(init);
        loopLambda.min_args++;
        return decl;
    }

    /** Done handling loop variables, and pushes them into the lexical scope.
     * Ready to parse the loop condition.
     */ 
    public void loopEnter() {
        checkLoop();
        LambdaExp loopLambda = (LambdaExp) current_scope;
        int ninits = loopLambda.min_args;
        loopLambda.max_args = ninits;
        Expression[] inits = new Expression[ninits];
        for (int i = ninits;  --i >= 0; )
            inits[i] = (Expression) exprStack.pop();
        LetExp let = (LetExp) loopLambda.getOuter();
        Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
        let.setBody(new ApplyExp(new ReferenceExp(fdecl), inits));
        lexical.push(loopLambda);
    }

    @Deprecated
    public void loopCond(Expression cond) {
        checkLoop();
        exprStack.push(cond);
    }

    @Deprecated
    public void loopBody(Expression body) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        loopLambda.body = body;
    }

    /** Recurse to next iteration of specified loop. */
    public Expression loopRepeat(LambdaExp loop, Expression... exps) {
        ScopeExp let = loop.getOuter();
        Declaration fdecl = let.firstDecl();  // The decls for loopLambda.
        return new ApplyExp(new ReferenceExp(fdecl), exps);
    }

    /** Finish building a loop and return resulting expression. */
    public Expression loopDone(Expression body) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        ScopeExp let = loopLambda.getOuter();
        loopLambda.body = body;
        lexical.pop(loopLambda);
        current_scope = let.getOuter();
        return let;
    }

    /** Combine loopRepeat and loopDone.
     * Assume loopCond and loopBody have been called.
     */
    public Expression loopRepeatDone(Expression... exps) {
        LambdaExp loopLambda = (LambdaExp) current_scope;
        ScopeExp let = loopLambda.getOuter();
        Expression cond = (Expression) exprStack.pop();
        Expression recurse = loopRepeat(loopLambda, exps);
        loopLambda.body = new IfExp(cond,
                                    new BeginExp(loopLambda.body, recurse),
                                    QuoteExp.voidExp);
        lexical.pop(loopLambda);
        current_scope = let.getOuter();
        return let;
    }

  public QuoteExp makeQuoteExp (Object value)
  {
    return QuoteExp.getInstance(value, this);
  }

  /**
   * Convenience method to make an Expression that coerces a value.
   * @param value to be coerced
   * @param type to coerce value to
   * @return expression that coerces value to type
   */
  public static ApplyExp makeCoercion(Expression value, Expression type)
  {
    Expression[] exps = new Expression[2];
    exps[0] = type;
    exps[1] = value;
    QuoteExp c = new QuoteExp(Convert.cast);
    return new ApplyExp(c, exps);
  }

    /**
     * Convenience method to make an Expression that coerces a value.
     * @param value to be coerced
     * @param type to coerce value to
     * @return expression that coerces value to type
     */
    public static ApplyExp makeCoercion(Expression value, Type type) {
        return makeCoercion(value, new QuoteExp(type));
    }

  /** If non-null, a helper method generated by getForNameHelper. */
  Method forNameHelper;

  /** Generate code to load a named Class without initializing it.
   */
  public void loadClassRef (ObjectType clas)
  {
    CodeAttr code = getCode();
    // Try an optimization
    if (curClass.getClassfileVersion() >= ClassType.JDK_1_5_VERSION)
      code.emitPushClass(clas);
    else if (clas == mainClass && mainLambda.isStatic()
        // moduleInstanceMainField may not have been set yet.
        && moduleInstanceMainField != null)
      {
        code.emitGetStatic(moduleInstanceMainField);
        code.emitInvokeVirtual(Type.objectType.getDeclaredMethod("getClass", 0));
      }
    else
      {
        String name = clas instanceof ClassType ? clas.getName()
          : clas.getInternalName().replace('/', '.');
        code.emitPushString(name);
        code.emitInvokeStatic(getForNameHelper());
      }
  }

  /** Generate a method to find a named Class without initializing it.
   * Generate a static helper method "class$" like javac generates for
   * 'CLASS.class', but does not initialize CLASS.  Also, we don't bother
   * catching exceptions, since the JVM doesn't require us to.  I.e. generates:
   * public static class $(String name)
   * { return Class.forName(name, false,
   *                        Class.forName(THISCLASSNAME).getClassLoader()); }
   * Note that we want the result to use the same ClassLoader as the caller,
   * which is why we generate a static helper method.
   */
  public Method getForNameHelper ()
  {
    if (forNameHelper == null)
      {
	/* #ifdef JAVA2 */
	Method save_method = method;
	method = curClass.addMethod("class$", Access.PUBLIC|Access.STATIC,
				    string1Arg, typeClass);
	forNameHelper = method;
	CodeAttr code = method.startCode();
	code.emitLoad(code.getArg(0));
	code.emitPushInt(0);
	code.emitPushString(mainClass.getName());
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 1));
	code.emitInvokeVirtual(typeClass.getDeclaredMethod("getClassLoader", 0));
	code.emitInvokeStatic(typeClass.getDeclaredMethod("forName", 3));
	code.emitReturn();
	method = save_method;
	/* #else */
	// forNameHelper = typeClass.getDeclaredMethod("forName", 1);
	/* #endif */
      }
    return forNameHelper;
  }

    public Environment getGlobalEnvironment() { return Environment.getCurrent(); }

  public Object resolve(Object name, boolean function)
  {
    Environment env = getGlobalEnvironment();
    Symbol symbol;
    if (name instanceof String)
      symbol = env.defaultNamespace().lookup((String) name);
    else
      symbol = (Symbol) name;
    if (symbol == null)
      return null;
    if (function && getLanguage().hasSeparateFunctionNamespace())
      return env.getFunction(symbol, null);
    return env.get(symbol, null);
  }

  /** A key we can pass from the compiler to identity a Compilation. */
  private int keyUninitialized;
  /** Chain of immediate Compilation whose setupLiterals hasn't been called. */
  private static Compilation chainUninitialized;
  /** Next in chain headed by chainUninitialized. */
  private Compilation nextUninitialized;

  /** Call-back from compiled code to initialize literals in immediate mode.
   * In non-immediate mode (i.e. generating class files) the compiler emits
   * code to "re-construct" literal values.  However, in immediate mode
   * that would be wasteful, plus we would get values that are similar (equals)
   * to but not necessarily identical (eq) to the compile-time literal.
   * So we need to pass the literal values to the compiled code, by using
   * reflection to initialize various static fields.  This method does that.
   * It is called at the start of the generated static initializer, which
   * helps makes things more consistent between immediate and non-immediate
   * mode.
   */
  public static void setupLiterals (int key)
  {
    Compilation comp = Compilation.findForImmediateLiterals(key);
    try
      {
        Class clas = comp.loader.loadClass(comp.mainClass.getName());

	/* Pass literal values to the compiled code. */
	for (Literal init = comp.litTable.literalsChain;  init != null;
	     init = init.next)
	  {
	    /* DEBUGGING:
	    OutPort out = OutPort.errDefault();
	    out.print("init["+init.index+"]=");
	    out.print(init.value);
	    out.println();
	    */
            clas.getDeclaredField(init.field.getName())
              .set(null, init.value);
	  }
        comp.litTable = null;
      }
    catch (Throwable ex)
      {
        WrappedException.rethrow(ex);
      }
  }

  public static synchronized int
  registerForImmediateLiterals (Compilation comp)
  {
    int i = 0;
    for (Compilation c = chainUninitialized;  c != null;  c = c.nextUninitialized)
      {
        if (i <= c.keyUninitialized)
          i = c.keyUninitialized + 1;
      }
    comp.keyUninitialized = i;
    comp.nextUninitialized = chainUninitialized;
    chainUninitialized = comp;
    return i;
  }

  public static synchronized Compilation findForImmediateLiterals (int key)
  {
    Compilation prev = null;
    for (Compilation comp = chainUninitialized; ; )
      {
        Compilation next = comp.nextUninitialized;
        if (comp.keyUninitialized == key)
          {
            if (prev == null)
              chainUninitialized = next;
            else
              prev.nextUninitialized = next;
            comp.nextUninitialized = null;
            return comp;
          }
        prev = comp;
        comp = next;
      }
  }

  /** Current lexical scope - map name to Declaration. */
  public NameLookup lexical;

  protected ScopeExp current_scope;

  protected SourceMessages messages;

  private static final ThreadLocal<Compilation> current =
    new InheritableThreadLocal<Compilation>();

  public static Compilation getCurrent ()
  {
    return current.get();
  }

  public static void setCurrent (Compilation comp)
  {
    current.set(comp);
  }

  public static Compilation setSaveCurrent (Compilation comp)
  {
    Compilation save = current.get();
    current.set(comp);
    return save;
  }

  public static void restoreCurrent (Compilation saved)
  {
    current.set(saved);
  }

  public String toString ()
  {
    return "<compilation "+mainLambda+">";
  }

    public ModuleInfo getMinfo() {
        return mainLambda.info;
    }
}
