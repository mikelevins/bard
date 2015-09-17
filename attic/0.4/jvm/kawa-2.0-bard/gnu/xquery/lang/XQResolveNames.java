// Copyright (c) 2003, 2004, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.xquery.lang;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.xml.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.StaticFieldLocation;
import gnu.kawa.reflect.SingletonType;
import gnu.kawa.functions.CompileNamedPart;
import gnu.xquery.util.NamedCollator;
import gnu.xquery.util.QNameUtils;
import java.util.*;

public class XQResolveNames extends ResolveNames
{
  public XQParser parser;

  /** Code number for the special <code>last</code> function. */
  public static final int LAST_BUILTIN = -1;

  /** Code number for the special <code>position</code> function. */
  public static final int POSITION_BUILTIN = -2;

  /** Code number for internal function to handle extensions. */
  public static final int HANDLE_EXTENSION_BUILTIN = -3;

  /** Code number for the special <code>compare</code> function. */
  public static final int COMPARE_BUILTIN = -4;

  /** Code number for the special <code>distinct-values</code> function. */
  public static final int DISTINCT_VALUES_BUILTIN = -5;

  /** Code number for the special <code>local-name</code> function. */
  public static final int LOCAL_NAME_BUILTIN = -6;

  /** Code number for the special <code>namespace-uri</code> function. */
  public static final int NAMESPACE_URI_BUILTIN = -7;

  /** Code number for the special <code>collection</code> function. */
  public static final int COLLECTION_BUILTIN = -8;

  /** Code number for the special <code>doc</code> function. */
  public static final int DOC_BUILTIN = -9;

  /** Code number for the special <code>doc-available</code> function. */
  public static final int DOC_AVAILABLE_BUILTIN = -10;

  /** Code number for the special <code>doc-available</code> function. */
  public static final int BASE_URI_BUILTIN = -11;

  /** Code number for the special <code>ressolve-uri</code> function. */
  public static final int RESOLVE_URI_BUILTIN = -12;

  /** Code number for internal function that maps prefix to uri. */
  public static final int RESOLVE_PREFIX_BUILTIN = -13;

  /** Code number for the special <code>static-base-uri</code> function. */
  public static final int STATIC_BASE_URI_BUILTIN = -14;

  /** Code number for the special <code>index-of</code> function. */
  public static final int INDEX_OF_BUILTIN = -15;

  /** Code number for the special <code>string</code> function. */
  public static final int STRING_BUILTIN = -16;

  /** Code number for the special <code>normalize-space</code> function. */
  public static final int NORMALIZE_SPACE_BUILTIN = -17;

  /** Code number for the special <code>unordered</code> function. */
  public static final int UNORDERED_BUILTIN = -18;

  /** Code number for the special <code>lang</code> function. */
  public static final int LANG_BUILTIN = -23;

  /** Code number for the special <code>name</code> function. */
  public static final int NAME_BUILTIN = -24;

  /** Code number for the special <code>deep-equal</code> function. */
  public static final int DEEP_EQUAL_BUILTIN = -25;

  /** Code number for the special <code>min</code> function. */
  public static final int MIN_BUILTIN = -26;

  /** Code number for the special <code>max</code> function. */
  public static final int MAX_BUILTIN = -27;

  /** Code number for the special <code>number</code> function. */
  public static final int NUMBER_BUILTIN = -28;

  /** Code number for the special <code>default-collation</code> function. */
  public static final int DEFAULT_COLLATION_BUILTIN = -29;

  /** Code number for the special <code>id</code> function. */
  public static final int ID_BUILTIN = -30;

  /** Code number for the special <code>idref</code> function. */
  public static final int IDREF_BUILTIN = -31;

  /** Code number for the special <code>root</code> function. */
  public static final int ROOT_BUILTIN = -32;

  public static final int CAST_AS_BUILTIN = -33;
  public static final int CASTABLE_AS_BUILTIN = -34;

  /** Value of {@code xs:QName()} constructor. */
  public static final int XS_QNAME_BUILTIN = -35;

  /** Like {@code XS_QNAME_BUILTIN}, but ignore the default
   * element namespace.  The is appropriate fro resolving atributes. */
  public static final int XS_QNAME_IGNORE_DEFAULT_BUILTIN = -36;

  public static final Declaration handleExtensionDecl
    = makeBuiltin("(extension)", HANDLE_EXTENSION_BUILTIN);

  public static final Declaration castAsDecl
    = makeBuiltin("(cast as)", CAST_AS_BUILTIN);

  public static final Declaration castableAsDecl
    = makeBuiltin("(castable as)", CASTABLE_AS_BUILTIN);

  /** Declaration for the <code>fn:last()</code> function. */
  public static final Declaration lastDecl
    = makeBuiltin("last", LAST_BUILTIN);

  public static final Declaration xsQNameDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "QName"), XS_QNAME_BUILTIN);

  public static final Declaration xsQNameIgnoreDefaultDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "(QName-ignore-default)"), XS_QNAME_IGNORE_DEFAULT_BUILTIN);

  public static final Declaration staticBaseUriDecl
    = makeBuiltin("static-base-uri", STATIC_BASE_URI_BUILTIN);

  public static final Declaration resolvePrefixDecl
    = makeBuiltin(Symbol.make(XQuery.SCHEMA_NAMESPACE, "(resolve-prefix)"),
                  RESOLVE_PREFIX_BUILTIN);

  /** Create a <code>Declaration</code> for a builtin function. */
  public static Declaration makeBuiltin (String name, int code)
  {
    return makeBuiltin (Symbol.make(XQuery.XQUERY_FUNCTION_NAMESPACE, name, "fn"),
			code);
  }

  /** Create a <code>Declaration</code> for a builtin function. */
  public static Declaration makeBuiltin (Symbol name, int code)
  {
    Declaration decl = new Declaration(name);
    decl.setProcedureDecl(true);
    decl.setCode(code);
    return decl;
  }

  public XQResolveNames ()
  {
    this(null);
  }

  void pushBuiltin (String name, int code)
  {
    lookup.push(makeBuiltin(name, code));
  }

  public XQResolveNames (Compilation comp)
  {
    super(comp);
    lookup.push(lastDecl);
    lookup.push(xsQNameDecl);
    lookup.push(staticBaseUriDecl);
    pushBuiltin("position", POSITION_BUILTIN);
    pushBuiltin("compare", COMPARE_BUILTIN);
    pushBuiltin("distinct-values", DISTINCT_VALUES_BUILTIN);
    pushBuiltin("local-name", LOCAL_NAME_BUILTIN);
    pushBuiltin("name", NAME_BUILTIN);
    pushBuiltin("namespace-uri", NAMESPACE_URI_BUILTIN);
    pushBuiltin("root", ROOT_BUILTIN);
    pushBuiltin("base-uri", BASE_URI_BUILTIN);
    pushBuiltin("lang", LANG_BUILTIN);
    pushBuiltin("resolve-uri", RESOLVE_URI_BUILTIN);
    pushBuiltin("collection", COLLECTION_BUILTIN);
    pushBuiltin("doc", DOC_BUILTIN);
    pushBuiltin("document", DOC_BUILTIN); // Obsolete
    pushBuiltin("doc-available", DOC_AVAILABLE_BUILTIN);
    pushBuiltin("index-of", INDEX_OF_BUILTIN);
    pushBuiltin("string", STRING_BUILTIN);
    pushBuiltin("normalize-space", NORMALIZE_SPACE_BUILTIN);
    pushBuiltin("unordered", UNORDERED_BUILTIN);
    pushBuiltin("deep-equal", DEEP_EQUAL_BUILTIN);
    pushBuiltin("min", MIN_BUILTIN);
    pushBuiltin("max", MAX_BUILTIN);
    pushBuiltin("number", NUMBER_BUILTIN);
    pushBuiltin("default-collation", DEFAULT_COLLATION_BUILTIN);
    pushBuiltin("id", ID_BUILTIN);
    pushBuiltin("idref", IDREF_BUILTIN);
  }

  public Namespace[] functionNamespacePath
    = XQuery.defaultFunctionNamespacePath;

  protected void push (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
        push(decl);
      }
  }

  void push (Declaration decl)
  {
    Compilation comp = getCompilation();
    Object name = decl.getSymbol();
    boolean function = decl.isProcedureDecl();
    if (name instanceof String)
      {
        int line = decl.getLineNumber();
        if (line > 0 && comp != null)
          {
            String saveFilename = comp.getFileName();
            int saveLine = comp.getLineNumber();
            int saveColumn = comp.getColumnNumber();
            comp.setLocation(decl);
            name = parser.namespaceResolve((String) name, function);
            comp.setLine(saveFilename, saveLine, saveColumn);
          }
        else
          name = parser.namespaceResolve((String) name, function);
        if (name == null)
          return;
        decl.setName(name);
      }

    Declaration old = lookup.lookup(name, XQuery.instance.getNamespaceOf(decl));
    if (old != null)
      {
        if (decl.context == old.context)
          ScopeExp.duplicateDeclarationError(old, decl, comp);
        else if (XQParser.warnHidePreviousDeclaration
                 && (! (name instanceof Symbol)
                     || ((Symbol) name).getNamespace() != null))
          comp.error('w', decl, "declaration ",
                     " hides previous declaration");
      }
    lookup.push(decl);
  }

  Declaration flookup (Symbol sym)
  {
    Environment env = XQuery.xqEnvironment;
    gnu.mapping.Location loc = env.lookup(sym, EnvironmentKey.FUNCTION);
    if (loc == null)
      return null;
    loc = loc.getBase();
    if (loc instanceof StaticFieldLocation)
      {
	Declaration decl = ((StaticFieldLocation) loc).getDeclaration();
	if (decl != null)
	  return decl;
      }
    Object val = loc.get(null);
    if (val != null)
      return procToDecl(sym, val);
    return null;
  }

  protected Expression visitReferenceExp (ReferenceExp exp, Void ignored)
  {
    return visitReferenceExp(exp, (ApplyExp) null);
  }

  protected Expression visitReferenceExp (ReferenceExp exp, ApplyExp call)
  {
    if (exp.getBinding() == null)
      {
	Object symbol = exp.getSymbol();
        boolean needFunction = exp.isProcedureName();
        boolean needType = exp.getFlag(ReferenceExp.TYPE_NAME);
        int namespace = call == null ? XQuery.VALUE_NAMESPACE
          : XQuery.namespaceForFunctions(call.getArgCount());
	Declaration decl = lookup.lookup(symbol, namespace);
        Symbol sym;
        if (decl != null)
          ;
	else if (symbol instanceof Symbol
            && "".equals((sym = (Symbol) symbol).getNamespaceURI()))
          {
            // kludge - use xxx_BUILTIN mechanism?  FIXME
            String name = sym.getLocalName();
            String mname;
            if ("request".equals(name))
              mname = "getCurrentRequest";
            else if ("response".equals(name))
              mname = "getCurrentResponse";
            else
              mname = null;
            if (mname != null)
              {
                Method meth =
                  ClassType.make("gnu.kawa.servlet.KawaServlet")
                  .getDeclaredMethod(mname, 0);
                return new ApplyExp(meth, Expression.noExpressions);
              }
          }
        else if (symbol instanceof Symbol)
          {
            // Never happens, I believe.
            decl = flookup((Symbol) symbol);
          }
        else // if (symbol instanceof String)
          {
            String name = (String) symbol;
            if (name.indexOf(':') < 0)
              {
                name = name.intern();
                if (needFunction)
                  {
                    for (int i = 0;  i < functionNamespacePath.length;  i++)
                      {
                        sym = functionNamespacePath[i].getSymbol(name);
                        decl = lookup.lookup(sym, namespace);
                        if (decl != null)
                          break;
                        decl = flookup(sym);
                        if (decl != null)
                          break;
                      }
                  }
              }
            if (decl == null)
              {
                sym = parser.namespaceResolve(name, needFunction);
                if (sym != null)
                  {
                    decl = lookup.lookup(sym, namespace);
                    if (decl == null
                        && (needFunction || needType))
                      {
                        String uri = sym.getNamespaceURI();
                        Type type = null;
                        if (XQuery.SCHEMA_NAMESPACE.equals(uri))
                          {
                            type = parser.interpreter.getStandardType(sym.getName());
                          }
                        else if (needType && uri == ""
                            && ! getCompilation().isPedantic())
                          {
                            type = parser.interpreter.getTypeFor(name);
                          }
                        if (type != null)
                          return new QuoteExp(type).setLine(exp);
                        if (uri != null && uri.length() > 6 &&
                            uri.startsWith("class:"))
                          {
                            ClassType ctype = ClassType.make(uri.substring(6));
                            return CompileNamedPart.makeExp(ctype, sym.getName());
                          }
                        decl = flookup(sym);
                      }
                  }
              }
          }
        if (decl != null)
          exp.setBinding(decl);
        else if (needFunction)
          error('e', "unknown function "+symbol);
        else if (needType)
          messages.error('e', exp, "unknown type "+symbol, "XPST0051");
        else
          messages.error('e', exp, "unknown variable $"+symbol, "XPST0008");
      }
    return exp;
  }

  protected Expression visitSetExp (SetExp exp, Void ignored)
  {
    Expression result = super.visitSetExp(exp, ignored);
    Declaration decl = exp.getBinding();
    Object name;
    Expression new_value;
    if (decl != null && ! getCompilation().immediate
	&& (name = decl.getSymbol()) instanceof Symbol
	&& XQuery.LOCAL_NAMESPACE.equals(((Symbol) name).getNamespaceURI())
        && (! ((new_value = exp.getNewValue()) instanceof ApplyExp)
            || ((ApplyExp) new_value).getFunction() != XQParser.getExternalFunction))
      {
	decl.setFlag(Declaration.PRIVATE_SPECIFIED);
	decl.setPrivate(true);
      }
    return result;
  }

  private Declaration moduleDecl;
  private Expression visitStatements (Expression exp)
  {
    // The tricky part here is interleaving declarations and statements
    // so that a variable declaration is only visible *after* we have
    // visited its initializing expression.
    if (exp instanceof BeginExp)
      {
        BeginExp bbody = (BeginExp) exp;
        Expression[] exps = bbody.getExpressions();
        int nexps = bbody.getExpressionCount();
        for (int i = 0;  i < nexps;  i++)
          {
            exps[i] = visitStatements(exps[i]);
          }
      }
    else if (exp instanceof SetExp)
      {
        Declaration decl = moduleDecl;
        SetExp sexp = (SetExp) exp;
        exp = visitSetExp(sexp, null);
        if (sexp.isDefining() && sexp.getBinding() == decl)
          {
            if (! decl.isProcedureDecl())
              push(decl);
            decl = decl.nextDecl();
          }
        moduleDecl = decl;
      }
    else
      exp = visit(exp, null);
    return exp;
  }

    static class CycleDetector extends ExpExpVisitor<Void> { 
        Map<Declaration,Integer> depsScanState = new HashMap<Declaration,Integer>();
        static final Integer SCANNING = 0;
        static final Integer SCANNED_CYCLE = 1;
        static final Integer SCANNED_NO_CYCLE = -1;

        //public boolean cycleSeen;

        protected Expression visitReferenceExp(ReferenceExp exp, Void ignored) {
            Declaration decl = exp.getBinding();
            if (decl != null && decl.context instanceof ModuleExp)
                scanDependencies(decl);
            return exp;
        }

        /** Return true if cycle detected. */
        public void scanDependencies(Declaration decl) {
            Integer state = depsScanState.get(decl);
            if (state != null) {
                if (state == SCANNING)
                    depsScanState.put(decl, SCANNED_CYCLE);
                return;
            }
            depsScanState.put(decl, SCANNING);
            Expression dval = decl.getValue();
            if (dval != null)
                visit(dval, null);
            state = depsScanState.get(decl);
            if (state == SCANNING)
                depsScanState.put(decl, SCANNED_NO_CYCLE);
        }

        public boolean scanVariable(Declaration decl) {
            scanDependencies(decl);
            Integer state = depsScanState.get(decl);
            return state == SCANNED_CYCLE;
         }
    }

  public void resolveModule(ModuleExp exp)
  {
    currentLambda = exp;
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
        if (decl.isProcedureDecl())
	  push(decl);
      }
    moduleDecl = exp.firstDecl();
    exp.body = visitStatements(exp.body);

    CycleDetector cycleDetector = new CycleDetector();

    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      {
        if (! decl.isProcedureDecl() && cycleDetector.scanVariable(decl))
            getCompilation().error('e',
                "cycle detected initializing $"+decl.getName(),
                "XQST0054", decl);

        // Remove old hidden declarations, for GC and speed.
        if (decl.getSymbol() != null)
          lookup.removeSubsumed(decl);
      }
  }

  /**
   * Coerce argument to NamedCallator, or return default collator.
   * @param args argument list
   * @param argno index in args of collator argument
   */
  Expression getCollator (Expression[] args, int argno)
  {
    if (args != null && args.length > argno)
      return new ApplyExp(ClassType.make("gnu.xquery.util.NamedCollator")
                            .getDeclaredMethod("find", 1),
                            new Expression[] { args[argno] });
    NamedCollator coll = parser.defaultCollator;
    return coll == null ? QuoteExp.nullExp : new QuoteExp(coll);
  }

  Expression withCollator (Method method, Expression[] args,
                           String name, int minArgs)
  {
    return withCollator(new QuoteExp(new PrimProcedure(method)),
                        args, name, minArgs);
  }

  /** Adjust call to add default collator if collator argument is missing. */
  Expression withCollator (Expression function, Expression[] args,
                           String name, int minArgs)
  {
    String err = WrongArguments.checkArgCount(name, minArgs, minArgs+1, args.length);
    if (err != null)
      return getCompilation().syntaxError(err);
    Expression[] xargs = new Expression[minArgs+1];
    System.arraycopy(args, 0, xargs, 0, minArgs);
    xargs[minArgs] = getCollator(args, minArgs);
    return new ApplyExp(function, xargs);
  }

  /** Adjust call to add default contex itemt if that argument is missing. */
  Expression withContext (Method method, Expression[] args,
                          String name, int minArgs)
  {
    String err = WrongArguments.checkArgCount(name, minArgs, minArgs+1,
                                              args.length);
    if (err != null)
      return getCompilation().syntaxError(err);
    if (args.length == minArgs)
      {
        Expression[] xargs = new Expression[minArgs+1];
        System.arraycopy(args, 0, xargs, 0, minArgs);
        Declaration dot = lookup.lookup(XQParser.DOT_VARNAME, false);
        if (dot == null)
          {
            String message = "undefined context for " + name;
            messages.error('e', message, "XPDY0002");
            return new ErrorExp(message);
          }
        xargs[minArgs] = new ReferenceExp(dot);
        args = xargs;
      }
    return new ApplyExp(method, args);
  }

  private Expression checkArgCount (Expression[] args, Declaration decl,
                                    int min, int max)
  {
    String err = WrongArguments.checkArgCount("fn:"+decl.getName(),
                                              min, max, args.length);
    if (err == null)
      return null;
    else
      return getCompilation().syntaxError(err);
  }

  protected Expression visitApplyExp (ApplyExp exp, Void ignored)
  {
    Expression func = exp.getFunction();
    NamespaceBinding namespaceSave = parser.constructorNamespaces;
    Object proc = exp.getFunctionValue();
    if (proc instanceof MakeElement)
      {
        MakeElement mk = (MakeElement) proc;
        NamespaceBinding nschain
          = NamespaceBinding.nconc(mk.getNamespaceNodes(), namespaceSave);
        mk.setNamespaceNodes(nschain);
        parser.constructorNamespaces = nschain;
      }
    if (func instanceof ReferenceExp)
      func = visitReferenceExp((ReferenceExp) func, exp);
    else
      func = visit(func, ignored);
    exp.setFunction(func);
    visitExps(exp.getArgs(), ignored);
    parser.constructorNamespaces = namespaceSave;
    func = exp.getFunction();
    if (func instanceof ReferenceExp)
      {
	Declaration decl = ((ReferenceExp) func).getBinding();
	int code;
        Expression err;
	if (decl != null && (code = decl.getCode()) < 0)
	  {
	    switch (code)
	      {
	      case POSITION_BUILTIN:
	      case LAST_BUILTIN:
		Symbol sym = code == LAST_BUILTIN ? XQParser.LAST_VARNAME
		  : XQParser.POSITION_VARNAME;
		decl = lookup.lookup(sym, false);
		if (decl == null)
		  error('e', "undefined context for " + sym.getName());
                else
                  // So CompileMisc:validateApplyValuesFilter
                  // can tell whether last() is used.
                  decl.setCanRead(true);
		return new ReferenceExp(sym, decl);
              case CAST_AS_BUILTIN:
              case CASTABLE_AS_BUILTIN:
                {
		  Expression[] args = exp.getArgs();
                  Expression texp = args[code == CAST_AS_BUILTIN ? 0 : 1];
                  Expression qexp = texp;
                  if (texp instanceof ApplyExp)
                    {
                      ApplyExp taexp = (ApplyExp) texp;
                      if (taexp.getFunction().valueIfConstant()
                          == XQParser.proc_OccurrenceType_getInstance)
                        qexp = taexp.getArg(0);
                    }
                  Object value = qexp.valueIfConstant();
                  String msg = null;
                  if (value == SingletonType.getInstance())
                    msg = "type to 'cast as' or 'castable as' must be atomic";
                  else if (value == XDataType.anyAtomicType)
                    msg = "type to 'cast as' or 'castable as' cannot be anyAtomicType";
                  else if (value == XDataType.anySimpleType)
                    msg = "type to 'cast as' or 'castable as' cannot be anySimpleType";
                  else if (value == XDataType.untypedType)
                    msg = "type to 'cast as' or 'castable as' cannot be untyped";
                  else if (value == XDataType.NotationType)
                    msg = "type to 'cast as' or 'castable as' cannot be NOTATION";
                  if (msg != null)
                    messages.error('e', texp, msg, "XPST0080");
                  boolean toQName = (value == Compilation.typeSymbol
                                     && ! (texp instanceof ApplyExp));
                  if (code == CAST_AS_BUILTIN)
                    {
                      if (toQName)
                        return visitApplyExp(XQParser.castQName(args[1], true), ignored);
                      func
                        = XQParser.makeFunctionExp("gnu.xquery.util.CastAs", "castAs");
                    }
                  else
                    {
                      if (toQName && args[0] instanceof QuoteExp)
                        {
                          value = ((QuoteExp) args[0]).getValue();  
                          try
                            {
                              QNameUtils.resolveQName(value,
                                                      parser.constructorNamespaces,
                                                      parser.prologNamespaces);
                              return XQuery.trueExp;
                            }
                          catch (RuntimeException ex)
                            {	
                              return XQuery.falseExp;
                            }
                        }
                      func = XQParser.makeFunctionExp("gnu.xquery.lang.XQParser",                                       "castableAs");
                    }
                  return new ApplyExp(func, args).setLine(exp);
                }
	      case XS_QNAME_BUILTIN:
	      case XS_QNAME_IGNORE_DEFAULT_BUILTIN:
		{
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
                  NamespaceBinding constructorNamespaces
                    = parser.constructorNamespaces;
                  if (code == XS_QNAME_IGNORE_DEFAULT_BUILTIN)
                    constructorNamespaces
                      = new NamespaceBinding(null, "", constructorNamespaces);
		  if (args[0] instanceof QuoteExp)
		    {
		      try
			{
			  Object val = ((QuoteExp) args[0]).getValue();
			  val = QNameUtils.resolveQName(val,
                                                        constructorNamespaces,
                                                        parser.prologNamespaces);
			  return new QuoteExp(val);
			}
		      catch (RuntimeException ex)
			{
			  return getCompilation().syntaxError(ex.getMessage());
			}
		    }
		  Expression[] xargs = {
		    args[0],
		    new QuoteExp(constructorNamespaces),
		    new QuoteExp(parser.prologNamespaces) };
		  Method meth
		    = (ClassType.make("gnu.xquery.util.QNameUtils")
		       .getDeclaredMethod("resolveQName", 3));
		  ApplyExp app = new ApplyExp(meth, xargs);
		  app.setFlag(ApplyExp.INLINE_IF_CONSTANT);
		  return app;
		}
	      case RESOLVE_PREFIX_BUILTIN:
		{
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
		  if (args[0] instanceof QuoteExp)
		    {
                      Object val = ((QuoteExp) args[0]).getValue();
                      String prefix = val == null ? null : val.toString();
                      val = QNameUtils.lookupPrefix(prefix,
                                                    parser.constructorNamespaces,
                                                    parser.prologNamespaces);
                      if (val == null)
                        return getCompilation()
                          .syntaxError("unknown namespace prefix '"
                                       +prefix+"'");
                      return new QuoteExp(val);
		    }
		  Expression[] xargs = {
		    args[0],
		    new QuoteExp(parser.constructorNamespaces),
		    new QuoteExp(parser.prologNamespaces) };
		  PrimProcedure pproc
		    = new PrimProcedure(ClassType.make("gnu.xquery.util.QNameUtils")
                                        .getDeclaredMethod("resolvePrefix", 3));
		  ApplyExp app = new ApplyExp(pproc, xargs);
		  app.setFlag(ApplyExp.INLINE_IF_CONSTANT);
		  return app;
		}
              case LOCAL_NAME_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("localName", 1);
                  return withContext(meth, exp.getArgs(), "fn:local-name", 0);
                }
              case NAME_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("name", 1);
                  return withContext(meth, exp.getArgs(), "fn:name", 0);
                }
              case NUMBER_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NumberValue")
                    .getDeclaredMethod("numberValue", 1);
                  return withContext(meth, exp.getArgs(), "fn:number", 0);
                }
              case ROOT_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("root", 1);
                  return withContext(meth, exp.getArgs(), "fn:root", 0);
                }
              case BASE_URI_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("baseUri", 1);
                  return withContext(meth, exp.getArgs(), "fn:base-uri", 0);
                }
              case LANG_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("lang", 2);
                  return withContext(meth, exp.getArgs(), "fn:lang", 1);
                }
              case ID_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("id$X", 3);
                  return withContext(meth, exp.getArgs(), "fn:id", 1);
                }
              case IDREF_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("idref", 2);
                  return withContext(meth, exp.getArgs(), "fn:idref", 1);
                }

              case STATIC_BASE_URI_BUILTIN:
                {
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 0, 0)) != null)
                    return err;
                  return getBaseUriExpr();
                }
              case NAMESPACE_URI_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.NodeUtils")
                    .getDeclaredMethod("namespaceURI", 1);
                  return withContext(meth, exp.getArgs(),
                                     "fn:namespace-uri", 0);
                }

              case NORMALIZE_SPACE_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.StringUtils")
                    .getDeclaredMethod("normalizeSpace", 1);
                  return withContext(meth, exp.getArgs(),
                                     "fn:normalize-space", 0);
                }

              case UNORDERED_BUILTIN:
                {
		  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
                  return args[0];
                }

              case COMPARE_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.StringUtils")
                    .getDeclaredMethod("compare", 3);
                  return withCollator(meth, exp.getArgs(), "fn:compare", 2);
                }

              case STRING_BUILTIN:
                return withContext(ClassType.make("gnu.xml.TextUtils")
                                   .getDeclaredMethod("asString", 1),
                                   exp.getArgs(), "fn:string", 0);

              case INDEX_OF_BUILTIN:
		{
                  Method meth = ClassType.make("gnu.xquery.util.SequenceUtils")
                    .getDeclaredMethod("indexOf$X", 4);
                  return withCollator(meth, exp.getArgs(), "fn:index-of", 2);
                }
              case COLLECTION_BUILTIN:
                {
                  Expression[] args = exp.getArgs();
                  ClassType cl = ClassType.make("gnu.xquery.util.NodeUtils");
                  Method meth = cl.getDeclaredMethod("collection", 2);
                  if ((err = checkArgCount(args, decl, 0, 1)) != null)
                    return err;
                  Expression base = getBaseUriExpr();
                  Expression uri = args.length > 0 ? args[0]
                    : QuoteExp.voidExp;
                  return new ApplyExp(meth, new Expression[]{ uri, base });
                }
              case DOC_BUILTIN:
              case DOC_AVAILABLE_BUILTIN:
                {
                  Expression[] args = exp.getArgs();
                  ClassType cl = ClassType.make("gnu.xquery.util.NodeUtils");
                  String mname;
                  if (code == DOC_BUILTIN)
                    {
                      mname = "docCached";
                      if (XQParser.warnOldVersion
                          && "document".equals(decl.getName()))
                        getCompilation()
                          .error('w', "replace 'document' by 'doc'");
                    }
                  else
                    mname = "availableCached";
                  Method meth = cl.getDeclaredMethod(mname, 2);
                  if ((err = checkArgCount(args, decl, 1, 1)) != null)
                    return err;
                  PrimProcedure pproc = new PrimProcedure(meth);
                  if (code == DOC_BUILTIN)
                      pproc.setSideEffectFree();
                  Expression base = getBaseUriExpr();
                  ApplyExp aexp
                    = new ApplyExp(pproc, new Expression[]{args[0], base});
                  if (code == DOC_BUILTIN)
                    aexp.setType(NodeType.documentNodeTest);
                  else
                    aexp.setType(XDataType.booleanType);
                  return aexp;
                }
              case RESOLVE_URI_BUILTIN:
                {
                  Expression[] args = exp.getArgs();
                  if ((err = checkArgCount(args, decl, 1, 2)) != null)
                    return err;
                  Expression[] margs = new Expression[2];
                  margs[0] = args[0];
                  if (args.length == 1)
                    margs[1] = getBaseUriExpr();
                  else
                    margs[1] = args[1];
                  Method meth = ClassType.make("gnu.xquery.util.QNameUtils")
                    .getDeclaredMethod("resolveURI", 2);
                  return new ApplyExp(meth, margs);
                }
              case DISTINCT_VALUES_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.DistinctValues")
                    .getDeclaredMethod("distinctValues$X", 3);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:distinct-values", 1);

                }
              case DEEP_EQUAL_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.SequenceUtils")
                    .getDeclaredMethod("deepEqual", 3);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:deep-equal", 2);
                }
              case MIN_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.MinMax")
                    .getDeclaredMethod("min", 2);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:min", 1);
                }
              case MAX_BUILTIN:
                {
                  Method meth = ClassType.make("gnu.xquery.util.MinMax")
                    .getDeclaredMethod("max", 2);
                  return withCollator(meth, exp.getArgs(),
                                      "fn:max", 1);
                }
              case DEFAULT_COLLATION_BUILTIN:
                if ((err = checkArgCount(exp.getArgs(), decl, 0, 0)) != null)
                  return err;
                NamedCollator coll = parser.defaultCollator;
                return QuoteExp.getInstance(coll != null ? coll.getName()
                          : NamedCollator.UNICODE_CODEPOINT_COLLATION);
              case HANDLE_EXTENSION_BUILTIN:
                {
                  Compilation comp = getCompilation();
                  Expression[] args = exp.getArgs();
                  int i = 0;
                  for (;  i < args.length - 1;  i += 2)
                    {
                      Expression pname = args[i];
                      String qname = (String) ((QuoteExp) pname).getValue();
                      Symbol psymbol = parser.namespaceResolve(qname, false);
                      if (psymbol == null)
                        ; // error emitted in namespaceResolve
                      else if (psymbol.getNamespaceURI().length() == 0)
                        comp.error('e', "pragma name cannot be in the empty namespace");
                      else
                        {
                          Expression replacement
                            = checkPragma(psymbol, args[i+1]);
                          if (replacement != null)
                            return replacement;
                        }
                    }
                  if (i < args.length)
                    return args[args.length-1];
                  String msg = "no recognized pragma or default in extension expression";
                  getMessages().error('e', msg, "XQST0079");
                  return new ErrorExp(msg);
                }
	      }
	  }
      }
    proc = exp.getFunctionValue();
    if (proc instanceof Type)
      {
        Expression[] args = exp.getArgs();
        if (args.length != 1)
          {
            messages.error('e', "type constructor requires a single argument");
            return exp;
          }
        return new ApplyExp(XQParser.makeFunctionExp("gnu.xquery.util.CastAs", "castAs"),
                            new Expression[] { exp.getFunction(), args[0] });
      }
    if (proc instanceof MakeElement)
      {
	MakeElement make = (MakeElement) proc;

	// Add namespaces nodes that might be needed.
	NamespaceBinding nsBindings = make.getNamespaceNodes();
        Symbol tag = make.tag;
        if (tag == null)
          tag = MakeElement.getTagName(exp);
	nsBindings = maybeAddNamespace(tag, false, nsBindings);
	Expression[] args = exp.getArgs();
        Symbol[] attrSyms = new Symbol[args.length];
        int nattrSyms = 0;
	for (int i = 0;  i < args.length;  i++)
	  {
	    Expression arg = args[i];
	    if (arg instanceof ApplyExp)
	      {
		ApplyExp app = (ApplyExp) arg;
		if (app.getFunction() == MakeAttribute.makeAttributeExp)
                  {
                    Symbol sym = MakeElement.getTagName(app);
                    if (sym != null)
                      {
                        for (int j = 0;  ;  j++)
                          {
                            if (j == nattrSyms)
                              {
                                attrSyms[nattrSyms++] = sym;
                                break;
                              }
                            if (sym.equals(attrSyms[j]))
                              {
                                getCompilation().setLine(app);
                                Symbol elementSym = MakeElement.getTagName(exp);
                                String elementName = elementSym == null ? null
                                  : elementSym.toString();
                                messages.error('e', XMLFilter.duplicateAttributeMessage(sym, elementName), "XQST0040");
                              }
                          }
                        nsBindings = maybeAddNamespace(sym, true, nsBindings);
                      }
                  }
	      }
	  }
	if (nsBindings != null)
	  make.setNamespaceNodes(nsBindings);
      }
    return exp;
  }

  public Expression
  checkPragma (Symbol name, Expression contents)
  {
    return null;
  }

  Expression getBaseUriExpr ()
  {
    Compilation comp = getCompilation();
    String staticBaseUri = parser.getStaticBaseUri();
    if (staticBaseUri != null)
      return QuoteExp.getInstance(staticBaseUri);
    else
      return gnu.kawa.functions.GetModuleClass.getModuleClassURI(comp);
  }

  static NamespaceBinding maybeAddNamespace(Symbol qname, boolean isAttribute,
					    NamespaceBinding bindings)
  {
    if (qname == null) // Happens if prevously-reported unknown prefix.
      return bindings;
    String prefix = qname.getPrefix();
    String uri = qname.getNamespaceURI();
    if (prefix == "")
      prefix = null;
    if (uri == "")
      uri = null;
    if (isAttribute && prefix == null && uri == null)
      return bindings;
    return NamespaceBinding.maybeAdd(prefix, uri, bindings);
  }

  /** Wrap a (known) procedure value as a Declaration. */
  static Declaration procToDecl (Object symbol, Object val)
  {
    Declaration decl = new Declaration(symbol);
    decl.setProcedureDecl(true);
    decl.noteValue(new QuoteExp(val));
    decl.setFlag(Declaration.IS_CONSTANT);
    return decl;
  }
}
