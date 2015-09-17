package kawa.standard;
import gnu.expr.*;
import kawa.lang.*;
import gnu.lists.*;
import java.util.Vector;
import gnu.mapping.*;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.kawa.functions.Convert;

public class object extends Syntax
{
  public static final object objectSyntax
    = new kawa.standard.object(SchemeCompilation.lambda);
  static { objectSyntax.setName("object"); }

  Lambda lambda;
  public static final Keyword accessKeyword = Keyword.make("access");
  public static final Keyword classNameKeyword = Keyword.make("class-name");
  public static final Keyword interfaceKeyword = Keyword.make("interface");
  public static final Keyword throwsKeyword = Keyword.make("throws");
  static final Keyword typeKeyword = Keyword.make("type");
  public static final Keyword allocationKeyword = Keyword.make("allocation");
  static final Keyword initKeyword = Keyword.make("init");
  static final Keyword initformKeyword = Keyword.make("initform");
  static final Keyword init_formKeyword = Keyword.make("init-form");
  static final Keyword init_valueKeyword = Keyword.make("init-value");
  static final Keyword init_keywordKeyword = Keyword.make("init-keyword");

  static final Symbol coloncolon = Namespace.EmptyNamespace.getSymbol("::");

  public object(Lambda lambda)
  {
    this.lambda = lambda;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (! (form.getCdr() instanceof Pair))
      return tr.syntaxError("missing superclass specification in object");
    Pair pair = (Pair) form.getCdr();
    ObjectExp oexp = new ObjectExp();
    if (pair.getCar() instanceof FString)
      {
        // oexp.setName(pair.getCar().toString());
	if (! (pair.getCdr() instanceof Pair))
	  return tr.syntaxError("missing superclass specification after object class name");
	pair = (Pair) pair.getCdr();
      }
    Object[] saved = scanClassDef(pair, oexp, tr);
    if (saved != null)
      rewriteClassDef(saved, tr);
    return oexp;
  }

  /** Does the first "scan-time" processing of the class/object definition.
   * Returns an array of values to be used at "rewrite-time".
   */
  public Object[] scanClassDef (Pair pair, ClassExp oexp, Translator tr)
  {
    tr.mustCompileHere();
    Object superlist = pair.getCar();
    Object components = pair.getCdr();
    Object classNamePair = null;
    LambdaExp method_list = null;
    LambdaExp last_method = null;
    long classAccessFlag = 0;
    // First pass (get Declarations).
    Vector inits = new Vector(20);
    for (Object obj = components;  obj != LList.Empty;  )
      {
	// The SyntaxForm scopes aren't used in scanClassDef, but they are
	// used in rewriteClassDef, and might as well make the code the same.
	while (obj instanceof SyntaxForm)
	  obj = ((SyntaxForm) obj).getDatum();
	if (! (obj instanceof Pair))
	  {
	    tr.error('e', "object member not a list");
	    return null;
	  }
	pair = (Pair) obj;
	Object pair_car = pair.getCar();
	while (pair_car instanceof SyntaxForm)
	  pair_car = ((SyntaxForm) pair_car).getDatum();
	obj = pair.getCdr(); // Next member.
	Object savedPos1 = tr.pushPositionOf(pair);
        if (pair_car instanceof Keyword)
          {
            while (obj instanceof SyntaxForm)
              obj = ((SyntaxForm) obj).getDatum();
            if (obj instanceof Pair)
              {
                if (pair_car == interfaceKeyword)
                  {
                    Object val = ((Pair) obj).getCar();
                    while (val instanceof SyntaxForm)
                      val = ((SyntaxForm) val).getDatum();
                    if (val == Boolean.FALSE)
                      oexp.setFlag(ClassExp.CLASS_SPECIFIED);
                    else
                      oexp.setFlag(ClassExp.INTERFACE_SPECIFIED|ClassExp.IS_ABSTRACT);
                    obj = ((Pair) obj).getCdr();
                    tr.popPositionOf(savedPos1);
                    continue;
                  }
                if (pair_car == classNameKeyword)
                  {
                    if (classNamePair != null)
                      tr.error('e', "duplicate class-name specifiers");
                    classNamePair = obj;
                    obj = ((Pair) obj).getCdr();
                    tr.popPositionOf(savedPos1);
                    continue;
                  }
                if (pair_car == accessKeyword)
                  {
                    Object savedPos2 = tr.pushPositionOf(obj);
                    classAccessFlag = addAccessFlags(((Pair) obj).getCar(),
                                                     classAccessFlag,
                                                     Declaration.CLASS_ACCESS_FLAGS,
                                                     "class", tr);
                    if (oexp.nameDecl == null)
                      tr.error('e', "access specifier for anonymous class");
                    tr.popPositionOf(savedPos2);
                    obj = ((Pair) obj).getCdr();
                    tr.popPositionOf(savedPos1);
                    continue;
                  }
              }
          }
        else if (pair_car instanceof Pair
                 && Lambda.isAnnotationSymbol(((Pair)pair_car).getCar()))
          {
            if (oexp.nameDecl == null)
              tr.error('e', "annotation for anonymous class");
            else
              oexp.nameDecl.addAnnotation(new LangExp(pair));
            continue;
          }
	if (! (pair_car instanceof Pair))
	  {
	    tr.error('e', "object member not a list");
	    return null;
	  }
	pair = (Pair) pair_car;
	pair_car = pair.getCar();
	while (pair_car instanceof SyntaxForm)
	  pair_car = ((SyntaxForm) pair_car).getDatum();
	if (pair_car instanceof String || pair_car instanceof Symbol
	    || pair_car instanceof Keyword)
	  { // Field declaration.
	    Pair typePair = null;
	    Object sname = pair_car;
	    Object args;
	    Declaration decl;
	    int allocationFlag = 0;
	    long accessFlag = 0;
	    if (sname instanceof Keyword)
	      {
		decl = null;
		args = pair;
	      }
	    else
	      {
		decl = oexp.addDeclaration(sname);
		decl.setSimple(false);
		decl.setFlag(Declaration.FIELD_OR_METHOD);
		Translator.setLine(decl, pair);
		args = pair.getCdr();
	      }
	    int nKeywords = 0;
	    boolean seenInit = false;
	    Pair initPair = null;
	    while (args != LList.Empty)
	      {
                while (args instanceof SyntaxForm)
                  args = ((SyntaxForm) args).getDatum();
		pair = (Pair) args;
		Pair keyPair = pair;
		Object key = Translator.stripSyntax(pair.getCar());
		Object savedPos2 = tr.pushPositionOf(pair);
		args = pair.getCdr();
		if ((key == coloncolon || key instanceof Keyword)
		    && args instanceof Pair)
		  {
		    nKeywords++;
		    pair = (Pair) args;
		    Object value = Translator.stripSyntax(pair.getCar());
		    args = pair.getCdr();
		    if (key == coloncolon || key == typeKeyword)
		      typePair = pair;
		    else if (key == allocationKeyword)
		      {
			if (allocationFlag != 0)
			  tr.error('e', "duplicate allocation: specification");
			if (matches(value, "class", tr)
			    || matches(value, "static", tr))
			  allocationFlag = Declaration.STATIC_SPECIFIED;
			else if (matches(value, "instance", tr))
			  allocationFlag = Declaration.NONSTATIC_SPECIFIED;
			else
			  tr.error('e', "unknown allocation kind '"+value+"'");
		      }
		    else if (key == initKeyword
			     || key == initformKeyword
			     || key == init_formKeyword
			     || key == init_valueKeyword)
		      {
			if (seenInit)
			  tr.error('e', "duplicate initialization");
			seenInit = true;
			// In the case of 'init-form: EXPR' the scope of EXPR
			// doesn't include this class;
			// in the case of 'init: EXPR' it does.
			if (key != initKeyword)
			  initPair = pair;
		      }
		    else if (key == init_keywordKeyword)
		      {
			if (! (value instanceof Keyword))
			  tr.error('e', "invalid 'init-keyword' - not a keyword");
			else if (((Keyword) value).getName()
				 != sname.toString())
			  tr.error('w', "init-keyword option ignored");
		      }
		    else if (key == accessKeyword)
		      {
                        Object savedPos3 = tr.pushPositionOf(pair);
                        accessFlag = addAccessFlags(value, accessFlag,
                                                    Declaration.FIELD_ACCESS_FLAGS,
                                                    "field", tr);
                        tr.popPositionOf(savedPos3);
		      }
		    else
		      {
			tr.error('w', "unknown slot keyword '"+key+"'");
		      }
		  }
		else if (args == LList.Empty && ! seenInit)
		  {
		    // CLtL:2 explicitly prohibits this as an extension.
		    initPair = keyPair;
		    seenInit = true;
		  }
		else if (args instanceof Pair
			 && nKeywords == 0 && ! seenInit && typePair == null
			 && (pair = (Pair) args).getCdr() == LList.Empty)
		  {
		    // Backward compatibility.
		    typePair = keyPair;
		    initPair = pair;
		    args = pair.getCdr();
		    seenInit = true;
		  }
                else if (key instanceof Pair
                         && Lambda.isAnnotationSymbol(((Pair)key).getCar()))
                  {
                    decl.addAnnotation(new LangExp(keyPair));
                  }
		else
		  {
		    args = null;  // Trigger error message
		    break;
		  }
		tr.popPositionOf(savedPos2);
	      }
	    if (args != LList.Empty)
	      {
		tr.error('e', "invalid argument list for slot '"
			 + sname + '\''+" args:"+(args==null?"null":args.getClass().getName()));
		return null;
	      }
	    if (seenInit)
	      {
		boolean isStatic
		  = allocationFlag == Declaration.STATIC_SPECIFIED;
		inits.addElement(decl != null ? (Object) decl
				 : isStatic ? Boolean.TRUE : Boolean.FALSE);
		inits.addElement(initPair);
	      }
	    if (decl == null)
	      {
		if (! seenInit)
		  {
		    tr.error('e', "missing field name");
		    return null;
		  }
	      }
	    else
	      {
		if (typePair != null)
                  {
                    decl.setTypeExp(new LangExp(typePair));
                    decl.setFlag(Declaration.TYPE_SPECIFIED);
                  }
		if (allocationFlag != 0)
		  decl.setFlag(allocationFlag);
		if (accessFlag != 0)
		  decl.setFlag(accessFlag);
                // FIXME Shouldn't need to noteValueUnknown when
                // the field is private or otherwise module-local.
                // However, this can trigger a bug if the field is
                // non-static but doesn't need a closure environment.
                // See Savannah bug #39940.
                // Fixing this properly is difficult.
                /* if ((accessFlag & Declaration.PRIVATE_ACCESS) == 0) */
                  decl.noteValueUnknown();
		decl.setCanRead(true);
		decl.setCanWrite(true);
	      }
	  }
	else if (pair_car instanceof Pair)
	  { // Method declaration.
	    Pair mpair = (Pair) pair_car;
	    Object mname = mpair.getCar();
            while (mname instanceof SyntaxForm)
              mname = ((SyntaxForm) mname).getDatum();
	    if (! (mname instanceof String)
		&& ! (mname instanceof Symbol))
	      {
		tr.error('e', "missing method name");
		return null;
	      }
	    LambdaExp lexp = new LambdaExp();
	    Declaration decl = oexp.addMethod(lexp, mname);
	    Translator.setLine(decl, mpair);
	    if (last_method == null)
	      method_list = lexp;
	    else
	      last_method.nextSibling = lexp;
	    last_method = lexp;
	  }
	else
	  tr.error ('e', "invalid field/method definition");
	tr.popPositionOf(savedPos1);
      }
    if (classAccessFlag != 0 && oexp.nameDecl != null)
      {
        oexp.nameDecl.setFlag(classAccessFlag);
        if ((classAccessFlag & Declaration.ABSTRACT_ACCESS) != 0)
          oexp.setFlag(ClassExp.IS_ABSTRACT);
      }

    if (classNamePair != null)
      {
        Expression classNameExp = tr.rewrite_car((Pair) classNamePair, false);
        Object classNameVal = classNameExp.valueIfConstant();
        String classNameSpecifier;
        boolean isString;
        /* #ifdef use:java.lang.CharSequence */
        isString = classNameVal instanceof CharSequence;
        /* #else */
        // isString = classNameVal instanceof CharSeq || classNameVal instanceof String;
        /* #endif */
        if (isString
            && (classNameSpecifier = classNameVal.toString()).length() > 0)
          oexp.classNameSpecifier = classNameSpecifier;
        else
            tr.errorWithPosition("class-name specifier must be a non-empty string literal", classNamePair);
      }

    Object[] result = {
      oexp,
      components,
      inits,
      method_list,
      superlist
    };
    return result;
  }

  public void rewriteClassDef (Object[] saved, Translator tr)
  {
    ClassExp oexp = (ClassExp) saved[0];
    Object components = saved[1];
    Vector inits = (Vector) saved[2];
    LambdaExp method_list = (LambdaExp) saved[3];
    Object superlist = saved[4];
    oexp.firstChild = method_list;

    int num_supers = Translator.listLength(superlist);
    if (num_supers < 0)
      {
        tr.error('e', "object superclass specification not a list");
        num_supers = 0;
      }
    Expression[] supers = new Expression[num_supers];
    for (int i = 0;  i < num_supers;  i++)
      {
	while (superlist instanceof SyntaxForm)
	  {
	    // FIXME - need to pass syntax.
	    superlist = ((SyntaxForm) superlist).getDatum();
	  }
	Pair superpair = (Pair) superlist;
	supers[i] = tr.rewrite_car(superpair, false);
        if (supers[i] instanceof ReferenceExp)
          {
            Declaration decl = Declaration.followAliases(((ReferenceExp) supers[i]).getBinding());
            Expression svalue;
            if (decl != null && (svalue = decl.getValue()) instanceof ClassExp)
              ((ClassExp) svalue).setFlag(ClassExp.HAS_SUBCLASS);
          }
	superlist = superpair.getCdr();
      }

    oexp.supers = supers;

    oexp.setTypes(tr);

    if (oexp.nameDecl != null)
      Lambda.rewriteAnnotations(oexp.nameDecl, tr);
    for (Declaration decl = oexp.firstDecl(); decl != null;  decl = decl.nextDecl())
      {
        Lambda.rewriteAnnotations(decl, tr);
      }

    // First a pass over init-form: specifiers, since these are evaluated
    // in a scope outside the current class.
    int len = inits.size();
    for (int i = 0;  i < len;  i += 2)
      {
	Object init = inits.elementAt(i+1);
	if (init != null)
          rewriteInit(inits.elementAt(i), oexp, (Pair) init, tr, null);
      }

    tr.push(oexp);

    // Pass to rewrite method/initializer bodies.
    LambdaExp meth = method_list;
    int init_index = 0;  // Input index in inits Vector.
    SyntaxForm componentsSyntax = null;
    for (Object obj = components;  obj != LList.Empty;  )
      {
	while (obj instanceof SyntaxForm)
	  {
	    componentsSyntax = (SyntaxForm) obj;
	    obj = componentsSyntax.getDatum();
	  }
	Pair pair = (Pair) obj;
	Object savedPos1 = tr.pushPositionOf(pair);
	Object pair_car = pair.getCar();
	SyntaxForm memberSyntax = componentsSyntax;
	while (pair_car instanceof SyntaxForm)
	  {
	    memberSyntax = (SyntaxForm) pair_car;
	    pair_car = memberSyntax.getDatum();
	  }
	try
	  {
	    obj = pair.getCdr(); // Next member.
            if (pair_car instanceof Keyword
                && obj instanceof Pair)
              {
                // Handled at scan time.
                obj = ((Pair) obj).getCdr();
                continue;
              }
	    pair = (Pair) pair_car;
	    pair_car = pair.getCar();
	    SyntaxForm memberCarSyntax = memberSyntax;
	    while (pair_car instanceof SyntaxForm)
	      {
		memberCarSyntax = (SyntaxForm) pair_car;
		pair_car = memberCarSyntax.getDatum();
	      }
            if (Lambda.isAnnotationSymbol(pair_car))
                ; // Skip
	    else if (pair_car instanceof String || pair_car instanceof Symbol
		|| pair_car instanceof Keyword)
	      { // Field declaration.
		Object type = null;
		int nKeywords = 0;
		Object args = pair_car instanceof Keyword ? pair : pair.getCdr();
		Pair initPair = null;
                SyntaxForm initSyntax = null;
		while (args != LList.Empty)
		  {
                    while (args instanceof SyntaxForm)
                      {
                        memberSyntax = (SyntaxForm) args;
                        args = memberSyntax.getDatum();
                      }
		    pair = (Pair) args;
		    Object key = pair.getCar();
                    while (key instanceof SyntaxForm)
                      key = ((SyntaxForm) key).getDatum();
		    Object savedPos2 = tr.pushPositionOf(pair);
		    args = pair.getCdr();
		    if ((key == coloncolon || key instanceof Keyword)
			&& args instanceof Pair)
		      {
			nKeywords++;
			pair = (Pair) args;
			Object value = pair.getCar();
			args = pair.getCdr();
			if (key == coloncolon || key == typeKeyword)
			  type = value;
			else if (key == initKeyword
				 || key == initformKeyword
				 || key == init_formKeyword
				 || key == init_valueKeyword)
			  {
			    initPair = pair;
                            initSyntax = memberSyntax;
			  }
			else
			  {
			    // handled in first pass.
			  }
		      }
		    else if (args == LList.Empty && initPair == null)
		      {
			// CLtL:2 explicitly prohibits this as an extension.
			initPair = pair;
                        initSyntax = memberSyntax;
		      }
		    else if (args instanceof Pair && nKeywords == 0
			     && initPair == null && type == null
			     && (pair = (Pair) args).getCdr() == LList.Empty)
		      {
			// Backward compatibility.
			type = key;
			initPair = pair;
                        initSyntax = memberSyntax;
			args = pair.getCdr();
		      }
		    else
		      {
			args = null;  // Trigger error message
			break;
		      }
		    tr.popPositionOf(savedPos2);
		  }
		if (initPair != null)
		  {
		    Object d = inits.elementAt(init_index++);
		    boolean isStatic = d instanceof Declaration
                      ? ((Declaration) d).getFlag(Declaration.STATIC_SPECIFIED)
                      : d == Boolean.TRUE;
		    if (inits.elementAt(init_index++) == null)
                      rewriteInit(d, oexp, initPair, tr, initSyntax);
		  }
	      }
	    else if (pair_car instanceof Pair)
	      { // Method declaration.
		ScopeExp save_scope = tr.currentScope();
		// If we saw a TemplateScope (in a SyntaxForm) specific to the
		// formal parameters,  pass it to rewrite so it can create a
		// renamed alias.  A TemplateScope that covers the formals
		// *and* the body we handle using setCurrentScope.
		if (memberSyntax != null)
		  tr.setCurrentScope(memberSyntax.getScope());
                if ("*init*".equals(meth.getName()))
                  meth.setReturnType(Type.voidType);
                Translator.setLine(meth, pair);
                LambdaExp saveLambda = tr.curMethodLambda;
                tr.curMethodLambda = meth;
		lambda.rewrite(meth, ((Pair) pair_car).getCdr(), pair.getCdr(), tr,
			       memberCarSyntax != null
			       && (memberSyntax == null
				   || memberCarSyntax.getScope() != memberSyntax.getScope())
			       ? memberCarSyntax.getScope()
			       : null);
                tr.curMethodLambda = saveLambda;
		if (memberSyntax != null)
		  tr.setCurrentScope(save_scope);
		meth = meth.nextSibling;
	      }
	    else
	      tr.syntaxError("invalid field/method definition");
	  }
	finally
	  {
	    tr.popPositionOf(savedPos1);
	  }
      }
    for (Declaration decl = oexp.firstDecl();
	 decl != null;  decl = decl.nextDecl())
      {
        Expression texp = decl.getTypeExpRaw();
        if (texp instanceof LangExp)
          {
            Pair typeSpecPair = (Pair) ((LangExp) texp).getLangValue(); 
            tr.exp2Type(typeSpecPair, decl, null/*FIXME*/);
          }
      }

    // If initMethod/clinitMethod were created by the "outer" (first) call
    // to rewriteInit, then we may need to fix up their outer chain.
    if (oexp.initMethod != null)
      oexp.initMethod.setOuter(oexp);
    if (oexp.clinitMethod != null)
      oexp.clinitMethod.setOuter(oexp);
    tr.pop(oexp);
  }

  private static void rewriteInit (Object d, ClassExp oexp, Pair initPair,
                                   Translator tr, SyntaxForm initSyntax)
  {
    boolean isStatic = d instanceof Declaration
      ? ((Declaration) d).getFlag(Declaration.STATIC_SPECIFIED)
      : d == Boolean.TRUE;
    LambdaExp initMethod = isStatic ? oexp.clinitMethod : oexp.initMethod;
    if (initMethod == null)
      {
        initMethod = new LambdaExp(new BeginExp());        
        initMethod.setClassMethod(true);
        initMethod.setReturnType(Type.voidType);
        if (isStatic)
          {
            initMethod.setName("$clinit$");
            oexp.clinitMethod = initMethod;
          }
        else
          {
            initMethod.setName("$finit$");
            oexp.initMethod = initMethod;
            // pseudo-this??  $finit$ is a static method - but (this) is valid.
            // Is type getting set?  FIXME
            initMethod.add(null, new Declaration(ThisExp.THIS_NAME));
          }
        initMethod.nextSibling = oexp.firstChild;
        oexp.firstChild = initMethod;
      }
    tr.push(initMethod);
    LambdaExp saveLambda = tr.curMethodLambda;
    tr.curMethodLambda = initMethod;
    Expression initValue = tr.rewrite_car(initPair, initSyntax);
    if (d instanceof Declaration)
      {
        Declaration decl = (Declaration) d;
        SetExp sexp = new SetExp(decl, initValue);
        sexp.setLocation(decl);
        decl.noteValueFromSet(sexp);
        initValue = sexp;
      }
    else
      initValue = Compilation.makeCoercion(initValue, new QuoteExp(Type.voidType));
    ((BeginExp) initMethod.body).add(initValue);
    tr.curMethodLambda = saveLambda;
    tr.pop(initMethod);
}

  /** True if <code>exp</code> matches <code>tag:</code>, <code>"tag"</code>,
   * or <code>'tag</code>.  The latter is recommended as a matter of style.
   */
  static boolean matches (Object exp, String tag, Translator tr)
  {
    String value;
    Object qvalue;
    Pair pair;
    if (exp instanceof Keyword)
      value = ((Keyword) exp).getName();
    else if (exp instanceof FString)
      value = ((FString) exp).toString();
    else if (exp instanceof Pair
             && (qvalue = tr.matchQuoted((Pair) exp)) instanceof gnu.mapping.SimpleSymbol)
      value = qvalue.toString();
    else
      return false;
    return tag == null || tag.equals(value);
  }

  static long addAccessFlags (Object value, long previous, long allowed,
                             String kind, Translator tr)
  {
    long flags = matchAccess(value, tr);
    if (flags == 0)
      tr.error('e', "unknown access specifier "+value);
    else if ((flags & ~allowed) != 0)
      tr.error('e', "invalid "+kind+" access specifier "+value);
    else if ((previous & flags) != 0)
      tr.error('w', "duplicate "+kind+" access specifiers "+value);
    return previous | flags;
  }

  static long matchAccess (Object value, Translator tr)
  {
    while (value instanceof SyntaxForm)
      value = ((SyntaxForm) value).getDatum();
    if (value instanceof Pair)
      {
        Pair p = (Pair) value;
        value = tr.matchQuoted((Pair) value);
        if (value instanceof Pair)
          return matchAccess2((Pair) value, tr);
      }
    return matchAccess1(value, tr);
  }

  private static long matchAccess2 (Pair pair, Translator tr)
  {
    long icar = matchAccess1(pair.getCar(), tr);
    Object cdr = pair.getCdr();
    if (cdr == LList.Empty || icar == 0)
      return icar;
    else if (cdr instanceof Pair)
      {
        long icdr = matchAccess2((Pair) cdr, tr);
        if (icdr != 0)
          return icar | icdr;
      }
    return 0;
  }

  private static long matchAccess1 (Object value, Translator tr)
  {
    if (value instanceof Keyword)
      value = ((Keyword) value).getName();
    else if (value instanceof FString)
      value = ((FString) value).toString();
    else if (value instanceof gnu.mapping.SimpleSymbol)
      value = value.toString();
    if ("private".equals(value))
      return Declaration.PRIVATE_ACCESS;
    if ("protected".equals(value))
      return Declaration.PROTECTED_ACCESS;
    if ("public".equals(value))
      return Declaration.PUBLIC_ACCESS;
    if ("package".equals(value))
      return Declaration.PACKAGE_ACCESS;
    if ("volatile".equals(value))
      return Declaration.VOLATILE_ACCESS;
    if ("transient".equals(value))
      return Declaration.TRANSIENT_ACCESS;
    if ("enum".equals(value))
      return Declaration.ENUM_ACCESS;
    if ("final".equals(value))
      return Declaration.FINAL_ACCESS;
    if ("abstract".equals(value))
      return Declaration.ABSTRACT_ACCESS;
    return 0;
  }

}
