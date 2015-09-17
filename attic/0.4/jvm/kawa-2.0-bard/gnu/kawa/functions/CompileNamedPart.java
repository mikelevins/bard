package gnu.kawa.functions;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.reflect.*;
import kawa.lang.Translator;

public class CompileNamedPart
{
  public static Expression validateGetNamedPart
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[1] instanceof QuoteExp)
        || ! (exp instanceof GetNamedExp))
      return exp;
    Expression context = args[0];
    Declaration decl = null;
    if (context instanceof ReferenceExp)
      {
        ReferenceExp rexp = (ReferenceExp) context;
        if ("*".equals(rexp.getName()))
          return makeGetNamedInstancePartExp(args[1]);
        decl = rexp.getBinding();
      }

    String mname = ((QuoteExp) args[1]).getValue().toString();
    Type type = context.getType();
    boolean isInstanceOperator = context == QuoteExp.nullExp;
    Compilation comp = visitor.getCompilation();
    Language language = comp.getLanguage();
    Type typeval = language.getTypeFor(context, false);
    ClassType caller = comp == null ? null
      : comp.curClass != null ? comp.curClass
      : comp.mainClass;
    GetNamedExp nexp = (GetNamedExp) exp;

    if (typeval != null)
      {
        if (mname.equals(GetNamedPart.CLASSTYPE_FOR))
          return new QuoteExp(typeval);

        if (typeval instanceof ObjectType)
          {
            if (mname.equals("new"))
              return nexp.setProcedureKind('N');
            if (mname.equals(GetNamedPart.INSTANCEOF_METHOD_NAME))
              return nexp.setProcedureKind('I');
            if (mname.equals(GetNamedPart.CAST_METHOD_NAME))
              return nexp.setProcedureKind('C');
          }
      }
    if (typeval instanceof ObjectType)
      {
        if (mname.length() > 1 && mname.charAt(0) == '.')
          {
            // The following would also work:
            // return nexp.setProcedureKind('D');
            // However, it makes optimzing the 'setter' case harder.
            return new QuoteExp(new NamedPart(typeval, mname, 'D'));
          }
        if (CompileReflect.checkKnownClass(typeval, comp) < 0)
          return exp;
        ObjectType otype = (ObjectType) typeval;
        PrimProcedure[] methods
          = ClassMethods.getMethods(otype,
                                    Compilation.mangleName(mname),
                                    '\0', caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            nexp.otype = otype;
            nexp.mname = mname;
            return nexp.setProcedureKind('S');
          }
        ApplyExp aexp = new ApplyExp(SlotGet.staticField, args);
        aexp.setLine(exp);
        return visitor.visitApplyOnly(aexp, required);
                            
      }
    if (typeval != null)
      {
        
      }

    /*
    if (type.isSubtype(Compilation.typeValues))
      {
        // FIXME
      }
    */

    if (type.isSubtype(Compilation.typeClassType)
        || type.isSubtype(Type.javalangClassType))
      // The container evaluates to a class (so we should look for a static
      // field or method), but we don't know which class at compile-time.
      // However, we should still optimize it somewhat, above.  FIXME.
      return exp;

    if (type instanceof ObjectType)
      {
        ObjectType otype = (ObjectType) type;
        PrimProcedure[] methods
          = ClassMethods.getMethods(otype, Compilation.mangleName(mname),
                                    'V', caller, language);
        if (methods != null && methods.length > 0)
          {
            nexp.methods = methods;
            return nexp.setProcedureKind('M');
          }

        if (type.isSubtype(typeHasNamedParts))
          {
            Object val;
            if (decl != null
                && (val = Declaration.followAliases(decl).getConstantValue()) != null)
              {
                HasNamedParts value = (HasNamedParts) val;
                if (value.isConstant(mname))
                  {
                    val = value.get(mname);
                    return QuoteExp.getInstance(val);
                  }
              }
            args = new Expression[] { args[0], QuoteExp.getInstance(mname) };
            return new ApplyExp(typeHasNamedParts.getDeclaredMethod("get", 1),
                                args).setLine(exp);
          }

        Member part = SlotGet.lookupMember(otype, mname, caller);
        if (part != null
            || (mname.equals("length") && type instanceof ArrayType))
          {
            // FIXME: future kludge to avoid re-doing SlotGet.getField.
            // args = new Expression[] { context, new QuoteExp(part) });
            ApplyExp aexp = new ApplyExp(SlotGet.field, args);
            aexp.setLine(exp);
            return visitor.visitApplyOnly(aexp, required);
          }
      }

    if (comp.warnUnknownMember())
      comp.error('w', "no known slot '"+mname+"' in "+type.getName());
    return exp;
  }

  public static Expression validateSetNamedPart
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    Expression[] args = exp.getArgs();
    if (args.length != 3 || ! (args[1] instanceof QuoteExp))
      {
        exp.visitArgs(visitor);
        return exp;
      }
    args[0] = visitor.visit(args[0], null);
    args[1] = visitor.visit(args[1], null);

    Expression context = args[0];
    String mname = ((QuoteExp) args[1]).getValue().toString();
    Type type = context.getType();
    Compilation comp = visitor.getCompilation();
    Language language = comp.getLanguage();
    Type typeval = language.getTypeFor(context);
    ClassType caller = comp == null ? null
      : comp.curClass != null ? comp.curClass
      : comp.mainClass;
    ApplyExp original = exp;
    if (typeval instanceof ClassType)
      {
        exp = new ApplyExp(SlotSet.set$Mnstatic$Mnfield$Ex, args);
      }
    else if (type instanceof ClassType)
      {
        Member part = SlotSet.lookupMember((ClassType) type, mname, caller);
        if (part != null)
          return visitor.visit(CompileReflect.makeSetterCall(args[0], part, args[2]), Type.voidType);
      }
    exp.setType(Type.voidType);
    if (exp == original)
      {
        args[2] = visitor.visit(args[2], null);
        return exp;
      }
    exp.setLine(original);
    return visitor.visit(exp, required);
  }

  public static Expression makeExp (Expression clas, Expression member)
  {
    ReferenceExp rexp;
    String combinedName = combineName(clas, member);
    Environment env = Environment.getCurrent();
    if (combinedName != null)
      {
        Translator tr = (Translator) Compilation.getCurrent();
        Symbol symbol = Namespace.EmptyNamespace.getSymbol(combinedName);
        Declaration decl = tr.lexical.lookup(symbol, false/*FIXME*/);
        if (! Declaration.isUnknown(decl))
          return new ReferenceExp(decl);

        Object property = null; // FIXME?
        if (symbol != null && env.isBound(symbol, property))
          return new ReferenceExp(combinedName);
      }
    if (clas instanceof ReferenceExp
        && (rexp = (ReferenceExp) clas).isUnknown())
      {
        Object rsym = rexp.getSymbol();
        Symbol sym = rsym instanceof Symbol ? (Symbol) rsym
          : env.getSymbol(rsym.toString());
        if (env.get(sym, null) == null)
          {
            String name = rexp.getName();
            try
              {
                Class cl = ClassType.getContextClass(name);
                clas = QuoteExp.getInstance(Type.make(cl));
              }
            catch (Exception ex)
              {
              }
          }
      }
    Expression[] args = { clas, member };
    GetNamedExp exp = new GetNamedExp(args);
    exp.combinedName = combinedName;
    return exp;
  }

  public static String combineName (Expression part1, Expression part2)
  {
    String name1;
    Object name2;
    if ((name2 = part2.valueIfConstant()) instanceof SimpleSymbol
        && ((part1 instanceof ReferenceExp
             && (name1 = ((ReferenceExp) part1).getSimpleName()) != null)
            || (part1 instanceof GetNamedExp
                && (name1 = ((GetNamedExp) part1).combinedName) != null)))
      return (name1+':'+name2).intern();
    return null;
  }

  public static Expression makeExp (Expression clas, String member)
  {
    return makeExp(clas, new QuoteExp(member));
  }

  public static Expression makeExp (Type type, String member)
  {
    return makeExp(new QuoteExp(type), new QuoteExp(member));
  }

  public static Expression validateNamedPart
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    NamedPart namedPart = (NamedPart) proc;
    switch (namedPart.kind)
      {
      case 'D':
        String fname = namedPart.member.toString().substring(1);
        Expression[] xargs = new Expression[2];
        xargs[1] = QuoteExp.getInstance(fname);
        SlotGet slotProc;
        if (args.length > 0)
          {
            xargs[0] = Compilation.makeCoercion(args[0],
                                            new QuoteExp(namedPart.container));
            slotProc = SlotGet.field;
          }
        else
          {
            xargs[0] = QuoteExp.getInstance(namedPart.container);
           slotProc = SlotGet.staticField;
          }
        ApplyExp aexp = new ApplyExp(slotProc, xargs);
        aexp.setLine(exp);
        return visitor.visitApplyOnly(aexp, required);
      }
    return exp;
  }

  public static Expression validateNamedPartSetter
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    NamedPart get = (NamedPart) ((NamedPart.Setter) proc).getGetter();
    if (get.kind == 'D')
      {
        Expression[] xargs = new Expression[3];
        xargs[1] = QuoteExp.getInstance(get.member.toString().substring(1));
        xargs[2] = exp.getArgs()[0];
        SlotSet slotProc;
        if (exp.getArgCount() == 1)
          {
            xargs[0] = QuoteExp.getInstance(get.container);
            slotProc = SlotSet.set$Mnstatic$Mnfield$Ex;
          }
        else if (exp.getArgCount() == 2)
          {
            xargs[0]
              = Compilation.makeCoercion(exp.getArgs()[0], new QuoteExp(get.container));
           slotProc = SlotSet.set$Mnfield$Ex;
          }
        else
          return exp;
        ApplyExp aexp = new ApplyExp(slotProc, xargs);
        aexp.setLine(exp);
        return visitor.visitApplyOnly(aexp, required);
      }
    return exp;
  }

  static final ClassType typeHasNamedParts
  = ClassType.make("gnu.mapping.HasNamedParts");

  public static Expression makeGetNamedInstancePartExp (Expression member)
  {
    String name;
    if (member instanceof QuoteExp)
      {
        Object val = ((QuoteExp) member).getValue();
        if (val instanceof SimpleSymbol)
          return QuoteExp.getInstance(new GetNamedInstancePart(val.toString()));
      }
    return new ApplyExp(Invoke.make,
                        new QuoteExp(ClassType.make("gnu.kawa.functions.GetNamedInstancePart")),
                        member);
  }

  public static Expression validateGetNamedInstancePart
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    Expression[] xargs;
    Procedure property;
    GetNamedInstancePart gproc = (GetNamedInstancePart) proc;
    if (gproc.isField)
      {
        xargs = new Expression[] { args[0], new QuoteExp(gproc.pname) };
        property = SlotGet.field;
      }
    else
      {
        int nargs = args.length;
        xargs = new Expression[nargs+1];
        xargs[0] = args[0];
        xargs[1] = new QuoteExp(gproc.pname);
        System.arraycopy(args, 1, xargs, 2, nargs-1);
        property = Invoke.invoke;
      }
    return visitor.visitApplyOnly(new ApplyExp(property, xargs), required);
  }

  public static Expression validateSetNamedInstancePart
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    Expression[] args = exp.getArgs();
    String pname = ((SetNamedInstancePart) proc).pname;
    Expression[] xargs = new Expression[]
      { args[0], new QuoteExp(pname), args[1] };
    return visitor.visitApplyOnly(new ApplyExp(SlotSet.set$Mnfield$Ex, xargs), required);
  }
}

class GetNamedExp extends ApplyExp
{
  /*
   * 'N' - new (make) - if methodName is "new".
   * 'I' - instance of - if methodName is INSTANCEOF_METHOD_NAME.
   * 'C' - cast - if methodName is CAST_METHOD_NAME.
   * 'T' - type - if methodName is CLASSTYPE_FOR
   * 'M' - non-static method
   * 'S' - static method
   * 'D' - if methodname starts with '.'
   */
  char kind;
  PrimProcedure[] methods;
  ObjectType otype;
  String mname;

  public String combinedName;

  public void apply (CallContext ctx) throws Throwable
  {
    if (combinedName != null)
      {
        Environment env = Environment.getCurrent();
        Symbol sym = env.getSymbol(combinedName);
        Object unb = gnu.mapping.Location.UNBOUND;
        Object property = null;  // FIXME?
        Object value = env.get(sym, property, unb);
        if (value != unb)
          {
            ctx.writeValue(value);
            return;
          }
      }
    super.apply(ctx);
  }

  public GetNamedExp(Expression[] args)
  {
    super(GetNamedPart.getNamedPart, args);
  }

  protected GetNamedExp setProcedureKind (char kind)
  {
    // Called from GetNamedPart.inline when the expression evaluates to a
    // procedure that takes (at least) a 'this' parameter.  If the
    // expression is in turn used in function call position it is normally
    // the first argment to ApplyToArgs, so setting the type to typeProcedure
    // allows ApplyToArgs.inline to be optimized away, and then later
    // the inline method in the GetNamedExp class can get called.
    this.type = Compilation.typeProcedure;
    this.kind = kind;
    return this;
  }

  public Expression validateApply (ApplyExp exp, InlineCalls visitor,
                                   Type required, Declaration decl)
  {
    Expression[] pargs = getArgs();
    Expression context = pargs[0];
    Expression[] args = exp.getArgs();
    Expression[] xargs;
    int adjust;
    switch (kind)
      {
      case 'M':
        decl = invokeDecl;
        xargs = new Expression[args.length+2];
        xargs[0] = pargs[0];
        xargs[1] = pargs[1];
        System.arraycopy(args, 0, xargs, 2, args.length);
        adjust = 2;
        break;
      case 'N': // new
        decl = makeDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = context;
        adjust = 1;
        break;
      case 'I': // instance-of
        decl = instanceOfDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        xargs[0] = args[0];
        xargs[1] = context;
        adjust = exp.firstSpliceArg > 0 ? 1 : 0;
        break;
      case 'C': // cast
        decl = castDecl;
        xargs = new Expression[args.length+1];
        System.arraycopy(args, 0, xargs, 1, args.length);
        xargs[0] = context;
        adjust = 1;
        break;
      case 'S': // invoke-static
        decl = invokeStaticDecl;
        xargs = new Expression[args.length+2];
        xargs[0] = context;
        xargs[1] = pargs[1];
        System.arraycopy(args, 0, xargs, 2, args.length);
        adjust = 2;
        break;
      default:
        return exp;
      }
    ApplyExp result = new ApplyExp(new ReferenceExp(decl), xargs);
    if (exp.firstSpliceArg >= 0)
      result.firstSpliceArg = exp.firstSpliceArg + adjust;
    result.setLine(exp);
    if (methods != null && kind == 'S') {
        return CompileInvoke.validateNamedInvoke(result, visitor,
                                                 otype, mname,
                                                 methods,
                                                 Invoke.invokeStatic,
                                                 required);
    }
    return visitor.visit(result, required);
  }

  public boolean side_effects ()
  {
    // The actual GetNamedExp that returns a method reference doesn't
    // have side-effects - though applying tha result does.
    if (kind == 'S' || kind == 'N' || kind == 'C' || kind == 'I')
      return false;
    if (kind == 'M')
      return getArgs()[0].side_effects();
    return true;
  }

  static final Declaration fieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "field");

  static final Declaration staticFieldDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.SlotGet", "staticField");

  static final Declaration makeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "make");

  static final Declaration invokeDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invoke");

  static final Declaration invokeStaticDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.reflect.Invoke", "invokeStatic");

  static final Declaration instanceOfDecl
  = Declaration.getDeclarationFromStatic("kawa.standard.Scheme", "instanceOf");

  static final Declaration castDecl
  = Declaration.getDeclarationFromStatic("gnu.kawa.functions.Convert", "as");
}
