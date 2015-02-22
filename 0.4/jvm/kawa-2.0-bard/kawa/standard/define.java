package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.expr.*;
import gnu.lists.*;
import gnu.kawa.reflect.Invoke;

/**
 * The Syntax transformer that re-writes the "%define" internal form.
 * This is used to implement define, define-private, and define-constant.
 * Syntax: <code>(%define name code type value)</code>.
 * The <code>name</code> is an identifier (<code>String</code> or
 * <code>Symbol</code>) or </code>Declaration</code>.
 * The <code>code</code> is an integer mask,
 * where 1 means type specified, 2 means a function definition,
 * 4 means private, 8 means constant, and 16 means an early constant.
 * As a special case, define-procedure is 1+2+8+16=27
 * The <code>type</code> is the declarated type or <code>null</code>.
 * The <code>value</code> is the initializing value.
 * @author	Per Bothner
 */

public class define extends Syntax
{
  public static final define defineRaw = new define(SchemeCompilation.lambda);

  Lambda lambda;

  String getName (int options)
  {
    if ((options & 4) != 0)
      return "define-private";
    else if ((options & 8) != 0)
      return "define-constant";
    else
      return "define";
  }

  public define(Lambda lambda)
  {
    this.lambda = lambda;
  }

  public void scanForm (Pair st, ScopeExp defs, Translator tr)
  {
    Pair p1 = (Pair) st.getCdr();
    Pair p2 = (Pair) p1.getCdr();
    Pair p3 = (Pair) p2.getCdr();
    SyntaxForm nameSyntax = null;
    Object name = p1.getCar();
    while (name instanceof SyntaxForm)
      {
	nameSyntax = (SyntaxForm) name;
	name = nameSyntax.getDatum();
      }
    int options = ((Number) Translator.stripSyntax(p2.getCar())).intValue();
    boolean makePrivate = (options & 4) != 0;
    boolean makeConstant = (options & 8) != 0;
    boolean makeCompoundProcedure = options == 27;

    name = tr.namespaceResolve(name);
    if (! (name instanceof Symbol))
      {
        tr.error('e', "'"+name+"' is not a valid identifier");
        name = null;
      }

    Object savePos = tr.pushPositionOf(p1);
    Declaration decl = tr.define(name, nameSyntax, defs);
    tr.popPositionOf(savePos);
    name = decl.getSymbol();
    if (makePrivate)
      {
	decl.setFlag(Declaration.PRIVATE_SPECIFIED);
	decl.setPrivate(true);
      }
    if (makeConstant)
      decl.setFlag(Declaration.IS_CONSTANT);
    if ((options & 16) != 0)
      decl.setFlag(Declaration.EARLY_INIT);
    decl.setFlag(Declaration.IS_SINGLE_VALUE);

    Expression value;
    if ((options & 2) != 0 && ! makeCompoundProcedure)
      {
	LambdaExp lexp = new LambdaExp();
	lexp.setSymbol(name);
        if (Compilation.inlineOk)
          {
            decl.setProcedureDecl(true);
            decl.setType(Compilation.typeProcedure);
            lexp.nameDecl = decl;
          }
	Translator.setLine(lexp, p1);
        value = lexp;
      }
    else
      value = null;
    SetExp sexp = new SetExp(decl, value);

    if (defs instanceof ModuleExp && ! makePrivate && ! makeConstant
        && (! Compilation.inlineOk || tr.sharedModuleDefs()))
      decl.setCanWrite(true);

    if ((options & 1) != 0)
      {
        decl.setTypeExp(new LangExp(p3));
	decl.setFlag(Declaration.TYPE_SPECIFIED);
      }

    st = Translator.makePair(st, this,
			     Translator.makePair(p1, sexp, p2));
    Translator.setLine(decl, p1);

    tr.pushForm(st);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair p1 = (Pair) form.getCdr();
    Pair p2 = (Pair) p1.getCdr();
    Pair p3 = (Pair) p2.getCdr();
    Pair p4 = (Pair) p3.getCdr();
    Object name = p1.getCar();
    int options = ((Number) Translator.stripSyntax(p2.getCar())).intValue();
    boolean makePrivate = (options & 4) != 0;
    boolean makeCompoundProcedure = options == 27;

    if (! (name instanceof SetExp))
      return tr.syntaxError(getName(options) + " is only allowed in a <body>");
    SetExp sexp = (SetExp) name;
    Declaration decl = sexp.getBinding();

    if (decl.getFlag(Declaration.TYPE_SPECIFIED))
      {
        Expression texp = decl.getTypeExp();
        if (texp instanceof LangExp)
          {
            Pair typeSpecPair = (Pair) ((LangExp) texp).getLangValue(); 
            decl.setType(tr.exp2Type(typeSpecPair));
          }
      }

    BeginExp bexp2 = null;
    boolean unknownValue;
    if ((options & 2) != 0 && ! makeCompoundProcedure)
      {
        LambdaExp lexp = (LambdaExp) sexp.getNewValue();
        Object formals = p4.getCar();
	Object body = p4.getCdr();
        lambda.rewrite(lexp, formals, body, tr, null);
        unknownValue = ! Compilation.inlineOk;
      }
    else
      {
        unknownValue = decl.context instanceof ModuleExp && ! makePrivate && decl.getCanWrite();
        if (makeCompoundProcedure) {
            tr.letStart();
            ClassType classGenericProc = ClassType.make("gnu.expr.GenericProc");
            Declaration gproc =
                tr.letVariable(null,
                               classGenericProc,
                               new ApplyExp(Invoke.make,
                                   new Expression[] {
                                       QuoteExp.getInstance(classGenericProc),
                                       QuoteExp.getInstance(decl.getName()) }));
            gproc.setFlag(Declaration.ALLOCATE_ON_STACK);
            tr.letEnter();
            BeginExp bexp1 = new BeginExp(); // early-init-code goes here
            Method addMethod = classGenericProc.getDeclaredMethod("add", 1);
            Method setPropMethod =
                classGenericProc.getDeclaredMethod("setProperty", 2);
            for (;;) {
                Keyword key = null;
                Object car = Translator.stripSyntax(p4.getCar());
                if (car instanceof Keyword) {
                    key = (Keyword) car;
                    Object cdr = p4.getCdr();
                    if (! (cdr instanceof Pair)
                        || Translator.safeCar(cdr) instanceof Keyword) {
                        tr.error('e', "missing value following keyword");
                        break;
                    }
                    p4 = (Pair) cdr;
                }
                Expression arg = tr.rewrite_car(p4, false);
                if (key != null) {
                    if (bexp2 == null)
                        bexp2 = new BeginExp();
                    bexp2.add(new ApplyExp(setPropMethod,
                                           new Expression[] {
                                               new ReferenceExp(decl),
                                               QuoteExp.getInstance(key),
                                               arg }));
                } else {
                    Declaration gdecl = arg instanceof LambdaExp ? gproc : decl;
                    Expression addCall =
                        new ApplyExp(addMethod,
                                     new Expression[] {
                                         new ReferenceExp(gdecl),
                                         arg });
                    if (arg instanceof LambdaExp) {
                        LambdaExp larg = (LambdaExp) arg;
                        String lname = larg.getName();
                        String dname = decl.getName();
                        if (lname == null || lname.equals(dname)) {
                            // Needed so PrimProcedure.getMethodFor
                            // can find this method.
                            if (decl.isPublic())
                                larg.setFlag(LambdaExp.PUBLIC_METHOD);
                            // FIXME Maybe set larg.nameDecl to decl?
                            // At least if we have a single LambdaExp?
                            // FIXME set the name?
                            // This "enables" ModuleMethod#resolveParameterTypes
                            // to search for the most specific method, which
                            // is expensive - and pull in Compilation and
                            // related classes.
                            // if (lname == null)
                            //    larg.setName(dname);
                        }
                        bexp1.add(addCall);
                    } else {
                        bexp2.add(addCall);
                    }
                }
                Object cdr = p4.getCdr();
                if (! (cdr instanceof Pair)) {
                    if (cdr != LList.Empty)
                        tr.error('e', "not a proper list");
                    break;
                }
                p4 = (Pair) cdr;
            }
            ReferenceExp gref = new ReferenceExp(gproc);
            gref.setFlag(ReferenceExp.ALLOCATE_ON_STACK_LAST);
            bexp1.add(gref);
            sexp.setNewValue(tr.letDone(BeginExp.canonicalize(bexp1)));
        } else
            sexp.setNewValue(tr.rewrite_car(p4, false));
      }
    if (unknownValue)
      decl.noteValueUnknown();
    else
      decl.noteValueFromSet(sexp);

    sexp.setDefining (true);
    if (makePrivate && ! (decl.getContext() instanceof ModuleExp))
      tr.error('w', "define-private not at top level");
    if (bexp2 != null)
        return new BeginExp(sexp, bexp2);
    return sexp;
  }
}
