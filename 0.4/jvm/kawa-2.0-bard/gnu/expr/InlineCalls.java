// Copyright (c) 2010, 2011  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.kawa.reflect.CompileReflect;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;
import gnu.kawa.util.IdentityHashTable;
import gnu.mapping.*;
import gnu.math.DFloNum;
import gnu.math.IntNum;
import gnu.math.BitOps;
import gnu.text.Char;

import java.lang.reflect.InvocationTargetException;

import gnu.kawa.functions.MakePromise;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.lists.ConstVector;
import gnu.lists.EmptyList;
import gnu.lists.PairWithPosition;

import java.util.List;
import java.util.HashMap;
import java.lang.reflect.Proxy;
import java.lang.annotation.ElementType;
/* #ifdef use:java.lang.invoke */
import java.lang.invoke.*;
/* #endif */

/**
 * The main Expression re-writing pass.
 * This pass handles type-checking (work in progress).
 * Also checks for calls to known Procedures, and may call
 * a procedure-specific handler, which may do inlining, constant-folding,
 * error-checking, and general munging.
 *
 * Should perhaps rename to something like "Validate" since
 * we do type-checking and other stuff beyond inlining.
 */

public class InlineCalls extends ExpExpVisitor<Type> {

    public static ThreadLocal<InlineCalls> currentVisitor
        = new ThreadLocal<InlineCalls>();

    public static Expression inlineCalls (Expression exp, Compilation comp) {
        InlineCalls visitor = new InlineCalls(comp);
        InlineCalls saved = currentVisitor.get(); // normally null
        try {
            currentVisitor.set(visitor);
            return visitor.visit(exp, null);
        } finally {
            currentVisitor.set(saved);
        }
    }

    public InlineCalls (Compilation comp) {
        setContext(comp);
    }

    VarValueTracker valueTracker = new VarValueTracker(this);

    public Expression visit(Expression exp, Type required) {
        Expression exp0 = exp;
        if (! exp.getFlag(Expression.VALIDATED)) {
            exp.setFlag(Expression.VALIDATED); // Protect against cycles.
            exp = ExpVisitor.visit(this, exp, required);
            exp.setFlag(Expression.VALIDATED);
        }
        if (required == ProcedureInCallContext.INSTANCE)
          required = null;
        if (required instanceof ValueNeededType && exp.getType().isVoid()) {
            if (exp == QuoteExp.voidExp)
              return QuoteExp.voidObjectExp;
            if (comp.warnVoidUsed())
                comp.error('w', "void-valued expression where value is needed",
                       exp0);
            // To avoid cascading warnings.
            return Compilation.makeCoercion(exp, Type.objectType);
        }
        return checkType(exp, required);
    }

    public Expression checkType(Expression exp, Type required) {
        Type expType = exp.getType();
        if (expType == Type.toStringType)
            expType = Type.javalangStringType;
        int cmp = required == null || expType == Type.neverReturnsType
            || required == Type.neverReturnsType
            ? 1
            : required.isCompatibleWithValue(expType);
        if (cmp < 0
            || (cmp == 0 && required.isInterface()
                && (exp instanceof QuoteExp || exp instanceof LambdaExp))) {
            if (exp instanceof LambdaExp
                && (required instanceof ClassType
                    || required instanceof ParameterizedType)) {
                ClassType reqraw = required instanceof ParameterizedType ? ((ParameterizedType) required).getRawType() : (ClassType) required;
                Method amethod = reqraw.checkSingleAbstractMethod();
                if (amethod != null) {
                    if (! ModuleExp.compilerAvailable()) {
                        if (! reqraw.isInterface())
                            comp.error('e', "cannot convert procedure to abstract class "+reqraw.getClass().getName()+" without bytecode compiler");
                        Class iface;
                        try {
                            iface = reqraw.getReflectClass();
                        }
                        catch (Exception ex) {
                            iface = null;
                        }
                        if (iface == null)
                            comp.error('e', "cannot find interface "+reqraw.getClass().getName());
                        Method makeProxy =
                            ClassType.make("gnu.kawa.reflect.ProceduralProxy")
                            .getDeclaredMethod("makeProxy", 2);
                        Expression[] args = {QuoteExp.getInstance(iface), exp};
                        return visit(new ApplyExp(makeProxy, args), required);
                    }
                    LambdaExp lexp = (LambdaExp) exp;
                    ObjectExp oexp = new ObjectExp();
                    oexp.setLocation(exp);
                    oexp.supers = new Expression[] { new QuoteExp(required) };
                    oexp.setTypes(getCompilation());
                    Object mname = amethod.getName();
                    oexp.addMethod(lexp, mname);
                    Declaration mdecl = oexp.addDeclaration(mname, Compilation.typeProcedure);
                    oexp.firstChild = lexp;
                    oexp.declareParts(comp);
                    return visit(oexp, required);
                }
            }
            if (required instanceof TypeValue) {
                Expression converted = ((TypeValue) required).convertValue(exp);
                if (converted != null)
                    return converted;
            }

            Language language = comp.getLanguage();
            comp.error(processingAnnotations() ? 'e' : 'w',
                       ("type "+language.formatType(expType)
                        +" is incompatible with required type "
                        +language.formatType(required)),
                       exp);
            if (required instanceof PrimType) {
                // Box the value to force a run-time ClassCastException.
                Type boxed = ((PrimType) required).boxedType();
                ApplyExp expb =
                    Compilation.makeCoercion(exp, boxed);
                expb.setType(required);
                expb.setFlag(Expression.VALIDATED);
                return expb;
            }
        }
        return exp;
    }

    private void setCanAccess(LambdaExp exp, Type required) {
        if (required != ProcedureInCallContext.INSTANCE)
            exp.setCanRead(true);
    }

    protected Expression visitApplyExp(ApplyExp exp, Type required) {
        Expression func = exp.func;
        // Replace (apply (lambda (param ...) body ...) arg ...)
        // by: (let ((param arg) ...) body ...).
        // Note this should be done *before* we visit the lambda, so we can
        // visit the body with params bound to the known args.
        if (func instanceof LambdaExp) {
            // This optimization in principle should be redundant, but leaving
            // it out currently causes worse type-inference and code generation.
            Expression inlined = inlineCall((LambdaExp) func, exp.args, false);
            if (inlined != null)
                return visit(inlined, required);
        }
        func = visit(func, typeForCalledFunction(func));
        exp.func = func;
        return func.validateApply(exp, this, required, null);
    }

    /** Return a required type for procedure application context.
     * This is ProcedureInCallContext.INSTANCE or null.
     * The value ProcedureInCallContext.INSTANCE indicates the expression
     * is used in application context and setCanCall is appropriate.
     * This means the function expression must be a lambda or reference.
     * (Consider a function that is an IfExp:  If the required type is
     * passed down to two branches that are both lambdas, we might think the
     * lambdas are called but not read and thus safe for inlining - but that
     * would be false, since we need the If to yield a procedure value.)
     */
    public static Type typeForCalledFunction(Expression exp) {
        return  (exp instanceof LambdaExp && ! (exp instanceof ClassExp))
            || exp instanceof ReferenceExp
            ? ProcedureInCallContext.INSTANCE
            : null;
    }

    /** Visit an ApplyExp assuming function and arguments have been visited. */
    public final Expression visitApplyOnly(ApplyExp exp, Type required) {
        return exp.func.validateApply(exp, this, required, null);
    }

    public static Integer checkIntValue(Expression exp) {
        if (exp instanceof QuoteExp) {
            QuoteExp qarg = (QuoteExp) exp;
            Object value = qarg.getValue();
            if (! qarg.isExplicitlyTyped() && value instanceof IntNum) {
                IntNum ivalue = (IntNum) value;
                if (ivalue.inIntRange())
                    return Integer.valueOf(ivalue.intValue());
            }
        }
        return null;
    }

    public static Long checkLongValue (Expression exp) {
        if (exp instanceof QuoteExp) {
            QuoteExp qarg = (QuoteExp) exp;
            Object value = qarg.getValue();
            if (! qarg.isExplicitlyTyped() && value instanceof IntNum) {
                IntNum ivalue = (IntNum) value;
                if (ivalue.inLongRange())
                    return Long.valueOf(ivalue.longValue());
            }
        }
        return null;
    }

    public QuoteExp fixIntValue (Expression exp) {
        Integer ival = InlineCalls.checkIntValue(exp);
        if (ival != null)
            return new QuoteExp(ival, comp.getLanguage().getTypeFor(Integer.TYPE));
        return null;
    }

    public QuoteExp fixLongValue(Expression exp) {
        Long ival = InlineCalls.checkLongValue(exp);
        if (ival != null)
            return new QuoteExp(ival, comp.getLanguage().getTypeFor(Long.TYPE));
        return null;
    }

    protected Expression visitQuoteExp(QuoteExp exp, Type required) {
        Object value;
        if (exp.getRawType() == null && ! exp.isSharedConstant()
            && (value = exp.getValue()) != null) {
            Language language = comp.getLanguage();
            Type vtype = language.getTypeFor(value.getClass());
            if (vtype == Type.toStringType)
                vtype = Type.javalangStringType;
            exp.type = vtype;
            Type primRequired;
            if (! exp.isExplicitlyTyped()) {
                if ((primRequired = PrimType.unboxedType(required)) != null) {
                    char sig1 = primRequired.getSignature().charAt(0);
                    if (value instanceof IntNum
                        && primRequired != LangPrimType.characterType
                        && primRequired != LangPrimType.characterOrEofType) {
                        IntNum ivalue = (IntNum) value;
                        Object ival = null;
                        switch (sig1) {
                        case 'B':
                            if (ivalue.inRange(Byte.MIN_VALUE, Byte.MAX_VALUE))
                                ival = Byte.valueOf(ivalue.byteValue());
                            break;
                        case 'S':
                            if (ivalue.inRange(Short.MIN_VALUE, Short.MAX_VALUE))
                                ival = Short.valueOf(ivalue.shortValue());
                            break;
                        case 'I':
                            if (ivalue.inRange(Integer.MIN_VALUE, Integer.MAX_VALUE))
                                ival = Integer.valueOf(ivalue.intValue());
                            break;
                        case 'J':
                            if (ivalue.inRange(Long.MIN_VALUE, Long.MAX_VALUE))
                                ival = Long.valueOf(ivalue.longValue());
                            break;
                        case 'F':
                            ival = Float.valueOf(ivalue.floatValue());
                            break;
                        case 'D':
                            ival = Double.valueOf(ivalue.doubleValue());
                            break;
                        default:
                            ivalue = null;
                        }
                        if (ival != null)
                            exp = new QuoteExp(ival, required);
                        else if (ivalue != null)
                            error('w', "integer "+ivalue+" not in range of "+required.getName());
                    }
                    if (value instanceof DFloNum) {
                        DFloNum dvalue = (DFloNum) value;
                        Object dval;
                        switch (sig1) {
                        case 'F':
                            dval = Float.valueOf(dvalue.floatValue());
                            break;
                        case 'D':
                            dval = Double.valueOf(dvalue.doubleValue());
                            break;
                        default:
                            dval = null;
                        }
                        if (dval != null)
                            exp = new QuoteExp(dval, required);
                        else
                            error('w', "saw float where "+required.getName()+" expected");
                    }
                    if (value instanceof Char) {
                        if (sig1 == 'C') {
                            int ival = ((Char) value).intValue();
                            if (ival >= 0 && ival <= 0xFFFF)
                                exp = new QuoteExp(Character.valueOf((char) ival), required);
                            else
                                error('w', "character scalar value "+ival+" not in range of "+required.getName());
                        } else
                            exp.setType(LangPrimType.characterType);
                    }

                } else if ((value instanceof IntNum) && required != null &&
                           "java.math.BigInteger".equals(required.getName())) {
                    exp = new QuoteExp(((IntNum)value).asBigInteger(), required);
                } else if (value instanceof Char) {
                    exp.setType(LangPrimType.characterType);
                }
            }
        }
        return exp;
    }

    protected Expression visitReferenceExp (ReferenceExp exp, Type required) {
        Declaration decl = exp.getBinding();
        if (decl != null && ! exp.getDontDereference()) {
            IntNum vals = valueTracker.declValueUsage.get(decl);
            if (vals != null) {
                if (VarValueTracker.maybeUninitialized(vals)
                    && ! decl.getFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS)) {
                    comp.error('w', "variable '"+exp.getName()+"' may be uninitialized here", exp);
                    decl.setFlag(Declaration.MAYBE_UNINITIALIZED_ACCESS);
                }
            }

            LambdaExp lval = decl.getLambdaValue();
            if (lval != null) {
                setCanAccess(lval, required);
                valueTracker.checkUninitializedVariables(lval, exp, null);
            }
            Expression dval = decl.getValue();
            if (deferableInit(dval) && ! dval.getFlag(Expression.VALIDATED)) {
                visit(dval, required);
            }

            // Replace references to a void variable (including one whose value
            // is the empty sequence in XQuery) by an empty constant.  This is
            // not so much an optimization as avoiding the complications and
            // paradoxes of variables and expression that are void.
            Type type = decl.getType();
            if (type != null && type.isVoid())
                return QuoteExp.voidExp;
        }
        if (decl != null && decl.field == null && ! decl.getCanWrite()
            && ! exp.getDontDereference()) {
            Expression dval = decl.getValue();
            if (dval instanceof QuoteExp && dval != QuoteExp.undefined_exp)
                return visitQuoteExp(new QuoteExp(((QuoteExp) dval).getValue(), decl.getType()), required);
            // We don't want to visit the body of a named function yet.
            // Though not doing so does hurt optimization.
            // See testsuite/inlining-test.scm:constant-propagation3
            if (dval != null && decl.nvalues == 1 && decl.values[0].kind == Declaration.ValueSource.APPLY_KIND) {
                dval = null;
            }
            if (dval instanceof ReferenceExp && ! decl.isAlias()) {
                ReferenceExp rval = (ReferenceExp) dval;
                Declaration rdecl = rval.getBinding();
                Type dtype = decl.getType();
                if (rdecl != null && ! rdecl.getCanWrite()
                    && (dtype == null || dtype == Type.objectType
                        // We could also allow (some) widening conversions.
                        || dtype == rdecl.getType()))
                    return visitReferenceExp(rval, required);
            }
            if (dval instanceof ClassExp && processingAnnotations()) {
                ClassExp cval = (ClassExp) dval;
                if (cval.compiledType != null)
                    return new QuoteExp(cval.compiledType, required);
            }
            if (! exp.isProcedureName() && decl.isClassMethod()) {
                // FIXME.  This shouldn't be that hard to fix.  For example,
                // we could treat a reference to a one-argument method foo as if it
                // were (lambda (x) (foo x)).  Or we could treat it as (this):foo.
                // However, it's a little tricky handling the general case.
                // (What about overloading?  Varargs methods?  Static methods?)  
                comp.error('e', "unimplemented: reference to method "+decl.getName()+" as variable");
                comp.error('e', decl, "here is the definition of ", "");
            }
        }
        decl = Declaration.followAliases(decl);
        if (decl != null) {
            if (required != ProcedureInCallContext.INSTANCE)
                decl.setCanRead(true);
            else {
                decl.setCanCall(true);
                // Avoid tricky optimization if we're interpreting.
                if (! comp.mustCompile)
                    decl.setCanRead();
            }
            Declaration ctx = exp.contextDecl();
            if (ctx != null)
                ctx.setCanRead(true);
        }
        return super.visitReferenceExp(exp, required);
    }

    protected Expression visitIfExp(IfExp exp, Type required) {
        Expression test = exp.test.visit(this, ValueNeededType.instance);
        if (test instanceof ReferenceExp) {
            Declaration decl = ((ReferenceExp) test).getBinding();
            if (decl != null)
            {
                Expression value = decl.getValue();
                if (value instanceof QuoteExp && value != QuoteExp.undefined_exp)
                    test = value;
            }
        }
        exp.test = test;
        VarValueTracker.forkPush(this);
        if (exitValue == null)
            exp.then_clause = visit(exp.then_clause, required);
        valueTracker.forkNext();
        if (exitValue == null && exp.else_clause != null)
            exp.else_clause = visit(exp.else_clause, required);
        VarValueTracker.forkPop(this);
        // truth: 1 - test is true; 0: test is false; -1 - test is unknown.
        int truth = ! (test instanceof QuoteExp) ? -1
            : comp.getLanguage().isTrue(((QuoteExp) test).getValue()) ? 1 : 0;
        if (exp.else_clause == null && truth <= 0
            && required instanceof ValueNeededType) {
            if (comp.warnVoidUsed())
                comp.error('w', "missing else where value is required", exp);
            if (truth == 0)
                return QuoteExp.voidObjectExp;
        }
        if (truth >= 0)
            return exp.select(truth != 0);
        if (test.getType().isVoid()) {
            boolean voidTrue = comp.getLanguage().isTrue(Values.empty);

            if (comp.warnVoidUsed())
                comp.error('w', "void-valued condition is always "+(truth!=0));
            return new BeginExp(test, exp.select(voidTrue));
        }
        return exp;
    }

    protected Expression visitBeginExp(BeginExp exp, Type required) {
        int last = exp.length - 1;
        for (int i = 0;  i <= last;  i++) {
            exp.exps[i] = visit(exp.exps[i], i < last ? null : required);
        }
        return exp;
    }

    protected Expression visitCaseExp(CaseExp exp, Type required) {
        Expression key = exp.key.visit(this, ValueNeededType.instance);
        
        // Inline the key when it is a ReferenceExp bound
        // to a known value (a QuoteExp).
        if (key instanceof ReferenceExp) {
            Declaration decl = ((ReferenceExp) key).getBinding();
            if (decl != null) {
                Expression value = decl.getValue();
                if (value instanceof QuoteExp
                        && value != QuoteExp.undefined_exp)
                    key = value;
            }
        }
        exp.key = key;

        // replaces a case containing only the default case
        if (exp.clauses.length == 0) {
            return new BeginExp(key, visit(exp.elseClause.exp, required));
        }

        // type checking for datums
        Expression lastIncomp = null;
        int incomps = 0;
        for (int i = 0; i < exp.clauses.length; i++) {
            for (int j = 0; j < exp.clauses[i].datums.length; j++) {
                Expression dexp = exp.clauses[i].datums[j];
                Object d = ((QuoteExp) dexp).getValue();
                if (d instanceof ConstVector
                        || (!(d instanceof EmptyList) && d instanceof PairWithPosition)) {
                    comp.error('w', "List and vectors will never be matched in a case clause", dexp);
                }
                if (key.getType().isCompatibleWithValue(dexp.getType()) == -1){
                    if (incomps < 2)
                        comp.error('w', "datum type incompatible with the key", dexp);
                    else if (incomps == 2)
                        lastIncomp = dexp;
                    incomps++;
                }
            }
        }
        // if more than 2 datums are incompatible we report a summary
        if (incomps > 2)
            comp.error('w', "there are " + (incomps - 2)
                        + " more datums that are incompatible with the key", lastIncomp);

        VarValueTracker.forkPush(this);
        if (exitValue == null) {
            exp.clauses[0].exp = visit(exp.clauses[0].exp, required);
            for (int i = 1; i < exp.clauses.length; i++) {
                if (exitValue == null) {
                    valueTracker.forkNext();
                    exp.clauses[i].exp = visit(exp.clauses[i].exp, required);
                }
            }
        }
        if (exitValue == null && exp.elseClause != null) {
            valueTracker.forkNext();
            exp.elseClause.exp = visit(exp.elseClause.exp, required);
        }
        VarValueTracker.forkPop(this);

        boolean isKeyKnown = key instanceof QuoteExp;

        Object keyValue = isKeyKnown ? ((QuoteExp) key).getValue() : null;

        if (exp.elseClause == null && required instanceof ValueNeededType) {
            boolean missing = !isKeyKnown || !exp.searchValue(keyValue);
            if (missing) {
                if (comp.warnVoidUsed())
                    comp.error('w', "missing else where value is required", exp);
            }

            if (isKeyKnown && missing) {
                return QuoteExp.voidObjectExp;
            }
        }

        // When the key is know at compile time, search a matching
        // datum and return the corresponding expression.
        if (isKeyKnown) {
            Expression e = exp.selectCase(keyValue);
            return (e != null) ? e : QuoteExp.voidObjectExp;
        }

        if (key.getType().isVoid()) {
            return new BeginExp(key,
                    exp.selectCase(QuoteExp.voidExp.getValue()));
        }
        return exp;
    }

    protected Expression visitScopeExp(ScopeExp exp, Type required) {
        exp.visitChildren(this, null);
        visitDeclarationTypes(exp);
        for (Declaration decl = exp.firstDecl();  decl != null;
             decl = decl.nextDecl()) {
            if (decl.type == null) {
                Expression val = decl.getValue();
                decl.type = Type.objectType;
                decl.setType(val != null && val != QuoteExp.undefined_exp
                             ? val.getType()
                             : Type.objectType);
            }
            visitAnnotations(decl);
        }
        return exp;
    }

    /** Visit any named functions that haven't been visit yet.
     * This should be called at the end of a LetExp or ModuleExp.
     */
    protected void visitRemainingDeclaredLambdas(ScopeExp exp) {
        for (Declaration decl = exp.firstDecl(); decl != null;
             decl = decl.nextDecl()) {
            Expression value = decl.getValueRaw();
            if (value instanceof LambdaExp && ! decl.isModuleLocal())
                visit(value, null);
        }
        for (Declaration decl = exp.firstDecl(); decl != null;
             decl = decl.nextDecl()) {
            Expression value = decl.getValueRaw();
            if (value instanceof LambdaExp
                && ! value.getFlag(Expression.VALIDATED)
                && decl.isModuleLocal()
                && comp.warnUnused())
                comp.error('w', decl, "no use of ", "");
        }
    }

    protected Expression visitModuleExp(ModuleExp exp, Type required) {
        LambdaExp saveLambda = currentLambda;
        currentLambda = exp;
        try {
            super.visitModuleExp(exp, required);
        } finally {
            currentLambda = saveLambda;
        }
        visitRemainingDeclaredLambdas(exp);
        return exp;
    }

    protected Expression visitLetExp (LetExp exp, Type required)
    {
        if (! (exp instanceof CatchClause) && ! (exp instanceof FluidLetExp))
        {
            for (Declaration decl = exp.firstDecl();  decl != null;
                 decl = decl.nextDecl())
            {
                Expression init = decl.getInitValue();
                if (init == QuoteExp.undefined_exp
                    && decl.getValueRaw() instanceof LambdaExp)
                    valueTracker.noteSet(decl, IntNum.make(~0));
                else
                    valueTracker.noteUnitialized(decl);
            }
        }

        for (Declaration decl = exp.firstDecl(); decl != null;
             decl = decl.nextDecl()) {
            Expression init = decl.getInitValue();
            if (decl.nvalues > 0
                && decl.values[0].kind == Declaration.ValueSource.LET_INIT_KIND
                && decl.values[0].base == exp) {
                valueTracker.noteSet(decl, IntNum.make(~0));
            }
            boolean typeSpecified = decl.getFlag(Declaration.TYPE_SPECIFIED);
            Type dtype = typeSpecified && init != QuoteExp.undefined_exp ? decl.getType() : null;
            if (deferableInit(init) && decl.getValueRaw() == init)
                ; // defer
            else
                init = visit(init, ValueNeededType.make(dtype));
            decl.setInitValue(init);
        }

        if (exitValue == null)
            exp.body = visit(exp.body, required);
        if (exp.body instanceof ReferenceExp) {
            ReferenceExp ref = (ReferenceExp) exp.body;
            Declaration d = ref.getBinding();
            if (d != null && d.context == exp && ! ref.getDontDereference()) {
                if (exp.firstDecl() == d && d.nextDecl() == null) // Single decl
                {
                    Expression init = d.getInitValue();
                    Expression texp = d.getTypeExp();
                    // Note this optimization does yield worse error messages
                    // than using CheckedTarget.  FIXME.
                    if (texp != QuoteExp.classObjectExp)
                        init = visitApplyOnly(Compilation.makeCoercion(init, texp), null);
                    return init;
                }
                // Can also optimize if n > 1, but have to check if any
                // other inits can cause side-effects.  Probably not worth it.
            }
        }
        visitRemainingDeclaredLambdas(exp);
        return exp;
    }

    protected boolean deferableInit(Expression init) {
        if (init instanceof LambdaExp)
            return ! (init instanceof ClassExp);
        if (init instanceof ApplyExp) {
            Object fun = ((ApplyExp) init).getFunctionValue();
            if (fun == MakePromise.makeDelay || fun == MakePromise.makeLazy)
                return true;
        }
        return false;
    }

    protected Expression visitFluidLetExp(FluidLetExp exp, Type required) {
        for (Declaration decl = exp.firstDecl();
             decl != null; decl = decl.nextDecl()) {
            decl.setCanRead(true);
            if (decl.base != null)
                decl.base.setCanRead(true);
        }
        return super.visitFluidLetExp(exp, required);
    }

    protected Expression visitLambdaExp(LambdaExp exp, Type required) {
        setCanAccess(exp, required);
        if (exp.getCallConvention() == Compilation.CALL_WITH_UNSPECIFIED)
            exp.setCallConvention(getCompilation());
        Declaration firstDecl = exp.firstDecl();
        if (firstDecl != null && firstDecl.isThisParameter()
            && ! exp.isClassMethod() && firstDecl.type == null) {
            firstDecl.setType(comp.mainClass);
        }
        if (exp.getClass() == LambdaExp.class) {
            if (exp.canFinishCondition != CanFinishMap.CAN_FINISH
                && exp.canFinishCondition != null) {
                exp.setReturnType(Type.neverReturnsType);
            }
            Declaration ldecl = exp.nameDecl;
            boolean unknownCalls = true;
            if (ldecl != null && ! exp.isClassMethod() && ldecl.isModuleLocal()) {
                int countApply = 0;
                for (ApplyExp app = ldecl.firstCall; app != null;
                     app = app.nextCall)
                    countApply++;
                if (countApply == ldecl.numReferences
                    // Data-flow from calls to a non-inlined module-level function
                    // isn't wrong, but it can lead to problems in captured
                    // variables if the actual argument is an inlined lambda,
                    // We don't implement the necessary re-writing.
                    && ! Compilation.avoidInline(exp)) {
                    // Some preliminary data-flow from a set of known call sites.
                    // This isn't fully implemented yet.
                    unknownCalls = false;
                    for (ApplyExp app = ldecl.firstCall; app != null;
                         app = app.nextCall) {
                        Expression func = app.getFunction();
                        int nargs = app.getArgCount();
                        Declaration p = firstDecl;
                        if (p != null && p.isThisParameter())
                            p = p.nextDecl();
                        for (int i = 0; p != null && i < exp.min_args;
                             p = p.nextDecl(), i++) {
                            if (! p.hasUnknownValue())
                                p.noteValueFromApply(app, i);
                        }
                    }
                }
            }
            if (unknownCalls) {
                for (Declaration p = firstDecl; p != null;  p = p.nextDecl()) {
                    if (! p.isThisParameter())
                        p.noteValueUnknown();
                }
            }
        }
        LambdaExp saveLambda = currentLambda;
        currentLambda = exp;
        try {
            visitScopeExp(exp, required);
        } finally {
            currentLambda = saveLambda;
        }
        if (exp.isClassMethod() && "*init*".equals(exp.getName())) {
            Expression bodyFirst = exp.getBodyFirstExpression();
            ClassType calledInit = exp.checkForInitCall(bodyFirst);
            ClassExp cexp = (ClassExp) exp.getOuter();
            ClassType superClass = cexp.instanceType.getSuperclass();
            if (calledInit != null) {
                if (calledInit != cexp.instanceType && calledInit != superClass)
                    comp.error('e', "call to <init> for not this or super class");
            } else if (superClass != null) {
                cexp.checkDefaultSuperConstructor(superClass, comp);
            }
        }
        return exp;
    }

    public void visitDefaultArgs (LambdaExp exp, Type required) {
        for (Declaration p = exp.firstDecl(); p != null; p = p.nextDecl()) {
            Expression init = p.getInitValue();
            if (init != null)
                p.setInitValue(visitAndUpdate(init, p.getType()));
        }
    } 

    protected Expression visitClassExp (ClassExp exp, Type required) {
        Expression result = super.visitClassExp(exp, required);
        if (! exp.explicitInit && ! exp.instanceType.isInterface())
            exp.checkDefaultSuperConstructor(exp.instanceType.getSuperclass(), comp);
        return result;
    }

    protected Expression visitTryExp (TryExp exp, Type required) {
        if (exp.getCatchClauses() == null && exp.getFinallyClause() == null)
            return visit(exp.try_clause, required);

        VarValueTracker.forkPush(this);
        exp.try_clause = exp.try_clause.visit(this, required);
        for (CatchClause clause = exp.catch_clauses;
             clause != null; clause = clause.getNext())  {
            valueTracker.forkNext();
            clause.visit(this, required); // FIXME update?
        }
        // It is possible none of the try_clause or catch_clauses are executed
        // before the finally_clause is executed, so treat as an empty branch.
        if (exp.finally_clause != null)
            valueTracker.forkNext();
        VarValueTracker.forkPop(this);
        if (exp.finally_clause != null)
            exp.finally_clause = exp.finally_clause.visit(this, null);
        return exp;
    }

    boolean processingAnnotations;
    /** If currently processing an annotation belonging to a declaration.
     * In this case expressions must resolve to constants,
     * annotations must resolve to know annotation types.
     */
    public boolean processingAnnotations () { return processingAnnotations; }

    protected void visitAnnotations(Declaration decl) {
        List<Expression> annotations = decl.annotations;
        if (annotations != null) {
            boolean saveProcessingAnnotations = processingAnnotations;
            processingAnnotations = true;
            try {
                int num = annotations.size();
                for (int i = 0;  i < num;  i++) {
                    Expression before = annotations.get(i);
                    Expression ann = visit(before, null);
                    Object aval = ann.valueIfConstant();
                    if (aval instanceof Proxy
                        && ((aval = Proxy.getInvocationHandler(aval))
                            instanceof AnnotationEntry)) {
                        AnnotationEntry ae = (AnnotationEntry) aval;
                        if (decl.isClassMethod() && !ae.hasTarget(ElementType.METHOD))
                            comp.error('e', "annotation "+ae.getAnnotationType().getName()+" allowed on methods", before);
                        if (decl.isClassField() && !ae.hasTarget(ElementType.FIELD))
                            comp.error('e', "annotation "+ae.getAnnotationType().getName()+" not allowed on fields", before);
                        if (decl.getValue() instanceof ClassExp
                            && !ae.hasTarget(ElementType.TYPE)
                            && !ae.hasTarget(ElementType.FIELD))
                            comp.error('e', "annotation "+ae.getAnnotationType().getName()+" not allowed on classes", before);
                    }
                    annotations.set(i, ann);
                }
            } finally {
                processingAnnotations = saveProcessingAnnotations;
            }
        }
    }

    protected Expression visitSetExp(SetExp exp, Type required) {
        Declaration decl = exp.getBinding();
        if (decl != null && decl.values != Declaration.unknownValueValues
            && exp.valueIndex >= 0) {
            IntNum setterMask = IntNum.make(~exp.valueIndex);
            valueTracker.noteSet(decl, setterMask);
        }
        if (decl != null && decl.getValueRaw() == exp.new_value
            && deferableInit(exp.new_value))
            ; // defer
        else {
            Type dtype = decl == null || decl.isAlias() ? null : decl.type;
            exp.new_value = visit(exp.new_value, ValueNeededType.make(dtype));
        }
        if (! exp.isDefining() && decl != null && decl.isClassMethod())
            comp.error('e', "can't assign to method "+decl.getName(), exp);
        if (decl != null && decl.getFlag(Declaration.TYPE_SPECIFIED)) {
            if (CompileReflect.checkKnownClass(decl.getType(), comp) < 0)
                decl.setType(Type.errorType);
        }
        /*
        if (decl != null && ! decl.getFlag(Declaration.TYPE_SPECIFIED)) {
            // This is a kludge to handle the a #!rest parameter that
           // is implicitly declared to be a Scheme <list>, but may be
           // assinged some other value, which is a legal Scheme idiom.
           // We could set implicitly set the parameter type to <list>,
            // but doing so improves type inference in the common case.
            Type declType = decl.getType();
            if (declType != null && ! exp.new_value.getType().isSubtype(declType))
	    decl.setType(Type.pointer_type);
        }
        */
        Declaration ctx = exp.contextDecl();
        if (ctx != null)
            ctx.setCanRead(true);
        return exp;
    }

    /* #ifdef use:java.lang.invoke */
    static final MethodType inlinerMethodType =
        MethodType.methodType(gnu.expr.Expression.class,
                              gnu.expr.ApplyExp.class,
                              gnu.expr.InlineCalls.class,
                              gnu.bytecode.Type.class,
                              gnu.mapping.Procedure.class);
    /* #else */
    // private static final Class[] inlinerMethodType =
    //     new Class[] { gnu.expr.ApplyExp.class,
    //                   gnu.expr.InlineCalls.class,
    //                   gnu.bytecode.Type.class,
    //                   gnu.mapping.Procedure.class };
    /* #endif */

    static
        /* #ifdef use:java.lang.invoke */
        MethodHandle resolveInliner(Procedure proc, String inliner,
                                    MethodType mtype)
        /* #else */
        // java.lang.reflect.Method resolveInliner(Procedure proc, String inliner,
        //                                         Class[] mtype)
        /* #endif */
        
        throws Throwable {
        int colon = inliner.indexOf(':');
        if (colon > 0) {
            String cname = inliner.substring(0, colon);
            String mname = inliner.substring(colon+1);
            Class clas = Class.forName(cname, true, proc.getClass().getClassLoader());
            /* #ifdef use:java.lang.invoke */
            return MethodHandles.lookup().findStatic(clas, mname, mtype);
            /* #else */
            // return clas.getDeclaredMethod(mname, mtype);
            /* #endif */
        }
        return null;
    }

    public Expression maybeInline(ApplyExp exp, Type required, Procedure proc) {
        try {
            Object inliner;
            synchronized (proc) {
                inliner = proc.getProperty(Procedure.validateXApplyKey, null);
                if (inliner == null && exp.firstSpliceArg < 0)
                    inliner = proc.getProperty(Procedure.validateApplyKey, null);
                if (inliner instanceof String) {
                    inliner = resolveInliner(proc, (String) inliner,
                                             inlinerMethodType);
                    if (inliner == null) {
                        error('e', "inliner property string for "+proc+" is not of the form CLASS:METHOD");
                        return null;
                    }
                }
            } /* end synchronized */
            if (inliner != null) {
                /* #ifdef use:java.lang.invoke */
                if (inliner instanceof MethodHandle)
                    return (Expression) ((MethodHandle) inliner).invokeExact(exp, this, required, proc);
                /* #endif */
                Object[] vargs = new Object[] { exp, this, required, proc };
                if (inliner instanceof Procedure)
                    return (Expression) ((Procedure) inliner).applyN(vargs);
                /* #ifndef use:java.lang.invoke */
                // else if (inliner instanceof java.lang.reflect.Method)
                //   return (Expression) ((java.lang.reflect.Method) inliner)
                //     .invoke(null, vargs);
                /* #endif */
            }
        } catch (Error ex) {
            throw ex;
        } catch (Throwable ex) {
            if (ex instanceof InvocationTargetException)
                ex = ((InvocationTargetException) ex).getTargetException();
            messages.error('e',
                           "caught exception in inliner for "+proc+" - "+ex, ex);
        }
        return null;
    }

    /** Attempt to inline a function call.
     * @param lexp function to inline
     * @param args list of actual arguments of function call
     * @param makeCopy true if the body of lexp should of copied; false
     *   if we can re-use lexp because it is no longer needed.
     * @return the inlined expression (a LetExp), or null if we
     *   weren't able to inline.
     */
    public static Expression inlineCall(LambdaExp lexp, Expression[] args,
                                        boolean makeCopy) {
        if (lexp.keywords != null)
            return null;
        boolean varArgs = lexp.max_args < 0;
        int fixed = lexp.min_args;
        if ((fixed == lexp.max_args
             && fixed == args.length)
            || (varArgs && args.length >= fixed)) {
            Declaration prev = null;
            IdentityHashTable mapper;
            Expression[] cargs;
            if (makeCopy) {
                mapper = new IdentityHashTable();
                cargs = Expression.deepCopy(args, mapper);
                if (cargs == null && args != null)
                    return null;
            } else {
                mapper = null;
                cargs = args;
            }
            if (varArgs) {
                cargs = new Expression[fixed+1];
                // Copy over fixed arguments.
                System.arraycopy(args, 0, cargs, 0, fixed);
                // Create list/array constructor for rest args.
                Expression[] xargs = new Expression[args.length-fixed+1];
                Declaration restArg = lexp.firstDecl();
                for (int i = fixed;  --i >= 0; )
                    restArg = restArg.nextDecl();
                xargs[0] = QuoteExp.getInstance(restArg.type);
                // Copy over rest args.
                System.arraycopy(args, fixed, xargs, 1, args.length-fixed);
                cargs[fixed] = new ApplyExp(Invoke.make, xargs);
            }
            int i = 0;
            LetExp let = new LetExp();
            for (Declaration param = lexp.firstDecl(); param != null; i++) {
                Declaration next = param.nextDecl();
                param.setInitValue(cargs[i]);
                if (makeCopy) {
                    Declaration ldecl =
                        let.addDeclaration(param.symbol, param.type);
                    if (param.typeExp != null) {
                        ldecl.typeExp = Expression.deepCopy(param.typeExp);
                        if (ldecl.typeExp == null)
                            return null;
                    
                    }
                    mapper.put(param, ldecl);
                } else {
                    lexp.remove(prev, param);
                    let.add(prev, param);
                }
                if ( ! param.getCanWrite()) {
                    param.nvalues = 0;
                    param.values = null;
                }
                param.noteValueFromLet(let);
                prev = param;
                param = next;
            }
            Expression body = lexp.body;
            if (makeCopy) {
                body = Expression.deepCopy(body, mapper);
                if (body == null && lexp.body != null)
                    return null;
            }
            let.body = body;
            lexp.body = null;
            lexp.setFlag(Expression.VALIDATED);
            lexp.setInlineOnly(true);
            return let;
        }
        /*
        if (lambda.min_args == 0 && lambda.max_args == -1)
        {
            Declaration pargs = lambda.firstDecl();
            Expression[] cargs = Expression.deepCopy(args, mapper);
            Declaration largs = new Declaration
                IdentityHashTable mapper = new IdentityHashTable();
            LetExp let = new LetExp();
            return let;
        }
        if (lambda.min_args != lambda.max_args)
        {
            // FUTURE
        }
        */
        return null;
    }

    /** New helper Type class, used for "lenient" conversions. */
    public static class LenientExpectedType extends Type {
        Type base;
        
        public static LenientExpectedType make(Type type) {
            return new LenientExpectedType(type);
        }

        LenientExpectedType(Type type) {
            super(type);
            base = type;
        }

        @Override
        public int compare(Type other) {
            return this == other ? 0 : -3;
        }

        @Override
        public Object coerceFromObject (Object obj) {
            return obj;
        }

        @Override
        public int isCompatibleWithValue(Type valueType) {
            if (base.getRawType().equals(base.getRawType()))
                return 1;
            return base.isCompatibleWithValue(valueType);
        }

        @Override
        public String toString() {
            return "LenientExpectedType["+base+']';
        }
    }

    public static class ProcedureInCallContext extends ObjectType {
        public static final ProcedureInCallContext INSTANCE = new ProcedureInCallContext();

        ProcedureInCallContext() {
            super("procedure-in-call-context");
        }

        public Type getImplementationType() {
            return Compilation.typeProcedure;
        }

        public int compare(Type other) {
            return getImplementationType().compare(other.getImplementationType());
        }
    }

    /** A marker type to indicate that void is invalid.
     * Only used as the required type, in e.g. rhs of assignment.
     */
    public static class ValueNeededType extends ObjectType {
        static final ValueNeededType instance
            = new ValueNeededType(null);

        Type actualType;

        ValueNeededType(Type actualType) {
            super("value-needed-type:"+actualType);
            this.actualType = actualType;
        }

        public static Type make(Type type) {
            if (type == null)
                return instance;
            if (type instanceof ValueNeededType || type == Type.objectType)
                return type;
            /* FUTURE not support by code yet
               return new ValueNeededType(type);
            */
            return type;
        }

        public Type getImplementationType() {
            return actualType;
        }

        public int compare(Type other) {
            return other.isVoid() ? -1 : 1;
        }
    }
}
