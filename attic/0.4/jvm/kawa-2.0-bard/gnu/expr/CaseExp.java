package gnu.expr;

import java.util.ArrayList;
import java.util.HashMap;

import gnu.bytecode.ClassType;
import gnu.bytecode.CodeAttr;
import gnu.bytecode.Label;
import gnu.bytecode.Method;
import gnu.bytecode.SwitchState;
import gnu.bytecode.Type;
import gnu.expr.Compilation;
import gnu.expr.ExpVisitor;
import gnu.expr.Expression;
import gnu.expr.Language;
import gnu.expr.StackTarget;
import gnu.expr.Target;
import gnu.kawa.functions.IsEqv;
import gnu.kawa.io.OutPort;
import gnu.kawa.lispexpr.LangObjType;
import gnu.kawa.lispexpr.LangPrimType;
import gnu.lists.ConstVector;
import gnu.lists.EmptyList;
import gnu.lists.PairWithPosition;
import gnu.mapping.CallContext;
import gnu.math.IntNum;
import gnu.text.Char;

/**
 * This class represents a case expression <blockquote>
 * 
 * <pre>
 * case case-key case-clause+
 * case case-key case-clause* case-else-clause
 *     case-key ::= expression
 *     case-clause ::= ((datum*) expression+)
 *        | ((datum*) => expression)
 *     case-else-clause ::= (else  expression+)
 *        | (else => expression)
 * </pre>
 * 
 * </blockquote>
 */
public class CaseExp extends Expression {

    /** 
     * This class represent a clause of the case expression,
     * of the form ((datum*) expression+) or (else expression+)*/
    public static class CaseClause {

        Expression[] datums;

        Expression exp;

        /** Constructor for an else clause of the form (else expression+) */
        public CaseClause(Expression exp) {
            this.datums = null;
            this.exp = exp;
        }

        /** Constructor for a clause of the form ((datum*) expression+) */
        public CaseClause(Expression[] datums, Expression exp) {
            this.datums = datums;
            this.exp = exp;
        }

    }

    /** The key of the case expression.*/
    Expression key;

    /** Clauses of the else expression.*/
    CaseClause[] clauses;

    /** Default clause of the else expression.*/
    CaseClause elseClause;

    /**
     * Constructor for a case expression with no else clause.
     * Arguments must be non null. clauses parameter must contain
     * at least one CaseClause.
     */
    public CaseExp(Expression key, CaseClause[] clauses) {
        this.key = key;
        this.clauses = clauses;
        this.elseClause = null;
        if (key == null || clauses == null || clauses.length == 0)
            throw new IllegalArgumentException(
                    "CaseExp constructor called with null arguments");
    }

    /**
     * Constructor for a case expression with an else clause.
     * Arguments must be non null.
     */
    public CaseExp(Expression key, CaseClause[] clauses, CaseClause elseClause) {
        this.key = key;
        this.clauses = clauses;
        this.elseClause = elseClause;
        if (key == null || clauses == null || elseClause == null)
            throw new IllegalArgumentException(
                    "CaseExp constructor called with null arguments");
    }

    @Override
    protected boolean mustCompile() {
        return false;
    }

    @Override
    public void apply(CallContext ctx) throws Throwable {
        Expression e = selectCase(key.eval(ctx));
        if (e != null)
            e.apply(ctx);
        else
            QuoteExp.voidExp.apply(ctx);
    }

    @Override
    public void print(OutPort out) {
        out.startLogicalBlock("(Case ", false, ")");
        out.setIndentation(-2, false);
        key.print(out);
        for (int i = 0; i < clauses.length; i++) {
            out.writeSpaceLinear();
            Expression[] datums = clauses[i].datums;
            Expression exp = clauses[i].exp;
            out.startLogicalBlock("(", false, ")");
            out.startLogicalBlock("(", false, ")");
            for (int j = 0; j < datums.length; j++) {
                if (j > 0)
                    out.print(' ');
                out.print(((QuoteExp) datums[j]).getValue());
            }
            out.endLogicalBlock(")");
            out.writeSpaceLinear();
            exp.print(out);
            out.endLogicalBlock(")");
        }
        if (elseClause != null) {
            out.writeSpaceLinear();
            out.startLogicalBlock("(else ", false, ")");
            elseClause.exp.print(out);
            out.endLogicalBlock(")");
        }
        out.endLogicalBlock(")");
    }

    static Method isEqvMethod = ClassType.make("gnu.kawa.functions.IsEqv")
            .getDeclaredStaticMethod("apply", 2);
    static Method hashCodeMethod = Type.objectType.getDeclaredMethod("hashCode", 0);

    @Override
    public void compile(Compilation comp, Target target) {

        CodeAttr code = comp.getCode();

        compileKey(comp);

        if (!code.reachableHere())
            return;

        // check if we are dealing only with integers
        // (intended as int, not long), or only with chars
        boolean integer =
            ((key.getType() == Type.intType
              && calculateDatumsType() == Type.intType)
             || ((key.getType() == LangPrimType.characterType
                  || key.getType() == LangPrimType.charType)
                 && calculateDatumsType() == LangPrimType.characterType));

        // Map that contains, for each integer hash (the key),
        // an array of pairs, each pair consisting of a datum
        // and an expression. All the datums in the array have
        // the same hash code.
        HashMap<Integer, ArrayList<Object>> hashToClauseMap 
            = new HashMap<Integer, ArrayList<Object>>();

        for (int i = 0; i < clauses.length; i++) {
            Expression e = clauses[i].exp;
            for (int j = 0; j < clauses[i].datums.length; j++) {
                Expression dexp = clauses[i].datums[j];
                Object d = calculateDatumValue(dexp);

                if (!integer && d instanceof ConstVector
                        || (!(d instanceof EmptyList) && d instanceof PairWithPosition)) {
                    continue;
                }
                int hash = d.hashCode();
                ArrayList<Object> a = hashToClauseMap.get(hash);
                if (a == null) {
                    a = new ArrayList<Object>();
                    hashToClauseMap.put(hash, a);
                }
                a.add(d);
                a.add(e);
            }
        }
        
        // Map that contains, for each expression (the key),
        // a label, that will be defined immediately before
        // compiling the expression. 
        HashMap<Expression, Label> expToLabelMap 
            = new HashMap<Expression, Label>();

        SwitchState sw = code.startSwitch();
        Label before_label = new Label();
        before_label.setTypes(code);
        Label defaultl = new Label();

        for (int h : hashToClauseMap.keySet()) {
            Label label = new Label(code);

            // when we are dealing with the conditionals
            // needed for collision detection, we have to
            // merge the stack maps, otherwise we get verify errors
            if (!integer) label.setTypes(code);

            label.setTypes(before_label);
            label.define(code);
            sw.insertCase(h, label, code);
            ArrayList<Object> dwes = hashToClauseMap.get(h);
            Object datum;
            for (int i = 0; i < dwes.size(); i = i + 2) {

                datum = dwes.get(i);
                Expression exp = (Expression) dwes.get(i + 1);

                // if integer is true we don't need to handle collisions,
                // so we can avoid generating a comparison between the key
                // and the current datum
                if (!integer) {
                    // collision handling

                    // check if we are dealing only with integers,
                    // in that case we optimize, avoiding boxing
                    if ((key.getType() == Type.intType || key.getType() == Type.longType)
                            && datum instanceof IntNum) {
                        IntNum idatum = (IntNum) datum;
                        key.compile(comp, key.getType());
                        // check if the datum is an int otherwise is a long
                        if (idatum.inIntRange() && key.getType() == Type.intType) {
                            int val = idatum.intValue();
                            code.emitPushInt(val);
                        } else {
                            StackTarget st = new StackTarget(Type.longType);
                            st.compileFromStack(comp, key.getType());
                            long val = idatum.longValue();
                            code.emitPushLong(val);
                        }
                        code.emitIfEq();
                    } else if((key.getType() == LangPrimType.charType
                               || key.getType() == LangPrimType.characterType)
                              && datum instanceof Char) {
                        // case in which we are comparing two characters
                        key.compile(comp, Type.intType);
                        int val = ((Char) datum).intValue();
                        code.emitPushInt(val);
                        code.emitIfEq();
                    } else {
                        // general case, comparing to objects using IsEqv
                        key.compile(comp, Type.objectType);
                        comp.compileConstant(datum, Target.pushObject);
                        code.emitInvokeStatic(isEqvMethod);
                        code.emitIfIntNotZero();
                    }
                }
                // check if the expression has been compiled yet,
                // if not, define a label and compile it, then store
                // both in the expToLabelMap.
                // When the same expression has been already compiled
                // we don't need to generate its bytecode again, we
                // can jump directly to the associated label, because
                // here each compiled expression must be followed by 
                // an exitSwitch (a jump to the code after the case).
                Label expLabel = expToLabelMap.get(exp);
                if (expLabel != null) {
                    code.emitGoto(expLabel);
                } else {
                    expLabel = new Label(code);
                    expToLabelMap.put(exp, expLabel);
                    expLabel.define(code);
                    exp.compile(comp, target);
                    sw.exitSwitch(code);
                }
                if (!integer) code.emitFi();
            }
            // if this point is reached and we are handling 
            // the general case, all the comparisons failed 
            // and we must jump to the default case
            if (!integer)
                code.emitGoto(defaultl);
        }

        sw.addDefault(code);
        defaultl.define(code);
        if (elseClause != null)
            elseClause.exp.compile(comp, target);
        else
            QuoteExp.voidExp.compile(comp, target);
        sw.finish(code);
    }

    /** 
     * Compiles the key of the case expression,
     * then computes the hash code, optimizing
     * according to the type.
     */
    private void compileKey(Compilation comp) {

        CodeAttr code = comp.getCode();

        if (key.getType() == Type.intType
            || key.getType() == Type.shortType
            || key.getType() == Type.byteType
            || key.getType() == LangPrimType.charType
            || key.getType() == LangPrimType.characterType) {
            // hasCode for an int is the int itself
            key.compile(comp, Type.intType);
        } else if (key.getType() == Type.longType) {
            // hashCode function for longs, inlined
            key.compile(comp, Type.longType);
            key.compile(comp, Type.longType);
            code.emitPushInt(32);
            code.emitShr();
            code.emitXOr();
            StackTarget st = new StackTarget(Type.intType);
            st.compileFromStack(comp, Type.longType);
        } else {
            // hashCode computed using hashCode method
            // of java.lang.Object
            key.compile(comp, Type.objectType);
            code.emitInvokeVirtual(hashCodeMethod);
        }
    }

    @Override
    protected <R, D> R visit(ExpVisitor<R, D> visitor, D d) {
        return visitor.visitCaseExp(this, d);
    }

    @Override
    protected <R, D> void visitChildren(ExpVisitor<R, D> visitor, D d) {
        for (int i = 0; visitor.exitValue == null && i < clauses.length; i++) {
            CaseClause clause = clauses[i];
            visitor.visitAndUpdate(clause.exp, d);
        }

        if (visitor.exitValue == null && elseClause != null)
            visitor.visitAndUpdate(elseClause.exp, d);
    }

    /** 
     * Given the expression generated from a datum
     * returns the value of the datum as an Object.
     */
    protected Object calculateDatumValue(Expression datum){
        if(datum instanceof QuoteExp)
            return ((QuoteExp) datum).value;
        if(datum instanceof ReferenceExp)
            return ((ReferenceExp) datum).getSymbol();
        throw new Error("Invalid Datum");
    }

    @Override
    protected Type calculateType() {

        Type t;
        CaseClause clause = clauses.length > 0 ? clauses[0] : null;
        if (clause != null) {
            t = clause.exp.getType();

            for (int i = 1; i < clauses.length; i++) {
                clause = clauses[i];
                t = Language.unionType(t, clause.exp.getType());
            }

            t = (elseClause != null) ?
                    Language.unionType(t, elseClause.exp.getType()) :
                    Language.unionType(t, Type.voidType);

        } else if (elseClause != null)
            t = elseClause.exp.getType();
        else
            throw new Error(
                    "Syntax Error: Case without any clause, at least a default clause is required");

        return t;
    }

    /**
     * Computes the union type of the case expression datums. 
     * Useful to know if the datums are all integer values.
     */
    protected Type calculateDatumsType() {
        Type t;

        boolean atLeastOne = clauses.length > 0 ? true : false;
        if (atLeastOne) {
            t = calculateDatumType(clauses[0].datums);

            for (int i = 1; i < clauses.length; i++) {
                t = Language
                        .unionType(t, calculateDatumType(clauses[i].datums));
            }

        } else if (elseClause != null)
            return Type.voidType;
        else
            throw new Error();
        return t;
    }

    /**
     * Computes the union type of the datums of a specified 
     * datum array.
     */
    private Type calculateDatumType(Expression[] datum) {
        Type t;
        t = resolveType(calculateDatumValue(datum[0]));
        for (int i = 1; i < datum.length; i++) {
            t = Language.unionType(t, resolveType(calculateDatumValue(datum[i])));
        }

        return t;
    }

    /**
     * Determines if the passed object is an IntNum, 
     * in that case decides if it is an int or a long. 
     * When the object is not integer, returns the Type
     * associated with the object class.
     */
    private Type resolveType(Object o) {
        if (o instanceof IntNum) {
            IntNum ii = (IntNum) o;
            if (ii.inIntRange())
                return Type.intType;
            else if (ii.inLongRange())
                return Type.longType;
            else
                return LangObjType.integerType;
        } else if (o instanceof Char)
            return LangPrimType.characterType;
        else if (o instanceof Character)
            return LangPrimType.charType;
        else
            return Type.make(o.getClass());

    }

    /**
     * Search a clause containing the specified key. 
     * If the clause is found returns true, false otherwise.
     */
    public boolean searchValue(Object keyValue) {
        Expression exp = selectCase(keyValue);
        Expression elseExp = (elseClause != null) ? elseClause.exp : null;
        return (exp != null && exp != elseExp);
    }

    /**
     * Search for a clause containing the specified key. 
     * If the clause is found returns the corresponding expression.
     */
    public Expression selectCase(Object keyValue) {

        for (int i = 0; i < clauses.length; i++) {
            Expression[] datums = clauses[i].datums;
            int pos = -1;
            for (int j = 0; j < datums.length; j++) {
                if (IsEqv.apply(keyValue, calculateDatumValue(datums[j])))
                    pos = j;
            }
            if (pos >= 0) {
                return clauses[i].exp;
            }
        }

        return (elseClause != null) ? elseClause.exp : null;
    }

}
