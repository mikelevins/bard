package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

/** Support for custom class-specific compile-time object builders.
 * This class as-is supports compiling the Scheme form:
 * <pre>
 * (TYPE carg1 ... cargK key1: karg1 ... keyL: kargL carg1 ... cargM)
 * </pre>
 * to:
 * <pre>
 * tmp = new TYPE(carg1, ..., cargK);
 * tmp.setKey1(karg1);
 * ...
 * tmp.setKeyM(kargM);
 * tmp.add(carg1);
 * ...
 * tmp.add(cargM);
 * return tmp;
 * </pre>
 * Methods in this class can be overridden in various ways to
 * provide custom mappings.
 * <em>This is a work in progress.</em>
 */

public class CompileBuildObject {
    ApplyExp exp;
    InlineCalls visitor;
    Type required;
    int keywordStart;
    ObjectType ctype;
    ClassType caller;

    public Compilation getCompilation() { return visitor.getCompilation(); }
    public Language getLanguage() { return getCompilation().getLanguage(); }

    public ObjectType getResultType() { return ctype; }
    public boolean resultTypeExtends(ObjectType other) {
        return ctype.isSubtype(other);
    }
    public boolean resultTypeExtends(Class other) {
        return ctype.isSubtype(ClassType.make(other));
    }

    public CompileBuildObject() {
    }

    public Expression getArg(int i) { return exp.getArg(i); }
    public int getArgCount() { return exp.getArgCount(); }
    public void setArg(int i, Expression arg) { exp.setArg(i, arg); }

    /** Insert an expression into the argument list. */
    public void insertArgument(int index, Expression arg) {
        Expression[] args = exp.getArgs();
        Expression[] xargs = new Expression[args.length+1];
        System.arraycopy(args, 0, xargs, 0, index);
        xargs[index] = arg;
        System.arraycopy(args, index, xargs, index+1, args.length-index);
        exp.setArgs(xargs);
        if (keywordStart >= index)
            keywordStart++;
    }

    public static CompileBuildObject make(ApplyExp exp, InlineCalls visitor,
                                          Type required, int keywordStart, ObjectType ctype, ClassType caller) {
        CompileBuildObject builder;
        String builderName = null;
        Compilation comp = visitor.getCompilation();
        Namespace ns = Namespace.valueOfNoCreate("gnu.kawa.reflect/ObjectBuilder");
        if (ns != null) {
            ObjectType btype = ctype;
            for (;;) {
                Symbol sym = ns.lookup(btype.getName());
                if (sym != null) {
                    ModuleExp mexp = comp.getModule();
                    Declaration builderDecl = comp.lookup(sym, Language.VALUE_NAMESPACE);
                    if (builderDecl != null) {
                        Object val = builderDecl.getValue().valueIfConstant();
                        if (val instanceof String)
                            builderName = (String) val;
                    }
                }
                if (! (btype instanceof ClassType))
                    break;
                btype = ((ClassType) btype).getSuperclass();
                if (btype == null || btype == Type.objectType)
                    break;
            }
        }
        if (builderName != null) {
            try {
                builder = (CompileBuildObject) Class.forName(builderName).newInstance();
            }
            catch (Exception ex) {
                comp.error('w', "while creating JavafxObjectBuilder for "+ctype+" - caught "+ex);
                builder = new CompileBuildObject();
            }
        }
        else
            builder = new CompileBuildObject();
        builder.exp = exp;
        builder.visitor = visitor;
        builder.required = required;
        builder.keywordStart = keywordStart;
        builder.ctype = ctype;
        builder.caller = caller;
        return builder;
    }

    boolean defaultConstructorSet;
    PrimProcedure defaultConstructor;

    public void setDefaultConstructor(PrimProcedure proc) {
        defaultConstructor = proc;
        defaultConstructorSet = true;
    }

    public PrimProcedure getDefaultConstructor() {
        if (! defaultConstructorSet) {
            Type rtype = ctype.getRealType();
            if (rtype instanceof ClassType) {
                Method meth = ((ClassType) rtype).getDefaultConstructor();
                if (meth != null)
                    defaultConstructor = new PrimProcedure(meth, getLanguage());
            }
            defaultConstructorSet = true;
        }
        return defaultConstructor;
    }

    public boolean hasDefaultConstructor() {
        return getDefaultConstructor() != null;
    }

    public boolean hasAddChildMethod() {
        return ClassMethods.selectApplicable(ClassMethods.getMethods(ctype, "add", 'V', null, getLanguage()), 2, false/*??*/) > 0;
    }

    public Member findNamedMember (String name) {
        // Look for field name for a "set" method.
        Member member = SlotSet.lookupMember(ctype, name, caller);
        if (member == null) {
            // Look for for an "add" method.
            // For example: (define b (JButton action-listener: ...))
            // maps to: (define b ...) (b:addActionListener ...)
            member = ctype.getMethod(ClassExp.slotToMethodName("add", name), SlotSet.type1Array);
            }
        return member;
    }

    public Expression buildSetter(Declaration target, Member member, Expression value) {
        return CompileReflect.makeSetterCall(new ReferenceExp(target), member, value);
    }

    public String getAddChildMethodName() {
        return "add";
    }

    public Expression buildAddChild(Declaration target, Expression child) {
        Expression[] iargs = {
            new ReferenceExp(target),
            QuoteExp.getInstance(getAddChildMethodName()),
            child
        };
        return new ApplyExp(Invoke.invoke, iargs);
    } 

    /** Check if we should use the builder rather than the default.
     * @return if true, caller should rewrite using the {@link #build} method;
     *   otherwise caller ({@link CompileInvoke}) should do the work itself.
     */
    public boolean useBuilder(int numCode, InlineCalls visitor) {
        if (getArgCount() > keywordStart && numCode > 0) // Have keywords
            return true;
        else if (numCode == MethodProc.NO_MATCH_TOO_MANY_ARGS
                 && hasDefaultConstructor()
                 && hasAddChildMethod()) {
            keywordStart = 1;
            return true;
        }
        else
            return false;
    }

    public Expression build() {
        Compilation comp = getCompilation();
        Expression[] args = exp.getArgs();
        StringBuffer errbuf = null;

        Expression e;
        if (keywordStart < args.length) {
            Expression[] xargs = new Expression[keywordStart];
            System.arraycopy(args, 0, xargs, 0, keywordStart);
            e = visitor.visit(new ApplyExp(exp.getFunction(), xargs), ctype);
        }
        else {
            ApplyExp ae = new ApplyExp(defaultConstructor, new Expression[] { args[0] }); 
            ae.setType(ctype);
            e = ae;
       }

        comp.letStart();
        Declaration adecl = comp.letVariable((String) null, ctype, e);
        adecl.setFlag(Declaration.ALLOCATE_ON_STACK);
        adecl.setCanRead(true);
        BeginExp begin = new BeginExp();
        int i = keywordStart;
        for (;  i + 1 < args.length; i += 2) {
            Object value = args[i].valueIfConstant();
            if (! (value instanceof Keyword))
                break;
            String name = ((Keyword) value).getName();
            Member slot = findNamedMember(name);
            if (slot == null) {
                comp.error('w', "no field or setter '"+name+"' in class "+ctype.getName());
            } else {
                 begin.add(visitor.visit(buildSetter(adecl, slot, args[i+1]), Type.voidType));
            }
        }

        for (; i < args.length;  i++) {
            begin.add(visitor.visit(buildAddChild(adecl, args[i]), null));
        }
        ReferenceExp aref = new ReferenceExp(adecl);
        aref.setFlag(ReferenceExp.ALLOCATE_ON_STACK_LAST);
        begin.add(aref);

        return visitor.checkType(comp.letDone(begin).setLine(exp), required);
    }
}
