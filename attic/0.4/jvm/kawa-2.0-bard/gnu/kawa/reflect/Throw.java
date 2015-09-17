package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

public class Throw extends Procedure1 implements Inlineable {
    public static final Throw primitiveThrow = new Throw("throw");

    public Throw(String name) {
        super(name);
        setProperty(Procedure.validateApplyKey,
                   "gnu.kawa.reflect.CompileReflect:validateThrow");
    }

    public static void doThrow(Object arg1) throws Throwable {
        throw ((Throwable) arg1);
    }

    public Object apply1 (Object arg1) throws Throwable {
        doThrow(arg1);
        return Values.empty;
    }

    public void compile(ApplyExp exp, Compilation comp, Target target) {
        exp.getArgs()[0].compile(comp, Type.javalangThrowableType);
        comp.getCode().emitThrow();
    }
}
