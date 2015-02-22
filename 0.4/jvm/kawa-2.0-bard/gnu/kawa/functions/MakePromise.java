package gnu.kawa.functions;
import gnu.mapping.*;

public class MakePromise extends Procedure1
{
    public static final MakePromise makeDelay = new MakePromise();
    public static final MakePromise makeLazy = new MakePromise();
    static {
        makeDelay.setName("make-delay");
        makeDelay.setProperty(Procedure.validateApplyKey,
                                "gnu.kawa.functions.CompileMisc:validateApplyMakePromise");
        makeLazy.setName("make-lazy");
        makeLazy.setProperty(Procedure.validateApplyKey,
                                "gnu.kawa.functions.CompileMisc:validateApplyMakePromise");
    }

    public static <T> Promise<T> makePromise(Procedure thunk) {
        return new Promise<T>((Procedure) thunk);
    }

    public static <T> Promise<T> makePromiseLazy(Procedure thunk) {
        Promise<T> p = new Promise<T>((Procedure) thunk);
        p.setForceValueIfPromise(true);
        return p;
    }

    public Object apply1 (Object thunk) {
        Procedure proc = (Procedure) thunk;
        if (this == makeLazy)
            return makePromiseLazy(proc);
        else
            return makePromise(proc);
    }
}
