package gnu.kawa.functions;
import gnu.bytecode.Type;
import gnu.mapping.*;

public class Convert extends Procedure2 {
    /** For explicit conversions we allow more converson. */
    boolean lenient;
    public static final Convert as = new Convert("as", true);
    static {
        as.setProperty(Procedure.validateApplyKey,
                       "gnu.kawa.functions.CompileMisc:validateApplyConvert");
        as.setProperty(Procedure.compilerXKey,
                       "gnu.kawa.functions.CompileMisc:compileConvert");
    }
    public static final Convert cast = new Convert("cast", false);
    static {
        cast.setProperty(Procedure.validateApplyKey,
                         "gnu.kawa.functions.CompileMisc:validateApplyConvert");
        cast.setProperty(Procedure.compilerXKey,
                         "gnu.kawa.functions.CompileMisc:compileConvert");
    }

    public Convert(String name, boolean lenient) {
        super(name);
        this.lenient = lenient;
    }

    public static Convert getInstance() {
        return as;
    }

    public Object apply2(Object arg1, Object arg2) {
        Type type;
        if (arg1 instanceof Class)
            type = Type.make((Class) arg1);
        else
            type = (Type) arg1;
        return type.coerceFromObject (arg2);
    }
}
