package gnu.kawa.reflect;

import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.*;
import java.lang.reflect.Array;

public class ArrayMake extends ProcedureN {
    Type elementType;

    public static final ArrayMake makeObjectArray =
        new ArrayMake(Type.objectType);

    public ArrayMake(Type elementType) {
        this.elementType = elementType;
        setProperty(Procedure.compilerXKey,
                    "gnu.kawa.reflect.CompileArrays:compileMake");
    }

    public static ArrayMake getInstance(Type elementType) {
        return elementType==Type.objectType ? makeObjectArray
            : new ArrayMake(elementType);
    }

    public Object applyN(Object[] args) {
        int nlen = args.length;
        if (elementType == Type.objectType) {
            Object[] arr = new Object[nlen];
            System.arraycopy(args, 0, arr, 0, nlen);
            return arr;
        } else {
            Class clas = elementType.getImplementationType().getReflectClass();
            Object arr = Array.newInstance(clas, nlen);
            for (int i = 0; i < nlen;  i++) {
                Object val = elementType.coerceFromObject(args[i]);
                java.lang.reflect.Array.set(arr, i, val);
            }
            return arr;
        }
    }

    public String toString() {
        return "#<procedure ArrayMake["+elementType+"]>";
    }
}
