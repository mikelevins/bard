package gnu.kawa.functions;
import gnu.bytecode.Type;
import gnu.expr.*;
import gnu.mapping.*;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.List;

/** A pseudo-function whose argument is splice into an outer argument list.
 * Represented by {@code ($splice$ arg)}.
 * If {@code arg} is the list or array {@code [a b c]}
 * then {@code (fun x ($splice$ arg) y)} is {@code (fun x a b c y)}.
 * Processed at compile-time only.
 */

public class MakeSplice extends Procedure1 {
    public static final MakeSplice instance = new MakeSplice();

    public static final QuoteExp quoteInstance = new QuoteExp(instance);

    public static Expression argIfSplice(Expression exp) {
        if (exp instanceof ApplyExp) {
            ApplyExp aexp = (ApplyExp) exp;
            if (aexp.getFunction() == quoteInstance)
                return aexp.getArg(0);
        }
        return null;
    }

    public Object apply1 (Object arg1) throws Throwable {
        throw new UnsupportedOperationException("$splice$ function should not be called");
    }

    public static int count(Object values) {
        if (values instanceof Object[])
            return ((Object[]) values).length;
        else if (values instanceof List<?>)
            return ((List<?>) values).size();
        else if (values.getClass().isArray())
            return Array.getLength(values);
        else
            throw new ClassCastException("value is neither List or array");
    }

    public static void copyTo(Object[] target, int start, int size,
                              Object values) {
        if (values instanceof Object[]) {
            Object[] arr = (Object[]) values;
            int nlen = arr.length;
            System.arraycopy(arr, 0, target, start, size);
        } else if (values instanceof List<?>) {
            for (Object val : (List<?>) values)
                target[start++] = val;
         } else if (values.getClass().isArray()) {
            for (int i = 0; i < size;  i++)
                target[start++] = Array.get(values, i);
        } else
            throw new ClassCastException("value is neither List or array");
    }

   public static void copyTo(Object target, int start, int size,
                             Object values, Type elementType) {
        if (elementType == Type.objectType) {
            copyTo((Object[]) target, start, size, values);
        } else if (values instanceof List<?>) {
            for (Object val : (List<?>) values) {
                Object value = elementType.coerceFromObject(val);
                java.lang.reflect.Array.set(target, start++, value);
            }
         } else if (values.getClass().isArray()) {
            for (int i = 0; i < size;  i++) {
                Object value = 
                    elementType.coerceFromObject(Array.get(values, i));
                java.lang.reflect.Array.set(target, start++, value);
            }
        } else
            throw new ClassCastException("value is neither List or array");
    }

    /** Helper method called by compiled code. */
    public static void addAll(ArrayList<Object> list, Object values) {
        if (values instanceof Object[]) {
            Object[] arr = (Object[]) values;
            int nlen = arr.length;
            for (int i = 0; i < nlen;  i++)
                list.add(arr[i]);
        } else if (values instanceof List<?>) {
            list.addAll((List<?>) values);
        } else if (values.getClass().isArray()) {
            int nlen = Array.getLength(values);
            for (int i = 0; i < nlen;  i++)
                list.add(Array.get(values, i));
        } else
            throw new ClassCastException("value is neither List or array");
    }
}
