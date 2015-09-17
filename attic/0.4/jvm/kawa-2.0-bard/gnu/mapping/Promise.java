package gnu.mapping;
import gnu.text.Printable;
import gnu.lists.Consumer;
import java.io.IOException;

/** Implement Scheme "promises".
 *
 * This is a final class, because in some cases (as required by SRFI-45)
 * we copy the fields of one Promise from other Promise, and such
 * copying would be dubious if either operand is a sub-class.
 *
 * @author Per Bothner
 */

public class Promise<T> implements Printable, Lazy<T> {
    Procedure thunk;

    /** If getValue yields another Lazy value, call getValue on that too. */
    boolean forceValueIfPromise;

    /** Set whether to recursively call getValue if getValue yields a Lazy value. */
    public void setForceValueIfPromise(boolean value) {
        forceValueIfPromise = value;
    }

    /** The result - or UNBOUND if it is not ready. */
    private volatile Object result = Location.UNBOUND;

    /** If an exception was thrown when getting value, store it here. */
    private Throwable throwable;

    /** Create new "blank" Promise.
     * Calling getValue just waits until one of setValue, setAlias,
     * setException or setThunk is invoked.
     * One of these is typically done by some other "producer" thread.
     */
    public Promise() {
    }

    /** Create a new Promise that will evaluate thunk when forced. */
    public Promise(Procedure thunk) {
        this.thunk = thunk;
    }

    /** Wrap value as a forced Promise. */
    public static <T> Lazy<T> makeBoundPromise (T value) {
        Promise<T> p = new Promise<T>(null);
        p.result = value;
        return p;
    }

    public static  Lazy<Object> coerceToLazy (Object value) {
        if (value instanceof Lazy<?>)
            return (Lazy<Object>) value;
        Promise<Object> p = new Promise<Object>(null);
        p.result = value;
        return p;
    }

    public synchronized T getValue () {
        Object r;
        for (;;) {
            synchronized (this) {
                while (isBlank()) {
                    try {
                        wait();
                    } catch (java.lang.InterruptedException ex) {
                    }
                }
                
                r = result;
                if (r == Location.UNBOUND && throwable == null) {
                    // thunk is non-null, because !isBlank().
                    try {
                        r = thunk.apply0 ();
                        if (result == Location.UNBOUND)
                            result = r;
                        else
                            r = result;
                        if (forceValueIfPromise && r instanceof Promise) {
                            Promise pr = (Promise) r;
                            synchronized(r) {
                                if (! pr.isBlank()) {
                                    moveFrom(pr);
                                    continue;
                                }
                            }
                        }
                    } catch (Throwable ex) {
                        throwable = ex;
                    }
                    thunk = null;
                }
                if (throwable != null)
                    WrappedException.rethrow(throwable);
            }
            if (forceValueIfPromise && r instanceof Lazy)
                return ((Lazy<T>) r).getValue();
            return (T) r;
        }
    }

    /** Copy fields of other into this, and set other to indirect to this.
     * This is used to implement the SRFI-45 requirements.
     */
    private void moveFrom(Promise other) {
        this.thunk = other.thunk;
        this.forceValueIfPromise = other.forceValueIfPromise;
        this.throwable = other.throwable;
        this.result = other.result;

        other.result = this;
        other.forceValueIfPromise = true;
        other.thunk = null;
        other.throwable = null;
    }

    public synchronized final boolean isBlank() {
        return thunk == null && result == Location.UNBOUND && throwable == null;
    }

    public void checkBlank() {
        if (! isBlank())
            throw new IllegalStateException();
    }

    /** Bind this promise to a given (eager) value. */
    public synchronized void setValue(Object value) {
         checkBlank();
         result = value;
         notifyAll();
    }

    /** Bind promise to be an alias of another Lazy value. */
    public synchronized void setAlias(Lazy promise) {
         checkBlank();
         result = promise;
         setForceValueIfPromise(true);
         notifyAll();
    }

    /** Bind this promise so forcing it throws the given exception. */
    public synchronized void setException(Throwable exception) {
         checkBlank();
         throwable = exception;
         notifyAll();
    }

    /** Bind this promise so forcing it evaluates the given procedure. */
    public synchronized void setThunk(Procedure thunk) {
         checkBlank();
         this.thunk = thunk;
         notifyAll();
    }

    /** Forces the argument, if needed.
     * I.e. calls {@link Lazy#getValue} once if the argument is Lazy.
     */
    public static Object force1 (Object arg) {
	if (arg instanceof Lazy)
	    arg = ((Lazy) arg).getValue();
	return arg;
    }

    /** Forces the argument, if needed, to a non-Lazy value.
     * I.e. calls {@link Lazy#getValue} as many times as needed.
     */
    public static Object force (Object arg) {
	while (arg instanceof Lazy) {
            Object val = ((Lazy) arg).getValue();
            if (arg == val)
                break;
            arg = val;
        }
	return arg;
    }

    /** If argument is Lazy, force it, unless already an instance of target.
     */
    public static Object force (Object arg, Class target) {
	while (arg instanceof Lazy && ! target.isInstance(arg)) {
            Object val = ((Lazy) arg).getValue();
            if (arg == val)
                break;
            arg = val;
        }
	return arg;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        try {
            print(sb);
        }
        catch (IOException ex) {
            return "caught "+ex;
        }
        return sb.toString();
    }

    public void print (Consumer out) {
        try {
            print((Appendable) out);
        }
        catch (IOException ex) {
            out.write("caught "+ex);
        }
    }

    public void print (Appendable out) throws IOException {
        Object r = result;
        if (r == Location.UNBOUND) {
            synchronized (this) {
                if (throwable != null) {
                    out.append("#<promise - force threw a ");
                    out.append(throwable.getClass().getName());
                    out.append('>');
                }
                else
                    out.append("#<promise - not forced yet>");
            }
        }
        else if (r == null)
            out.append("#<promise - forced to null>");
        else {
            out.append("#<promise"+" - forced to a ");
            out.append(r.getClass().getName());
            out.append('>');
        }
    }
}
