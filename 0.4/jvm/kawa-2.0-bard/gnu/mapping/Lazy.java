package gnu.mapping;

/** A lazy value is one that may be calculated on demand.
 * The primary implementation class is {@link Promise}, but
 * {@link Future} also implements {@code Lazy}.
 * Primitive operations (such as arithmetic) require non-lazy (or "eager") values;
 * in Kawa these are automatically evaluated ("forced")
 * using {@link Promise#force} or equivalent.
 * <p>
 * In Kawa all objects are considered eager (non-lazy) unless the
 * object's class implements {@code Lazy}, though note that an eager value
 * may contain lazy components (for example an eager vector of lazy values).
 * Note that the compiler assumes that {@link Promise#force} is a no-op and
 * does not need to be emitted unless the type of the value implements
 * {@code Lazy}, or the class is {@code Object}.
 * (See {@link gnu.kawa.reflect.LazyType#maybeLazy}.)
 */

public interface Lazy<T> {
    /** Return the actual value.
     * Note that {@code getValue()} may return {@code this},
     * if this object isn't actually lazy. 
     * It may also return another lazy value.
     * Normally you should use {@code Promise.force{val}}
     * to extra a non-lazy (eager) value.
     */
    public T getValue();
}
