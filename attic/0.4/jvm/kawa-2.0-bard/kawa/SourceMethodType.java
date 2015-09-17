package kawa;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;

@Retention(RetentionPolicy.RUNTIME)
@java.lang.annotation.Target(value={METHOD})

/** Used to encode language-specific type information for functions.
 * Each string is as returned by {@link gnu.epr.Language#encodeType}.
 * The 0'th string encodes the return type, with sucessing strings
 * encoding a parameter type.  A String may be empty or missing (because
 * the array is too short) in which case use the Java types instead.
 */

public @interface SourceMethodType {
    String[] value();
}
