package kawa;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;

@Retention(RetentionPolicy.RUNTIME)
@java.lang.annotation.Target(value={METHOD})

/** Used to encode language-specific type information for variables.
 * The value is a string as returned by {@link gnu.epr.Language#encodeType}.
 */

public @interface SourceType {
    String value();
}
