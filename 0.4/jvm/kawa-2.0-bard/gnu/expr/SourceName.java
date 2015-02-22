// Copyright (c) 2011  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;

/** Annotation to record a declaration's source symbol. */

@Retention(RetentionPolicy.RUNTIME)
@java.lang.annotation.Target(value={FIELD})
public @interface SourceName {
    String name();
    String prefix() default "";
    String uri() default "";
}
