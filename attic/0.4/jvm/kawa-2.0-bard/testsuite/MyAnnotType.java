import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;

@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(value={CONSTRUCTOR, FIELD, LOCAL_VARIABLE, METHOD, PACKAGE, PARAMETER, TYPE})
public @interface MyAnnotType {
  byte bvalue() default 1;
  short svalue() default 2;
  int ivalue() default 3;
  long lvalue() default 4;
  float fvalue() default 1.5f;
  double dvalue() default 2.5;
  boolean blvalue() default true;
  char chvalue() default 'X';
  String name() default "()";
  Class clvalue() default java.io.File.class;
  String[] names();
  ElementType etype() default ElementType.PACKAGE;
  ElementType[] etypes() default ElementType.PACKAGE;
}
