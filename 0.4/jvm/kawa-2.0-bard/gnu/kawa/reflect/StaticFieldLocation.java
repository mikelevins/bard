package gnu.kawa.reflect;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;

public class StaticFieldLocation extends FieldLocation
{
  public StaticFieldLocation(String cname, String fname)
  {
    super(null, ClassType.make(cname), fname);
  }

  public StaticFieldLocation(ClassType type, String mname)
  {
    super(null, type, mname);
  }

  public StaticFieldLocation (java.lang.reflect.Field field)
  {
    super(null, field);
  }

  public Object get (Object defaultValue)
  {
    Object val = super.get(defaultValue);
    if (val instanceof kawa.lang.Macro)
      getDeclaration();
    return val;
  }

  public static StaticFieldLocation
  define(Environment environ, Symbol sym, Object property,
	 String cname, String fname)
  {
    StaticFieldLocation loc = new StaticFieldLocation(cname, fname);
    environ.addLocation(sym, property, loc);
    return loc;
  }

  public static StaticFieldLocation make (Declaration decl)
  {
    gnu.bytecode.Field fld = decl.field;
    ClassType ctype = fld.getDeclaringClass();
    StaticFieldLocation loc = new StaticFieldLocation(ctype, fld.getName());
    loc.setDeclaration(decl);
    //maybe setKindFlags();
    return loc;
  }

  public static StaticFieldLocation make (/*Object name,*/ String cname, String fldName)
  { 
    return new StaticFieldLocation(cname, fldName);
  }
}
