package gnu.kawa.reflect;
import gnu.bytecode.Type;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;

public class StaticGet extends Procedure0 implements Inlineable
{
  ClassType ctype;
  String fname;
  gnu.bytecode.Field field;
  java.lang.reflect.Field reflectField;

  StaticGet (Class clas, String fname)
  {
    ctype = (ClassType) gnu.bytecode.Type.make(clas);
    this.fname = fname;
  }

  public StaticGet (ClassType ctype, String name, Type ftype, int flags)
  {
    this.ctype = ctype;
    this.fname = name;
    field = ctype.getField(name);
    if (field == null)
      field = ctype.addField(name, ftype, flags);
  }

  public Object apply0 ()
  {
    if (reflectField == null)
      {
	Class clas = ctype.getReflectClass();
	try
	  {
	    reflectField = clas.getField (fname);
	  }
	catch (NoSuchFieldException ex)
	  {
	    throw new RuntimeException("no such field "+fname
				       +" in "+clas.getName());
	  }
      }
    try
      {
	return reflectField.get(null);
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "+fname);
      }
  }

  private gnu.bytecode.Field getField ()
  {
    if (field == null)
      {
	field = ctype.getField(fname);
	if (field == null)
	  field = ctype.addField(fname, Type.make(reflectField.getType()),
				 reflectField.getModifiers());
      }
    return field;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    getField();
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitGetStatic(field);
    target.compileFromStack(comp, field.getType());
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return getField().getType();
  }
}
