package kawa.lang;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;

// Should be called PrimGetField for consistency.

public class GetFieldProc extends Procedure1 implements Inlineable
{
  ClassType ctype;
  Field field;

  public GetFieldProc (Class clas, String fname)
  {
    this ((ClassType) Type.make(clas), fname);
  }

  public GetFieldProc (ClassType ctype, String fname)
  {
    this.ctype = ctype;
    this.field = Field.searchField(ctype.getFields(), fname);
  }

  public GetFieldProc (ClassType ctype, String name, Type ftype, int flags)
  {
    this.ctype = ctype;
    field = ctype.getField(name);
    if (field == null)
      field = ctype.addField(name, ftype, flags);
  }

  public Object apply1 (Object arg1)
  {
    try
      {
	java.lang.reflect.Field reflectField = field.getReflectField();
	return reflectField.get(arg1);
      }
    catch (NoSuchFieldException ex)
      {
	throw new RuntimeException ("no such field " + field.getSourceName()
				    + " in " + ctype.getName());
      }
    catch (IllegalAccessException ex)
      {
	throw new RuntimeException("illegal access for field "
				   + field.getSourceName());
      }
  }

  private gnu.bytecode.Field getField ()
  {
    return field;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    ClassLoader loader = ctype.getReflectClass().getClassLoader();
    if (loader instanceof gnu.bytecode.ArrayClassLoader)
      {
        ApplyExp.compile(exp, comp, target);
        return;
      }
    exp.getArgs()[0].compile(comp, ctype);
    gnu.bytecode.CodeAttr code = comp.getCode();
    code.emitGetField(field);
    target.compileFromStack(comp, field.getType());
  }

  public gnu.bytecode.Type getReturnType (Expression[] args)
  {
    return getField().getType();
  }
}
