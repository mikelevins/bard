package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.io.*;

public class ArrayNew extends Procedure1 implements Externalizable {
    Type element_type;

    public ArrayNew(Type element_type) {
        this.element_type = element_type;
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.reflect.CompileArrays:validateArrayNew");
         setProperty(Procedure.compilerXKey,
                    "gnu.kawa.reflect.CompileArrays:compileNew");
    }

  public boolean isSideEffectFree ()
  {
    return true;
  }

  public Object apply1 (Object count)
  {
    Class clas = element_type.getImplementationType().getReflectClass();
    return java.lang.reflect.Array.newInstance(clas,
					       ((Number) count).intValue());
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(element_type);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    element_type = (Type) in.readObject();
  }
}
