package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.io.*;
import java.lang.reflect.Array;

public class ArrayGet extends Procedure2 implements Externalizable {
    Type element_type;

    public ArrayGet(Type element_type) {
        this.element_type = element_type;
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.reflect.CompileArrays:validateArrayGet");
        setProperty(Procedure.compilerXKey,
                    "gnu.kawa.reflect.CompileArrays:compileGet");
    }

  public Object apply2 (Object array, Object index)
  {
    Object value = Array.get(array, ((Number) index).intValue());
    return element_type.coerceToObject(value);
  }
  
  public boolean isSideEffectFree ()
  {
    return true;
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
