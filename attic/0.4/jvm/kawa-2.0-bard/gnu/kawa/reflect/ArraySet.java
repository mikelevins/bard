package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import java.io.*;

public class ArraySet extends Procedure3 implements Externalizable {
    Type element_type;

    public ArraySet(Type element_type) {
        this.element_type = element_type;
        setProperty(Procedure.validateApplyKey,
                    "gnu.kawa.reflect.CompileArrays:validateArraySet");
        setProperty(Procedure.compilerXKey,
                    "gnu.kawa.reflect.CompileArrays:compileSet");
    }

  public Object apply3 (Object array, Object index, Object value)
  {
    value = element_type.coerceFromObject(value);
    if (element_type instanceof PrimType)
        value = ((PrimType) element_type).convertToRaw(value);
    java.lang.reflect.Array.set(array,
				((Number) index).intValue(),
                                value);
    return Values.empty;
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
