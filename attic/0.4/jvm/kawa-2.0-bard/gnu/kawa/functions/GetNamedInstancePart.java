package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.kawa.reflect.*;
import java.io.*;

/** The value of the Kawa Scheme expression '*:PART-NAME'.
 * This function invokes a method or accesses a field,
 * if the PART-NAME starts with a '.'.
 *
 * This syntax is semi-deprecated, since instead of
 * (*:method-name instance args ...) you can now write
 * (instance:method-name args ...), and
 * instead of (*:.field-name instance) you can write
 * instance:field-name (without the parentheses).
 */

public class GetNamedInstancePart extends ProcedureN
  implements Externalizable, HasSetter
{
  String pname;
  boolean isField;

  public GetNamedInstancePart ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileNamedPart:validateGetNamedInstancePart");
  }

  public GetNamedInstancePart (String name)
  {
    this();
    setPartName(name);
  }

  public void setPartName (String name)
  {
    setName("get-instance-part:"+name);
    if (name.length() > 1 && name.charAt(0) == '.')
      {
        isField = true;
        pname = name.substring(1);
      }
    else
      {
        isField = false;
        pname = name;
      }
  }

  public int numArgs() { return isField ? 0x1001 : 0xfffff001; }
  public Object applyN (Object[] args)
    throws Throwable
  {
    checkArgCount(this, args.length);
    if (isField)
      return SlotGet.field(args[0], pname);
    else
      {
        Object[] xargs = new Object[args.length+1];
        xargs[0] = args[0];
        xargs[1] = pname;
        System.arraycopy(args, 1, xargs, 2, args.length-1);
        return Invoke.invoke.applyN(xargs);
      }
  }

  public Procedure getSetter()
  {
    if (! isField)
      throw new RuntimeException("no setter for instance method call");
    return new SetNamedInstancePart(pname);
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(isField ? ("."+pname) : pname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setPartName((String) in.readObject());
  }
}

class SetNamedInstancePart extends Procedure2
  implements Externalizable
{
  String pname;

  public SetNamedInstancePart ()
  {
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileNamedPart:validateSetNamedInstancePart");
  }

  public SetNamedInstancePart (String name)
  {
    this();
    setPartName(name);
  }

  public void setPartName (String name)
  {
    setName("set-instance-part:."+name);
    pname = name;
  }

  public Object apply2 (Object instance, Object value)
    throws Throwable
  {
    SlotSet.setField(instance, pname, value);
    return Values.empty;
  }


  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(pname);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setPartName((String) in.readObject());
  }
}
