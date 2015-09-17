package gnu.kawa.lispexpr;
import gnu.mapping.*;
import java.io.*;
import gnu.bytecode.ClassType;
import gnu.kawa.functions.GetNamedPart;

public class ClassNamespace extends Namespace implements Externalizable
{
  ClassType ctype;

  public ClassType getClassType ()
  {
    return ctype;
  }

  public static ClassNamespace getInstance (String name, ClassType ctype)
  {
    synchronized (nsTable)
      {
	Object old = nsTable.get(name);
	if (old instanceof ClassNamespace)
	  return (ClassNamespace) old;
	ClassNamespace ns = new ClassNamespace(ctype);
	nsTable.put(name, ns);
	return ns;
      }
  }

  public ClassNamespace ()
  {
  }

  public ClassNamespace (ClassType ctype)
  {
    this.setName("class:"+ctype.getName());
    this.ctype = ctype;
  }

  public Object get (String name)
  {
    try
      {
        return GetNamedPart.getTypePart(ctype, name);
      }
    catch (Throwable ex)
      {
        throw WrappedException.rethrow(ex);
      }
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(ctype);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    ctype = (ClassType) in.readObject();
    setName("class:"+ctype.getName());
  }

  public Object readResolve() throws ObjectStreamException
  {
    String name = getName();
    if (name != null)
      {
	Namespace ns = (Namespace) nsTable.get(name);
	if (ns instanceof ClassNamespace)
	  return ns;
	nsTable.put(name, this);
      }
    return this;
  }
}
