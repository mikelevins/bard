package kawa.lang;
//import java.lang.reflect.Field; // Confuses Gcj.  Should FIX gcj.
import java.lang.reflect.Modifier;
import java.util.Vector;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.expr.Compilation;

public class Record
{
  public String getTypeName()
  {
    return getClass().getName();
  }

  public static boolean isRecord (Object obj) { return obj instanceof Record; }

  public int hashCode()
  {
    java.lang.reflect.Field[] fields = getClass().getFields();
    int hash = 12345;
    for (int i = 0;  i < fields.length;  i++)
      {
	java.lang.reflect.Field field = fields[i];
	Object value;
	try
	  {
	    value = field.get(this);
	  }
	catch (IllegalAccessException ex)
	  {
	    continue;
	  }
	if (value != null)
	  hash ^= value.hashCode();
      }
    return hash;
  }

  static java.lang.reflect.Field getField (Class clas, String fname)
    throws NoSuchFieldException
  {
    ClassType ctype = (ClassType) Type.make(clas);
    for (gnu.bytecode.Field fld = ctype.getFields();
	 fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    != Modifier.PUBLIC)
	  continue;
	if (! fld.getSourceName().equals(fname))
	  continue;
	return fld.getReflectField();
      }
    throw new NoSuchFieldException();
  }

  public Object get (String fname, Object defaultValue)
  {
    Class clas = getClass();
    try
      {
	return getField(clas, fname).get(this);
      }
    catch (NoSuchFieldException ex)
      {
	//return defaultValue;
	throw new GenericError("no such field "+fname+" in "+clas.getName());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public Object put (String fname, Object value)
  {
    return set1 (this, fname, value);
  }

  public static Object set1 (Object record, String fname, Object value)
  {
    Class clas = record.getClass();
    try
      {
	java.lang.reflect.Field fld = getField(clas, fname);
	Object old = fld.get(record);
	fld.set(record, value);
	return old;
      }
    catch (NoSuchFieldException ex)
      {
	//throw new UnboundLocation(fname);
	throw new GenericError("no such field "+fname+" in "+clas.getName());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError("illegal access for field "+fname);
      }
  }

  public boolean equals (Object obj)
  {
    if (this == obj)
      return true;
    Class thisClass = getClass();
    if (obj == null || obj.getClass() != thisClass)
      return false;
    ClassType ctype = (ClassType) Type.make(thisClass);
    for (gnu.bytecode.Field fld = ctype.getFields();
	 fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    != Modifier.PUBLIC)
	  continue;
	Object value1, value2;
	try
	  {
	    java.lang.reflect.Field field = fld.getReflectField();
	    value1 = field.get(this);
	    value2 = field.get(obj);
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
	if (! (value1.equals(value2)))
	  return false;
      }
    return true;
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(200);
    buf.append("#<");
    buf.append(getTypeName());
    ClassType ctype = (ClassType) Type.make(getClass());
    for (gnu.bytecode.Field fld = ctype.getFields();
	 fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    != Modifier.PUBLIC)
	  continue;
	Object value;
	try
	  {
	    java.lang.reflect.Field field = fld.getReflectField();
	    value = field.get(this);
	  }
	catch (Exception ex)
	  {
	    value = "#<illegal-access>";
	  }
	buf.append(' ');
	buf.append(fld.getSourceName());
	buf.append(": ");
	buf.append(value);
      }
    buf.append(">");
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print(toString());
  }

  public static ClassType makeRecordType (String name, LList fnames)
  {
    ClassType superClass = ClassType.make("kawa.lang.Record");
    String mangledName = Compilation.mangleNameIfNeeded(name);
    ClassType clas = new ClassType(mangledName);
    clas.setSuper(superClass);
    clas.setModifiers(Access.PUBLIC|Access.SUPER);

    // Generate the (default) constructor.
    Method constructor = clas.addMethod ("<init>", Type.typeArray0,
					  Type.voidType, Access.PUBLIC);
    Method superConstructor
      = superClass.addMethod ("<init>", Type.typeArray0,
			       Type.voidType, Access.PUBLIC);
    gnu.bytecode.CodeAttr code = constructor.startCode();
    code.emitPushThis();
    code.emitInvokeSpecial(superConstructor);
    code.emitReturn();
    if (! name.equals(mangledName))
      {
	Method meth = clas.addMethod ("getTypeName", Type.typeArray0,
				      Compilation.typeString, Access.PUBLIC);
	code = meth.startCode();
	code.emitPushString(name);
	code.emitReturn();
      }

    //StringBuffer fnamesBuf = new StringBuffer(100);
    gnu.bytecode.Field fld;
    while (fnames != LList.Empty)
      {
	Pair pair = (Pair) fnames;
	String fname = pair.getCar().toString();
	//fnamesBuf.append(fname);  fnamesBuf.append('\n');
	fld = clas.addField(Compilation.mangleNameIfNeeded(fname),
			    Type.pointer_type, Access.PUBLIC);
	fld.setSourceName(fname.intern());
	fnames = (LList) pair.getCdr();
      }
    /*
    fld = clas.addField("$FieldNames$", Compilation.typeString,
		      Access.PUBLIC|Access.STATIC|Access.FINAL);
    ConstantValueAttr attr = new ConstantValueAttr(fnamesBuf.toString());
    attr.addToFrontOf(fld);
    */

    byte[][] arrays = new byte[1][];
    String[] names = new String[1];
    names[0] = mangledName;
    arrays[0] = clas.writeToArray();
    ArrayClassLoader loader = new ArrayClassLoader(names, arrays);
    try
      {
	Class reflectClass = loader.loadClass (mangledName);
	Type.registerTypeForClass(reflectClass, clas);
	return clas;
      }
    catch (ClassNotFoundException ex)
      {
	throw new InternalError (ex.toString());
      }
  }

  public static LList typeFieldNames (Class clas)
  {
    LList list = LList.Empty;
    /*
    try
      {
	Field fld = clas.getDeclaredField("$FieldNames$");
	String names = (String) fld.get(null);
	int nfields = 0;
	int limit = names.length() - 1;
	
	int ifield;
	while (limit > 0)
	  {
	    int start = names.lastIndexOf('\n', limit - 1);
	    String fname = names.substring(start + 1, limit);
	    list = new Pair(fname.intern(), list);
	    limit = start;
	  }
	return list;
      }
    catch (Exception ex)
      {
      }
    */
    ClassType ctype = (ClassType) Type.make(clas);
    gnu.bytecode.Field field = ctype.getFields();
    Vector vec = new Vector(100);
    for (;  field != null;  field = field.getNext())
      {
	if ((field.getModifiers() & (Modifier.STATIC|Modifier.PUBLIC))
	    == Modifier.PUBLIC)
	  vec.addElement(SimpleSymbol.valueOf(field.getSourceName()));
      }
    for (int i = vec.size();  --i >= 0; )
      {
	list = new Pair(vec.elementAt(i), list);
      }
    return list;
  }

  public static LList typeFieldNames (ClassType ctype)
  {
    return typeFieldNames(ctype.getReflectClass());
  }
}
