package kawa.lang;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.lists.*;

public class RecordConstructor extends ProcedureN
{
  ClassType type;
  Field[] fields;

  public RecordConstructor (ClassType type, Field[] fields)
  {
    this.type = type;
    this.fields = fields;
  }

  public RecordConstructor (Class clas, Field[] fields)
  {
    this((ClassType) Type.make(clas), fields);
  }

  public RecordConstructor (Class clas)
  {
    init((ClassType) Type.make(clas));
  }

  public RecordConstructor (ClassType type)
  {
    init(type);
  }

  private void init(ClassType type)
  {
    this.type = type;
    Field list = type.getFields();
    int count = 0;
    for (Field fld = list;  fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Access.PUBLIC|Access.STATIC))
	    == Access.PUBLIC)
	  count++;
      }
    fields = new Field[count];
    int i = 0;
    for (Field fld = list;  fld != null;  fld = fld.getNext())
      {
	if ((fld.getModifiers() & (Access.PUBLIC|Access.STATIC))
	    == Access.PUBLIC)
	  fields[i++] = fld;
      }
  }

  public RecordConstructor (Class clas, Object fieldsList)
  {
    this((ClassType) Type.make(clas), fieldsList);
  }

  public RecordConstructor (ClassType type, Object fieldsList)
  {
    this.type = type;
    if (fieldsList == null)
      init(type);
    else
      {
	int nfields = LList.listLength(fieldsList, false);
	this.fields = new Field[nfields];
	Field list = type.getFields();
	for (int i = 0;  i < nfields;  i++)
	  {
	    Pair pair = (Pair) fieldsList;
	    String fname = pair.getCar().toString();
	    for (Field fld = list;  ;  fld = fld.getNext())
	      {
		if (fld == null)
		  throw new RuntimeException ("no such field "+fname+" in "+type.getName());
		if (fld.getSourceName() == fname)
		  {
		    this.fields[i] = fld;
		    break;
		  }
	      }
	    fieldsList = pair.getCdr();
	  }
      }
  }

  public int numArgs()
  {
    int nargs = fields.length;
    return (nargs<<12)|nargs;
  }

  public String getName()
  {
    return type.getName()+" constructor";
  }

  public Object applyN (Object[] args)
  {
    Object obj;
    try
      {
	obj = type.getReflectClass().newInstance();
      }
    catch (InstantiationException ex)
      {
	throw new GenericError (ex.toString());
      }
    catch (IllegalAccessException ex)
      {
	throw new GenericError (ex.toString());
      }
    if (args.length != fields.length)
      throw new WrongArguments(this, args.length);
    for (int i = 0;  i < args.length;  i++)
      {
	Field fld = fields[i];
	try
	  {
	    fld.getReflectField().set(obj, args[i]);
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException("illegal access for field "+fld.getName(), ex);
	  }
      }
    return obj;
  }
}
