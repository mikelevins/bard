package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;

public class FieldLocation<T> extends ClassMemberLocation<T>
{
  Declaration decl;
  /** The cached location of the field, if final.
   * This is the value of this Location.  Howeve, if INDIRECT_LOCATION is
   * set and CONSTANT is cleared, then we need to do an extra indirection. */
  Object value;
  static final int SETUP_DONE = 1; // FIXME - do we still need this?

  /** Flag that indicates that field value has type Location.
   * Hence <code>get</code> of this Location requires an extra indirection. */
  static final int INDIRECT_LOCATION = 2;
  /** The actual value (following any indirection) is constant.
   * I.e. if INDIRECT_LOCATION is set, then that Location has isConstant set,
   * Otherwise the actual value is a final field. */
  static final int CONSTANT = 4;
  /** Flag that indicates that the value field has been set.
   * If INDIRECT_LOCATION has been set, but not CONSTANT, then
   * the <code>value</code> is a Location we need to indirect.
   * If CONSTANT is set, then this is the actual (final) value.
   * Not set unless at least one of INDIRECT_LOCATION or CONSTANT are set. */
  static final int VALUE_SET = 8;
  // The PROCEDURE and SYNTAX flags aren't current used by getDeclaration,
  // but probably should be, assuming we can count on them.
  public static final int PROCEDURE = 16;
  public static final int SYNTAX = 32;
  /** True if the flags <code>PROCEDURE|SYNTAX|INDIRECT_LOCATION|CONSTANT</code>
   * are valid. */
  public static final int KIND_FLAGS_SET = 64;
  private int flags;

  public boolean isIndirectLocation ()
  { return (flags & INDIRECT_LOCATION) != 0; }

  public void setProcedure ()
  {
    flags |= PROCEDURE|CONSTANT|KIND_FLAGS_SET;
  }

  public void setSyntax ()
  {
    flags |= SYNTAX|CONSTANT|KIND_FLAGS_SET;
  }

  void setKindFlags ()
  {
    String fname = getMemberName();
    gnu.bytecode.Field fld = getDeclaringClass().getDeclaredField(fname);
    if (fld == null) throw new RuntimeException("No field found for "+this);
    int fflags = fld.getModifiers();
    Type ftype = fld.getType();
    if (ftype.isSubtype(Compilation.typeLocation))
      flags |= INDIRECT_LOCATION;
    if ((fflags & Access.FINAL) != 0)
      {
        if ((flags & INDIRECT_LOCATION) == 0)
          {
            flags |= CONSTANT;
            if (ftype.isSubtype(Compilation.typeProcedure))
              flags |= PROCEDURE;
            if (ftype instanceof ClassType
                && ((ClassType) ftype).isSubclass("kawa.lang.Syntax"))
              flags |= SYNTAX;
          }
        else
          {
            Location loc = (Location) getFieldValue();
            if (loc instanceof FieldLocation)
              {
                FieldLocation<T> floc = (FieldLocation<T>) loc;
                if ((floc.flags & KIND_FLAGS_SET) == 0)
                  floc.setKindFlags();
                flags |= (floc.flags & (SYNTAX|PROCEDURE|CONSTANT));
                if ((floc.flags & CONSTANT) != 0)
                  {
                    if ((floc.flags & VALUE_SET) != 0)
                      {
                        value = floc.value;
                        flags |= VALUE_SET;
                      }
                  }
                else
                  {
                    value = floc;
                    flags |= VALUE_SET;
                  }
              }
            else if (loc.isConstant())
              {
                Object val = loc.get(null);
                // if (val == null) ????;
                if (val instanceof Procedure)
                  flags |= PROCEDURE;
                if (val instanceof kawa.lang.Syntax) // FIXME
                  flags |= SYNTAX;
                flags |= CONSTANT|VALUE_SET;
                value = val;
              }
          }
      }
    flags |= KIND_FLAGS_SET;
  }

  public boolean isProcedureOrSyntax ()
  {
    if ((flags & KIND_FLAGS_SET) == 0)
      setKindFlags();
    return (flags & (PROCEDURE+SYNTAX)) != 0;
  }

  public FieldLocation(Object instance, String cname, String fname)
  {
    super(instance, ClassType.make(cname), fname);
  }

  public FieldLocation(Object instance, ClassType type, String mname)
  {
    super(instance, type, mname);
  }

  public FieldLocation (Object instance, java.lang.reflect.Field field)
  {
    super(instance, field);
    type = (ClassType) Type.make(field.getDeclaringClass());
  }

  public void setDeclaration (Declaration decl)
  {
    this.decl = decl;
  }

  public Field getField ()
  {
    return type.getDeclaredField(mname);
  }

  /** Get the type of the field. */
  public Type getFType ()
  {
    return type.getDeclaredField(mname).getType();
  }

  public synchronized Declaration getDeclaration ()
  {
    if ((flags & KIND_FLAGS_SET) == 0)
      setKindFlags();
    Declaration d = decl;
    if (d == null)
      {
	String fname = getMemberName();
	ClassType t = getDeclaringClass();
	gnu.bytecode.Field procField = t.getDeclaredField(fname);
	if (procField == null)
	  return null;
        ModuleInfo info = ModuleInfo.find(t);
        ModuleExp mexp = info.getModuleExp();
        for (d = mexp.firstDecl();  d != null; d = d.nextDecl())
          {
            if (d.field != null && d.field.getName().equals(fname))
              break;
          }
        if (d == null)
          throw new RuntimeException("no field found for "+this);
        decl = d;
      }
    return d;
  }

  void setup ()
  {
    synchronized (this)
      {
	if ((flags & SETUP_DONE) != 0)
	  return;
	super.setup();
        if ((flags & KIND_FLAGS_SET) == 0)
          setKindFlags();
        flags |= SETUP_DONE;
      }
  }

  public T get () { return get(null, true); }

  public T get (T defaultValue) { return get(defaultValue, false); }

  T get (T defaultValue, boolean throwIfUnbound)
  {
    try
      {
        setup();
      }
    catch (Exception ex)
      {
        if (throwIfUnbound) throw new UnboundLocationException(this); 
        return defaultValue;
      }
    Object v;
    if ((flags & VALUE_SET) != 0)
      {
        v = value;
        if ((flags & CONSTANT) != 0)
          return (T) v;
      }
    else
      {
	v = getFieldValue();
	if ((type.getDeclaredField(mname).getModifiers() & Access.FINAL) != 0)
	  {
	    flags |= VALUE_SET;
	    if ((flags & INDIRECT_LOCATION) == 0)
              flags |= CONSTANT;
	    value = v;
	  }
      }
    if ((flags & INDIRECT_LOCATION) != 0)
      {
	Location loc = (Location) v;
        if (throwIfUnbound)
          v = loc.get();
        else if (! loc.isBound())
          return defaultValue;
	if (loc.isConstant())
	  {
	    flags |= CONSTANT;
	    value = v;
	  }
      }
    return (T) v;
  }

  private T getFieldValue ()
  {
    super.setup(); // Set rfield, if needed.
    try
      {
        return (T) rfield.get(instance);
      }
    catch (Exception ex)
      {
	throw WrappedException.rethrow(ex);
      }
  }

  public void set (Object newValue)
  {
    setup();
    if ((flags & INDIRECT_LOCATION) == 0)
      {
	try
	  {
	    rfield.set(instance, newValue);
	  }
	catch (Exception ex)
	  {
	    throw WrappedException.wrapIfNeeded(ex);
	  }
      }
    else
      {
	Object v;
	if ((flags & VALUE_SET) != 0)
	  v = value;
	else
	  {
	    flags |= VALUE_SET;
	    v = getFieldValue();
	    value = v;
	  }
	((Location) v).set(newValue);
      }
  }

  public Object setWithSave (T newValue)
  {
    if ((flags & KIND_FLAGS_SET) == 0)
      setKindFlags();
    if ((flags & INDIRECT_LOCATION) == 0)
      return super.setWithSave(newValue);
    else
      {
	Object v;
	if ((flags & VALUE_SET) != 0)
	  v = value;
	else
	  {
	    flags |= VALUE_SET;
	    v = getFieldValue();
	    value = v;
	  }
	return ((Location) v).setWithSave(newValue);
      }
  }

  public void setRestore (Object oldValue)
  {
    if ((flags & INDIRECT_LOCATION) == 0)
      super.setRestore(oldValue);
    else
      ((Location) value).setRestore(oldValue);
  }

  public boolean isConstant ()
  {
    if ((flags & KIND_FLAGS_SET) == 0)
      setKindFlags();
    if ((flags & CONSTANT) != 0)
      return true;
    if (isIndirectLocation())
      {
	Object v;
	if ((flags & VALUE_SET) != 0)
	  v = value;
	else
	  {
            try
              {
                setup();
              }
            catch (Exception ex)
              {
                return false;
              }
	    v = getFieldValue();
	    flags |= VALUE_SET;
	    value = v;
	  }
	return ((Location) v).isConstant();
      }
    return false;
  }

  public boolean isBound ()
  {
    if ((flags & KIND_FLAGS_SET) == 0)
      setKindFlags();
    if ((flags & CONSTANT) != 0 || (flags & INDIRECT_LOCATION) == 0)
      return true;
    Object v;
    if ((flags & VALUE_SET) != 0)
      v = value;
    else
      {
        try
          {
            setup();
          }
        catch (Exception ex)
          {
            return false;
          }
	v = getFieldValue();
	flags |= VALUE_SET;
	value = v;
      }
    return ((Location) v).isBound();
  }

  public static FieldLocation make (Object instance, Declaration decl)
  {
    gnu.bytecode.Field fld = decl.field;
    ClassType ctype = fld.getDeclaringClass();
    FieldLocation loc = new FieldLocation(instance, ctype, fld.getName());
    loc.setDeclaration(decl);
    //maybe setKindFlags();
    return loc;
  }

  public static FieldLocation make (/*Object name,*/ Object instance, String cname, String fldName)
  {
    return new FieldLocation(/*name,*/ instance, ClassType.make(cname), fldName);
  }

  public String toString()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append("FieldLocation[");
    if (instance != null)
      {
	sbuf.append(instance);
	sbuf.append(' ');
      }
    sbuf.append(type == null ? "(null)" : type.getName());
    sbuf.append('.');
    sbuf.append(mname);
    /* DEBUGGING:
    sbuf.append(" #:");
    sbuf.append(id);
    */
    sbuf.append(']');
    return sbuf.toString();
  }
}
