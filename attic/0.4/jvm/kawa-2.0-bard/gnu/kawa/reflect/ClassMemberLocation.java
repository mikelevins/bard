package gnu.kawa.reflect;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;

/** A Location whose value is that of a named field/method of an object.
 * The object is used as the owning Location's value.
 * (For now, only fields are supported.)
 */

public abstract class ClassMemberLocation<T> extends Location<T>
{
  Object instance;
  ClassType type;
  /** Member (method or field) name. */
  String mname;
  java.lang.reflect.Field rfield;

  public final Object getInstance () { return instance; }
  public final void setInstance (Object obj) { instance = obj; }

  public ClassMemberLocation(Object instance, ClassType type, String mname)
  {
    this.instance = instance;
    this.type = type;
    this.mname = mname;
  }

  public ClassMemberLocation(Object instance, Class clas, String mname)
  {
    this.instance = instance;
    this.type = (ClassType) Type.make(clas);
    this.mname = mname;
  }

  public ClassMemberLocation(Object instance, java.lang.reflect.Field field)
  {
    this.instance = instance;
    this.rfield = field;
    this.mname = field.getName();
  }

  public String getMemberName()
  {
    return mname;
  }

  public ClassType getDeclaringClass()
  {
    return type;
  }

  void setup ()
  {
    if (rfield == null)
      {
	Class clas;
	try
	  {
	    clas = type.getReflectClass();
	  }
	catch (RuntimeException ex)
	  {
            RuntimeException uex
              = new UnboundLocationException(null, "Unbound location - "
                                             + ex.toString());
            /* #ifdef use:java.lang.Throwable.getCause */
            uex.initCause(ex);
            /* #endif */
            throw uex;
	  }
        try
          {
            rfield = clas.getField(mname);
          }
        catch (java.lang.NoSuchFieldException ex)
          {
            RuntimeException uex
              = new UnboundLocationException(null, "Unbound location "
                                             + " - no field " + mname
                                             + " in " + type.getName());
            /* #ifdef use:java.lang.Throwable.getCause */
            uex.initCause(ex);
            /* #endif */
	    throw uex;
          }
      }
  }

  public java.lang.reflect.Field getRField ()
  {
    java.lang.reflect.Field rfld = this.rfield;
    if (rfld == null)
      {
	Class clas
= null;
	try
	  {
	    clas = type.getReflectClass();
            rfld = clas.getField(mname);
	    this.rfield = rfld;
	  }
	catch (Exception ex)
	  {
	    return null;
	  }
      }
    return rfld;
  }

  /** Return the {@code Class} this member is in. */

  public Class getRClass ()
  {
    java.lang.reflect.Field rfld = this.rfield;
    if (rfld != null)
      return rfld.getDeclaringClass();
    try
      {
        return type.getReflectClass();
      }
    catch (Exception ex)
      {
        return null;
      }   
  }

  public T get (T defaultValue)
  {
    java.lang.reflect.Field rfld = getRField();
    if (rfld == null)
      return defaultValue;

    try
      {
        return (T) rfld.get(instance);
      }
    catch (IllegalAccessException ex)
      {
	throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public boolean isConstant ()
  {
    java.lang.reflect.Field rfld = getRField();
    return rfld != null && (rfield.getModifiers() & Access.FINAL) != 0;
  }

  public boolean isBound ()
  {
    java.lang.reflect.Field rfld = getRField();
    return rfld != null;
  }

  public void set (T value)
  {
    setup();
    try
      {
        rfield.set(instance, value);
	return;
      }
    catch (IllegalAccessException ex)
      {
	throw WrappedException.wrapIfNeeded(ex);
      }
    // This is a bit of a kludge  FIXME.
    //setLocation(loc, new TrivialLocation(getEnvironment(loc)));
    //setValue(loc, value);
  }

  public static void define (Object instance, java.lang.reflect.Field rfield,
			     String uri, Language language, Environment env)
    throws IllegalAccessException
  {
    Object fvalue = rfield.get(instance);
    Type ftype = Type.make(rfield.getType());
    boolean isAlias = ftype.isSubtype(Compilation.typeLocation);
    boolean isProcedure = ftype.isSubtype(Compilation.typeProcedure);
    int rModifiers = rfield.getModifiers();
    boolean isFinal = (rModifiers & Access.FINAL) != 0;
    Object fdname = (isFinal && (fvalue instanceof Named && ! isAlias)
		     ? ((Named) fvalue).getSymbol()
		     : Compilation.demangleName(rfield.getName(), true));
    Symbol sym;
    if (fdname instanceof Symbol)
      sym = (Symbol) fdname;
    else
      {
	sym = Symbol.make(uri == null ? "" : uri,
			  fdname.toString().intern());
      }
    Location loc;
    Object property = null;
    if (isAlias && isFinal)
      {
	loc = (Location) fvalue;
      }
    else
      {
	if (isFinal)
	  property = language.getEnvPropertyFor(rfield, fvalue);
        boolean isStatic = (rModifiers & Access.STATIC) != 0;
        if (isStatic)
          loc = new StaticFieldLocation(rfield);
        else
          loc = new FieldLocation(instance, rfield);

      }
    env.addLocation(sym, property, loc);
  }

  /** Import all the public fields of an object. */
  public static void defineAll (Object instance, Language language, Environment env)
    throws IllegalAccessException
  {
    Class clas = instance.getClass();
    java.lang.reflect.Field[] fields = clas.getFields();
    for (int i = fields.length;  --i >= 0; )
      {
	java.lang.reflect.Field field = fields[i];
	String fname = field.getName();
	if (fname.startsWith(Declaration.PRIVATE_PREFIX)
	    || fname.endsWith("$instance"))
	      continue;
	define(instance, field, null, language, env);
      }
  }
}
