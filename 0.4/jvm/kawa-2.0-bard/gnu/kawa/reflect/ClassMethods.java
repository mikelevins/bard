package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.bytecode.ClassType;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.FString;
import java.util.ArrayList;
import java.util.List;

public class ClassMethods extends Procedure2
{
  public static final ClassMethods classMethods = new ClassMethods();
  static { classMethods.setName("class-methods"); }

  /** Create a method or generic of the matching methods.
   * @param arg0 a Class, ClassType, or a String, FString or Symbol
   *  that names a class.
   * @param arg1 a method name (a String, FString, or Symbol)
   * Loosely the same as GetNamedPart.make(arg0, arg1),
   * but with some extra conversions and checks.
   */
  public Object apply2 (Object arg0, Object arg1)
  {
    return apply(this, arg0, arg1);
  }

  public static MethodProc apply(Procedure thisProc, Object arg0, Object arg1)
  {
    ClassType dtype;
    String mname;
    if (arg0 instanceof Class)
      arg0 = Type.make((Class) arg0);
    if (arg0 instanceof ClassType)
      dtype = (ClassType) arg0;
    else if (arg0 instanceof String || arg0 instanceof FString
	     || arg0 instanceof Symbol)
      dtype = ClassType.make(arg0.toString());
    else
      throw new WrongType(thisProc, 0, null);
    if (arg1 instanceof String || arg1 instanceof FString
	|| arg1 instanceof Symbol)
      mname = arg1.toString();
    else
      throw new WrongType(thisProc, 1, null);
    if (! ("<init>".equals(mname)))
      mname = Compilation.mangleName(mname);
    MethodProc result = apply(dtype, mname, '\0', Language.getDefaultLanguage());
    if (result == null)
      throw new RuntimeException("no applicable method named `"+mname+"' in "
                                 +dtype.getName());
    return result;
  }

  private static int removeRedundantMethods(List<Method> methods)
  {
    // Remove over-ridden methods.
    int mlength = methods.size();
  loopi:
    for (int i = 1;  i < mlength; )
    {
      Method method1 = (Method) methods.get(i);
      ClassType class1 = method1.getDeclaringClass();
      Type[] types1 = method1.getParameterTypes();
      int tlen = types1.length;
      for (int j = 0;  j < i;  j++)
	{
	  Method method2 = (Method) methods.get(j);
	  Type[] types2 = method2.getParameterTypes();
	  if (tlen != types2.length)
	    continue;
	  int k;
	  for (k = tlen;  --k >= 0;  )
	    {
	      Type pt1 = types1[k];
	      Type pt2 = types2[k];
	      if (pt1 instanceof TypeVariable)
		  pt1 = ((TypeVariable) pt1).getRawType();
	      if (pt2 instanceof TypeVariable)
		  pt2 = ((TypeVariable) pt2).getRawType();
	      if (pt1 != pt2)
		break;
	    }
	  if (k >= 0)
	    continue;
	  if (class1.isSubtype(method2.getDeclaringClass()))
              methods.set(j, method1);
	  methods.set(i, methods.get(mlength - 1));
	  mlength--;
	  // Re-do current i, since methods[i] replaced.
	  continue loopi;
	}
      i++;
    }
    return mlength;
  }
    
    /** Return the methods of a class with the specified name and flag.
     * @param caller if non-null, check that methods are accessible in it.
     * @return an array containing the methods.
     */
    public static PrimProcedure[] getMethods(ObjectType dtype, String mname,
                                             char mode,
                                             ClassType caller,
                                             Language language) {
        // FIXME kludge until we handle "language types".
        if (dtype == Type.tostring_type)
            dtype = Type.string_type;
        MethodFilter filter = new MethodFilter(mname, 0, 0, caller,
                                               mode == 'P' ? null : dtype);
        boolean named_class_only = mode == 'P' || "<init>".equals(mname);
        ArrayList<Method> methods = new ArrayList<Method>();
        ParameterizedType ptype;
        ObjectType rtype;
        if (dtype instanceof ParameterizedType) {
            ptype = (ParameterizedType) dtype;
            rtype = ptype.getRawType();
        } else {
            ptype = null;
            rtype = dtype;
        }
        rtype.getMethods(filter, named_class_only ? 0 : 2, methods);
        if (! named_class_only &&
            // If not redundant (i.e. not a normal ClassType),
            // also search Object.
            ! (dtype instanceof ClassType && ! dtype.isInterface()))
            Type.pointer_type.getMethods(filter, 0, methods);

        int mlength = (named_class_only ? methods.size()
                       : removeRedundantMethods(methods));
        PrimProcedure[] result = new PrimProcedure[mlength];
        int count = 0;
        for (int i = mlength;  --i >= 0; ) {
            Method method = methods.get(i);
            PrimProcedure pproc
                = new PrimProcedure(method, mode, language, ptype);
            if (! named_class_only) {
                ClassType mdclass = method.getDeclaringClass();
                if (mdclass != dtype) {
                    Type itype = dtype.getImplementationType();
                    if (itype instanceof ClassType
                        && ! (((ClassType) itype).isInterface()
                              && mdclass == Type.objectType)) {
                        // Override declared type of method so it matches
                        // receiver, like javac does, for improved binary
                        // compatibility.
                        pproc.setMethodForInvoke(new Method(method, (ClassType) itype));
                    }
                }
            }
            result[count++] = pproc;
        }
        return result;
    }

  /** Re-order the methods such that the ones that are definite
   * applicable (all argtypes is subset of parameter type) are first;
   * those possibly applicable next (argtype overlaps parameter types);
   * and ending with those definitely not applicable (some argtype does
   * overlap its parameter type).
   * @return ((number of definitely applicable methods) << 32
   *          + (number of possibly applicable methods.
   */
  public static long selectApplicable(PrimProcedure[] methods,
                                      Type[] atypes, Type restType)
  {
    int limit = methods.length;
    int numDefApplicable = 0;
    int numPosApplicable = 0;
    for (int i = 0;  i < limit;  )
      {
        int code = methods[i].isApplicable(atypes, restType);
        if (code < 0)
          { // Definitely not applicable.
            // swap(methods[limit-1], methods[i]):
            PrimProcedure tmp = methods[limit-1];
            methods[limit-1] = methods[i];
            methods[i] = tmp;
            limit--;
          }
        else if (code > 0)
          { // Definitely applicable.
            // swap(methods[numDefApplicable], methods[i]):
            PrimProcedure tmp = methods[numDefApplicable];
            methods[numDefApplicable] = methods[i];
            methods[i] = tmp;
            numDefApplicable++;
            i++;
          }
        else
          { // Possibly applicable.
            numPosApplicable++;
            i++;
          }
      }
    return (((long) numDefApplicable) << 32) + (long) numPosApplicable;
  }

  /** Select methods that have the right number of parameters.
   * @return number of methods that apply, NO_MATCH_TOO_FEW_ARGS,
   *  or NO_MATCH_TOO_MANY_ARGS.
   */
  public static int selectApplicable(PrimProcedure[] methods,
                                     int numArgs, boolean maybeMore)
  {
    int limit = methods.length;
    int numTooManyArgs = 0;
    int numTooFewArgs = 0;
    int numOk = 0;
    for (int i = 0;  i < limit;  )
      {
        int num = methods[i].numArgs();
        int min = Procedure.minArgs(num);
        int max = Procedure.maxArgs(num);
        boolean ok = false;
        if (numArgs < min && !maybeMore)
          numTooFewArgs++;
        else if (numArgs > max && max >= 0)
          numTooManyArgs++;
        else
          ok = true;
        if (ok)
          {
            numOk++;
            i++;
          }
        else
          { // Not applicable.
            // swap(methods[limit-1], methods[i]):
            PrimProcedure tmp = methods[limit-1];
            methods[limit-1] = methods[i];
            methods[i] = tmp;
            limit--;
          }
      }
    return numOk > 0 ? numOk
      : numTooFewArgs > 0 ? MethodProc.NO_MATCH_TOO_FEW_ARGS
      : numTooManyArgs > 0 ? MethodProc.NO_MATCH_TOO_MANY_ARGS
      : 0;
  }

  /** Find methods.
   * @param dtype class to search
   * @param mname method name (already mangled, if need be).
   * @param mode one of 'P' (use invokespecial). 'V' (require this argument
   *  even if method is static), or '\0' (otherwise).
   */
  public static MethodProc apply(ObjectType dtype, String mname,
                                 char mode, Language language)
  {
    PrimProcedure[] methods = getMethods(dtype, mname, mode, null, language);
    GenericProc gproc = null;
    PrimProcedure pproc = null;
    for (int i = 0;  i < methods.length;  i++)
      {
        PrimProcedure cur = methods[i];
        if (pproc != null && gproc == null)
          {
            gproc = new GenericProc();
            gproc.add(pproc);
          }
        pproc = cur;
        if (gproc != null)
          gproc.add(pproc);
      }
    if (gproc != null)
      {
        gproc.setName(dtype.getName()+"."+mname);
        return gproc;
      }
    return pproc;
  }

  /** Convert an expression to a name.
   * @return a String if the expression has the form of a symbol or
   *   string literal, mangled as needed; otherwise null
   */
  static String checkName(Expression exp, boolean reversible)
  {
    if (exp instanceof QuoteExp)
      {
        Object name = ((QuoteExp) exp).getValue();
	String nam;
        if (name instanceof FString || name instanceof String)
	  nam = name.toString();
	else if (name instanceof Symbol)
	  nam = ((Symbol) name).getName();
	else
	  return null;
	if (Language.isValidJavaName(nam))
	  return nam;
	return Compilation.mangleName(nam, reversible);
      }
    return null;
  }

  /** Convert an expression to a name.
   * @return a String if the expression has the form of a symbol or
   *   string literal, with no mangling; otherwise null
   */
  static String checkName(Expression exp)
  {
    if (exp instanceof QuoteExp)
      {
        Object name = ((QuoteExp) exp).getValue();
        if (name instanceof FString || name instanceof String)
	  return name.toString();
	else if (name instanceof Symbol)
	  return ((Symbol) name).getName();
	else
	  return null;
      }
    return null;
  }
}

class MethodFilter implements gnu.bytecode.Filter
{
  String name;
  int nlen;
  int modifiers;
  int modmask;
  ClassType caller;
  ObjectType receiver;

  public MethodFilter(String name, int modifiers, int modmask,
		      ClassType caller, ObjectType receiver)
  {
    this.name = name;
    this.nlen = name.length();
    this.modifiers = modifiers;
    this.modmask = modmask;
    this.caller = caller;
    this.receiver = receiver;
  }

  public boolean select(Object value)
  {
    gnu.bytecode.Method method = (gnu.bytecode.Method) value;
    String mname = method.getName();
    int mmods = method.getModifiers();
    if ((mmods & modmask) != modifiers
        || (mmods & Access.SYNTHETIC) != 0
	|| ! mname.startsWith(name))
      return false;
    int mlen = mname.length();
    char c;
    if (mlen != nlen
	&& (mlen != nlen + 2
	    || mname.charAt(nlen) != '$'
	    || ((c = mname.charAt(nlen+1)) != 'V' && c != 'X'))
	&& (mlen != nlen + 4
	    || ! mname.endsWith("$V$X")))
      return false;
    ClassType declaring = receiver instanceof ClassType ? (ClassType) receiver
      : method.getDeclaringClass();
    return caller == null
      || caller.isAccessible(declaring, receiver, method.getModifiers());
  }
}
