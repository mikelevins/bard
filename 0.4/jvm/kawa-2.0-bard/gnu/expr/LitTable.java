package gnu.expr;
import java.io.*;
import gnu.bytecode.*;
import java.lang.reflect.Array;
import java.util.*;
import gnu.mapping.*;
/* #ifdef use:java.util.regex */
import java.util.regex.*;
/* #endif */

/** Manages the literals of a Compilation.
 * Implements ObjectOutput, because we use externalization to determine
 * how literals get compiled into code that re-creates the literal. */

public class LitTable implements ObjectOutput
{
  Compilation comp;
  ClassType mainClass;

  /* #ifdef use:java.util.IdentityHashMap */ 
  IdentityHashMap literalTable = new IdentityHashMap(100);
  /* #else */
  // Hashtable literalTable = new Hashtable(100);
  /* #endif */

  /** A table mapping objects to public static final field literals.
   * When we a need a literal for a value that is an instance of some
   * class we automatically search the class for static fields.
   * We use a {@code Table2D} primarily to make use of weak references,
   * but we also use the 2nd argument:
   * {@code staticTable(value, null, defaultValue)} yields a Literal
   * if there is a public static final field for {@code value},
   * and {@code defaultValue} otherwise.
   * {@code staticTable(class, Boolean.TRUE, null) != null} if and only if
   * we have scanned {@code class} (a {@code java.lang.Class} object).
   */
  static Table2D staticTable =  new Table2D (100);

  int literalsCount;

  /** Remembers literals to initialize (in <clinit>). */
  Literal literalsChain;

  public LitTable(Compilation comp)
  {
    this.comp = comp;
    this.mainClass = comp.mainClass;
  }

  public void emit() throws IOException
  {
    // We use two passes.  The first generates the graph of
    // objects and how they are generated.
    // The second pass actually emits code.
    // The reason for using two passes is so we can detect cycles
    // and sharing using the first pass.  This generates better code:
    // If an object is only used once, and is not a top-level literal,
    // they we don't need to allocate a Field for it.  And if an object
    // does not cyclically depend on itself, we can allocate *and*
    // initialize using a single call, which generates better code.

    // Here is the first pass.
    for (Literal init = literalsChain;  init != null;
	 init = init.next)
      {
	writeObject(init.value);
      }

    // Here is the second pass.
    for (Literal init = literalsChain;  init != null;
	 init = init.next)
      {
	emit(init, true);
      }

    // For speedier garbage collection.
    literalTable = null;
    literalsCount = 0;
  }

  Object[] valueStack = new Object[20];
  Type[] typeStack = new Type[20];
  int stackPointer;
  
  void push(Object value, Type type)
  {
    if (stackPointer >= valueStack.length)
      {
	Object[] newValues = new Object[2 * valueStack.length];
	Type[] newTypes = new Type[2 * typeStack.length];
	System.arraycopy(valueStack, 0, newValues, 0, stackPointer);
	System.arraycopy(typeStack, 0, newTypes,  0, stackPointer);
	valueStack = newValues;
	typeStack = newTypes;
      }
    valueStack[stackPointer] = value;
    typeStack[stackPointer] = type;
    stackPointer++;
  }

  void error(String msg)
  {
    throw new Error(msg);
  }

  public void flush()
  {
  }

  public void close()
  {
  }

  public void write(int b) throws IOException
  {
    error("cannot handle call to write(int) when externalizing literal");
  }

  public void writeBytes(String s) throws IOException
  {
    error("cannot handle call to writeBytes(String) when externalizing literal");
  }

  public void write(byte[] b) throws IOException
  {
    error("cannot handle call to write(byte[]) when externalizing literal");
  }

  public void write(byte[] b, int off, int len) throws IOException
  {
    error("cannot handle call to write(byte[],int,int) when externalizing literal");
  }

  public void writeBoolean(boolean v)
  {
    push(new Boolean(v), Type.booleanType);
  }

  public void writeChar(int v)
  {
    push(new Character((char) v), Type.charType);
  }

  public void writeByte(int v)
  {
    push(new Byte((byte) v), Type.byteType);
  }

  public void writeShort(int v)
  {
    push(new Short((short) v), Type.shortType);
  }

  public void writeInt(int v)
  {
    push(new Integer(v), Type.intType);
  }

  public void writeLong(long v)
  {
    push(new Long(v), Type.longType);
  }

  public void writeFloat(float v)
  {
    push(new Float(v), Type.floatType);
  }

  public void writeDouble(double v)
  {
    push(new Double(v), Type.doubleType);
  }

  public void writeUTF(String v) 
  {
    push(v, Type.string_type);
  }

  public void writeChars(String v) 
  {
    push(v, Type.string_type);
  }

  public void writeObject(Object obj) throws IOException
  {
    Literal lit = findLiteral(obj);

    // Usually a no-op, but if the literalTable is a Hashtable (rather
    // than an IdentityHashMap) then we might find a literal whose
    // value is equals to obj, but not identical.  This can lead to trouble,
    // e.g. if one is a Pair and the other is a PairWithPosition.
    /* #ifndef use:java.util.IdentityHashMap */
    // obj = lit.value;
    /* #endif */

    if ((lit.flags & (Literal.WRITTEN|Literal.WRITING)) != 0)
      {
	// It is referenced more than once, so we we need a Field
	// to save the value.
	if (lit.field == null
	    && obj != null && ! (obj instanceof String))
	  lit.assign(this);
	if ((lit.flags & Literal.WRITTEN) == 0)
	  lit.flags |= Literal.CYCLIC;
      }
    else
      {
	lit.flags |= Literal.WRITING;
	int oldStack = stackPointer;
	if (obj instanceof gnu.lists.FString
	    && ((gnu.lists.FString) obj).size() < 65535)
	  { // Optimization.
	    push(obj.toString(), Type.string_type);
	  }
	else if (obj instanceof Externalizable)
	  {
	    ((Externalizable) obj).writeExternal(this);
	  }
	else if (obj instanceof Object[])
	  {
	    Object[] arr = (Object[]) obj;
	    for (int i = 0;  i < arr.length;  i++)
	      {
		writeObject(arr[i]);
	      }
	  }
	else if (obj == null
                 || obj instanceof String || lit.type instanceof ArrayType)
	  {
	    // nothing to do
	  }
        else if (obj instanceof java.math.BigInteger)
          {
            writeChars(obj.toString());
          }
        else if (obj instanceof java.math.BigDecimal)
          {
            java.math.BigDecimal dec = (java.math.BigDecimal) obj;
            /* #ifdef JAVA2 */
            writeObject(dec.unscaledValue());
            writeInt(dec.scale());
            /* #else */
            // writeChars(obj.toString());
            /* #endif */
          }
	else if (obj instanceof Integer)
	  push(obj, Type.intType);
	else if (obj instanceof Short)
	  push(obj, Type.shortType);
	else if (obj instanceof Byte)
	  push(obj, Type.byteType);
	else if (obj instanceof Long)
	  push(obj, Type.longType);
	else if (obj instanceof Double)
	  push(obj, Type.doubleType);
	else if (obj instanceof Float)
	  push(obj, Type.floatType);
	else if (obj instanceof Character)
	  push(obj, Type.charType);
        else if (obj instanceof Class)
          push(obj, Type.java_lang_Class_type);
        /* #ifdef use:java.util.regex */
        else if (obj instanceof Pattern)
          {
            Pattern pat = (Pattern) obj;
            push(pat.pattern(), Type.string_type);
            push(Integer.valueOf(pat.flags()), Type.intType);
          }
        /* #endif */
	else
	  error(obj.getClass().getName()+" does not implement Externalizable");
	int nargs = stackPointer - oldStack;
	if (nargs == 0)
	  {
	    lit.argValues = gnu.mapping.Values.noArgs;
	    lit.argTypes = Type.typeArray0;
	  }
	else
	  {
	    lit.argValues = new Object[nargs];
	    lit.argTypes = new Type[nargs];
	    System.arraycopy(valueStack, oldStack, lit.argValues, 0, nargs);
	    System.arraycopy(typeStack, oldStack, lit.argTypes, 0, nargs);
	    stackPointer = oldStack;
	  }
	lit.flags |= Literal.WRITTEN;
      }
    push(lit, lit.type);
  }

  public Literal findLiteral (Object value)
  {
    if (value == null)
      return Literal.nullLiteral;
    Literal literal = (Literal) literalTable.get(value);
    if (literal != null)
      return literal;
    if (comp.immediate)
      return new Literal (value, this);
    Class valueClass = value.getClass();
    Type valueType = Type.make(valueClass);

    synchronized (staticTable)
      {
	literal = (Literal) staticTable.get(value, null, null);
	if ((literal == null || literal.value != value)
	    && valueType instanceof ClassType)
	  {
	    // Add all the static final public fields to staticTable.
	    int needed_mod = Access.STATIC | Access.FINAL | Access.PUBLIC;
	    Class fldClass = valueClass;
	    ClassType fldType = (ClassType) valueType;
	    while (staticTable.get(fldClass, Boolean.TRUE, null) == null)
	      {
		// This is a convention to note that we've scanned valueType.
		staticTable.put(fldClass, Boolean.TRUE, fldClass);
		for (Field fld = fldType.getFields();
		     fld != null;  fld = fld.getNext())
		  {
		    if ((fld.getModifiers() & needed_mod) == needed_mod)
		      {
			try
			  {
			    java.lang.reflect.Field rfld = fld.getReflectField();
			    Object litValue = rfld.get(null);
			    if (litValue == null
				|| ! fldClass.isInstance(litValue))
			      continue;
			    Literal lit = new Literal (litValue, fld, this);
			    staticTable.put(litValue, null, lit);
			    if (value == litValue)
			      literal = lit;
			  }
			catch (Exception ex)
			  {
			    error("caught "+ex+" getting static field "+fld);
			  }
		      }
		  }
		fldClass = fldClass.getSuperclass();
		if (fldClass == null)
		  break;
		fldType = (ClassType) Type.make(fldClass);
	      }
	  }
      }

    if (literal != null)
      literalTable.put(value, literal);
    else
      literal = new Literal (value, valueType, this);
    return literal;
  }

  Method getMethod (ClassType type, String name,
		    Literal literal, boolean isStatic)
  {
    Type[] argTypes = literal.argTypes;
    Method method = type.getDeclaredMethods();
    int argLength = argTypes.length;
    Method best = null;
    long bestArrayArgs = 0;
    boolean ambiguous = false;
    Type[] bParameters = null;
  methodLoop:
    for (; method != null;  method = method.getNext())
      {
	if (! name.equals(method.getName()))
	  continue;
	boolean mstatic = method.getStaticFlag();
	if (isStatic != mstatic)
	  continue;
	// One bit set for each array parameter.
	long arrayArgs = 0;
	Type[] mParameters = method.getParameterTypes();
	int iarg = 0;  int iparam = 0;
	for (;; iarg++, iparam++)
	  {
	    if (iarg == argLength && iparam == mParameters.length)
	      {
		if (best == null || (bestArrayArgs != 0 && arrayArgs == 0))
		  {
		    best = method;
		    bParameters = mParameters;
		    bestArrayArgs = arrayArgs;
		  }
		else if (arrayArgs == 0)
		  {
		    // Now see which of 'best' and 'method' is more specific.

		    // True if we know best cannot be the more specific.
		    boolean not1 = false;
		    // True if we know new method cannot be the more specific.
		    boolean not2 = false;
		    for (int j = argLength;  --j >= 0; )
		      {
			int c = bParameters[j].compare(mParameters[j]);
			if (c != 1)
			  {
			    not2 = true;
			    if (not1)
			      break;
			  }
			if (c != -1)
			  {
			    not1 = true;
			    if (not2)
			      break;
			  }
		      }
		    if (not1)
		      {
			best = method;
			bParameters = mParameters;
		      }
		    ambiguous = not1 && not2;
		  }
		continue methodLoop;  // Look for other matches.
	      }
	    if (iarg == argLength || iparam == mParameters.length)
	      continue methodLoop;  // fail on this method
	    Type aType = argTypes[iarg];
	    Type pType = mParameters[iparam];
	    if (aType.isSubtype(pType))
	      ; // OK so far
	    else if (pType instanceof ArrayType && iparam < 64
		     && (aType == Type.intType || aType == Type.shortType))
	      {
		int count = ((Number) literal.argValues[iarg]).intValue();
		if (count < 0 && type.getName().equals("gnu.math.IntNum"))
		  count -= 0x80000000; // special hack for IntNum.
		Type elementType = ((ArrayType) pType).getComponentType();
		if (count < 0 || iarg + count >= argLength)
		  continue methodLoop;  // fail on this method
		else
		  {
		    for (int j = count;  --j >= 0; )
		      {
			Type t = argTypes[iarg + j + 1];
			if (elementType instanceof PrimType
			    ? elementType.getSignature() != t.getSignature()
			    : ! t.isSubtype(elementType))
			  continue methodLoop;  // fail on this method
		      }
		    iarg += count;
		    arrayArgs |= 1 << iparam;
		  }
	      }
	    else
	      {
	      continue methodLoop;  // fail on this method
	      }
	  }
      }
    if (ambiguous)
      return null;
    if (bestArrayArgs != 0)
      {
	Object[] args = new Object[bParameters.length];
	Type[] types = new Type[bParameters.length];
	int iarg = 0;  int iparam = 0;
	for (;; iarg++, iparam++)
	  {
	    if (iarg == argLength)
	      break;
	    Type pType = bParameters[iparam];
	    if ((bestArrayArgs & (1 << iparam)) == 0)
	      {
		args[iparam] = literal.argValues[iarg];
		types[iparam] = literal.argTypes[iarg];
	      }
	    else
	      {
		int count = ((Number) literal.argValues[iarg]).intValue();
		boolean isIntNum = type.getName().equals("gnu.math.IntNum");
		if (isIntNum)
		  count -= 0x80000000; // special hack for IntNum.
		Type elementType = ((ArrayType) pType).getComponentType();
		types[iparam] = pType;
		args[iparam] = Array.newInstance(elementType.getReflectClass(),
						 count);
		Object[] argValues = literal.argValues;
		if (isIntNum)
		  {
		    // Special kludge for IntNum:  words are Externalized
		    // in big-endian (network) order, but the representation
		    // is little-endian.
		    int[] arr = (int[]) args[iparam];
		    for (int j = count;  j > 0;  j--)
		      arr[count - j]
			= ((Integer) argValues[iarg + j]).intValue();
		  }
		else
		  {
		    for (int j = count;  --j >= 0; )
		      Array.set(args[iparam], j, argValues[iarg + 1 + j]);
		  }
		Literal arrayLiteral = new Literal(args[iparam], pType);
		if (elementType instanceof ObjectType)
		  arrayLiteral.argValues = (Object[]) args[iparam];
		args[iparam] = arrayLiteral;
		iarg += count;
	      }
	  }
	literal.argValues = args;
	literal.argTypes = types;
      }
    return best;
  }

  void putArgs(Literal literal, CodeAttr code)
  {
    Type[] argTypes = literal.argTypes;
    int len = argTypes.length;
    for (int i = 0;  i < len;  i++)
      {
	Object value = literal.argValues[i];
	if (value instanceof Literal)
	  emit((Literal) value, false);
	else
	  comp.compileConstant(value, new StackTarget(argTypes[i]));
      }
  }

  private void store (Literal literal, boolean ignore, CodeAttr code)
  {
    if (literal.field != null)
      {
	if (! ignore)
	  code.emitDup(literal.type);
	code.emitPutStatic(literal.field);
      }
    literal.flags |= Literal.EMITTED;
  }

  void emit(Literal literal, boolean ignore)
  {
    CodeAttr code = comp.getCode();
    if (literal.value == null)
      {
	if (! ignore)
	  code.emitPushNull();
      }
    else if (literal.value instanceof String)
      {
	if (! ignore)
	  code.emitPushString(literal.value.toString ());
      }
    else if ((literal.flags & Literal.EMITTED) != 0)
      {
	if (! ignore)
	  code.emitGetStatic(literal.field);
      }
    else if (literal.value instanceof Object[])
      {
	int len = literal.argValues.length;
	Type elementType = ((ArrayType) literal.type).getComponentType();
	code.emitPushInt(len);
	code.emitNewArray(elementType);
	store(literal, ignore, code);
	for (int i = 0;  i < len;  i++)
	  {
	    Literal el = (Literal) literal.argValues[i];
	    if (el.value == null)
	      continue;
	    code.emitDup(elementType);
	    code.emitPushInt(i);
	    emit(el, false);
	    code.emitArrayStore(elementType);
	  }
      }
    else if (literal.type instanceof ArrayType)
      {
	code.emitPushPrimArray(literal.value, (ArrayType) literal.type);
	store(literal, ignore, code);
      }
    else if (literal.value instanceof Class)
      {
        Class clas = (Class) literal.value;
        if (clas.isPrimitive())
          {
            String cname = clas.getName();
            if (cname.equals("int"))
              cname = "integer";
            cname = "java.lang."
              +Character.toUpperCase(cname.charAt(0))
              +cname.substring(1);
            code.emitGetStatic(ClassType.make(cname).getDeclaredField("TYPE"));
          }
        else
          comp.loadClassRef((ObjectType)Type.make(clas));
	store(literal, ignore, code);
      }
    else if (literal.value instanceof ClassType
	     && ! ((ClassType) literal.value).isExisting())
      {
	// We need to special case ClassTypes that are (currently)
	// non-existing, because the corresponding reflective Class
	// needs to be loaded using the correct ClassLoader.
        ClassType ct = (ClassType) literal.value;
        boolean isPair = literal.value instanceof PairClassType;
        ClassType typeType = isPair ? ClassType.make("gnu.expr.PairClassType")
            : Compilation.typeType;
        Type[] atypes = new Type[isPair ? 2 : 1];
        for (int i = atypes.length;  --i >= 0; )
            atypes[i] = Type.javalangClassType;
        Method meth = typeType.getDeclaredMethod("make", atypes);
	comp.loadClassRef((ClassType) ct);
        if (isPair)
            comp.loadClassRef(((PairClassType) ct).instanceType);
	code.emitInvokeStatic(meth);
	code.emitCheckcast(Compilation.typeClassType);
	store(literal, ignore, code);
      }
    else
      {
	ClassType type = (ClassType) literal.type;
	boolean useDefaultInit = (literal.flags & Literal.CYCLIC) != 0;
	Method method = null;
	boolean makeStatic = false;
	if (! useDefaultInit)
	  {
	    // Look for matching "valueOf" or "make" method.
            // (For backward compatibility for we prefer Symbol's 'make'
            // method over 'valueOf' - they differ in argument order.)
              if (! (literal.value instanceof Symbol))
                method = getMethod(type, "valueOf", literal, true);
              else if (literal.value instanceof SimpleSymbol)
                method = getMethod(Compilation.typeSymbol, "valueOf", literal, true);
            if (method == null
                // Values.make has return type Object, so use the constructor.
                && ! (literal.value instanceof Values))
              {
                String mname = "make";
                /* #ifdef use:java.util.regex */
                if (literal.value instanceof Pattern)
                  mname = "compile";
                /* #endif */
                method = getMethod(type, mname, literal, true);
              }
	    // otherwise look for matching constructor;
	    if (method != null)
	      makeStatic = true;
	    else if (literal.argTypes.length > 0)
	      method = getMethod(type, "<init>", literal, false);

	    if (method == null)
	      useDefaultInit = true;
	  }
	if (useDefaultInit)
	  {
	    method = getMethod(type, "init", literal, false);
            if (method == null)
                method = getMethod(type, "set", literal, false);
	    // otherwise error;
	  }
	if (method == null && literal.argTypes.length > 0)
	  error("no method to construct "+literal.type);
	if (makeStatic)
	  {
	    putArgs(literal, code);
	    code.emitInvokeStatic(method);
	  }
	else if (useDefaultInit)
	  {
	    code.emitNew(type);
	    code.emitDup(type);
	    Method init0 = type.getDeclaredMethod("<init>", 0);
	    code.emitInvokeSpecial(init0);
	  }
	else
	  {
	    code.emitNew(type);
	    code.emitDup(type);
	    putArgs(literal, code);
	    code.emitInvokeSpecial(method);
	  }
	Method resolveMethod
	  = makeStatic || literal.value instanceof Values ? null
          : type.getDeclaredMethod("readResolve", 0);
	if (resolveMethod != null)
	  {
	    code.emitInvokeVirtual(resolveMethod);
	    type.emitCoerceFromObject(code);
	  }
	store(literal, ignore && ! (useDefaultInit && method != null), code);
	if (useDefaultInit && method != null)
	  {
	    if (! ignore)
	      code.emitDup(type);
	    putArgs(literal, code);
	    code.emitInvokeVirtual(method);
	  }
      }
  }

}
