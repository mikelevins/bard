// Copyright (c) 1999, 2000, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.kawa.lispexpr.LangObjType;
import gnu.kawa.reflect.CompileArrays;
import gnu.lists.ConsumerWriter;
import kawa.SourceMethodType;
import java.io.Writer;
import java.lang.reflect.Array;
import gnu.kawa.functions.MakeSplice;

/** A primitive Procedure implemented by a plain Java method. */

public class PrimProcedure extends MethodProc {
    private Type retType;

    /** The types of the method parameters.
     * If known, the types have been coerced to Language-specific parameters.
     * Does not include the implicit static link argument of some constructors.
     */
    private Type[] argTypes;

    private Method method;

    /** Actual method to invoke. Normally same as method.
     * However, may have declaring class of the actual caller. 
     * This is similar to what javac does for improved binary compatibility. */
    private Method methodForInvoke;

    private int op_code;
    /** 'P' means use invokespecial;
     * 'V' means expect a target (this) argument, even if method is static;
     * '\0' means don't expect a target. */
    private char mode;
    private boolean sideEffectFree;

    /** If non-null, the LambdaExp that this PrimProcedure implements. */
    private LambdaExp source;

    private java.lang.reflect.Member member;

    public final int opcode() { return op_code; }

    public Type getReturnType () { return retType; }
    public void setReturnType (Type retType) { this.retType = retType; }

    public boolean isSpecial() { return mode == 'P'; }

    public Type getReturnType (Expression[] args) { return retType; }

    public ClassType getDeclaringClass() {
        return methodForInvoke == null ? null
            : methodForInvoke.getDeclaringClass();
    }

    public Method getMethod () { return method; }

    public void setMethodForInvoke(Method m) {
        methodForInvoke = m;
        setOpcode(m);
    }

  public boolean isSideEffectFree ()
  {
    return sideEffectFree;
  }

  public void setSideEffectFree ()
  {
    sideEffectFree = true;
  }

    /** Return true iff the last parameter is a "rest" argument. */
    public boolean takesVarArgs() {
        return takesVarArgs(method);
    }

    public static boolean takesVarArgs(Method method) {
        if (method != null) {
            if ((method.getModifiers() & Access.VARARGS) != 0)
                return true;
            String name = method.getName();
            return name.endsWith("$V") || name.endsWith("$V$X");
        }
        return false;
    }

  public boolean takesContext()
  {
    return method != null && takesContext(method);
  }

  public static boolean takesContext(Method method)
  {
    return method.getName().endsWith("$X");
  }

    /** Support passing an explicit array to a varargs function.
     * This is a kludge inherited from Java to support backwards
     * compatibility after various methods were converte to take varargs.
     * If Java5-style VARARS we allow both a variable-length argument list,
     * or if the last argument already is an array we can use it as is.
     * The tricky part is we sometimes have to distinguish these cases
     * at run-time - see the logic for createVarargsArrayIfNeeded in
     * compileArgs.
     * FIXME This is needless and unreliable complexity.  We should by default
     * create a varargs array - even if the actual argument is an array.
     * People should now use splices instead.
     */
    public static boolean explicitArrayAsVarArgsAllowed = true;

    public int isApplicable(Type[] argTypes, Type restType) {
        int app = super.isApplicable(argTypes, restType);
    int nargs = argTypes.length;
    if (explicitArrayAsVarArgsAllowed
        && app == -1 && method != null && restType == null
        && (method.getModifiers() & Access.VARARGS) != 0
        && nargs > 0 && argTypes[nargs-1] instanceof ArrayType)
      {
        // For a Java5-style VARARGS method, you're also allowed to
        // explicitly pass in an array as the last argument.
        Type[] tmp = new Type[nargs];
        System.arraycopy(argTypes, 0, tmp, 0, nargs-1);
        tmp[nargs-1] = ((ArrayType) argTypes[nargs-1]).getComponentType();
        return super.isApplicable(tmp, null);
      }
    return app;
  }

    public boolean isAbstract() {
        return method != null 
            && (method.getModifiers() & Access.ABSTRACT) != 0;
    }

  public final boolean isConstructor()
  {
    // invokespecial == primitive-constructor
    return opcode() == 183 && mode != 'P';
  }

  /** Whether we are passed an argument for the 'target' / 'receiver' / 'this'.
   * Normally this is false for static methods and true for non-static
   * methods.  However, we may need to be able to call a static method using
   * {@code object.name(args...)} (Java syntax) or
   * {@code (invoke object 'name args...)} (Scheme syntax).
   * This includes when the {@code object} is implied.
   * In this case we need to ignore the first argument's value.
   */
  public boolean takesTarget ()
  {
    return mode != '\0';
  }

  /** The (minimum, number) of arguments.
   * Doesn't not count implicit CallContext argument.
   * Does count 'this' argument for non-static methods.
   * Does count an implicit staticLink argument for constructor.
   */
  public int numArgs()
  {
    int num = argTypes.length;
    if (takesTarget())
      num++;
    if (takesContext())
      num--;
    return takesVarArgs() ? (num - 1) + (-1 << 12) : num + (num << 12);
  }

  public int match0 (CallContext ctx)
  {
    return matchN(ProcedureN.noArgs, ctx);
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    Object[] args = { arg1 };
    return matchN(args, ctx);
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    Object[] args = { arg1, arg2 };
    return matchN(args, ctx);
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3 };
    return matchN(args, ctx);
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    Object[] args = { arg1, arg2, arg3, arg4 };
    return matchN(args, ctx);
  }

    public int matchN(Object[] args, CallContext ctx) {
        int nargs = args.length;
        boolean takesVarArgs = takesVarArgs();
        int fixArgs = minArgs();
        if (nargs < fixArgs)
            return NO_MATCH_TOO_FEW_ARGS|fixArgs;
        if (! takesVarArgs && nargs > fixArgs)
            return NO_MATCH_TOO_MANY_ARGS|fixArgs;
        int paramCount = argTypes.length;
        Type elementType = null;
        Object restArray = null;
        int extraCount = (takesTarget() || isConstructor()) ? 1 : 0;
        boolean takesContext = takesContext();
        Object[] rargs = new Object[paramCount];
        if (takesContext)
            rargs[--paramCount] = ctx;
        Object extraArg;
        if (takesVarArgs) {
            Type restType = argTypes[paramCount-1];
            if (restType == Compilation.scmListType || restType == LangObjType.listType) {
                // FIXME
                rargs[paramCount-1] = gnu.lists.LList.makeList(args, fixArgs);
                nargs = fixArgs;
                elementType = Type.objectType;
            } else {
                ArrayType restArrayType = (ArrayType) restType;
                elementType = restArrayType.getComponentType();
                Class elementClass = elementType.getReflectClass();
                restArray = Array.newInstance(elementClass, nargs-fixArgs);
                rargs[paramCount-1] = restArray;
            }
        }
        if (isConstructor())
            extraArg = args[0];
        else if (extraCount != 0) {
            try {
                extraArg = getDeclaringClass().coerceFromObject(args[0]);
            } catch (ClassCastException ex) {
                return NO_MATCH_BAD_TYPE|1;
            }
        } else
            extraArg = null;
        for (int i = extraCount;  i < args.length; i++) {
            Object arg = args[i];
            Type type = i < fixArgs ? argTypes[i-extraCount]
                : elementType == null ? null : elementType;
            if (type != Type.objectType) {
                try {
                    arg = type.coerceFromObject(arg);
                } catch (ClassCastException ex) {
                    return NO_MATCH_BAD_TYPE|(i+1);
                }
            }
            if (i < fixArgs)
                rargs[i-extraCount] = arg;
            else if (restArray != null) { // I.e. using array rather than LList.
                if (type instanceof PrimType)
                    arg = ((PrimType) type).convertToRaw(arg);
                Array.set(restArray, i - fixArgs, arg);
            }
        }
        ctx.value1 = extraArg;
        ctx.values = rargs;
        ctx.proc = this;
        return 0;
    }

  public void apply (CallContext ctx) throws Throwable
  {
    int arg_count = argTypes.length;
    boolean is_constructor = isConstructor();
    boolean slink = is_constructor && getDeclaringClass().hasOuterLink();

    try
      {
	if (member == null)
	  {
	    Class clas = getDeclaringClass().getReflectClass();
	    Class[] paramTypes = new Class[arg_count+(slink?1:0)];
	    for (int i = arg_count; --i >= 0; )
	      paramTypes[i+(slink?1:0)] = argTypes[i].getReflectClass();
            if (slink)
              paramTypes[0] = getDeclaringClass().getOuterLinkType().getReflectClass();
	    if (is_constructor)
	      member = clas.getConstructor(paramTypes);
	    else if (method != Type.clone_method)
	      member = clas.getMethod(method.getName(), paramTypes);
	  }
	Object result;
	if (is_constructor)
          {
            Object[] args = ctx.values;
            if (slink)
              {
                int nargs = args.length + 1;
                Object[] xargs = new Object[nargs];
                System.arraycopy(args, 0, xargs, 1, nargs-1);
                xargs[0] = ((PairClassType) ctx.value1).staticLink;
                args = xargs;
              }

            result = (((java.lang.reflect.Constructor) member)
                      .newInstance(args));
          }
        else if (method == Type.clone_method)
          {
            // The special Type.clone_method is only used for array types.
            Object arr = ctx.value1;
            Class elClass = arr.getClass().getComponentType();
            int n = java.lang.reflect.Array.getLength(arr);
            result = java.lang.reflect.Array.newInstance(elClass, n);
            System.arraycopy(arr, 0, result, 0, n);
          }
	else
	  result = retType.coerceToObject(((java.lang.reflect.Method) member)
					  .invoke(ctx.value1, ctx.values));
        if (! takesContext())
          ctx.consumer.writeObject(result);
      }
    catch (java.lang.reflect.InvocationTargetException ex)
      {
	throw ex.getTargetException();
      }
  }

  public PrimProcedure (String className, String methodName, int numArgs)
  {
    this(ClassType.make(className).getDeclaredMethod(methodName, numArgs));
  }

  public PrimProcedure(java.lang.reflect.Method method, Language language)
  {
    this(((ClassType) language.getTypeFor(method.getDeclaringClass()))
         .getMethod(method), language);
  }

  public PrimProcedure(Method method)
  {
    init(method);
    this.retType = method.getName().endsWith("$X") ? Type.objectType
      : method.getReturnType();
  }

  public PrimProcedure(Method method, Language language)
  {
    this(method, '\0', language, null);
  }

    public PrimProcedure(Method method, char mode, Language language,
			 ParameterizedType parameterizedType) {
        this.mode = mode;

        init(method);
        // This stuff deals with that a language may have its own mapping
        // from Java types to language types, for coercions and other reasons.
        Type[] pTypes = this.argTypes;
        int nTypes = pTypes.length;
        argTypes = null;
        String[] annotTypes;
        try {
            SourceMethodType sourceType = method.getAnnotation(SourceMethodType.class);
            annotTypes = sourceType == null ? null : sourceType.value();
        } catch (Throwable ex) {
            annotTypes = null;
        }
        for (int i = nTypes;  --i >= 0; ) {
            Type javaType = pTypes[i];
            Type langType = decodeType(javaType, annotTypes, i+1,
                                       parameterizedType, language);
            if (javaType != langType) {
                if (argTypes == null) {
                    argTypes = new Type[nTypes];
                    System.arraycopy(pTypes, 0, argTypes, 0, nTypes); 
                }
                argTypes[i] = langType;
            }
        }
        if (argTypes == null)
            argTypes = pTypes;
        if (isConstructor())
            retType = getDeclaringClass();
        else if (method.getName().endsWith("$X"))
            retType = Type.objectType;
        else {
            retType = decodeType(method.getReturnType(),
                                 annotTypes, 0, parameterizedType, language);
            // Kludge - toStringType doesn't have methods.
            // It shouldn't be used as the "type" of anything -
            // it's just a type with a coercion.  FIXME.
            if (retType == Type.toStringType)
                retType = Type.javalangStringType;
        }
    }

    static Type decodeType(Type javaType, String[] annotTypes, int annotIndex,
                           ParameterizedType parameterizedType,
                           Language lang) {
        String annotType = annotTypes != null && annotTypes.length > annotIndex
            ? annotTypes[annotIndex] : null;
        return lang.decodeType(javaType, annotType, parameterizedType);
    }

    private void setOpcode(Method m) {
        int flags = m.getModifiers();
        if ((flags & Access.STATIC) != 0)
            this.op_code = 184;  // invokestatic
        else {
            ClassType mclass = m.getDeclaringClass();
            if (mode == 'P')
                this.op_code = 183;  // invokespecial
            else {
                mode = 'V';
                if ("<init>".equals(m.getName()))
                    this.op_code = 183;  // invokespecial
                else if ((mclass.getModifiers() & Access.INTERFACE) != 0)
                    this.op_code = 185;  // invokeinterface
                else
                    this.op_code = 182;  // invokevirtual
            }
        }
    }

  private void init(Method method)
  {
    this.method = method;
    this.methodForInvoke = method;
    setOpcode(method);
    Type[] mtypes = method.getParameterTypes();
    if (isConstructor() && method.getDeclaringClass().hasOuterLink())
      {
        int len = mtypes.length-1;
        Type[] types = new Type[len];
        System.arraycopy(mtypes, 1, types, 0, len);
        mtypes = types;
      }
    this.argTypes = mtypes;
  }

  public PrimProcedure(Method method, LambdaExp source)
  {
    this(method);
    this.retType = source.getReturnType();
    this.source = source;
  }

  public PrimProcedure(int opcode, Type retType, Type[] argTypes)
  {
    this.op_code = opcode;
    this.retType = retType;
    this.argTypes= argTypes;
  }

  public static PrimProcedure makeBuiltinUnary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[1];
    args[0] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public static PrimProcedure makeBuiltinBinary(int opcode, Type type)
  {
    // FIXME - should cache!
    Type[] args = new Type[2];
    args[0] = type;
    args[1] = type;
    return new PrimProcedure(opcode, type, args);
  }

  public PrimProcedure(int op_code, ClassType classtype, String name,
		       Type retType, Type[] argTypes)
  {
    this.op_code = op_code;
    method = classtype.addMethod (name, op_code == 184 ? Access.STATIC : 0,
				  argTypes, retType);
    methodForInvoke = method;
    this.retType = retType;
    this.argTypes= argTypes;
    mode = op_code == 184 ? '\0' : 'V';
  }

  /** True if there is no 'this' parameter. */
  public final boolean getStaticFlag()
  {
    return method == null 
      || method.getStaticFlag()
      || isConstructor();
  }

  public final Type[] getParameterTypes() { return argTypes; }

  /** Compile arguments and push unto stack.
   * @param args arguments to evaluate and push.
   * @param startArg Normally 0, but 1 in the case of a constructor,
   *   or the case of "static" method of a non-static class.
   * @param thisType If we are calling a non-static function,
   *   then args[0] is the receiver and thisType is its expected class.
   *   If thisType==Type.voidType, ignore argTypes[0].  (It is used to to
   *   pass a link to a closure environment, which was pushed by our caller.)
   *   If thisType==null, no special handling of args[0] or argTypes[0].
   */
    private void compileArgs(ApplyExp exp, Expression[] args, int startArg, Type thisType, Compilation comp)
 {
    boolean variable = takesVarArgs();
    String name = getName();
    Type arg_type = null;
    gnu.bytecode.CodeAttr code = comp.getCode();
    int skipArg = thisType == Type.voidType ? 1 : 0;
    int arg_count = argTypes.length - skipArg;
    if (takesContext())
      arg_count--;
    int nargs = args.length - startArg;
    boolean is_static = thisType == null || skipArg != 0;

    // Do we need to check at runtime whether a final argument to a VARARGS
    // is an array or need to be wrapped in an array?
    // See comment at explicitArrayAsVarArgsAllowed.
    boolean createVarargsArrayIfNeeded = false;
    if (explicitArrayAsVarArgsAllowed
        && variable && (method.getModifiers() & Access.VARARGS) != 0
        && exp.firstSpliceArg < 0
        && nargs > 0 && argTypes.length > 0
        && nargs == arg_count + (is_static ? 0 : 1))
      {
        Type lastType = args[args.length-1].getType();
        Type lastParam = argTypes[argTypes.length-1];
        if (lastParam.isCompatibleWithValue(lastType) >= 0)
          {
            if (lastParam instanceof ArrayType // should always be true
                && (((ArrayType) lastParam).getComponentType()).isCompatibleWithValue(lastType) >= 0)
                createVarargsArrayIfNeeded = true; 
             variable = false;
          }
      }
    int fix_arg_count = variable ? arg_count - (is_static ? 1 : 0) : args.length - startArg;
    Declaration argDecl = source == null ? null : source.firstDecl();
    if (argDecl != null && argDecl.isThisParameter())
      argDecl = argDecl.nextDecl();
    for (int i = 0; ; ++i)
      {
        if (variable && i == fix_arg_count)
          {
            arg_type = argTypes[arg_count-1+skipArg];
	    if (arg_type == Compilation.scmListType || arg_type == LangObjType.listType)
	      {
		gnu.kawa.functions.MakeList.compile(args, startArg+i, comp);
		break;
	      }
            if (startArg+i+1== args.length
                && exp.firstSpliceArg==startArg+i) {
                // See if final argument is a splice of an array we can re-use.
                Expression spliceArg = MakeSplice.argIfSplice(args[startArg+i]);
                Type spliceType = spliceArg.getType();
                if (spliceType instanceof ArrayType) {
                    Type spliceElType = ((ArrayType) spliceType).getComponentType();
                    Type argElType = ((ArrayType) arg_type).getComponentType();
                    if (spliceElType == argElType
                        || (argElType == Type.objectType
                            && spliceElType instanceof ObjectType)
                        || (argElType instanceof ClassType
                            && spliceElType instanceof ClassType
                            && spliceElType.isSubtype(argElType))) {
                        spliceArg.compileWithPosition(comp, Target.pushObject);
                        i = nargs;
                        break;
                    }
                }
            }
                
            arg_type = ((ArrayType) arg_type).getComponentType();
            CompileArrays.createArray(arg_type, comp,
                                      args, startArg+i, args.length);
            i = nargs;
            break;
          }
        if (i >= nargs)
          break;
        boolean createVarargsNow = createVarargsArrayIfNeeded && i + 1 == nargs;
        if (i >= fix_arg_count)
          {
            code.emitDup(1); // dup array.
            code.emitPushInt(i - fix_arg_count);
          }
        else
          arg_type = argDecl != null && (is_static || i > 0) ? argDecl.getType()
	    : is_static ? argTypes[i + skipArg]
            : i==0 ? thisType
            : argTypes[i-1];
	comp.usedClass(arg_type);
        Type argTypeForTarget = createVarargsNow ? Type.objectType : arg_type;
	Target target =
	  source == null ? CheckedTarget.getInstance(argTypeForTarget, name, i+1)
	  : CheckedTarget.getInstance(argTypeForTarget, source, i);
	args[startArg+i].compileNotePosition(comp, target, args[startArg+i]);
        if (createVarargsNow) // Only if explicitArrayAsVarArgsAllowed
          {
            // Wrap final argument in array if not already an array:
            // Emit: (arg instanceof T[] ? (T[]) arg : new T[]{arg})
            Type eltype = ((ArrayType) arg_type).getComponentType();
            code.emitDup();
            code.emitInstanceof(arg_type);
            code.emitIfIntNotZero();
            code.emitCheckcast(arg_type);
            code.emitElse();
            code.emitPushInt(1);
            code.emitNewArray(eltype); // Stack: value array
            code.emitDupX(); // Stack: array value array
            code.emitSwap(); // Stack: array array value
            code.emitPushInt(0); // Stack array array value 0
            code.emitSwap(); // Stack: array array 0 value
            eltype.emitCoerceFromObject(code);
            code.emitArrayStore(eltype); // Stack: array
            code.emitFi();
          }
        if (i >= fix_arg_count)
          code.emitArrayStore(arg_type);
	if (argDecl != null && (is_static || i > 0))
	  argDecl = argDecl.nextDecl();
      }
  }

    public boolean compile(ApplyExp exp, Compilation comp, Target target) {
        if (exp.firstKeywordArgIndex != 0)
            return false;
        // We can optimize splice arguments if they're in the "varargs"
        // section of the argument list.
        if (exp.firstSpliceArg >= 0
            && (! takesVarArgs() || minArgs() > exp.firstSpliceArg))
            return false;

    gnu.bytecode.CodeAttr code = comp.getCode();
    ClassType mclass = getDeclaringClass();
    Expression[] args = exp.getArgs();
    if (isConstructor())
      {
        if (exp.getFlag(ApplyExp.MAY_CONTAIN_BACK_JUMP))
          {
            // JVM spec for Java6:
            // "There must never be an uninitialized class instance
            // on the operand stack or in a local variable when
            // any backwards branch is taken."
            // Hence re-write:
            //   (make Foo a1 backward_jump_containing_expression a3 ...)
            // to:
            //   (let ((t1 a1)
            //         (t2 backward_jump_containing_expression)
            //         (t3 q2) ...)
            //     (make Foo a1 t1 t2 t3 ...))
            int nargs = args.length;
            comp.letStart();
            Expression[] xargs = new Expression[nargs];
            xargs[0] = args[0];
            for (int i = 1;  i < nargs;  i++)
              {
                Expression argi = args[i];
                // A modest optimzation: Don't generate temporary if not needed.
                // But this also avoids a possible VerifyError in the case
                // of LambdaExp, since if we set the latter's nameDecl to the
                // new temporary, loading the temporary can get confused.
                if (! (argi instanceof QuoteExp
                       || argi instanceof LambdaExp
                       || argi instanceof ReferenceExp))
                  {
                    Declaration d = comp.letVariable(null, argi.getType(), argi);
                    d.setCanRead(true);
                    argi = new ReferenceExp(d);
                  }
                xargs[i] = argi;
              }
            comp.letEnter();
            LetExp let = comp.letDone(new ApplyExp(exp.func, xargs));
            let.compile(comp, target);
            return true;
          }
        code.emitNew(mclass);
        code.emitDup(mclass);
      }
    int spliceCount = exp.spliceCount();
    String arg_error = WrongArguments.checkArgCount(this,
                                                    args.length-spliceCount,
                                                    spliceCount>0);
    if (arg_error != null)
      comp.error('e', arg_error);

    compile(getStaticFlag() ? null : mclass, exp, comp, target);
    return true;
  }

  void compile (Type thisType, ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    gnu.bytecode.CodeAttr code = comp.getCode();
    int startArg = 0;
    if (isConstructor())
      {
        ClassType mclass = getDeclaringClass();
        if (mclass.hasOuterLink())
          {
            ClassExp.loadSuperStaticLink(args[0], mclass, comp);
          }
        thisType = null;
        startArg = 1;
      }
    // Handle: (invoke-special ThisClass (this) '*init* arg ....)
    // (This test is perhaps not quote as robust as it should be.)
    else if (opcode() == 183 && mode == 'P' && "<init>".equals(method.getName()))
      {
        // Specifically handle passing a static-link.
        ClassType mclass = getDeclaringClass();
        if (mclass.hasOuterLink())
          {
            code.emitPushThis();
            // Push the incoming static-link.
            code.emitLoad(code.getCurrentScope().getVariable(1));
            thisType = null;
            startArg = 1;
          }
      }
    else if (takesTarget() && method.getStaticFlag())
      startArg = 1;
    compileArgs(exp, args, startArg, thisType, comp);

    if (method == null)
      {
        code.emitPrimop (opcode(), args.length, retType);
        target.compileFromStack(comp, retType);
      }
    else
      {
        compileInvoke(comp, methodForInvoke, target,
                      exp.isTailCall(), op_code, retType);
      }
  }

  /** Emit the actual invoke operation, after arguments have been pushed.
   * Does whatever magic is needed to pass the result to target,
   * including passing CallContext or special handling of ConsumerTarget.
   */
  public static void
  compileInvoke (Compilation comp, Method method, Target target,
                 boolean isTailCall, int op_code, Type returnType)
  {
    CodeAttr code = comp.getCode();
    comp.usedClass(method.getDeclaringClass());
    comp.usedClass(method.getReturnType());
    if (! takesContext(method))
      {
        code.emitInvokeMethod(method, op_code);
      }
    else if (target instanceof IgnoreTarget
               || (target instanceof ConsumerTarget
                 && ((ConsumerTarget) target).isContextTarget()))
      {
        Field consumerFld = null;
        Variable saveCallContext = null;
        comp.loadCallContext();
        if (target instanceof IgnoreTarget)
          {
            ClassType typeCallContext = Compilation.typeCallContext;
            consumerFld = typeCallContext.getDeclaredField("consumer");
            
            // Consumer saveConsumer = ctx.consumer;
            // ctx.consumer = VoidConsumer.instance:
            code.pushScope();
            saveCallContext = code.addLocal(typeCallContext);
            code.emitDup();
            code.emitGetField(consumerFld);
            code.emitStore(saveCallContext);
            code.emitDup();
            code.emitGetStatic(ClassType.make("gnu.lists.VoidConsumer")
                               .getDeclaredField("instance"));
            code.emitPutField(consumerFld);
          }
        code.emitInvokeMethod(method, op_code);
        if (isTailCall)
          {
            comp.loadCallContext();
            code.emitInvoke(Compilation.typeCallContext
                            .getDeclaredMethod("runUntilDone", 0));
          }
        if (target instanceof IgnoreTarget)
          {
            // ctx.consumer = saveConsumer
            comp.loadCallContext();
            code.emitLoad(saveCallContext);
            code.emitPutField(consumerFld);
            code.popScope();
         }
        return;
      }
    else
      {
        comp.loadCallContext();
        returnType = Type.objectType;
        code.pushScope();
        Variable saveIndex = code.addLocal(Type.intType);
        comp.loadCallContext();
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("startFromContext", 0));
        code.emitStore(saveIndex);
        code.emitWithCleanupStart();
        code.emitInvokeMethod(method, op_code);
        code.emitWithCleanupCatch(null);
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("cleanupFromContext", 1));
        code.emitWithCleanupDone();
        comp.loadCallContext();
        code.emitLoad(saveIndex);
        code.emitInvokeVirtual(Compilation.typeCallContext.
                               getDeclaredMethod("getFromContext", 1));
        code.popScope();
      }
    if (method.getReturnType() == Type.neverReturnsType)
      { 
        // Currently only go here if !takesContext(method).  FIXME: We should
        // use annotations or something to figure out return type of methods
        // that take a context (and whose return type is thus void).
        compileReachedUnexpected(code);
      }
    else {
        if (method.getReturnType() instanceof TypeVariable) {
            if (returnType instanceof ClassType) {
                // This avoids an unneeded exception handler (if the
                // target is a CheckedTarget), and a needless call to
                // Promise.force.  Also, if the target.getType() is
                // different from the returnType, it's probably better
                // to convert to returnType first - it might make it
                // easier to find the right conversion, or emit a more
                // accurate error message.
                code.emitCheckcast(returnType);
            } else
                returnType = method.getReturnType().getRawType();
        }

      target.compileFromStack(comp, returnType);
    }
  }

    public static void compileReachedUnexpected(CodeAttr code) {
        if (code.reachableHere()) {
            code.emitGetStatic(ClassType.make("gnu.expr.Special")
                               .getDeclaredField("reachedUnexpected"));
            code.emitThrow();
        }
    }

  public Type getParameterType(int index)
  {
    if (takesTarget())
      {
        if (index == 0)
          return isConstructor() ? Type.objectType
            : getDeclaringClass();
        index--;
      }
    int lenTypes = argTypes.length;
    if (index < lenTypes - 1)
      return argTypes[index];
    boolean varArgs = takesVarArgs();
    if (index < lenTypes && ! varArgs)
      return argTypes[index];
    Type restType = argTypes[lenTypes - 1];
    if (restType instanceof ArrayType)
      return ((ArrayType) restType).getComponentType();
    else // Should be LList or some other Sequence class.
      return Type.objectType;
  }

  // This is null in JDK 1.1 and something else in JDK 1.2.
  private static ClassLoader systemClassLoader
  = PrimProcedure.class.getClassLoader();

    /** Return the index of the most specific method.
     * An approximation of the algorithm in JLS3
     * 15.12.2.5 "Choosing the Most Specific Method." */
    public static int mostSpecific(PrimProcedure[] procs, int length) {
        if (length <= 1) // Handles length==0 and length==1.
            return length - 1;
        // best is non-negative if there is a single most specific method.
        int best = 0;
        // This array (which is allocated lazily) is used if there is is a
        // set of bestn methods none of which are more specific
        // than the others.
        int[] bests = null;
        // The active length of the bests array.
        int bestn = 0;
        outer:
        for (int i = 1;  i < length;  i++) {
            PrimProcedure method = procs[i];
            if (best >= 0) {
                PrimProcedure winner
                    = (PrimProcedure) mostSpecific(procs[best], method);
                if (winner == null) {
                    if (bests == null)
                        bests = new int[length];
                    bests[0] = best;
                    bests[1] = i;
                    bestn = 2;
                    best = -1;
                } else if (winner == method) {
                    best = i;
                    bestn = i;
                }
            } else {
                for (int j = 0;  j < bestn;  j++) {
                    PrimProcedure old = procs[bests[j]];
                    PrimProcedure winner
                        = (PrimProcedure) mostSpecific(old, method);
                    if (winner == old)
                        continue outer;
                    if (winner == null) {
                        bests[bestn++] = i;
                        continue outer;
                    }
                }
                // At this point method is more specific than bests[0..bestn-1].
                best = i;
                bestn = i;
            }
        }
        if (best < 0 && bestn > 1) {
            PrimProcedure first = procs[bests[0]];
            for (int j = 0;  j < bestn;  j++) {
                int m = bests[j];
                PrimProcedure method = procs[m];
                if (j > 0 && ! overrideEquivalent(first, method))
                    return -1;
                if (! method.isAbstract()) {
                    if (best >= 0)
                        return -1;
                    best = m;
                }
            }
            return best >= 0 ? best : bests[0];
        }
        return best;
    }

  public static PrimProcedure getMethodFor (Procedure pproc, Expression[] args)
  {
    return getMethodFor(pproc, null, args, Language.getDefaultLanguage());
  }

  /** Search for a matching static method in a procedure's class.
   * @return a PrimProcedure that is suitable, or null. */
  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Expression[] args,
					    Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(pproc, decl, atypes, language);
  }

  public static PrimProcedure getMethodFor (Procedure pproc, Declaration decl,
					    Type[] atypes, Language language)
  {
    if (pproc instanceof GenericProc)
      {
	GenericProc gproc = (GenericProc) pproc;
	MethodProc[] methods = gproc.methods;
	pproc = null;
	for (int i = gproc.count;  --i >= 0; )
	  {
              int applic = methods[i].isApplicable(atypes, null/*FIXME*/);
	    if (applic < 0)
	      continue;
	    if (pproc != null)
	      return null; // Ambiguous.
	    pproc = methods[i];
	  }
	if (pproc == null)
	  return null;
      }
    if (pproc instanceof PrimProcedure)
      {
	PrimProcedure prproc = (PrimProcedure) pproc;
	if (prproc.isApplicable(atypes, null/*FIXME*/) >= 0)
	  return prproc;
      }
    Class pclass = getProcedureClass(pproc);
    if (pclass == null)
      return null;
    return getMethodFor((ClassType) Type.make(pclass), pproc.getName(),
			decl, atypes, language);
  }

  public static void disassemble$X (Procedure pproc, CallContext ctx)
    throws Exception
  {
    gnu.lists.Consumer cons = ctx.consumer;
    disassemble(pproc, cons instanceof Writer ? (Writer) cons: new ConsumerWriter(cons));
  }

  public static void disassemble (Procedure proc, Writer out)
    throws Exception
  {
    disassemble(proc, new ClassTypeWriter(null, out, 0));
  }

  public static void disassemble (Procedure proc, ClassTypeWriter cwriter)
    throws Exception
  {
    if (proc instanceof GenericProc)
      {
        GenericProc gproc = (GenericProc) proc;
        int n = gproc.getMethodCount();
        cwriter.print("Generic procedure with ");
        cwriter.print(n);
        cwriter.println(n == 1 ? " method." : "methods.");
        for (int i = 0;  i < n;  i++)
          {
            Procedure mproc = gproc.getMethod(i);
            if (mproc != null)
              {
                cwriter.println();
                disassemble(mproc, cwriter);
              }
          }
        return;
      }
    String pname = null;
    Class cl = proc.getClass();
    if (proc instanceof ModuleMethod)
      cl = ((ModuleMethod) proc).module.getClass();
    else if (proc instanceof PrimProcedure)
      {
        Method pmethod = ((PrimProcedure) proc).methodForInvoke;
        if (pmethod != null)
          {
            cl = pmethod.getDeclaringClass().getReflectClass();
            pname = pmethod.getName();
          }
      }
    ClassLoader loader = cl.getClassLoader();
    if (loader == null) loader = ClassLoader.getSystemClassLoader();
    String cname = cl.getName();
    String rname = cname.replace('.', '/') + ".class";
    ClassType ctype = new ClassType();
    java.io.InputStream rin = loader.getResourceAsStream(rname);
    if (rin == null)
      throw new RuntimeException("missing resource "+rname); // FIXME exception
    ClassFileInput cinput = new ClassFileInput(ctype, rin);
    cwriter.setClass(ctype);
    java.net.URL resource = loader.getResource(rname);
    cwriter.print("In class ");
    cwriter.print(cname);
    if (resource != null)
      {
        cwriter.print(" at ");
        cwriter.print(resource);
      }
    cwriter.println();
    if (pname == null)
      {
        pname = proc.getName();
        if (pname == null)
          {
            cwriter.println("Anonymous function - unknown method.");
            return;
          }
        pname = Compilation.mangleName(pname);
      }
    for (Method method = ctype.getMethods();
         method != null; method = method.getNext())
      {
        String mname = method.getName();
        if (mname.equals(pname)) {
          cwriter.printMethod(method);
        }
      }
    cwriter.flush();
  }

  public static Class getProcedureClass (Object pproc)
  {
    Class procClass;
    if (pproc instanceof ModuleMethod)
      procClass = ((ModuleMethod) pproc).module.getClass();
    else
      procClass = pproc.getClass();
    try
      {
	if (procClass.getClassLoader() == systemClassLoader)
	  return procClass;
      }
    catch (SecurityException ex)
      {
      }
    return null;
  }

  /** Get PrimProcedure for matching method in given class. */
  public static PrimProcedure
  getMethodFor (Class procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    return getMethodFor((ClassType) Type.make(procClass),
			name, decl, args, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
                Expression[] args, Language language)
  {
    int nargs = args.length;
    Type[] atypes = new Type[nargs];
    for (int i = nargs;  --i >= 0;) atypes[i] = args[i].getType();
    return getMethodFor(procClass, name, decl, atypes, language);
  }

  public static PrimProcedure
  getMethodFor (ClassType procClass, String name, Declaration decl,
		Type[] atypes, Language language)
  {
    PrimProcedure best = null;
    int bestCode = -1;
    boolean bestIsApply = false;
    try
      {
        if (name == null)
          return null;
        String mangledName = Compilation.mangleName(name);
        String mangledNameV = mangledName + "$V";
        String mangledNameVX = mangledName + "$V$X";
        String mangledNameX = mangledName + "$X";
	boolean applyOk = true; // Also look for "apply" and "apply$V".
	for (Method meth = procClass.getDeclaredMethods();
	   meth != null;  meth = meth.getNext())
          {
            int mods = meth.getModifiers();
            if ((mods & (Access.STATIC|Access.PUBLIC))
                != (Access.STATIC|Access.PUBLIC))
	      {
		if (decl == null || decl.base == null)
		  continue;
	      }
            String mname = meth.getName();
	    boolean isApply;
	    if (mname.equals(mangledName)
		|| mname.equals(mangledNameV)
		|| mname.equals(mangledNameX)
		|| mname.equals(mangledNameVX))
	      {
		isApply = false;
	      }
	    else if (applyOk
		     && (mname.equals("apply") || mname.equals("apply$V")))
	      {
		isApply = true;
	      }
            else
              continue;
	    if (! isApply)
	      {
		// If we saw a real match, ignore "apply".
		applyOk = false;
		if (bestIsApply)
		  {
		    best = null;
		    bestCode = -1;
		    bestIsApply = false;
		  }
	      }
	    PrimProcedure prproc = new PrimProcedure(meth, language);
	    prproc.setName(name);
	    int code = prproc.isApplicable(atypes, null/*FIXME*/);
	    if (code < 0 || code < bestCode)
	      continue;
	    if (code > bestCode)
	      {
		best = prproc;
	      }
	    else if (best != null)
	      {
		best = (PrimProcedure) MethodProc.mostSpecific(best, prproc);
		if (best == null)
		  { // Ambiguous.
		    if (bestCode > 0)
		      return null;
		  }
	      }
	    bestCode = code;
	    bestIsApply = isApply;
          }
      }
    catch (SecurityException ex)
      {
      }
    return best;
  }

  public String getName()
  {
    String name = super.getName();
    if (name != null)
      return name;
    name = getVerboseName();
    setName(name);
    return name;
  }

  public String getVerboseName()
  {
    StringBuffer buf = new StringBuffer(100);
    if (method == null)
      {
	buf.append("<op ");
	buf.append(op_code);
	buf.append('>');
      }
    else
      {
	buf.append(getDeclaringClass().getName());
	buf.append('.');
	buf.append(method.getName());
      }
    buf.append('(');
    for (int i = 0; i < argTypes.length; i++)
      {
	if (i > 0)
	  buf.append(',');
	buf.append(argTypes[i].getName());
      }
    buf.append(')');
    return buf.toString();
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append(retType == null ? "<unknown>" : retType.getName());
    buf.append(' ');
    buf.append(getVerboseName());
    return buf.toString();
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print("#<primitive procedure ");
    ps.print(toString());
    ps.print ('>');
  }
}
