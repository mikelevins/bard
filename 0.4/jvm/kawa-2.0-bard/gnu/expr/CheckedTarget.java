// Copyright (c) 1999  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.WrongType;

/** Same as StackTarget, but catch ClassCastException.
 * Generate code so that if coercion fails, catch ClassCastException,
 * and re-throw a WrongType.  This gives better error messages. */

public class CheckedTarget extends StackTarget
{
  LambdaExp proc;
  String procname;
  /** 1-origin argument index,
      or WrongType.ARG_CAST, or WrongType-ARG_VARNAME. */
  int argno;

  public CheckedTarget(Type type)
  {
    super(type);
    argno = WrongType.ARG_CAST;
  }

  public CheckedTarget(Type type, LambdaExp proc, int argno)
  {
    super(type);
    this.proc = proc;
    this.procname = proc.getName();
    this.argno = argno;
  }

  public CheckedTarget(Type type, String procname, int argno)
  {
    super(type);
    this.procname = procname;
    this.argno = argno;
  }

  public static Target getInstance(Type type, String procname, int argno)
  {
    return (type == Type.objectType ? Target.pushObject
            : new CheckedTarget(type, procname, argno));
  }

  public static Target getInstance(Type type, LambdaExp proc, int argno)
  {
    return (type == Type.objectType ? Target.pushObject
            : new CheckedTarget(type, proc, argno));
  }

  public static Target getInstance(Type type)
  {
    return (type == Type.objectType ? Target.pushObject
            : new CheckedTarget(type));
  }

  public static Target getInstance (Declaration decl)
  {
    return getInstance(decl.getType(), decl.getName(), WrongType.ARG_VARNAME);
  }

    protected StackTarget getClonedInstance(Type type) {
        CheckedTarget target = new CheckedTarget(type);
        target.procname = this.procname;
        target.proc = this.proc;
        target.argno = this.argno;
        return target;
    }

  static ClassType typeClassCastException;
  static ClassType typeWrongType;
  static Method initWrongTypeStringMethod;
  static Method initWrongTypeProcMethod;

  private static void initWrongType()
  {
    if (typeClassCastException == null)
      typeClassCastException = ClassType.make("java.lang.ClassCastException");
    if (typeWrongType == null)
      {
        typeWrongType= ClassType.make("gnu.mapping.WrongType");
        Type[] args = new Type[4];
        args[0] = typeClassCastException;
        args[1] = Compilation.javaStringType;
        args[2] = Type.intType;
	args[3] = Type.objectType;
        initWrongTypeStringMethod
          = typeWrongType.addMethod("<init>", Access.PUBLIC,
                                    args, Type.voidType);
        args = new Type[4];
        args[0] = typeClassCastException;
        args[1] = Compilation.typeProcedure;
        args[2] = Type.intType;
	args[3] = Type.objectType;
        initWrongTypeProcMethod
          = typeWrongType.addMethod("<init>", Access.PUBLIC,
                                    args, Type.voidType);
      }
  }

    protected void doCoerce(Compilation comp) {
        emitCheckedCoerce(comp, proc, procname, argno, type, null);
    }

  public static void emitCheckedCoerce(Compilation comp,
                                       String procname, int argno, Type type)
  {
    forceLazyIfNeeded(comp, Type.objectType, type);
    emitCheckedCoerce(comp, null, procname, argno, type, null);
  }

  public static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                       int argno, Type stackType, Type type, Variable argValue)
  {
    forceLazyIfNeeded(comp, stackType, type);
    emitCheckedCoerce(comp, proc, proc.getName(), argno, type, argValue);
  }

  static void emitCheckedCoerce(Compilation comp, LambdaExp proc,
                                String procname, int argno, Type type,
				Variable argValue)
  {
    CodeAttr code = comp.getCode();
    // If we're not in a try statement, it is more efficient to defer
    // the handler to the end of the procedure (thus avoiding a goto,
    // and potentially improving locality).  If we're in a try statement,
    // we can't safely do that
    boolean isInTry = code.isInTry();
    initWrongType();
    Label startTry = new Label(code);
    Scope tmpScope;
    if (argValue == null && type != Type.toStringType)
      {
	tmpScope = code.pushScope();
	argValue = code.addLocal(Type.objectType);
	code.emitDup(1);
	code.emitStore(argValue);
      }
    else
      tmpScope = null;
    int startPC = code.getPC();
    startTry.define(code);
    emitCoerceFromObject(type, comp);

    int endPC = code.getPC();
    // If no cast was needed, no code has been generated.
    // Thus endPC is equal to startPC and we can stop safely.
    // Also, toStringType can never raise an exception, so we don't need
    // to catch it.
    if (endPC == startPC
	|| type == Type.toStringType)
      {
        // FIXME should remove generated store to argValue, by truncating
        // PC to startPC.  But take care with startPC.position.
	if (tmpScope != null)
	  code.popScope();
	return;
      }

    Label endTry = new Label(code);
    endTry.define(code);

    Label endLabel = new Label(code);
    endLabel.setTypes(code);
    if (isInTry)
      code.emitGoto(endLabel);
    int fragment_cookie = 0;
    code.setUnreachable();
    if (! isInTry)
      fragment_cookie = code.beginFragment(endLabel);
    code.addHandler(startTry, endTry, typeClassCastException);
    // Push arguments:
    // ClassCastException is already pushed
    boolean thisIsProc = false;
    if (proc != null && proc.isClassGenerated()
        && ! comp.method.getStaticFlag())
      {
        if (comp.method.getDeclaringClass() == proc.getCompiledClassType(comp))
          thisIsProc = true;
      }
    int line = comp.getLineNumber();
    if (line > 0)
      code.putLineNumber(line);
    code.emitNew(typeWrongType);
    code.emitDupX(); // dup_x1
    code.emitSwap();
    if (thisIsProc)
      code.emitPushThis();
    else
      code.emitPushString(procname == null && argno != WrongType.ARG_CAST
			  ? "lambda"
			  : procname);
    code.emitPushInt(argno);
    code.emitLoad(argValue);
    code.emitInvokeSpecial(thisIsProc ? initWrongTypeProcMethod
                           : initWrongTypeStringMethod);
    if (tmpScope != null)
      code.popScope();
    code.emitThrow();
    if (isInTry)
      endLabel.define(code);
    else
      code.endFragment(fragment_cookie);
  }
}
