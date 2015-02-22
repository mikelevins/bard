package gnu.kawa.reflect;
import gnu.bytecode.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.functions.MakeSplice;

public class CompileArrays
{
  public static Expression validateArrayNew
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(ArrayType.make(((ArrayNew) proc).element_type));
    return exp;
  }

  public static Expression validateArrayLength
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(gnu.kawa.lispexpr.LangPrimType.intType); // FIXME
    return exp;
  }

  public static Expression validateArrayGet
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(((ArrayGet) proc).element_type);
    return exp;
  }

  public static Expression validateArraySet
  (ApplyExp exp, InlineCalls visitor, Type required, Procedure proc)
  {
    exp.visitArgs(visitor);
    exp.setType(Type.void_type);
    return exp;
  }

    public static boolean compileGet(ApplyExp exp, Compilation comp,
                                     Target target, Procedure proc) {
        if (! exp.isSimple())
            return false;
        Type element_type = ((ArrayGet) proc).element_type;
        Expression[] args = exp.getArgs();
        args[0].compile(comp, ArrayType.make(element_type));
        args[1].compile(comp, Type.int_type);
        CodeAttr code = comp.getCode();
        code.emitArrayLoad(element_type);
        target.compileFromStack(comp, element_type);
        return true;
    }

    public static boolean compileSet(ApplyExp exp, Compilation comp,
                                     Target target, Procedure proc) {
        if (! exp.isSimple())
            return false;
        Type element_type = ((ArraySet) proc).element_type;
        Expression[] args = exp.getArgs();
        args[0].compile(comp, ArrayType.make(element_type));
        args[1].compile(comp, Type.int_type);
        args[2].compile(comp, element_type);
        comp.getCode().emitArrayStore(element_type);
        comp.compileConstant(Values.empty, target);
        return true;
    }

    public static boolean compileNew(ApplyExp exp, Compilation comp,
                                     Target target, Procedure proc) {
        if (! exp.isSimple())
            return false;
        Type element_type = ((ArrayNew) proc).element_type;
        CodeAttr code = comp.getCode();
        exp.getArgs()[0].compile(comp, Type.intType);
        code.emitNewArray(element_type.getImplementationType());
        target.compileFromStack(comp, ArrayType.make(element_type));
        return true;
    }

    public static boolean compileLength(ApplyExp exp, Compilation comp,
                                        Target target, Procedure proc) {
        if (! exp.isSimple())
            return false;
        Type element_type = ((ArrayLength) proc).element_type;
        exp.getArgs()[0].compile(comp, ArrayType.make(element_type));
        CodeAttr code = comp.getCode();
        code.emitArrayLength();
        target.compileFromStack(comp, gnu.kawa.lispexpr.LangPrimType.intType);
        return true;
    }

    public static boolean compileMake(ApplyExp exp, Compilation comp,
                                      Target target, Procedure proc) {
        Type elementType = ((ArrayMake) proc).elementType;
        Expression[] args = exp.getArgs();
        createArray(elementType, comp, args, 0, args.length);
        target.compileFromStack(comp, ArrayType.make(elementType));
        return true;
    }

    /** Optimized code generation of array creation with splicing support. */
    public static void createArray(Type elementType, Compilation comp,
                                   Expression[] args, int start, int end) {
        CodeAttr code = comp.getCode();
        // Count non-splice arguments. 
        int countNonSplice = 0;
        int lastSplice = -1;
        for (int i = start; i < end; i++) {
            if (MakeSplice.argIfSplice(args[i]) == null)
                countNonSplice++;
            else
                lastSplice = i;
        }
        code.pushScope();
        code.emitPushInt(countNonSplice);

        Variable arrSizeVar;
        if  (lastSplice < 0)
            arrSizeVar = null;
        else {
            arrSizeVar = code.addLocal(Type.intType);
            code.emitStore(arrSizeVar);
        }

        ClassType utilType = ClassType.make("gnu.kawa.functions.MakeSplice");
        Method countMethod = utilType.getDeclaredMethod("count", 1);
        Method copyToMethod4 = utilType.getDeclaredMethod("copyTo", 4);
        Method copyToMethod5 = utilType.getDeclaredMethod("copyTo", 5);

        Variable[] tmpVars = new Variable[end-start];
        Variable[] sizeVars = new Variable[end-start];

        if (lastSplice >= 0) {
            // First pass to calculate sizes.
            for (int i = start; i < end; i++) {
                Expression arg = args[i];
                Expression argIfSplice = MakeSplice.argIfSplice(arg);
                if (argIfSplice != null
                    || (arg.side_effects() && i < lastSplice)) {
                    if (argIfSplice != null)
                        argIfSplice.compile(comp, Target.pushObject);
                    else
                        arg.compile(comp, elementType);
                    Variable tmpVar =
                        code.addLocal(argIfSplice != null ? Type.objectType
                                      : elementType);
                    code.emitStore(tmpVar);
                    tmpVars[i-start] = tmpVar;
                    if (argIfSplice != null) {
                        Variable sizeVar = code.addLocal(Type.intType);
                        sizeVars[i-start] = sizeVar;
                        // emit: int size[i] = count(tmp[i]);
                        code.emitLoad(tmpVar);
                        code.emitInvoke(countMethod);
                        code.emitDup();
                        code.emitStore(sizeVar);
                        // emit: arrSize += size[i];
                        code.emitLoad(arrSizeVar);
                        code.emitAdd();
                        code.emitStore(arrSizeVar);
                    }
                }
            }
        }
        // emit: elementType[] arr = new elementType[arrSize];
        if (lastSplice >= 0)
            code.emitLoad(arrSizeVar);
        code.emitNewArray(elementType.getImplementationType());
        // emit: int offset = 0;
        Variable offsetVar = null;

        for (int i = start; i < end; i++) {
            // The target array is the top of the stack.
            code.emitDup();
            Expression arg = args[i];
            Expression argIfSplice = MakeSplice.argIfSplice(arg);
            if (argIfSplice != null) {
                // emit: copy vari elements into arr[offset:offset+sizei];
                if (offsetVar == null) {
                    offsetVar = code.addLocal(Type.intType);
                    code.emitPushInt(i-start);
                    code.emitStore(offsetVar);
                }
                code.emitLoad(offsetVar);
                code.emitLoad(sizeVars[i-start]);
                code.emitLoad(tmpVars[i-start]);
                if (elementType == Type.objectType)
                    code.emitInvoke(copyToMethod4);
                else {
                    comp.compileConstant(elementType, Target.pushObject);
                    code.emitInvoke(copyToMethod5);
                }
                //  emit: offset += sizei;
                code.emitLoad(offsetVar);
                code.emitLoad(sizeVars[i-start]);
                code.emitAdd();
                code.emitStore(offsetVar);
            } else {
                // emit: arr[offset++] = arg;
                if (offsetVar == null)
                    code.emitPushInt(i-start);
                else
                    code.emitLoad(offsetVar);
                Variable savedValue = tmpVars[i-start];
                if (savedValue != null)
                    code.emitLoad(savedValue);
                else
                    arg.compile(comp, elementType);
                code.emitArrayStore(elementType);
                if (offsetVar != null)
                    code.emitInc(offsetVar, (short) 1);
            }
        }
        code.popScope();
    }
}
