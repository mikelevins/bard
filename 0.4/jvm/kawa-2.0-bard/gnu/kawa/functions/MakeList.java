package gnu.kawa.functions;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.LangObjType;

/**
 * Implement the Scheme standard function "list".
 * @author Per Bothner
 */

public class MakeList extends ProcedureN implements Inlineable
{
  public static final MakeList list = new MakeList();
  static { list.setName("list"); }

  public static Object list$V (Object[] args)
  {
    Object result = LList.Empty;
    for (int i = args.length;  --i >= 0; )
      result = new Pair (args[i], result);
    return result;
  }

  public Object applyN (Object[] args)
  {
    return list$V(args);
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    compile(args, 0, comp);
    target.compileFromStack(comp, exp.getType());
  }

  public static void compile (Expression[] args, int offset,
                              Compilation comp)
  {
    int len = args.length - offset;
    CodeAttr code = comp.getCode();
    if (len == 0)
      {
	new QuoteExp(LList.Empty).compile(comp, Target.pushObject);
      }
    else if (len <= 4)
      {
	for (int i = 0;  i < len;  i++)
	  args[offset+i].compile(comp, Target.pushObject);
	Method method
	  = Compilation.scmListType.getDeclaredMethod("list"+len, null);
	code.emitInvokeStatic(method);
      }
    else
      {
	args[offset].compile(comp, Target.pushObject);
	Method method
	  = Compilation.scmListType.getDeclaredMethod("list1", null);
	code.emitInvokeStatic(method);
	code.emitDup(1);
	offset++;  len--;

	while (len >= 4)
	  {
	    args[offset].compile(comp, Target.pushObject);
	    args[offset+1].compile(comp, Target.pushObject);
	    args[offset+2].compile(comp, Target.pushObject);
	    args[offset+3].compile(comp, Target.pushObject);
	    len -= 4;  offset += 4;
	    method = Compilation.scmListType.getDeclaredMethod("chain4", null);
	    code.emitInvokeStatic(method);
	  }

	while (len > 0)
	  {
	    args[offset].compile(comp, Target.pushObject);
	    len -= 1;  offset += 1;
	    method = Compilation.scmListType.getDeclaredMethod("chain1", null);
	    code.emitInvokeStatic(method);
	  }
	code.emitPop(1);
      }
  }

  public Type getReturnType (Expression[] args)
  {
    return args.length > 0 ? Compilation.typePair : LangObjType.listType;
  }

}
