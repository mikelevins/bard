package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.mapping.*;
import gnu.mapping.Location;  // As opposed to gnu.bytecode.Location.
import gnu.expr.*;
import gnu.bytecode.*;
import gnu.kawa.functions.GetNamedPart;
import gnu.kawa.lispexpr.LispLanguage;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.SlotGet;
import gnu.kawa.reflect.StaticFieldLocation;

/**
 * The Syntax transformer that re-writes the Kawa "location" primitive.
 * @author	Per Bothner
 */

public class location extends Syntax
{
  public static final location location = new location();
  static { location.setName("location"); }

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("missing argument to location");
    Pair pair = (Pair) obj;
    if (pair.getCdr() != LList.Empty)
      return tr.syntaxError ("extra arguments to location");
    //    Expression arg = tr.rewrite(pair.getCar());
    Expression[] args = { location.rewrite(tr.rewrite(pair.getCar()), tr) };
    return Invoke.makeInvokeStatic(thisType, "makeLocationProc", args);
  }

  private static ClassType thisType = ClassType.make("kawa.standard.location");

  public static Expression rewrite (Expression arg, Translator tr)
  {
    if (arg instanceof ReferenceExp)
      {
	ReferenceExp rexp = (ReferenceExp) arg;
	rexp.setDontDereference(true);
	Declaration decl = rexp.getBinding();
	if (decl != null)
          {
            decl.maybeIndirectBinding(tr);
            decl = Declaration.followAliases(decl);
            decl.setCanRead(true);
            decl.setCanWrite(true);
          }
	return rexp;
      }
    if (arg instanceof ApplyExp)
      {
	ApplyExp aexp = (ApplyExp) arg;
        Expression afunc = aexp.getFunction();
        Expression[] aargs = aexp.getArgs();
        int aalen = aargs.length;
        Object aproc = afunc.valueIfConstant();
        StaticFieldLocation sloc = null;

        if (aproc == GetNamedPart.getNamedPart && aalen == 2) {
            Expression exp = rewriteApply(aargs[0], aargs[1], tr);
            if (exp != null)
                return exp;
        }
        if (aproc == Scheme.applyToArgs && aalen == 3
            && aargs[0].valueIfConstant() == SlotGet.staticField) {
            Expression exp = rewriteApply(aargs[1], aargs[2], tr);
            if (exp != null)
                return exp;
        }
	Expression[] args = new Expression[aalen + 1];
	args[0] = afunc;
	System.arraycopy(aargs, 0, args, 1, aalen);
        return new ApplyExp(getMakeProcLocProc(), args);
      }
    return tr.syntaxError("invalid argument to location");
  }

    /** Helper for handling special cases to take locations of ApplyExp.
     * Specifically for static fields and static member classes.
     */
    static Expression rewriteApply(Expression classExp, Expression nameExp,
                                   Compilation comp) {
        ClassType caller = comp.curClass;
        Object cls = classExp.valueIfConstant();
        if (cls instanceof Class)
            cls = Type.make((Class) cls);
        Object nam = nameExp.valueIfConstant();
        if (cls instanceof ClassType && nam instanceof SimpleSymbol) {
            String name = nam.toString();
            ClassType ctype = (ClassType) cls;
            Member member = SlotGet.lookupMember(ctype, name, caller);
            if (member.getStaticFlag()) {
                if (member instanceof Field) {
                    StaticFieldLocation sloc =
                        new StaticFieldLocation(ctype, Compilation.mangleNameIfNeeded(name));
                    ReferenceExp rexp = new ReferenceExp(sloc.getDeclaration());
                    rexp.setDontDereference(true);
                    return rexp;
                }
                else if (member instanceof ClassType) {
                    ClassType cltype = (ClassType) member;
                    if (cltype.isExisting()) {
                        try {
                            Class clas = cltype.getReflectClass();
                            if (clas != null)
                                return new QuoteExp(clas);
                        } catch (Exception ex) {
                            // silenty ignored
                        }
                    }
                }
            }

        }
        return null;
    }

    private static PrimProcedure makeProcLocProc;
    public static synchronized PrimProcedure getMakeProcLocProc() {
        if (makeProcLocProc == null) {
            makeProcLocProc = new PrimProcedure(ClassType.make("kawa.standard.location").getDeclaredMethod("makeProcLocation$V", 2));
        }
        return makeProcLocProc;
    }

  public static Location
  makeProcLocation$V (Procedure proc, Object[] args)
  {
    int nargs = args.length;
    if (proc instanceof gnu.kawa.functions.ApplyToArgs
        && nargs > 0
        && args[0] instanceof Procedure) // FIXME
      {
        proc = (Procedure) args[0];
        if (proc instanceof LocationProc && nargs == 1)
          return ((LocationProc) proc).getLocation();
        Object[] rargs = new Object[nargs-1];
        System.arraycopy(args, 1, rargs, 0, rargs.length);
        return new ProcLocation(proc, rargs);
      }
    if (proc instanceof LocationProc && nargs == 0)
      return ((LocationProc) proc).getLocation();
    return new ProcLocation(proc, args);
  }

  public static Procedure
  makeLocationProc (Location loc)
  {
    return new LocationProc(loc);
  }
}
