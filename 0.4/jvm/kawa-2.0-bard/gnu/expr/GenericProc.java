// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import gnu.mapping.*;
import gnu.bytecode.Type;

/** A collection of MethodProcs;  one is chosen at apply time. */

public class GenericProc extends MethodProc
{
  protected MethodProc[] methods;
  int count;
  int minArgs;
  int maxArgs;

  public GenericProc (String name)
  {
    setName(name);
  }

  public GenericProc ()
  {
  }

  public int getMethodCount ()
  {
    return count;
  }

  public MethodProc getMethod (int i)
  {
    return i >= count ? null : methods[i];
  }

  public int numArgs()
  {
    return minArgs | (maxArgs << 12);
  }

  protected synchronized void addAll (MethodProc[] procs)
  {
    int n = procs.length;
    if (methods == null)
      methods = new MethodProc[n];
    for (int i = 0;  i < n;  i++)
      add(procs[i]);
  }

  public synchronized void addAtEnd (MethodProc method)
  {
    int oldCount = count;
    if (methods == null)
      methods = new MethodProc[8];
    else if (oldCount >= methods.length)
      {
        MethodProc[] copy = new MethodProc[2 * methods.length];
        System.arraycopy(methods, 0, copy, 0, oldCount);
        methods = copy;
      }

    methods[oldCount] = method;

    int n = method.minArgs();
    if (n < minArgs || count==0)
      minArgs = n;
    n = method.maxArgs();
    if (n == -1 || n > maxArgs)
      maxArgs = n;
    count = ++oldCount;
  }

  public synchronized void add(MethodProc method)
  {
    int oldCount = count;
    addAtEnd(method);

    for (int i = 0;  i < oldCount;  i++)
      {
	MethodProc best = MethodProc.mostSpecific(method, methods[i]);
	if (best == method)
          {
            System.arraycopy(methods, i, methods, i + 1, oldCount - i);
            methods[i] = method;
            break;
          }
      }
  }

  /* Possibly optimization.  Likewise for apply0, apply2, apply3, apply4.
  public Object apply1 (Object arg1) throws Throwable
  {
    if (numArgs() != 0x1001)
      {
	Object[] args = { arg1 };
	return applyN(args);
      }
    CallContext ctx = CallContext.getInstance();
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
        if (method.match1(arg1, ctx) == 0)
	  return method.applyV(ctx);
      }
    throw new WrongType(this, WrongType.ARG_UNKNOWN, null);
  }
  */

  public Object applyN(Object[] args) throws Throwable
  {
    if (count == 1)
      return methods[0].applyN(args);
    checkArgCount(this, args.length);
    CallContext ctx = CallContext.getInstance();
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int m = method.matchN(args, ctx);
        if (m == 0)
	  return ctx.runUntilValue();
      }
    throw new WrongType(this, WrongType.ARG_UNKNOWN, null);
  }

    @Override
    public int isApplicable(Type[] args, Type restType)
  {
    int best = -1;
    for (int i = count;  --i >= 0; )
      {
        MethodProc method = methods[i];
        int result = method.isApplicable(args, restType);
        if (result == 1)
          return 1;
        if (result == 0)
          best = 0;
      }
    return best;
  }

  public int match0 (CallContext ctx)
  {
    if (count == 1)
      return methods[0].match0(ctx);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match0(ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public int match1 (Object arg1, CallContext ctx)
  {
    if (count == 1)
      return methods[0].match1(arg1, ctx);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match1(arg1, ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public int match2 (Object arg1, Object arg2, CallContext ctx)
  {
    if (count == 1)
      return methods[0].match2(arg1, arg2, ctx);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match2(arg1, arg2, ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public int match3 (Object arg1, Object arg2, Object arg3, CallContext ctx)
  {
    if (count == 1)
      return methods[0].match3(arg1, arg2, arg3, ctx);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match3(arg1, arg2, arg3, ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public int match4 (Object arg1, Object arg2, Object arg3, Object arg4,
		     CallContext ctx)
  {
    if (count == 1)
      return methods[0].match4(arg1, arg2, arg3, arg4, ctx);
    for (int i = 0;  i < count;  i++)
      {
        MethodProc method = methods[i];
	int code = method.match4(arg1, arg2, arg3, arg4, ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public int matchN (Object[] args, CallContext ctx)
  {
    if (count == 1)
      return methods[0].matchN(args, ctx);
    int alen = args.length;
    Type[] atypes = new Type[alen];
    Language language = Language.getDefaultLanguage();
    // As a rough approximation of finding the "best match", and also
    // an approximation of what we do when selecting a method at compile-time,
    // let's make a pre-pass to check which methods are applicable.
    for (int j = 0;  j < alen;  j++)
      {
        Object arg = args[j];
        Type atype;
        if (arg == null)
          atype = Type.nullType;
        else
          {
            Class aclass = arg.getClass();
            if (language != null)
              atype = language.getTypeFor(aclass);
            else
              atype = Type.make(aclass);
          }
        atypes[j] = atype;
      }
    int[] codes = new int[count];
    int defCount = 0;
    int maybeCount = 0;
    int bestIndex = -1;
    for (int i = 0;  i < count;  i++)
      {
          int code = methods[i].isApplicable(atypes, null);
        if (defCount == 0 && code >= 0)
          bestIndex = i;
        if (code > 0)
          defCount++;
        else if (code == 0)
          maybeCount++;
        codes[i] = code;
      }
    if (defCount == 1 || (defCount == 0 && maybeCount == 1))
      return methods[bestIndex].matchN(args, ctx);
    for (int i = 0;  i < count;  i++)
      {
        int code = codes[i];
        if (code < 0 || (code == 0 && defCount > 0))
          continue;
        MethodProc method = methods[i];
	code = method.matchN(args, ctx);
	if (code == 0)
	  return 0;
      }
    ctx.proc = null;
    return NO_MATCH;
  }

  public void setProperty (Keyword key, Object value)
  {
    String name = key.getName();
    if (name == "name")
      setName(value.toString());
    else if (name == "method")
      add((MethodProc) value);
    else
      super.setProperty(key.asSymbol(), value);
  }

  public final void setProperties (Object[] args)
  {
    int alen = args.length;
    for (int i = 0;  i < alen;  i++)
      {
	Object arg = args[i];
	if (arg instanceof Keyword)
          setProperty((Keyword) arg, args[++i]);
	else
	  add((MethodProc) arg);
      }
  }

  /** Create a GenericProc from one or more methods, plus properties. */
  public static GenericProc make (Object[] args)
  {
    GenericProc result = new GenericProc();
    result.setProperties(args);
    return result;
  }

  public static GenericProc makeWithoutSorting (Object... args)
  {
    GenericProc result = new GenericProc();

    int alen = args.length;
    for (int i = 0;  i < alen;  i++)
      {
	Object arg = args[i];
	if (arg instanceof Keyword)
          result.setProperty((Keyword) arg, args[++i]);
	else
	  result.addAtEnd((MethodProc) arg);
      }

    return result;
  }
}
