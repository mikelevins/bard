// Copyright (c) 1999, 2004  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.mapping;
import gnu.bytecode.Type;
import gnu.expr.Language;

/** Exception thrown when a procedure parameter has the wrong type. */

public class WrongType extends WrappedException
{
  /** Number of the argument, 1-origin.
   * <br>
   * Can be an integer >= 1, or one of the values <code>ARG_UNKNOWN</code>,
   * <code>ARG_VARNAME</code>, or <code>ARG_DESCRIPTION</code>.
   */
  public int number;

  /** <code>number==ARG_UNKNOWN</code> means unknown argument number. */
  public static final int ARG_UNKNOWN = -1;

  /** <code>number==ARG_VARNAME</code> means not a call,
   * <code>procname</code> is a variable name.*/
  public static final int ARG_VARNAME = -2;

  /** <code>number==ARG_DESCRIPTION</code> means not a call,
   * <code>procname</code> describes the target. (deprecated/unused) */
  public static final int ARG_DESCRIPTION = -3;

  /** <code>number==ARG_CAST</code> means a general cast. */
  public static final int ARG_CAST = -4;

  /** Name of <code>Procedure</code> that threw the exception (if non-null). */
  public String procname;

  /** The <code>Procedure</code> that threw the exception (if non-null). */
  public Procedure proc;

  /** The actual argument that was bad. */
  public Object argValue;

  /** The expected parameter type (a Type or TypeValue), or a string name/description. */
  public Object expectedType;

  public WrongType(String name, int n, String u)
  {
    super(null, null);
    procname = name;
    number = n;
    expectedType = u;
  }

  public WrongType(Procedure proc, int n, ClassCastException ex)
  {
    super(ex);
    this.proc = proc;
    this.procname = proc.getName();
    this.number = n;
  }

  public WrongType(ClassCastException ex, Procedure proc, int n,
                   Object argValue)
  {
    this(proc, n, ex);
    this.argValue = argValue;
  }

  public WrongType(Procedure proc, int n, Object argValue)
  {
    this.proc = proc;
    this.procname = proc.getName();
    this.number = n;
    this.argValue = argValue;
  }

  public WrongType(Procedure proc, int n, Object argValue, Type expectedType)
  {
    this.proc = proc;
    this.procname = proc.getName();
    this.number = n;
    this.argValue = argValue;
    this.expectedType = expectedType;
  }

  public WrongType(int n, Object argValue, Type expectedType)
  {
    this.number = n;
    this.argValue = argValue;
    this.expectedType = expectedType;
  }

  public WrongType(Procedure proc, int n, Object argValue, String expectedType)
  {
    this(proc.getName(), n, argValue, expectedType);
    this.proc = proc;
  }

  public WrongType(String procName, int n, Object argValue, String expectedType)
  {
    this.procname = procName;
    this.number = n;
    this.argValue = argValue;
    this.expectedType = expectedType;
  }

  public WrongType(String procname, int n, ClassCastException ex)
  {
    super(ex);
    this.procname = procname;
    this.number = n;
  }
 
  public WrongType (ClassCastException ex, String procname, int n,
                    Object argValue)
  {
    this(procname, n, ex);
    this.argValue = argValue;
  }

  /** @deprecated */
  public static WrongType make(ClassCastException ex, Procedure proc, int n)
  {
    return new WrongType(proc, n, ex);
  }

  /** @deprecated */
  public static WrongType make(ClassCastException ex, String procname, int n)
  {
    return new WrongType(procname, n, ex);
  }

  /** This interface is designed for a compact call sequence. */
  public static WrongType make(ClassCastException ex, Procedure proc, int n,
			       Object argValue)
  {
    WrongType wex = new WrongType(proc, n, ex);
    wex.argValue = argValue;
    return wex;
  }

  /** This interface is designed for a compact call sequence. */
  public static WrongType make(ClassCastException ex, String procname, int n,
			       Object argValue)
  {
    WrongType wex = new WrongType(procname, n, ex);
    wex.argValue = argValue;
    return wex;
  }

  @Override
  public String getMessage()
  {
    StringBuffer sbuf = new StringBuffer(100);
    if (number == ARG_DESCRIPTION)
      {
        sbuf.append(procname);
      }
    else if (number == ARG_CAST || number == ARG_VARNAME)
      {
        sbuf.append("Value");
      }
    else
      {
        sbuf.append("Argument ");
        if (number > 0)
          {
            sbuf.append('#');
            sbuf.append(number);
          }
      }
    if (argValue != null)
      {
	sbuf.append(" '");
	String argString = argValue.toString();
	if (argString.length() > 50)
	  {
	    sbuf.append(argString.substring(0, 47));
	    sbuf.append("...");
	  }
	else
	  sbuf.append(argString);
	sbuf.append("'");
      }
    if (procname != null && number != ARG_DESCRIPTION)
      {
        sbuf.append(number == ARG_VARNAME ? " for variable '" : " to '");
        sbuf.append(procname);
        sbuf.append("'");
      }
    sbuf.append(" has wrong type");
    if (argValue != null)
      {
	sbuf.append(" (");
        Class wrongClass = argValue.getClass();
        Language currentLang = Language.getDefaultLanguage();
        String wrongClassname = currentLang == null ? wrongClass.getName()
          : currentLang.formatType(currentLang.getTypeFor(wrongClass));
        sbuf.append(wrongClassname);
	sbuf.append(")");
      }
    Object expectType = expectedType;
    if (expectType == null && number > 0 && proc instanceof MethodProc)
      expectType = ((MethodProc) proc).getParameterType(number-1);
    if (expectType != null && expectType != Type.pointer_type)
      {
	sbuf.append(" (expected: ");
	if (expectType instanceof Type)
	  expectType = ((Type) expectType).getName();
	else if (expectType instanceof Class)
	  expectType = ((Class) expectType).getName();
	sbuf.append(expectType);
	sbuf.append(")");
      }
    Throwable ex = getCause();
    if (ex != null)
      {
	String msg = ex.getMessage();
	if (msg != null)
	  {
	    sbuf.append(" (");
	    sbuf.append(msg);
	    sbuf.append(')');
	  }
      }
    return sbuf.toString();
  }
}
