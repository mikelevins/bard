package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import java.io.*;

/**
 * Implement autoloading of Syntax (including macros).
 * A named class is loaded, and apply requests are forwarded to it.
 * @author	Per Bothner
 */

public class AutoloadSyntax extends Syntax implements Externalizable
{
  /** The name of the class that defines the macro/builtin.
   * It must be the name of a class in the CLASSPATH (for example:
   * "kawa.standard.list").  Either the class must extend Syntax
   * (and have a default constructor), or the class must be a ModuleMody,
   * (whose apply0 () is expected to define the Syntax in the
   * global environment. */
  String className;

  Environment env;

  /** The loaded syntax, or null if it has not yet been loaded. */
  Syntax loaded;

  public AutoloadSyntax ()
  {
  }

  public AutoloadSyntax (String name, String className)
  {
    super(name);
    this.className = className;
  }

  public AutoloadSyntax (String name, String className, Environment env)
  {
    super(name);
    this.className = className;
    this.env = env;
  }

  public void print(java.io.PrintWriter ps)
  {
    ps.print(toString());
  }

  public String toString ()
  {
    StringBuffer sbuf = new StringBuffer(100);
    sbuf.append("#<syntax ");
    if (getName() != null)
      {
	sbuf.append(getName());
	sbuf.append(' ');
      }
    if (loaded != null)
      sbuf.append("autoloaded>");
    else
      {
	sbuf.append("autoload ");
	sbuf.append(className);
	sbuf.append(">");
      }
    return sbuf.toString();
  }

  private void throw_error (String prefix)
  {
    throw new GenericError (prefix + className
				+ " while autoloading "
				+ (getName() == null ? "" : getName().toString()));
  }

  /** Load the class named in className. */
  void load ()
  {
    String name = this.getName();
    try
      {
	Object value = Class.forName (className).newInstance ();
        if (value instanceof Syntax)
	  {
	    loaded = (Syntax) value;	
	    if (name != null && loaded.getName() == null)
	      loaded.setName(name);
	  }
	else
	  throw_error ("failed to autoload valid syntax object ");
      }
    catch (ClassNotFoundException ex)
      {	throw_error ("failed to find class "); }
    catch (InstantiationException ex)
      { throw_error ("failed to instantiate class "); }
    catch (IllegalAccessException ex)
      { throw_error ("illegal access in class "); }
    catch (UnboundLocationException e)
      { throw_error ("missing symbol '" + e.getMessage () + "' "); }
    catch (WrongArguments ex)
      { throw_error ("type error"); }
  }

  public void scanForm (Pair st, ScopeExp defs, Translator tr)
  {
    if (loaded == null)
      {
	try
	  {
	    load ();
	  }
	catch (RuntimeException e)
	  {
	    tr.syntaxError(e.getMessage ());
            return;
	  }
      }
    loaded.scanForm(st, defs, tr);
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    if (loaded == null)
      {
	try
	  {
	    load ();
	  }
	catch (GenericError e)
	  {
	    return tr.syntaxError (e.getMessage ());
	  }
	catch (WrongType e)
	  {
	    return tr.syntaxError (e.getMessage ());
	  }
      }
    Syntax saveSyntax = tr.currentSyntax;
    tr.currentSyntax = loaded;
    try
      {
	return loaded.rewriteForm(form, tr);
      }
    finally
      {
	tr.currentSyntax = saveSyntax;
      }
  }

  public void writeExternal(ObjectOutput out) throws IOException
  {
    out.writeObject(getName());
    out.writeObject(className);
  }

  public void readExternal(ObjectInput in)
    throws IOException, ClassNotFoundException
  {
    setName((String) in.readObject());
    className = (String) in.readObject();
  }
}
