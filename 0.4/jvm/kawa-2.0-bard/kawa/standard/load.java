package kawa.standard;
import java.io.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.io.Path;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import gnu.lists.*;
import gnu.text.*;
import kawa.Shell;

public class load extends Procedure1or2 {
  boolean relative;

  public load (String name, boolean relative)
  {
    super(name);
    this.relative = relative;
  }

  public static final load load = new load("load", false);
  public static final load loadRelative = new load("load-relative", true);

  public final Object apply1 (Object arg1)
    throws Throwable
  {
    return apply2 (arg1, Environment.getCurrent ());
  }

  public final Object apply2 (Object name, Object arg2)
    throws Throwable
  {
    Environment env = (Environment) arg2;
    Path path = Path.valueOf(name);
    if (relative)
      {
        Path curPath = (Path) Shell.currentLoadPath.get();
        if (curPath != null)
          path = curPath.resolve(path);
      }
    InputStream fs;
    try
      {
        fs = path.openInputStream();
      }
    catch (java.io.FileNotFoundException e)
      {
        Class clas = null;
        if (! relative && name instanceof CharSequence)
          {
            try
              { 
                clas = Class.forName(name.toString());
              }
            catch (Exception e2)
              {
                // Handled below since clas==null
              }
          }
        if (clas == null)
          throw new RuntimeException ("cannot load "+e.getMessage());
        Shell.runClass(clas, env);
        return Values.empty;
      }
    try
      {
        Shell.runFile(fs, path, env, true, 0);
	return Values.empty;
      }
    catch (SyntaxException ex)
      {
	throw new RuntimeException("load: errors while compiling '"+
				   name+"':\n"+ex.getMessages().toString(20));
      }
  }
}
