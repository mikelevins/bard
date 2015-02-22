package kawa.lang;
import gnu.mapping.*;
import gnu.bytecode.ClassType;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.text.SourceMessages;

/** Procedure to read and compile and entire file.
 * Creates a .zip archive containing the resulting classes.
 * @author	Per Bothner
 */

public class CompileFile
{
  public static final Compilation read (String name, SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    try
      {
	InPort fstream = InPort.openFile(name);
	Compilation result = read(fstream, messages);
	fstream.close();
	return result;
      }
    catch (java.io.FileNotFoundException e)
      {
	throw new WrappedException("compile-file: file not found: " + name, e);
      }
    catch (java.io.IOException e)
      {
	throw new WrappedException("compile-file: read-error: " + name, e);
      }
  }

  public static final Compilation read (InPort port, SourceMessages messages)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    return Language.getDefaultLanguage().parse(port, messages, 0);
  }
}
