package gnu.bytecode;
import java.io.*;

/** Application that lists the number of bytes in named methods.
 * Useful for regression testing of code generation and inlining.
 */

public class ListCodeSize
{
  public static void usage()
  {
    System.err.println("Usage: class methodname ...");
    System.exit(-1);
  }

  static void print (Method method)
  {
    System.out.print(method);
    CodeAttr code = method.getCode();
    if (code == null)
      System.out.print(": no code");
    else
      {
        System.out.print(": ");
        System.out.print(code.getPC());
        System.out.print(" bytes");
      }
    System.out.println();
  }

  public static final void main (String[] args)
  {
    if (args.length == 0)
      usage();
    String filename = args[0];
    try
      {
	java.io.InputStream inp = new FileInputStream(filename);

        ClassType ctype = new ClassType();
        new ClassFileInput(ctype, inp);

        if (args.length == 1)
          {
            for (Method method = ctype.getMethods();  method != null;
                 method = method.getNext())
              {
                print(method);
              }
          }
        else
          {
            for (int i = 1;  i < args.length;  i++)
              {
                for (Method method = ctype.getMethods();  method != null;
                     method = method.getNext())
                  {
                    StringBuffer sbuf = new StringBuffer();
                    sbuf.append(method.getName());
                    method.listParameters(sbuf);
                    sbuf.append(method.getReturnType().getName());
                    if (sbuf.toString().startsWith(args[i]))
                      print(method);
                  }
              }
          }
      }
    catch (java.io.FileNotFoundException e)
      {
	System.err.println("File "+filename+" not found");
	System.exit(-1);
      }
    catch (java.io.IOException e)
      {
	System.err.println(e);
        e.printStackTrace();
	System.exit(-1);
      }
  }
}
