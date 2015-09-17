// Copyright (c) 1997, 2008  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;
import java.util.zip.*;
import java.net.URL;

/** Application to read a ClassType from a DataInputStream (.class file).
 * 
 * To print out the contents of a class file foo.class, you can use
 * the class <code>dump</code> as an application:
 * <pre>
 * java gnu.bytecode.dump foo.class
 * </pre>
 * This will print out the constant pool, fields, methods, superclass,
 * and implemented interfaces of class <code>foo</code>.
 * It is useful for printing out more detailed information
 * than <code>javap</code> does.
 * 
 * @author Per Bothner
 */

public class dump extends ClassFileInput
{
  ClassTypeWriter writer;

  /**
   * @param str input stream, positioned just after the magic number
   */
  dump (InputStream str, ClassTypeWriter writer)
       throws IOException, ClassFormatError
  {
    super(str);
    this.ctype = new ClassType();
    readFormatVersion();

    readConstants();
    readClassInfo();
    readFields();
    readMethods();
    readAttributes(ctype);

    writer.print(ctype);
    writer.flush();
  }

  public ConstantPool readConstants () throws IOException
  {
    ctype.constants = super.readConstants();
    return ctype.constants;
  }

  public Attribute readAttribute (String name, int length, AttrContainer container)
    throws IOException
  {
    return super.readAttribute (name, length, container);
  }

  static int readMagic (InputStream in) throws IOException
  {
    int magic = 0;
    for (int j = 0;  j < 4;  j++)
      {
        int b = in.read();
        if (b < 0)
          break;
        magic = (magic << 8) | (b & 0xff);
      }
    return magic;
  }

  public static void process (InputStream in, String filename,
                              OutputStream out, int flags)
    throws IOException
  {
    process(in, filename, new ClassTypeWriter(null, out, flags));
  }

  public static void process (InputStream in, String filename,
                              Writer out, int flags)
    throws IOException
  {
    process(in, filename, new ClassTypeWriter(null, out, flags));
  }

  public static void process (InputStream in, String filename,
                              ClassTypeWriter out)
    throws IOException
  {
    InputStream inp = new BufferedInputStream(in);
    inp.mark(5);
    int magic = readMagic(inp);
    if (magic == 0xcafebabe)
      {
        out.print("Reading .class from ");
        out.print(filename);
        out.println('.');
        new dump(inp, out);
      }
    else if (magic == (('P' << 24) | ('K' << 16) | (3 << 8) | 4))
      {
        inp.reset();
        out.print("Reading classes from archive ");
        out.print(filename);
        out.println('.');
        ZipInputStream zin = new ZipInputStream(inp);
        ZipEntry zent;
        while ((zent = zin.getNextEntry()) != null)
          {
            String name = zent.getName();
            if (zent.isDirectory())
              {
                out.print("Archive directory: ");
                out.print(name);
                out.println('.');
              }
            else
              {
                out.println();
                magic = readMagic(zin);
                if (magic == 0xcafebabe)
                  {
                    out.print("Reading class member: ");
                    out.print(name);
                    out.println('.');
                    new dump(zin, out);
                  }
                else
                  {
                    out.print("Skipping non-class member: ");
                    out.print(name);
                    out.println('.');
                  }
              }
          }
        System.exit(-1);
      }
    else
      {
        System.err.println("File "+filename+" is not a valid .class file");
        System.exit(-1);
      }
  }

  /** Reads a .class file, and prints out the contents to System.out.
   * Very rudimentary - prints out the constant pool, and field and method
   * names and types, but only minimal attributes (i.e. no dis-assembly yet).
   * @param args One argument - the name of a .class file.
   */
  public static void main (String[] args)
  {
    int alen = args.length;
    ClassTypeWriter out = new ClassTypeWriter(null, System.out, 0);
    if (alen == 0)
      usage(System.err);
    for (int i = 0; i < alen; i++)
      {
        String filename = args[i];
        if (filename.equals("-verbose") || filename.equals("--verbose"))
          {
            out.setFlags(ClassTypeWriter.PRINT_VERBOSE);
            continue;
          }
        boolean isURL = uriSchemeSpecified(filename);
        InputStream in;
        try
          {
            if (isURL)
              {
                boolean isJarURL = filename.startsWith("jar:");
                if (isJarURL)
                  {
                    String part = filename.substring(4);
                    // If no URL scheme follows "jar:", then assume "file:".
                    if (! uriSchemeSpecified(part))
                      {
                        int excl = part.indexOf('!');
                        if (excl >= 0)
                          {
                            String filepart = part.substring(0, excl);
                            /* #ifdef JAVA5 */ 
                            filepart = new File(filepart).toURI().toURL().toString();
                            /* #else */
                            // filepart = new File(filepart).toURL().toString();
                            /* #endif */
                            filename = "jar:" + filepart + part.substring(excl);
                          }
                      }
                    // Allow "jar:xxxx!foo.bar.baz" -> "jar:xxxx!/foo/bar/baz.class"
                    if (part.indexOf("!/") < 0)
                      {
                        int excl = filename.lastIndexOf('!');
                        if (excl <= 0)
                          isJarURL = false;
                        else if (filename.indexOf('/', excl) < 0)
                          {
                            part = filename.substring(excl+1);
                            part = part.replace('.', '/');
                            filename = filename.substring(0, excl+1)
                              + '/' + part + ".class";
                          }
                      }
                  }
                try
                  {
                    URL url = new URL(filename);
                    try
                      {
                        in = url.openConnection().getInputStream();
                      }
                    catch (java.util.zip.ZipException e1)
                      {
                        if (isJarURL)
                          {
                            String filepart = url.getFile();
                            int sl = filepart.lastIndexOf('!');
                            if (sl > 0)
                              filepart = filepart.substring(0, sl);
                            try
                              {
                                new URL(filepart).openConnection().getInputStream();
                              }
                            catch (java.io.FileNotFoundException e2)
                              {
                                System.err.print("Jar File for URL ");
                                System.err.print(filepart);
                                System.err.println(" not found.");
                                System.exit(-1);
                              }
                          }
                        throw e1;
                      }
                  }
                catch (java.io.FileNotFoundException e1)
                  {
                    System.err.print("File for URL ");
                    System.err.print(filename);
                    System.err.println(" not found.");
                    System.exit(-1);
                    in = null;
                  }
                catch (java.util.zip.ZipException e1)
                  {
                    System.err.print("Error opening zip archive ");
                    System.err.print(filename);
                    System.err.println(" not found.");
                    e1.printStackTrace();
                    if (e1.getCause() != null)
                      e1.getCause().printStackTrace();
                    System.exit(-1);
                    in = null;
                  }
              }
            else
              {
                try
                  {
                    in = new FileInputStream(filename);
                  }
                catch (java.io.FileNotFoundException e1)
                  {
                    // If this a class name rather than a file name?
                    ClassLoader loader;
                    try
                      {
                        Class clas = ObjectType.getContextClass(filename);
                        loader = clas.getClassLoader();
                      }
                    catch (NoClassDefFoundError e2)
                      {
                        loader = ObjectType.getContextClassLoader();
                      }
                    catch (Throwable e2)
                      {
                        System.err.print("File ");
                        System.err.print(filename);
                        System.err.println(" not found.");
                        System.exit(-1);
                        loader = null;
                        in = null;
                      }
                    // Ok - we found a class - now find the class file.
                    String clfilename = filename.replace('.', '/') + ".class";
                    if (loader == null)
                      loader = ClassLoader.getSystemClassLoader();
                    try
                      {
                        java.net.URL resource = loader.getResource(clfilename);
                        in = resource.openConnection().getInputStream();
                        filename = resource.toString();
                      }
                    catch (Throwable ex)
                      {
                        System.err.print("Can't find .class file for class ");
                        System.err.print(filename);
                        System.err.print(" - ");
                        System.err.println(ex);
                        System.exit(-1);
                        in = null;
                      }
                  }
              }
            process(in, filename, out);
          }
        
        catch (java.io.IOException e)
          {
            e.printStackTrace();
            System.err.println("caught ");
            System.err.print(e);
            System.exit(-1);
          }
      }
  }

  /** Helper routine to get the scheme part of a URI.
   * The scheme part is "http:" or "file:" or "ftp:" most commonly.
   * This functions searches for the first ':' that doesn't follow a '/'.
   * @return The length of the scheme component, not counting the colon,
   * (or alternatively the index of the colon), or -1 if the is no scheme.
   *
   * Duplicates gnu.kawa.io.Path.uriSchemeLength, to make gnu.bytecode standalone.
   */
  static int uriSchemeLength (String uri)
  {
    int len = uri.length();
    for (int i = 0;  i < len;  i++)
      {
	char ch = uri.charAt(i);
	if (ch == ':')
	  return i;
        if (i == 0 ? ! Character.isLetter(ch)
            : (! Character.isLetterOrDigit(ch)
               && ch != '+' && ch != '-' && ch != '.'))
	  return -1;
      }
    return -1;
  }

  /** Tests if a URL has a scheme.
   * For convenience, we treat a 1-character "scheme" as an
   * MS-DOS-style "drive letter" - i.e. not a scheme.
   *
   * Duplicates gnu.kawa.io.Path.uriSchemeSpecified, to make gnu.bytecode
   * standalone.
   */
  static boolean uriSchemeSpecified (String name)
  {
    int ulen = uriSchemeLength(name);
    if (ulen == 1 && File.separatorChar == '\\')
      {
        char drive = name.charAt(0);
        return ! ((drive >= 'a' && drive <= 'z')
                  || (drive >= 'A' && drive <= 'Z'));
      }
    return ulen > 0;
  }

  public static void usage(PrintStream err)
  {
    err.println("Prints and dis-assembles the contents of JVM .class files.");
    err.println("Usage: [--verbose] class-or-jar ...");
    err.println("where a class-or-jar can be one of:");
    err.println("- a fully-qualified class name; or");
    err.println("- the name of a .class file, or a URL reference to one; or");
    err.println("- the name of a .jar or .zip archive file, or a URL reference to one.");
    err.println("If a .jar/.zip archive is named, all its.class file members are printed.");
    err.println();
    err.println("You can name a single .class member of an archive with a jar: URL,");
    err.println("which looks like: jar:jar-spec!/p1/p2/cl.class");
    err.println("The jar-spec can be a URL or the name of the .jar file.");
    err.println("You can also use the shorthand syntax: jar:jar-spec!p1.p2.cl");
    System.exit(-1);
  }
}
