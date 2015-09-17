package gnu.bytecode;
import java.util.Hashtable;
import java.net.URL;
import java.io.*;

/** Load classes from a set of byte arrays.
 * @author	Per Bothner
 */

public class ArrayClassLoader extends ClassLoader
{
  /** Map String to union(byte[], ClassType). */
  Hashtable map = new Hashtable(100);
  /** Map String to Class. */
  Hashtable cmap = new Hashtable(100);

  /** If non-null, context to use for finding resources. */
  URL context;

  public ArrayClassLoader ()
  {
  }

  public ArrayClassLoader (ClassLoader parent)
  {
    super(parent);
  }

  /** Get base URL to use for finding resources, or null if none is set. */
  public URL getResourceContext () { return context; }

  /** Set base URL to use for finding resources. */
  public void setResourceContext (URL context) { this.context = context; }

  /** Load classes from the given byte arrays.
    By convention, the classes we manage are named "lambda"+<INTEGER>. */
  public ArrayClassLoader (byte[][] classBytes)
  {
    for (int i = classBytes.length;  --i >= 0; )
      addClass("lambda" + i, classBytes[i]);
  }

  public ArrayClassLoader (String[] classNames, byte[][] classBytes)
  {
    for (int i = classBytes.length;  --i >= 0; )
      addClass(classNames[i], classBytes[i]);
  }

  public void addClass(Class clas)
  {
    cmap.put(clas.getName(), clas);
  }

  public void addClass(String name, byte[] bytes)
  {
    map.put(name, bytes);
  }

  public void addClass (ClassType ctype)
  {
    map.put(ctype.getName(), ctype);
  }

  public InputStream getResourceAsStream(String name)
  {
    InputStream in = super.getResourceAsStream(name);
    if (in == null && name.endsWith(".class"))
      {
        String cname = name.substring(0, name.length()-6).replace('/', '.');
        Object r = map.get(cname);
        if (r instanceof byte[])
          return new ByteArrayInputStream((byte[]) r);
      }
    return in;
  }

  protected URL findResource(String name)
  {
    if (context != null)
      {
        try
          {
            URL url = new URL(context, name);
            url.openConnection().connect();
            return url;
          }
        catch (Exception ex)
          {
            // Fall through ...
          }
      }
    return super.findResource(name);
  }

  public Class loadClass (String name, boolean resolve)
    throws ClassNotFoundException
  {
    Class clas = loadClass(name);
    if (resolve)
      resolveClass(clas);
    return clas;
  }

    /** Load named class.
     * Note we deliberately don't follow the Java2 delegation model,
     * in order to allow classes to be overridden and replaced.
     * Specifically, we depend on this for the "session class-loader".
     */
    public Class loadClass (String name)
        throws ClassNotFoundException {
        Object r = cmap.get(name);
        if (r != null)
            return (Class) r;
        synchronized (this) {
            r = map.get(name);
            if (r instanceof ClassType) {
                ClassType ctype = (ClassType) r;
                if (ctype.isExisting())
                    r = ctype.reflectClass;
                else
                    r = ctype.writeToArray();
            }
            if (r instanceof byte[]) {
                byte[] bytes = (byte[]) r;
                Class clas = defineClass(name, bytes, 0, bytes.length);
                cmap.put(name, clas);
                return clas;
            }
            else if (r == null)
                return getParent().loadClass(name);
            else
                return (Class) r;
        }
    }

  /* #ifdef JAVA2 */
  public static Package getContextPackage (String cname)
  {
    ClassLoader loader;
    try
      {
        loader = Thread.currentThread().getContextClassLoader();
        if (loader instanceof ArrayClassLoader)
          return ((ArrayClassLoader) loader).getPackage(cname);
      }
    catch (java.lang.SecurityException ex)
      {
      }
    return Package.getPackage(cname);
  }
  /* #endif */
}
