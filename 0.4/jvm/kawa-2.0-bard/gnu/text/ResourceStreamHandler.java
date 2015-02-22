// Copyright (c) 2007  Per M.A. Bothner.
// This is free software; for terms and warranty disclaimer see ../../COPYING.

package gnu.text;
import java.io.*;
import java.net.*;

/** Handler for {@code "class-resource:"} URLs.
 * These are "indirect URLs" implemented using ClassLoader.getResource().
 * Their syntax is: either:
 * {@code class-resource:/PACKAGE/CLASS} or
 * {@code class-resource:/CLASS} or
 * {@code class-resource:/PACKAGE/RESOURCE} or
 * {@code class-resource:/RESOURCE}.
 * The former two are "base URLs" which need to be resolved.
 * The latter two are resolved resource names.
 */

public class ResourceStreamHandler extends URLStreamHandler
{
  /** A special URI-scheme for accessing resources relative to a ClassLoader.
   * The resource is found using ClassLoader's getResource method.
   * The actual ClassLoader is found using getClassLoaderForURI. */
  public static final String CLASS_RESOURCE_URI_PREFIX = "class-resource:/";

  /** The length of CLASS_RESOURCE_URI_PREFIX, including ":/". */
  public static final int CLASS_RESOURCE_URI_PREFIX_LENGTH = 16;

  ClassLoader cloader;

  public ResourceStreamHandler (ClassLoader cloader)
  {
    this.cloader = cloader;
  }

  public static URL makeURL (Class clas)
    throws java.net.MalformedURLException
  {
    String cname = clas.getName();
    int dot = cname.lastIndexOf('.');
    /* #ifdef JAVA5 */
    StringBuilder sbuf = new StringBuilder();
    /* #else */
    // StringBuffer sbuf = new StringBuffer();
    /* #endif */
    sbuf.append(CLASS_RESOURCE_URI_PREFIX);
    if (dot >= 0)
      {
        sbuf.append(cname.substring(0, dot));
        sbuf.append('/');
        cname = cname.substring(dot+1);
      }
    sbuf.append(cname);
    String str = sbuf.toString();
    ClassLoader loader = clas.getClassLoader();
    return new URL(null, str, new ResourceStreamHandler(loader));
  }

  public URLConnection openConnection (URL u) throws IOException
  {
    String ustr = u.toString();
    String rstr = ustr.substring(CLASS_RESOURCE_URI_PREFIX_LENGTH);
    int sl = rstr.indexOf('/');
    if (sl > 0)
      rstr = rstr.substring(0, sl).replace('.', '/') + rstr.substring(sl);
    URL url = cloader.getResource(rstr);
    if (url == null)
      throw new FileNotFoundException(ustr);
    return url.openConnection();
  }
}
