package gnu.kawa.io;
import java.io.*;
import java.net.*;
import gnu.mapping.WrappedException; // FIXME - move to gnu.kawa.util

/** A Path that wraps a URL. */

public class URLPath extends URIPath
{
  final URL url;

  URLPath (URL url)
  {
    /* #ifdef use:java.net.URI */
    super(toUri(url));;
    /* #else */
    // super(url.toString());
    /* #endif */
    this.url = url;
  }

  public static URLPath valueOf (URL url)
  {
    return new URLPath(url);
  }

  public boolean isAbsolute ()
  {
    return true;
  }

  public long getLastModified ()
  {
    return getLastModified(url);
  }

  public static long getLastModified (URL url)
  {
    try
      {
        return url.openConnection().getLastModified();
      }
    catch (Exception ex)
      {
        return 0;
      }
  }

  public long getContentLength ()
  {
    return getContentLength(url);
  }

  public static int getContentLength (URL url)
  {
    try
      {
        return url.openConnection().getContentLength();
      }
    catch (Exception ex)
      {
        return -1;
      }
  }

  public URL toURL ()
  {
    return url;
  }

  /* #ifdef use:java.net.URI */
  public static URI toUri (URL url)
  {
    try
      {
        /* #ifdef JAVA5 */
        return url.toURI();
        /* #else */
        // return new URI(url.toString());
        /* #endif */
      }
    catch (Exception ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public URI toUri () { return toUri(url); }
  public String toURIString () { return url.toString(); }
  /* #else */
  // public String toUri () { return url.toString(); }
  // public String toURIString () { return uri.toString(); }
  /* #endif */

  public Path resolve (String relative)
  {
    try
      {
        return valueOf(new URL(url, relative));
      }
    catch (Exception ex)
      {
        throw WrappedException.wrapIfNeeded(ex);
      }
  }

  public static InputStream openInputStream (URL url) throws IOException
  {
    return url.openConnection().getInputStream();
  }

  public InputStream openInputStream () throws IOException
  {
    return openInputStream(url);
  }

  public static OutputStream openOutputStream (URL url) throws IOException
  {
    String str = url.toString();
    // Note JDK (upto 1.5.0, at least) throws an UnknownServiceException
    // "protocol doesn't support output" if you do getOutputStream on
    // a "file:" URL.  That seems lame, but let's avoid that!
    if (str.startsWith("file:"))
      {
        /* #ifdef use:java.net.URI */
        try { return new FileOutputStream(new File(new URI(str))); }
        catch (Exception ex) { }
        /* #else */
        // return new FileOutputStream(new File(str.substring(5)));
        /* #endif */
      }
    URLConnection conn = url.openConnection();
    conn.setDoInput(false);
    conn.setDoOutput(true);
    return conn.getOutputStream();
  }

  public OutputStream openOutputStream () throws IOException
  {
    return openOutputStream(url);
  }
}
