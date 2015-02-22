// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.servlet;

import java.io.*;
import java.net.*;
import java.util.*;
import gnu.lists.*;
import gnu.kawa.io.InPort;

/** A representation of an http request as it is being handled.
 * It abstracts over different http server's API - specially, there are are
 * concrete implementations on top of JDK6's com.sun.net.httpserver,
 * javax.servlet.http, and CGI (on top of servlets).
 */

public abstract class HttpRequestContext
{
  public static final int HTTP_OK = 200;
  public static final int HTTP_NOT_FOUND = 404;

  static final int STATUS_SENT = -999;
  public int statusCode = HTTP_OK;
  public String statusReasonPhrase = null;

  /** This is a bit of a kludge, to import servlet functions into Scheme.
   * (The kludge is that we really shouldn't be using a static.)
   * If importServletDefinitions == 1, then we're running in (or compiling for)
   * a web server context; if it is 2 we specifically support servlets.
   */
  public static int importServletDefinitions;

  protected static final ThreadLocal<HttpRequestContext> instance
    = new ThreadLocal<HttpRequestContext>();

  public static HttpRequestContext getInstance()
  {
    HttpRequestContext hctx = instance.get();
    if (hctx == null)
      throw new UnsupportedOperationException("can only be called by http-server");
    return hctx;
  }

  public static HttpRequestContext getInstance(String command)
  {
    HttpRequestContext hctx = instance.get();
    if (hctx == null)
      throw new UnsupportedOperationException(command + " can only be called within http-server");
    return hctx;
  }

  public static void setInstance (HttpRequestContext ctx)
  {
    instance.set(ctx);
  }

  public abstract InputStream getRequestStream();

  public InPort getRequestPort()
  {
    return new InPort(getRequestStream());
  }

  public String getRequestBodyChars ()
    throws IOException
  {
    InputStream is = getRequestStream();
    Reader reader = new InputStreamReader(is);
    int buflen = 1024;
    char[] buf = new char[buflen];
    int pos = 0;
    for (;;)
      {
        int avail = buflen - pos;
        if (avail <= 0)
          {
            char[] tmp = new char[2*buflen];
            System.arraycopy(buf, 0, tmp, 0, buflen);
            buf = tmp;
            buflen += buflen;
          }
        int count = reader.read(buf, pos, avail);
        if (count < 0)
          break;
        pos += count;
      }
    reader.close();
    String str = new String(buf, 0, pos);
    return str;
  }

  /** Return an OutputStream for the result body.
   * Multiple calls will return the same OutputStream.
   */
  public abstract OutputStream getResponseStream ();

  ServletPrinter consumer;

  public ServletPrinter getConsumer ()
    throws IOException
  {
    if (consumer == null)
      consumer = new ServletPrinter(this, 8192);
    return consumer;
  }

  /** Try to reset (delete) any response generated so far.
   * @param headersAlso if response headers should also be reset.
   * @return true on success, false if it's too late.
   */
  public abstract boolean reset (boolean headersAlso);

  public String getRequestParameter (String name)
  {
    List<String> p = getRequestParameters().get(name);
    return p == null || p.isEmpty() ? null : p.get(0);
  }
  public abstract Map<String, List<String>> getRequestParameters ();

  public abstract URI getRequestURI();

  /** Returns the context path, relative to the server root.
   * This is an initial substring of the {@link #getRequestPath}.
   * Like {@code ServletContext#getContextPath}, but ends with a {@code '/'}.
   * The string {@code getRequestURI()} is the same as the concatenation of
   * {@code getContextPath()}, {@code getScriptPath()},
   * and {@code getLocalPath()}.
   */
  public abstract String getContextPath ();

  /** Returns the path of the script, relative to the context.
   * Like {@code ServletRequestt#getServletPath}, but ends with a {@code '/'},
   * and does not start with one.  (The reason for this is to produce URIs
   * that work better with operations like resolve-uri.)
   */
  public String getScriptPath () { return scriptPath; }
  /** Returns the remainder of the request path, relative to the script.
   */
  public String getLocalPath () { return localPath; }

  String scriptPath = "", localPath = "";
  public void setScriptAndLocalPath (String scriptPath, String localPath)
  {
    this.scriptPath = scriptPath;
    this.localPath = localPath;
  }

  public abstract String getPathTranslated ();

  public String getRequestPath()
  {
    return getRequestURI().getPath();
  }

  public String getRequestScheme ()
  {
    return "http";
  }

  public InetSocketAddress getLocalSocketAddress ()
  {
    return new InetSocketAddress(getLocalHost(), getLocalPort());
  }

  public String getLocalIPAddress ()
  {
    return getLocalHost().getHostAddress();
  }

  public InetAddress getLocalHost ()
  {
    try
      {
        return InetAddress.getLocalHost();
      }
    catch (Exception ex)
      {
        throw new RuntimeException(ex);
      }
  }

  public abstract int getLocalPort();

  public InetSocketAddress getRemoteSocketAddress ()
  {
    return new InetSocketAddress(getRemoteHost(), getRemotePort());
  }

  public abstract InetAddress getRemoteHost ();
  public abstract String getRemoteIPAddress ();
  public abstract int getRemotePort ();

  public StringBuffer getRequestURLBuffer ()
  {
    StringBuffer sbuf = new StringBuffer();
    sbuf.append(getRequestScheme());
    sbuf.append("://");
    String host = getRequestHeader("Host");
    if (host != null)
      sbuf.append(host);
    else
      {
        sbuf.append(getLocalIPAddress());
        sbuf.append(':');
        sbuf.append(getLocalPort());
      }
    sbuf.append(getRequestPath());
    return sbuf;
  }

  public abstract String getQueryString();
  public abstract String getRequestMethod();

  public abstract String getRequestHeader (String name);
  public abstract List<String> getRequestHeaders (String name);
  public abstract Map<String,List<String>> getRequestHeaders ();

  public abstract void setResponseHeader(String name, String value);

  public void setContentType(String type)
  {
    setResponseHeader("Content-Type", type);
  }

  protected String normalizeToContext (String path)
  {
    if (path.length() > 0 && path.charAt(0) == '/')
      path = path.substring(1);
    else
      path = getScriptPath() + path;
    if (path.indexOf("..") >= 0)
      {
        path = URI.create(path).normalize().toString();
        if (path.startsWith("../"))
          return null;
      }
    return path;
  }

  /** Returns the URL of a resource.
   * The resource is relative to the script path, if the path is relative;
   * otherwise (if it starts with a {@code '/'} it is relative to the context path.
   */
  public abstract URL getResourceURL (String path);

  /** Get attribute from the server context. */
  public abstract Object getAttribute(String name);
  /** Set attribute in the server context. */
  public abstract void setAttribute(String name, Object value);

  /** Send headers.
   * @param reasonCode response code - e.g. 200 for OK.
   * @param reasonPhrase response string - e.g. "OK" or "Not Found".
   * @param responseLength response length in bytes, or -1 (unspecified).
   *  Note this is different from HttpExchange.sendResponseHeaders.
   * This method must be called before getResponseStream.
   * Implementations should set statusCode to STATUS_SENT.
   */
  public abstract void sendResponseHeaders(int reasonCode, String reasonPhrase, long responseLength)
    throws IOException;

  public void sendNotFound(String path)
    throws IOException
  {
    String msg = "The requested URL "+path+" was not found on this server.\r\n";
    byte[] bmsg = msg.getBytes();
    sendResponseHeaders(HTTP_NOT_FOUND, null, bmsg.length);
    OutputStream out = getResponseStream();
    try
      {
        out.write(bmsg);
      }
    catch (IOException ex)
      {
        throw new RuntimeException(ex);
      }
  }

  public abstract void log (String message);
  public abstract void log (String message, Throwable ex);
}
