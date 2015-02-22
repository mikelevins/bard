// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see COPYING.

package gnu.kawa.servlet;
import java.io.*;
import java.net.*;
import java.util.*;
import gnu.expr.ModuleBody;
import gnu.mapping.*;
import gnu.lists.Consumer;
import gnu.kawa.io.Path;
import com.sun.net.httpserver.*;

/** Web server support glue built on JDK 6's built-in HttpServer.
 */

public class KawaHttpHandler
  implements HttpHandler
{
  int counter;
  Path resourceRoot;

  static {
    if (HttpRequestContext.importServletDefinitions == 0)
      HttpRequestContext.importServletDefinitions = 1;
  }

  public KawaHttpHandler(String resourceRoot)
  {
    this.resourceRoot = Path.valueOf(resourceRoot);
  }

  public KawaHttpHandler(Path resourceRoot)
  {
    this.resourceRoot = resourceRoot;
  }

  public void handle(HttpExchange t) throws IOException
  {
    KawaHttpHandler.Context hctx; 
    HttpRequestContext tctx = HttpRequestContext.instance.get();
    if (tctx instanceof KawaHttpHandler.Context)
      {
        hctx = (KawaHttpHandler.Context) tctx;
      }
    else
      {
        hctx = new KawaHttpHandler.Context();
        HttpRequestContext.setInstance(hctx);
      }
    hctx.setExchange(t, this);

    CallContext ctx = CallContext.getInstance();
    Consumer saveConsumer = ctx.consumer;
    ServletPrinter consumer = hctx.getConsumer();
    try
      {
        ctx.consumer = consumer;
        ctx.consumer.startDocument();

        KawaAutoHandler.run(hctx, ctx);
      }
    catch (Error ex)
      {
        throw ex;
      }
    catch (Throwable ex)
      {
        hctx.log("Caught an exception: ", ex);
        hctx.reset(true);
        hctx.setContentType("text/plain");
        if (hctx.statusCode != HttpRequestContext.STATUS_SENT)
          hctx.statusCode = 500;
        ctx.consumer.write("internal exception: ");
        ctx.consumer.writeObject(ex);
        ctx.consumer.write("\nSee logs for specifics.");
      }
    finally
      {
        ctx.consumer.endDocument();
        ctx.consumer = saveConsumer;
        t.close();
      }
  }

  public static HttpServer serverInstance;
  public static int serverBacklog = 0;
  public static HttpServer getServerInstance() throws IOException
  {
    if (serverInstance == null)
      serverInstance = HttpServer.create();
    return serverInstance;
  }
  public static void addAutoHandler(String uriRoot, String resourceRoot)
    throws IOException
  {
    HttpServer server = getServerInstance();
    int rlen = resourceRoot.length();
    if (rlen > 0 && resourceRoot.charAt(rlen-1) != '/')
      resourceRoot = resourceRoot + "/";
    if (uriRoot.length() == 0 || uriRoot.charAt(0) != '/')
      uriRoot = "/" + uriRoot;
    server.createContext(uriRoot, new KawaHttpHandler(resourceRoot));
  }
  public static void startServer(int port) throws IOException
  {
    HttpServer server = getServerInstance();
    server.bind(new InetSocketAddress(port), serverBacklog);
    server.setExecutor(null); // creates a default executor
    server.start();
  }

  public static class Context extends HttpRequestContext
  {
    KawaHttpHandler httpHandler;
    HttpExchange exchange;
    Headers requestHeaders;
    Headers responseHeaders;
    HttpContext context;
    Map<String,Object> attributes;
    URI requestURI;
    Map<String,List<String>> requestParameters;

    public void setExchange (HttpExchange exchange, KawaHttpHandler httpHandler)
    {
      this.statusCode = HTTP_OK;
      this.statusReasonPhrase = null;
      this.exchange = exchange;
      this.requestHeaders = exchange.getRequestHeaders();
      this.responseHeaders = exchange.getResponseHeaders();
      context = exchange.getHttpContext();
      requestURI = exchange.getRequestURI();
      requestParameters = null;
      this.httpHandler = httpHandler;
      consumer = null;
    }

    public URL getResourceURL (String path)
    {
      try
        {
          String p = path;
          Path root = httpHandler.resourceRoot;
          path = normalizeToContext(path);
          if (path == null)
            return null;
          Path rpath = root.resolve(path);
          if (! rpath.exists())
            return null;
          return rpath.toURL();
        }
      catch (Exception ex)
        {
          return null;
        }
    }

    public InputStream getRequestStream ()
    {
      return exchange.getRequestBody();
    }

    public OutputStream getResponseStream ()
    {
      return exchange.getResponseBody();
    }

    public boolean reset (boolean headersAlso)
    {
      if (statusCode == STATUS_SENT)
        return false;
      if (headersAlso)
        responseHeaders.clear();
      return consumer == null || consumer.reset(headersAlso);
    }

    public Map<String, List<String>> getRequestParameters ()
    {
      if (requestParameters == null)
        {
          requestParameters = new LinkedHashMap<String, List<String>>();
          try
            {
              parseQuery(requestURI.getRawQuery(), requestParameters);
              parsePostParameters(exchange, requestParameters);
            }
          catch (Exception ex)
            {
              log("caught "+ex+" in "+getClass().getName()+".getRequestParameters");
            }
        }
      return requestParameters;
    }

    public String getRequestHeader (String name)
    {
      return requestHeaders.getFirst(name);
    }

    public List<String> getRequestHeaders (String name)
    {
      return requestHeaders.get(name);
    }

    public Headers getRequestHeaders ()
    {
      return requestHeaders;
    }

    public URI getRequestURI()
    {
      return requestURI;
    }

    public String getContextPath ()
    {
      return context.getPath();
    }

    public String getPathTranslated ()
    {
      String path = getRequestPath();
      int npath = path.length();
      if (npath > 0 && path.charAt(npath-1) == '/')
        path = path.substring(0, --npath);
      if (npath > 0 && path.charAt(0) == '/')
        path = path.substring(1);
      Path root = httpHandler.resourceRoot;
      return root+path;
    }

    public String getRequestScheme ()
    {
      return context.getServer() instanceof HttpsServer ? "https" : "http";
    }

    public InetSocketAddress getLocalSocketAddress ()
    {
      return exchange.getLocalAddress();
    }

    public InetAddress getLocalHost ()
    {
      return exchange.getLocalAddress().getAddress();
    }

    public int getLocalPort()
    {
      return exchange.getLocalAddress().getPort();
    }

    public InetSocketAddress getRemoteSocketAddress ()
    {
      return exchange.getRemoteAddress();
    }

    public String getRemoteIPAddress ()
    {
      return getRemoteHost().getHostAddress();
    }

    public InetAddress getRemoteHost ()
    {
      return exchange.getRemoteAddress().getAddress();
    }

    public int getRemotePort()
    {
      return exchange.getRemoteAddress().getPort();
    }

    public String getRequestMethod()
    {
      return exchange.getRequestMethod();
    }

    public String getQueryString()
    {
      return requestURI.getQuery();
    }


    public void setResponseHeader(String name, String value)
    {
      responseHeaders.set(name, value);
    }

    public void setContentType(String type)
    {
      setResponseHeader("Content-Type", type);
    }

    public Object getAttribute(String name)
    {
      if (attributes == null)
        attributes = context.getAttributes();
      return attributes.get(name);
    }

    public void setAttribute(String name, Object value)
    {
      if (attributes == null)
        attributes = context.getAttributes();
      attributes.put(name, value);
    }

    public void sendResponseHeaders(int reasonCode, String reasonPhrase, long responseLength)
      throws IOException
    {
      if (responseLength <= 0)
        responseLength = responseLength < 0 ? 0 : -1;
      exchange.sendResponseHeaders(reasonCode, responseLength);
      statusCode = HttpRequestContext.STATUS_SENT;
    }

    /* The following two methods (parseQuery and parsePostParameters) were
     * donated for use in Kawa under the Kawa license by the original author
     * Leonardo Marcelino <leonardo.marcelino@gmail.com>, on April 1 2010.
     * The methods are taken from this blog posting:
     * http://leonardom.wordpress.com/2009/08/06/getting-parameters-from-httpexchange/
     * For Kawa, they were modified to be standalone static methods,
     * and I changed the map value type to List<String>.
     */
    @SuppressWarnings("unchecked")
    public static void parseQuery(String query, Map<String, List<String>> parameters)
      throws UnsupportedEncodingException
    {
      if (query != null)
        {
          String pairs[] = query.split("[&]");

          for (String pair : pairs)
            {
              String param[] = pair.split("[=]");

              String key = null;
              String value = null;
              if (param.length > 0)
                key = URLDecoder.decode(param[0],
                                        System.getProperty("file.encoding"));

              if (param.length > 1)
                value = URLDecoder.decode(param[1],
                                          System.getProperty("file.encoding"));

              List<String> values = parameters.get(key);
              if (values != null)
                {
                  values.add(value);
                }
              else
                {
                  ArrayList<String> list = new ArrayList<String>(1);
                  list.add(value);
                  parameters.put(key, list);
                }
            }
        }
    }

    /* See copyright note for parseQuery. */
    public static void parsePostParameters(HttpExchange exchange,
                                           Map<String,List<String>> parameters)
      throws IOException
    {
      if ("post".equalsIgnoreCase(exchange.getRequestMethod()))
        {
          @SuppressWarnings("unchecked")
            InputStreamReader isr =
            new InputStreamReader(exchange.getRequestBody(),"utf-8");
          BufferedReader br = new BufferedReader(isr);
          String query = br.readLine();
          parseQuery(query, parameters);
        }
    }

    public void log (String message)
    {
      System.err.println(message);
    }

    public void log (String message, Throwable ex)
    {
      System.err.println(message);
      ex.printStackTrace(System.err);
    }
  }
}
