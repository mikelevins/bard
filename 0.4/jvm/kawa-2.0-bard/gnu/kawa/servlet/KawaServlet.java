// Copyright (c) 2001, 2010  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import javax.servlet.*;
import javax.servlet.http.*;
import gnu.mapping.*;
import gnu.xml.*;
import java.io.*;
import java.util.*;
import java.net.*;
import gnu.kawa.io.Path;

/** Generic HttpServlet wrapper that support the Kawa web page script APIs. */

public abstract class KawaServlet
extends HttpServlet
{
  static {
    if (HttpRequestContext.importServletDefinitions < 2)
      HttpRequestContext.importServletDefinitions = 2;
  }

  public void run(CallContext ctx) throws Throwable
  {
    throw new AbstractMethodError();
  }
  public void run(HttpRequestContext hctx, CallContext ctx) throws Throwable
  {
    run(ctx);
  }

   public void doPost (HttpServletRequest request,
		       HttpServletResponse response)
     throws ServletException, IOException
  {
    doGet(request, response);
  }

  public void doGet(HttpServletRequest request, HttpServletResponse response)
    throws ServletException, IOException
  {
    CallContext ctx = CallContext.getInstance();
    HttpRequestContext hctx = HttpRequestContext.instance.get();
    Context sctx;
    if (hctx instanceof Context)
      sctx = (Context) hctx;
    else
      {
        sctx = new Context();
        Context.setInstance(sctx);
      }
    sctx.init(this, request, response);
    ctx.consumer = sctx.getConsumer();
    // FIXME should path be relative to context?
    Path.setCurrentPath(Path.valueOf(request.getRequestURL().toString()));
    ctx.values = Values.noArgs;

    /* FIXME should use fluid binding - or ThreadLocation!
    gnu.expr.Language language = gnu.expr.Language.getDefaultLanguage();
    String lang = language.getName();
    Environment env = Environment.getCurrent();
    if (lang == "XQuery")
      {
	env.defineValue("request", request);
	env.defineValue("response", response);
	env.defineValue("servlet", this);
	env.defineValue("out", out);
      }
    else
      {
	env.defineValue("*request*", request);
	env.defineValue("*response*", response);
	env.defineValue("*servlet*", this);
	env.defineValue("*out*", out);
      }
    */

    try
      {
        ctx.consumer.startDocument();
	run(sctx, ctx);
        ctx.consumer.endDocument();
      }
    catch (Error ex)
      {
        throw ex;
      }
    catch (Throwable throwable)
      {
	// Clear partial output on an error.
	response.reset();
	if (throwable instanceof WrappedException)
	  {
	    Throwable cause = ((WrappedException) throwable).getCause();
	    if (cause != null)
	      throwable = cause;
	  }
	throw new ServletException(throwable);
      }
    finally
      {
        sctx.servlet = null;
        sctx.request = null;
        sctx.response = null;
      }
  }

  public static class Context extends HttpRequestContext
  {
    HttpServletRequest request;
    HttpServletResponse response;
    HttpServlet servlet;
    ServletConfig config;
    ServletContext context;
    Map<String,Object> requestParameters;

    public void init(HttpServlet servlet, HttpServletRequest request,
                     HttpServletResponse response)
    {
      if (response == null)
        throw new Error("init with null response");
      this.servlet = servlet;
      this.request = request;
      this.response = response;
      this.config = servlet.getServletConfig();
      this.context = config.getServletContext();
      this.statusCode = HTTP_OK;
      this.statusReasonPhrase = null;
      this.requestParameters = null;
      this.consumer = null; // FIXME should re-use
    }

    public static Context getInstance(String command)
    {
      HttpRequestContext hctx = instance.get();
      if (! (hctx instanceof KawaServlet.Context))
        throw new UnsupportedOperationException(command + " can only be called within servlet-supporting http-server");
      return (KawaServlet.Context) hctx;
    }

    public static HttpServletRequest getCurrentRequest()
    {
      return getInstance("request").getRequest();
    }

    public static HttpServletResponse getCurrentResponse()
    {
      return getInstance("response").getResponse();
    }

    public HttpServletRequest getRequest ()
    {
      return request;
    }

    public HttpServletResponse getResponse ()
    {
      return response;
    }

    public ServletConfig getServletConfig ()
    {
      return config;
    }

    public ServletContext getServletContext ()
    {
      return context;
    }

    public URI getRequestURI()
    {
      String path = getRequestPath();
      String query = getQueryString();
      try
        {
          return new URI(null, null, path, query, null);
        }
      catch (URISyntaxException ex)
        {
          throw new RuntimeException(ex);
        }
    }

    public String getContextPath ()
    {
      return request.getContextPath() + "/";
    }

    public String getQueryString()
    {
      return request.getQueryString();
    }

    public String getRequestMethod()
    {
      return request.getMethod();
    }

    public String getRequestPath()
    {
      return request.getRequestURI();
    }

    public String getRequestScheme ()
    {
      return request.getScheme();
    }

    public InetAddress getLocalHost ()
    {
      try
        {
          return InetAddress.getByName(request.getLocalName());
        }
      catch (Exception ex)
        {
          throw new RuntimeException(ex);
        }
    }

    public String getLocalIPAddress ()
    {
      return request.getLocalAddr();
    }

    public int getLocalPort()
    {
      return request.getLocalPort();
    }

    public String getRemoteIPAddress ()
    {
      return request.getRemoteAddr();
    }

    public InetAddress getRemoteHost ()
    {
      try
        {
          return InetAddress.getByName(request.getRemoteHost());
        }
      catch (Exception ex)
        {
          throw new RuntimeException(ex);
        }
    }

    public int getRemotePort()
    {
      return request.getRemotePort();
    }

    public StringBuffer getRequestURLBuffer ()
    {
      return request.getRequestURL();
    }

    public String getServletPath()
    {
      return request.getServletPath();
    }

    public String getPathTranslated ()
    {
      return request.getPathTranslated();
    }

    public Map<String,List<String>> getRequestParameters ()
    {
      Map<String,List<String>> map = new LinkedHashMap<String,List<String>>();
      java.util.Enumeration<String> e = request.getParameterNames();
      while (e != null && e.hasMoreElements())
        {
          String key = e.nextElement();
          String[] values = request.getParameterValues(key);
          int nvalues = values.length;
          if (nvalues == 0)
            continue;
          ArrayList<String> list = new ArrayList<String>(nvalues);
          for (int i = 0;  i < nvalues;  i++)
            list.add(values[i]);
          map.put(key, list);
        }
      return map;
    }

    public String getRequestHeader (String name)
    {
      return request.getHeader(name);
    }

    public Map<String,List<String>> getRequestHeaders ()
    {
      Map<String,List<String>> map = new LinkedHashMap<String,List<String>>();
      java.util.Enumeration<java.lang.String> e = request.getHeaderNames();
      while (e != null && e.hasMoreElements())
        {
          String key = e.nextElement();
          map.put(key, getRequestHeaders(key));
        }
      return map;
    }

    public List<String> getRequestHeaders (String name)
    {
      java.util.Enumeration<java.lang.String> e = request.getHeaders(name);
      List<String> list = new ArrayList();
      while (e.hasMoreElements())
        {
          list.add(e.nextElement());
        }
      return list;
    }

    public InputStream getRequestStream ()
    {
      try
        {
          return request.getInputStream();
        }
      catch (IOException ex)
        {
          throw new RuntimeException(ex);
        }
    }

    public OutputStream getResponseStream ()
    {
      try
        {
          return response.getOutputStream();
        }
      catch (IOException ex)
        {
          throw new RuntimeException(ex);
        }
    }

    public void setResponseHeader(String name, String value)
    {
      response.setHeader(name, value);
    }

    public void setContentType(String type)
    {
      response.setContentType(type);
    }

    public boolean reset (boolean headersAlso)
    {
      if (statusCode == STATUS_SENT)
        return false;
      try
        {
          if (headersAlso)
            response.reset();
          else
            response.resetBuffer();
        }
      catch (IllegalStateException ex)
        {
          return false;
        }
      return true;
    }

    public URL getResourceURL (String path)
    {
      path = normalizeToContext(path);
      if (path == null)
        return null;
      try
        {
          return context.getResource("/" + path);
        }
      catch (Exception ex)
        {
          return null;
        }
    }

    public Object getAttribute(String name)
    {
      return context.getAttribute(name);
    }

    public void setAttribute(String name, Object value)
    {
      context.setAttribute(name, value);
    }

    public void sendResponseHeaders(int reasonCode, String reasonPhrase, long responseLength)
      throws IOException
    {
      if (response == null)
        throw new Error("null response");
      if (responseLength >= 0 && responseLength <= Integer.MAX_VALUE)
        response.setContentLength((int) responseLength);
      //if (reasonCode >= 200 && reasonCode < 300)
      response.setStatus(reasonCode);
      //      else        response.sendError(reasonCode, reasonPhrase);
      statusCode = HttpRequestContext.STATUS_SENT;
    }

    public void log (String message)
    {
      context.log(message);
    }

    public void log (String message, Throwable ex)
    {
      context.log(message, ex);
    }
  }
}
