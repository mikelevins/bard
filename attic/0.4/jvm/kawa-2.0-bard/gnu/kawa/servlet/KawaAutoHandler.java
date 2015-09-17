// Copyright (c) 2003, 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.servlet;
import gnu.expr.*;
import gnu.kawa.io.InPort;
import gnu.kawa.io.Path;
import gnu.kawa.io.URLPath;
import gnu.mapping.*;
import gnu.text.SourceMessages;
import gnu.text.SyntaxException;
import java.io.*;
import java.net.*;
import java.util.*;

/**
 * The generic framework for auto-configuring web page scripts.
 * This works using either a servlet (J2EE) engine, or using
 * the HttpServer built in to JDK 6.
 *
 * The implementation borrows ideas from Apache Jakarta Tomcat Jasper.
 *
 * @author Ivelin Ivanov
 * @author Tom Reilly
 * @author Per Bothner
 */

public class KawaAutoHandler
{
  /*private*/ static final String MODULE_MAP_ATTRIBUTE = "gnu.kawa.module-map";

  public static void run(HttpRequestContext hctx, CallContext ctx) throws Throwable
  {
    boolean saveClass = hctx.getRequestParameter("qexo-save-class") != null;
    Object mod = getModule(hctx, ctx, saveClass);

    if (mod instanceof ModuleBody)
      ((ModuleBody) mod).run(ctx);
  }

  public static Object getModule(HttpRequestContext hctx, CallContext ctx, boolean saveClass)
    throws Exception
  {
    String path = hctx.getRequestPath();
    // Adjust to relative to context.
    path = path.substring(hctx.getContextPath().length());
    Hashtable mmap
      = (Hashtable) hctx.getAttribute(MODULE_MAP_ATTRIBUTE);
    if (mmap == null)
      {
        mmap = new Hashtable();
        hctx.setAttribute(MODULE_MAP_ATTRIBUTE, mmap);
      }

    ModuleContext mcontext
      = (ModuleContext) hctx.getAttribute("gnu.kawa.module-context");
    if (mcontext == null)
      mcontext = ModuleContext.getContext();
    mcontext.addFlags(ModuleContext.IN_HTTP_SERVER);
    if (hctx.getClass().getName().endsWith("KawaServlet$Context"))
      mcontext.addFlags(ModuleContext.IN_SERVLET);
    ModuleInfo minfo = (ModuleInfo) mmap.get(path);
    long now = System.currentTimeMillis();
    ModuleManager mmanager = mcontext.getManager();
    // Avoid checking the disk too much
    if (minfo != null
        && now - minfo.lastCheckedTime < mmanager.lastModifiedCacheTime)
      return mcontext.findInstance(minfo);

    int plen = path.length();
    // Temporarily preset script-path to "" while doing getResourceURL.
    hctx.setScriptAndLocalPath("", "");
    // If the path matches a directory rather than a file, keep looking.
    URL url = (plen == 0 || path.charAt(plen-1) == '/') ? null
      : hctx.getResourceURL(path);
    Path absPath = url == null ? null : URLPath.valueOf(url);
    String upath = path;
    if (url == null || absPath.isDirectory())
      {
        if (url != null) // i.e. if path is a directory without final '/'.
          upath = path = path + '/';
        String xpath = path;
        for (;;)
          {
            int sl = xpath.lastIndexOf('/');
            if (sl < 0)
              break;
            xpath = xpath.substring(0, sl);
            upath = xpath + "/+default+";
            url = hctx.getResourceURL(upath);
            if (url != null)
              {
                hctx.setScriptAndLocalPath(path.substring(0, sl+1), path.substring(sl+1));
                absPath = URLPath.valueOf(url);
                break;
              }
          }
      }
    else
      {
        hctx.setScriptAndLocalPath(path, "");
      }

    if (absPath == null || absPath.isDirectory())
      {
        //	hctx.sendNotFound(path);
        String msg = "The requested URL "+path+" was not found on this server. "
          +hctx.getResourceURL("/")+"\r\n";
        byte[] bmsg = msg.getBytes();
        hctx.sendResponseHeaders(HttpRequestContext.HTTP_NOT_FOUND, null, bmsg.length);
        OutputStream out = hctx.getResponseStream();
        try
          {
            out.write(bmsg);
          }
        catch (IOException ex)
          {
            throw new RuntimeException(ex);
          }
	return null;
      }

    String urlString = url.toExternalForm();
    if (minfo == null || ! urlString.equals(minfo.getSourceAbsPathname()))
      minfo = mmanager.findWithURL(url);
    if (minfo.checkCurrent(mmanager, now))
      return mcontext.findInstance(minfo);

    mmap.put(path, minfo);

    InputStream resourceStream = absPath.openInputStream();
    if (! (resourceStream instanceof BufferedInputStream))
      // Not just a performance issue, since we need marks to be supported
      // for Language.detect below.
      resourceStream = new BufferedInputStream(resourceStream);

    Language language
      = Language.getInstanceFromFilenameExtension(path);
    if (language != null)
      hctx.log("Compile "+path+" - a "+language.getName()+" source file (based on extension)")
      ;
    else
      {
        language = Language.detect(resourceStream);
        if (language != null)
          hctx.log("Compile "+path+" - a "+language.getName()+" source file (detected from content)");
        else
          {
            if (path != upath)
              {
                String msg = "The requested URL "+path+" was not found on this server."
                  +" upath="+upath+".\r\n";
                byte[] bmsg = msg.getBytes();
                hctx.sendResponseHeaders(HttpRequestContext.HTTP_NOT_FOUND,null,  bmsg.length);
                OutputStream out = hctx.getResponseStream();
                try
                  {
                    out.write(bmsg);
                  }
                catch (IOException ex)
                  {
                    throw new RuntimeException(ex);
                  }
                return null;
              }

            String contentType = absPath.probeContentType();
            if (contentType != null)
                hctx.setContentType(contentType);

            // should set content length.
            long len = absPath.getContentLength();
            hctx.sendResponseHeaders(200, null, len);
            OutputStream out = hctx.getResponseStream();
            byte[] buffer = new byte[4*1024];
            for (;;)
              {
                int n = resourceStream.read(buffer);
                if (n < 0)
                  break;
                out.write(buffer, 0, n);
              }
            resourceStream.close();
            out.close();
            return null;
          }
      }
    InPort port = new InPort(resourceStream, absPath);
    Language.setCurrentLanguage(language);
    SourceMessages messages = new SourceMessages();
    Compilation comp;
    try
      {
        //comp = language.parse(port, messages, minfo);
        comp = language.parse(port, messages,
                              Language.PARSE_PROLOG|Language.PARSE_IMMEDIATE,
                              minfo);
        //comp.immediate = true;
        /*
        int dot = path.indexOf('.');
        if (dot < 0)
          dot = path.length();
        String name = path.substring(path.lastIndexOf('/')+1, dot);
        comp.getModule().setName(name);
        */
      }
    catch (SyntaxException ex)
      {
        if (ex.getMessages() != messages)
          throw ex;
        // Otherwise handled below ...
        comp = null; // Needed to avoid spurious compilation error.
      }

    Class cl = null;
    if (! messages.seenErrors())
      {
        ModuleExp mexp = comp.getModule();
        // FIXME - the env should probably be session-specific.
        Environment env = Environment.getCurrent();
        cl = (Class) ModuleExp.evalModule1(env, comp, url, null);
      }

    if (messages.seenErrors())
      {
        // FIXME: we could output a nice pretty HTML table of the errors
        // or show the script with the errors highlighted
        // In that case error message needs to be properly escaped,
        // and HTML headers added.
        String msg = "script syntax error:\n" + messages.toString(20); 
        ((ServletPrinter) ctx.consumer).addHeader("Content-type", "text/plain");
        hctx.sendResponseHeaders(500, "Syntax errors", -1);
        ctx.consumer.write(msg);
        minfo.cleanupAfterCompilation();
        return null;
      }

    minfo.setModuleClass(cl);

    //if (saveClass)  FIXME
    //  comp.outputClass(context.getRealPath("WEB-INF/classes")+'/');

    return mcontext.findInstance(minfo);
  }
}
