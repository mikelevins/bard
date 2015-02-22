// Copyright (c) 2010  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ../../../COPYING.

package gnu.kawa.servlet;

import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import java.io.*;
import java.net.*;
import java.util.*;
import javax.servlet.*;
import javax.servlet.http.*;

/**
 * Handle auto-configuring web page scripts using a servlet engine.
 * This is a wrapper that forwards to {@link KawaAutoHandler}.
 */
public class KawaPageServlet extends KawaServlet
{
  private ServletContext context;

  public void init(ServletConfig config)
      throws ServletException
  {
    super.init(config);
    context = config.getServletContext();
  }

  public void run(HttpRequestContext hctx, CallContext ctx) throws Throwable
  {
    KawaAutoHandler.run(hctx, ctx);
  }
}
