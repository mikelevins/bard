package gnu.kawa.servlet;
import gnu.text.*;
import gnu.mapping.*;
import gnu.expr.*;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.*;
import javax.servlet.http.*;

/** The Servlet for a browser-base "read-eval-print-loop" session.
 * The session state and logic is in ReplSession. */

public class ReplServlet extends HttpServlet
{
  public void init(ServletConfig config) throws ServletException
  {
  }

  public void doPost(HttpServletRequest request, 
                     HttpServletResponse response)
    throws java.io.IOException
  {
    String command = request.getParameter("command");
    HttpSession hsession = request.getSession(true);
    ReplSession rsession = (ReplSession) hsession.getValue("repl");
    if (rsession == null)
      {
        rsession = new ReplSession();
        hsession.putValue("repl", rsession);
      }

    PrintWriter out = response.getWriter();
    String requestURI = request.getRequestURI();
    StringBuffer resultBuffer = new StringBuffer();
    resultBuffer.append("<?xml version=\"1.0\"?>\n");
    resultBuffer.append("<result xmlns=\"http://www.w3.org/1999/xhtml\">");
    java.io.BufferedReader in = request.getReader();
    StringBuffer sbuf = new StringBuffer();
    for (;;)
      {
        int ch = in.read();
        if (ch < 0)
          break;
        sbuf.append((char) ch);
      }
    String inputData  = sbuf.toString();
    if ("line".equals(command))
      rsession.appendInputLine(inputData);
    else if ("action".equals(command))
      rsession.appendInput(inputData);
    String result = rsession.waitOutput();
    resultBuffer.append(result);
    resultBuffer.append("</result>");

    response.setContentType("text/xml");
    response.setHeader("Pragma", "no-cache");
    response.setHeader("Expires", "0");
    response.setHeader("Cache-Control", "no-store");
		
    out.print(resultBuffer.toString());
    out.close();
  }
}
