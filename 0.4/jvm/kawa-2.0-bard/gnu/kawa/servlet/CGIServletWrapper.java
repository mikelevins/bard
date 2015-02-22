// Copyright (c) 2002, 2013  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.servlet;
import java.util.*;
import java.io.*;
import java.net.*;
import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.descriptor.JspConfigDescriptor;

/** Wrapper class to allow a Servlet to be run as a CGI script.
 * Invoked as:
 *   java CGI_ARGS... gnu.kawa.servlet.CGIServletWrapper SERVLET
 * where CGI_ARGS... are properties set from CGI environment variables (for
 * example -DPATH_INFO="foo") and SERVLET is the name of the servlet class.
 * The cgi-wrapper program in ../../../bin can used do this.
 */

public class CGIServletWrapper
    implements ServletConfig {
    String servletName;
    Servlet servlet;
    CGIRequest request;
    CGIResponse response;
    String characterEncoding;

    static final Enumeration nullEnumeration
        = new gnu.lists.SeqPosition(gnu.lists.LList.Empty, 0, false);

    public static void main(String[] args) {
        try {
            CGIServletWrapper wrapper = new CGIServletWrapper();
            Class servClass = Class.forName(args[0]);
            wrapper.servletName = args[0];
            HttpServlet servlet = (HttpServlet) servClass.newInstance();
            wrapper.servlet = servlet;
            servlet.init(wrapper);
            CGIResponse response = wrapper.getResponse();
            servlet.service(wrapper.getRequest(), response);
            response.flushBuffer();
        } catch (Throwable ex) {
            ex.printStackTrace();
            System.exit(-1);
        }
    }

    // ServletConfig methods

    ServletContext context;

    public ServletContext getServletContext() {
        if (context == null)
            context = new CGIContext();
        return context;
    }

    public String getServletName() {
        return servletName;
    }
    public String getInitParameter(String name) {
        return null;  // FIXME
    }

    public Enumeration getInitParameterNames() {
        return nullEnumeration;
    }

    public class CGIRequest implements HttpServletRequest {

        String getCgiEnvVar(String name) {
            return System.getProperty(name);
        }

        public String getAuthType() {
            return getCgiEnvVar("AUTH_TYPE");
        }

        public javax.servlet.http.Cookie[] getCookies() {
            return null; // FIXME
        }

        public long getDateHeader(String str) {
            return -1; // FIXME
        }

        public String getHeader(String str) {
            return null; // FIXME
        }

        public Enumeration<String> getHeaders(String str) {
            return null; // FIXME
        }

        public Enumeration getHeaderNames() {
            return null; // FIXME
        }

        public int getIntHeader(String str) {
            return -1;  // FIXME
        }

        public String getMethod() {
            String method = getCgiEnvVar("REQUEST_METHOD");
            return method == null ? "GET" : method;
        }

        public String getPathInfo() {
            return getCgiEnvVar("PATH_INFO");
        }

        public String getPathTranslated() {
            return getCgiEnvVar("PATH_TRANSLATED");
        }

        public String getContextPath() {
            return ""; // FIXME
        }

        public String getQueryString() {
            return getCgiEnvVar("QUERY_STRING");
        }

        public String getRemoteUser() {
            return getCgiEnvVar("REMOTE_USER");
        }

        public boolean isUserInRole(String role) {
            return false; // FIXME
        }

        public java.security.Principal getUserPrincipal() {
            return null; // FIXME
        }

        public String getRequestedSessionId() {
            return null;  // FIXME
        }

        public Part getPart(java.lang.String name) {
            return null; // FIXME
        }

        public java.util.Collection<Part> getParts() {
            return null; // FIXME
        }

        public void login(String username, String password)
            throws ServletException {
            throw new ServletException();
        }

        public void logout() { // FIXME
        }

        public boolean authenticate(HttpServletResponse response) throws ServletException {
            throw new ServletException();
        }

        public boolean isRequestedSessionIdValid() {
            return false;  // FIXME
        }

        public boolean isRequestedSessionIdFromCookie() {
            return false;  // FIXME
        }

        public boolean isRequestedSessionIdFromURL() {
            return false;  // FIXME
        }

        public boolean isRequestedSessionIdFromUrl() {
            return false;  // FIXME
        }

        public HttpSession getSession(boolean b) {
            return null; // FIXME
        }

        public HttpSession getSession() {
            return null; // FIXME
        }

        public String getServletPath() {
            return getCgiEnvVar("SCRIPT_NAME");
        }

        public String getRequestURI() {
            String script = getServletPath();
            String path = getPathInfo();
            if (script == null)
                return path;
            if (path == null)
                return script;
            return script + '/' + path;
        }

        public StringBuffer getRequestURL() {
            // Copied from Tomcat 4.0:
            
            StringBuffer url = new StringBuffer(100);
            String scheme = getScheme();
            int port = getServerPort();
            if (port < 0)
                port = 80; // Work around java.net.URL bug
            url.append(scheme);
            url.append("://");
            url.append(getServerName());
            if ((scheme.equals("http") && (port != 80))
                || (scheme.equals("https") && (port != 443))) {
                url.append(':');
                url.append(port);
            }
            url.append('/');
            url.append(getRequestURI());
            return url;
        }

        public String getScheme() {
            return getServerPort() == 443 ? "https" : "http";  // FIXME
        }

        public String getRemoteAddr() {
            return getCgiEnvVar("REMOTE_ADDR");
        }

        public String getRemoteHost() {
            String host = getCgiEnvVar("REMOTE_HOST");
            return host != null ? host : getRemoteAddr();
        }

        public Locale getLocale() {
            return null; // FIXME
        }

        public java.util.Enumeration getLocales() {
            return null; // FIXME
        }

        public boolean isSecure() {
            return getServerPort() == 443; // FIXME
        }

        public RequestDispatcher getRequestDispatcher(String path) {
            return getServletContext().getRequestDispatcher(path);
        }

        public String getRealPath(String path) {
            return getServletContext().getRealPath(path);
        }

        public String getProtocol() {
            return getCgiEnvVar("SERVER_PROTOCOL");
        }

        public int getRemotePort() {
            return -1;  // FIXME
        }

        public String getLocalName() {
            return getServerName();  // is this resonable?
        }

        public String getLocalAddr() {
            try {
                return InetAddress.getByName(getLocalName()).getHostAddress();
            } catch (UnknownHostException ex) {
                return "127.0.0.1";
            }
        }

        public int getLocalPort() {
            return getServerPort();  // is this resonable?
        }

        public int getServerPort() {
            String port = getCgiEnvVar("SERVER_PORT");
            if (port != null) {
                try {
                    return Integer.parseInt(port);
                } catch (Exception ex) {
                }
            }
            return -1;
        }

        public String getServerName() {
            return getCgiEnvVar("SERVER_NAME");
        }

        public String getParameter(String name) {
            return null; // FIXME
        }

        public Enumeration getParameterNames() {
            return null; // FIXME
        }

        public String[] getParameterValues(String name) {
            return null; // FIXME
        }

        public java.util.Map getParameterMap() {
            return null; // FIXME
        }

        public java.io.BufferedReader getReader() {
            return null; // FIXME
        }

        public ServletInputStream getInputStream() {
            return null; // FIXME
        }

        public ServletContext getServletContext() {
            return CGIServletWrapper.this.getServletContext();
        }

        public AsyncContext startAsync() {
            throw new IllegalStateException();
        }

        public AsyncContext startAsync(ServletRequest servletRequest,
                                       ServletResponse servletResponse) {
            throw new IllegalStateException();
        }

        public boolean isAsyncStarted() {
            return false;
        }

        public boolean isAsyncSupported() {
            return false;
        }

        public AsyncContext getAsyncContext() {
            throw new IllegalStateException();
        }

        public DispatcherType getDispatcherType() {
            return DispatcherType.REQUEST;
        }

        java.util.Hashtable<String,Object> attributes;

        public Object getAttribute(String name) {
            return attributes == null ? null : attributes.get(name);
        }
        public Enumeration<String> getAttributeNames() {
            return attributes.keys();
        }
        public void setAttribute(String name, Object value) {
            if (value == null)
                removeAttribute(name);
            else {
                if (attributes == null)
                    attributes = new java.util.Hashtable();
                attributes.put(name, value);
            }
        }
        public void removeAttribute(String name) {
            if (attributes != null)
                attributes.remove(name);
        }
        public String getContentType() {
            return getCgiEnvVar("CONTENT_TYPE");
        }

        public int getContentLength() {
            String length = getCgiEnvVar("CONTENT_LENGTH");
            if (length != null) {
                try {
                    return Integer.parseInt(length);
                } catch (Exception ex) {
                }
            }
            return 0;
        }

        public String getCharacterEncoding() { return characterEncoding; }

        public void setCharacterEncoding(String enc) {
            characterEncoding = enc;
        }
    }

    public class CGIResponse
        extends ServletOutputStream
        implements HttpServletResponse {

        String sawContentType;
        byte buffer[] = null;
        int bufpos = 0;
        PrintStream out = System.out;

        ArrayList<String> headers = new ArrayList<String>();
        int statusCode = 0;
        String statusString;

        public void addCookie(Cookie cookie) {
            // FIXME
        }

        public boolean containsHeader(String str) {
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                if (headers.get(i).equals(str))
                    return true;
            return false;
        }

        public String encodeURL(String str) {
            return null;  // FIXME
        }
        public String encodeRedirectURL(String str) {
            return null;  // FIXME
        }
        public String encodeUrl(String str) {
            return null;  // FIXME
        }
        public String encodeRedirectUrl(String str) {
            return null;  // FIXME
        }

        public void sendError(int i, String str) {
            statusCode = i;
            statusString = str;
        }
        public void sendError(int i) {
            statusCode = i;
            statusString = null;
        }
        public void sendRedirect(String str) {
            // ignore FIXME
        }

        public void setDateHeader(String str, long l) {
            // ignore FIXME
        }
        public void addDateHeader(String str, long l) {
            // ignore FIXME
        }

        public void setHeader(String label, String value) {
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                if (headers.get(i).equals(label)) {
                    if (label.equalsIgnoreCase("Content-type"))
                        sawContentType = value;
                    headers.set(i+1, value);
                    break;
                }
            addHeader(label, value);
        }

        public void addHeader(String label, String value) {
            if (label.equalsIgnoreCase("Content-type"))
                sawContentType = value;
            headers.add(label);
            headers.add(value);
        }

        public void setIntHeader(String str, int i) {
            setHeader(str, Integer.toString(i));
        }

        public void addIntHeader(String str, int i) {
            addHeader(str, Integer.toString(i));
        }

        public void setStatus(int i) {
            statusCode = i;
            statusString = null;
        }

        public void setStatus(int i, String str) {
            statusCode = i;
            statusString = str;
        }

        public int getStatus() {
            return statusCode;
        }

        public String getContentType() {
            return sawContentType;
        }

        public String getHeader(String name) {
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                if (headers.get(i).equals(name))
                    return headers.get(i+1);
            return null;
        }

        public java.util.Collection<String> getHeaders(java.lang.String name) {
            ArrayList<String> result = new ArrayList<String>();
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                if (headers.get(i).equals(name))
                    result.add(headers.get(i+1));
            return result;
        }

        public java.util.Collection<String> getHeaderNames() {
            ArrayList<String> result = new ArrayList<String>();
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                result.add(headers.get(i));
            return result;
        }

        public Locale getLocale() {
            return null; // FIXME
        }

        public java.util.Enumeration getLocales() {
            return null; // FIXME
        }

        public void setLocale (java.util.Locale locale) {
            // FIXME
        }

        public void resetBuffer() {
            bufpos = 0;
        }

        public void reset() {
            // FIXME
            resetBuffer();
        }

        public boolean isCommitted() {
            return committed;
        }

        public void flushBuffer() throws IOException {
            committ();
            if (bufpos > 0)
                out.write(buffer, 0, bufpos);
            bufpos = 0;
        }

        public void setBufferSize(int size) {
            if (bufpos > 0 || committed)
                throw new IllegalStateException();
            buffer = new byte[size];
        }

        public int getBufferSize() {
            return buffer == null ? defaultBufferSize : buffer.length;
        }

        public void setContentType(String type) {
            setHeader("Content-Type", type);
        }

        public void setContentLength(int len) {
            setIntHeader("Content-Length", len);
        }

        public String getCharacterEncoding() { return characterEncoding; }

        public void setCharacterEncoding(String enc) {
            characterEncoding = enc;
        }

        private static final int defaultBufferSize = 2048;

        private void allocateBuffer() {
            if (buffer == null)
                buffer = new byte[defaultBufferSize];
        }

        public void write(int c) throws java.io.IOException {
            allocateBuffer();
            int bufsize = buffer.length;
            if (buffer.length == 0) {
                committ();
                out.write(c);
            } else {
                if (bufpos >= buffer.length)
                    flushBuffer();
                buffer[bufpos++] = (byte) c;
            }
        }

        public ServletOutputStream getOutputStream() {
            return this;
        }

        PrintWriter writer;

        public java.io.PrintWriter getWriter() {
            if (writer == null)
                writer = new PrintWriter(out);
            return writer;
        }

        boolean committed;

        private void printHeader(String label, String value) {
            out.print(label);
            out.print(": ");
            out.println(value); // FIXME - need to quote?
        }

        private void printHeaders() {
            if (statusCode != 0) {
                out.print("Status: ");
                out.print(statusCode);
                if (statusString != null) {
                    out.print(' ');
                    out.print(statusString);
                }
                out.println();
            }
            if (sawContentType == null)
                setContentType("text/html"); // FIXME
            int num = headers.size();
            for (int i = 0;  i < num;  i += 2)
                printHeader(headers.get(i).toString(),
                            headers.get(i + 1).toString());
            //  if (sawContentType == null) writeRaw("Content-Type: text/html"); FIXME
            out.println();
        }

        private void committ() throws IOException {
            if (! committed) {
                printHeaders();
                committed = true;
            }
        }


    }

    public class CGIContext implements ServletContext {

        public String getContextPath() {
            return ""; // FIXME
        }

        public ServletContext getContext(String path) {
            return null;
        }

        public int getMajorVersion() {
            return 3;
        }
        public int getMinorVersion() {
            return 0;
        }

        public int getEffectiveMajorVersion() {
            throw new UnsupportedOperationException();
        }

        public int getEffectiveMinorVersion() {
            throw new UnsupportedOperationException();
        }

        public String getMimeType(String file) {
            return null;
        }

        public java.util.Set getResourcePaths(String path) {
            return null;
        }
        public java.net.URL getResource(String path) {
            return null;
        }
        public java.io.InputStream getResourceAsStream(String path) {
            return null;
        }

        public RequestDispatcher getRequestDispatcher(String path) {
            return null; // FIXME
        }

        public RequestDispatcher getNamedDispatcher(String path) {
            return null;
        }

        public Servlet getServlet(String name) {
            return null;
        }

        public Enumeration getServlets() {
            return nullEnumeration;
        }
        public Enumeration getServletNames() {
            return nullEnumeration;
        }
        public void log(String message) { }
        public void log(Exception ex, String mgs) { }
        public void log(String msg, Throwable ex) { }

        public String getRealPath(String path) {
            return null; // FIXME
        }

        public String getServerInfo() {
            return "Kawa CGI/servlet wrapper";
        }

        public String getInitParameter(String name) {
            return null;  // FIXME
        }

        public Enumeration getInitParameterNames() {
            return nullEnumeration;
        }
        public boolean setInitParameter(String name,
                                        String value) {
            throw new UnsupportedOperationException();
        }

        java.util.Hashtable<String,Object> attributes;

        public Object getAttribute(String name) {
            return attributes == null ? null : attributes.get(name);
        }

        public Enumeration<String> getAttributeNames() {
            return attributes.keys();
        }

        public void setAttribute(String name, Object value) {
            if (value == null)
                removeAttribute(name);
            else {
                if (attributes == null)
                    attributes = new java.util.Hashtable();
                attributes.put(name, value);
            }
        }

        public void removeAttribute(String name) {
            if (attributes != null)
                attributes.remove(name);
        }

        public java.lang.String getServletContextName() {
            return null;
        }
    
        public ServletRegistration.Dynamic addServlet(String servletName,
                                                      String className) {
            throw new UnsupportedOperationException();
        }

        public ServletRegistration.Dynamic addServlet(String servletName,
                                                      Servlet servlet) {
            throw new UnsupportedOperationException();
        }

        public ServletRegistration.Dynamic addServlet(String servletName,
                                                      Class<? extends Servlet> servletClass) {
            throw new UnsupportedOperationException();
        }

        public <T extends Servlet> T createServlet(java.lang.Class<T> clazz)
            throws ServletException {
            throw new UnsupportedOperationException();
        }

        public ServletRegistration getServletRegistration(String servletName) {
            throw new UnsupportedOperationException();
        }

        public java.util.Map<java.lang.String,? extends ServletRegistration> getServletRegistrations() {
            throw new UnsupportedOperationException();
        }

        public FilterRegistration.Dynamic addFilter(String filterName,
                                                    String className) {
            throw new UnsupportedOperationException();
        }

        public FilterRegistration.Dynamic addFilter(String filterName,
                                                    Filter filter) {
            throw new UnsupportedOperationException();
        }

        public FilterRegistration.Dynamic addFilter(String filterName,
                                                    Class<? extends Filter> filterClass) {
            throw new UnsupportedOperationException();
        }

        public <T extends Filter> T createFilter(Class<T> clazz) {
            throw new UnsupportedOperationException();
        }

        public FilterRegistration getFilterRegistration(String filterName) {
            throw new UnsupportedOperationException();
        }

        public java.util.Map<java.lang.String,? extends FilterRegistration> getFilterRegistrations() {
            throw new UnsupportedOperationException();
        }

        public SessionCookieConfig getSessionCookieConfig() {
            throw new UnsupportedOperationException();
        }

        public void setSessionTrackingModes(java.util.Set<SessionTrackingMode> sessionTrackingModes) {
            throw new UnsupportedOperationException();
        }

        public java.util.Set<SessionTrackingMode> getDefaultSessionTrackingModes() {
            throw new UnsupportedOperationException();
        }

        public java.util.Set<SessionTrackingMode> getEffectiveSessionTrackingModes() {
            throw new UnsupportedOperationException();
        }

        public void addListener(java.lang.String className) {
            throw new UnsupportedOperationException();
        }

        public <T extends java.util.EventListener> void addListener(T t) {
            throw new UnsupportedOperationException();
        }

        public void addListener(java.lang.Class<? extends java.util.EventListener> listenerClass) {
            throw new UnsupportedOperationException();
        }

        public <T extends java.util.EventListener> T createListener(java.lang.Class<T> clazz) {
            throw new UnsupportedOperationException();
        }

        public JspConfigDescriptor getJspConfigDescriptor() {
            throw new UnsupportedOperationException();
        }

        public ClassLoader getClassLoader() {
            return servlet.getClass().getClassLoader();
        }

        public void declareRoles(java.lang.String... roleNames) {
            throw new UnsupportedOperationException();
        }
    }

    public CGIRequest getRequest() {
        if (request == null)
            request = new CGIRequest();
        return request;
    }

    public CGIResponse getResponse() {
        if (response == null)
            response = new CGIResponse();
        return response;
    }
}
