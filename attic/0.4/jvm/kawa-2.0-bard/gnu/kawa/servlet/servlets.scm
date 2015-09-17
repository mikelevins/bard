(module-export
 ;; imported from HTTP.scm:
 response-header response-content-type response-status
 request-method request-scheme error-response
 request-local-socket-address request-local-IP-address
 request-local-port request-local-host
 request-remote-socket-address request-remote-IP-address
 request-remote-port request-remote-host
 request-header request-header-map request-URI
 request-context-path request-script-path request-local-path
 request-path request-uri request-url request-path-translated
 request-query-string request-parameter request-parameters
 request-parameter-map
 ;; Local definitions:
 current-servlet current-servlet-context current-servlet-config
 servlet-context-realpath get-response get-request
 request-servlet-path request-path-info)

(require 'http)

(define (current-servlet) ::javax.servlet.http.HttpServlet
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet"):getServlet))

(define (current-servlet-context) ::javax.servlet.ServletContext
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet-context"):getServletContext))

(define (current-servlet-config) ::javax.servlet.ServletConfig
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "current-servlet-config"):getServletConfig))

(define (get-response) ::javax.servlet.http.HttpServletResponse
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "get-response"):getResponse))

(define (get-request) ::javax.servlet.http.HttpServletRequest
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "get-request"):getRequest))

(define (servlet-context-realpath #!optional (path ::String "")) ::String
  (((gnu.kawa.servlet.KawaServlet$Context:getInstance "servlet-context-realpath"):getServletContext):getRealPath))

(define (request-servlet-path) ::String
  ((gnu.kawa.servlet.KawaServlet$Context:getInstance "request-servlet-path"):getServletPath))

(define (request-path-info) ::String
  (((gnu.kawa.servlet.KawaServlet$Context:getInstance "request-path-info"):getRequest):getPathInfo))

  