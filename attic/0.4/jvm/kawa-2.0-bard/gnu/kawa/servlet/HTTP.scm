(module-export response-header response-content-type response-status
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
	       request-input-stream request-input-port request-body-string)

(define (response-header key value)
  ((static-field <gnu.kawa.xml.MakeResponseHeader> 'makeResponseHeader)
   key value))

(define (response-content-type type)
  (response-header '|Content-Type| type))

(define (response-status (code :: <int>) #!optional (message :: <String>  #!null))
  (response-header '|Status|
		     (format (if (eq? message #!null) "~d " "~d ~a")
			     code message)))

;; For now the same as response-status, "Error" is the default for message.
(define (error-response (code :: <int>)
			#!optional (message :: <String>  "Error"))
  (response-header '|Status|
		     (format "~d ~a" code message)))

(define (request-method) :: <String>
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-method"):getRequestMethod))

(define (request-scheme) ::String
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-scheme"):getRequestScheme))

(define (request-local-socket-address)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-local-socket-address"):getLocalSocketAddress))

(define (request-local-IP-address)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-local-IP-address"):getLocalIPAddress))

(define (request-local-port)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-local-port"):getLocalPort))

(define (request-local-host)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-local-host"):getLocalHost))

(define (request-remote-socket-address)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-remote-socket-address"):getRemoteSocketAddress))

(define (request-remote-IP-address)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-remote-IP-address"):getRemoteIPAddress))

(define (request-remote-port)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-remote-port"):getRemotePort))

(define (request-remote-host)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-remote-host"):getRemoteHost))

(define (request-header name)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-header"):getRequestHeader name))

(define (request-header-map)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-header-map"):getRequestHeaders))

(define (request-URI) :: URI
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-URI"):getRequestURI))

(define (request-context-path) :: URI
  (URI ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-context-path"):getContextPath)))
(define (request-script-path) :: URI
  (URI ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-script-path"):getScriptPath)))
(define (request-local-path) :: URI
  (URI ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-local-path"):getLocalPath)))

(define (request-path) :: String
  (URI ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-path"):getRequestPath)))

;; Deprecated.
(define (request-uri) :: String
  (request-path))

(define (request-url) ::java.lang.StringBuffer
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-path"):getRequestURLBuffer))

(define (request-path-translated) ::String
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-path-translated"):getPathTranslated))

(define (request-query-string) ::object
  (let ((query ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-query-string"):getQueryString)))
    (if (eq? query #!null) #f query)))

(define (request-parameter (name :: <String>) #!optional (default #!null))
  :: <String>
  (let ((value :: <java.lang.String>
	       ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-parameter"):getRequestParameter name)))
    (if (eq? value #!null) default value)))

(define (request-parameters (name :: String))
  (let* ((instance (gnu.kawa.servlet.HttpRequestContext:getInstance "request-parameters"))
	 (plist :: java.util.List ((instance:getRequestParameters):get name)))
  (gnu.mapping.Values:make plist)))

(define (request-parameter-map)
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-parameter-map"):getRequestParameters))

(define (request-body-string) ::string
(let ((r
   ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-body-string"):getRequestBodyChars)))
  ;;(format (current-error-port) "request-body-string-> [~a]~%~!" r)
  r))

(define (request-input-stream) ::java.io.InputStream
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-input-stream"):getRequestStream))

(define (request-input-port) ::input-port
  ((gnu.kawa.servlet.HttpRequestContext:getInstance "request-input-port"):getRequestPort))
