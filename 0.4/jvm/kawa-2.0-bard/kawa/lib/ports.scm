(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)
(require <kawa.lib.strings>)
(require <kawa.lib.characters>)
(require <kawa.lib.numbers>)

(define (open-input-file (name :: path)) :: <input-port>
  (invoke-static <input-port> 'openFile name))

(define (open-binary-input-file (name :: path)) ::gnu.kawa.io.BinaryInPort
  (gnu.kawa.io.BinaryInPort:openFile name))

(define (open-output-file (name :: path)) :: <output-port>
  (invoke-static <output-port> 'openFile name))

(define (open-binary-output-file (name ::path)) ::gnu.kawa.io.BinaryOutPort
  (gnu.kawa.io.BinaryOutPort:openFile name))

(define (call-with-port (port ::java.io.Closeable) (proc ::procedure))
  (try-finally
   (proc port)
   (close-port port)))

(define (call-with-input-file (path :: path) (proc :: <procedure>))
 (let ((port :: <input-port> (open-input-file path)))
    (try-finally
     (proc port)
     (close-input-port port))))

(define (call-with-output-file (path :: path) (proc :: <procedure>))
  (let ((port :: <output-port> (open-output-file path)))
    (try-finally
     (proc port)
     (close-output-port port))))

(define (with-input-from-file (pathname :: path) (proc :: <procedure>))
  (let ((port :: <input-port> (gnu.kawa.io.InPort:openFile pathname))
	(save :: <input-port> (gnu.kawa.io.InPort:inDefault)))
    (try-finally
     (begin
       (gnu.kawa.io.InPort:setInDefault port)
       (proc))
     (begin
       (gnu.kawa.io.InPort:setInDefault save)
       (*:close port)))))       

(define (with-output-to-file (path :: path) (proc :: <procedure>))
  (let* ((port :: <output-port> (gnu.kawa.io.OutPort:openFile path))
	 (save :: <output-port> (gnu.kawa.io.OutPort:outDefault)))
    (try-finally
     (begin
       (gnu.kawa.io.OutPort:setOutDefault port)
       (proc))
     (begin
       (gnu.kawa.io.OutPort:setOutDefault save)
       (invoke port 'close)))))

(define (input-port? x) :: <boolean>
  (instance? x <input-port>))

(define (output-port? x) :: <boolean>
  (instance? x <output-port>))

(define (textual-port? obj) ::boolean
  (or (input-port? obj) (output-port? obj)))

(define (binary-port? obj) ::boolean
  (or (gnu.kawa.io.BinaryInPort? obj) (gnu.kawa.io.BinaryOutPort? obj)))

(define (port? x)
  (or (input-port? x) (output-port? x)))

(define (input-port-open? (port ::input-port))
  (port:isOpen))

(define (output-port-open? (port ::output-port))
  (port:isOpen))

(define-alias-parameter current-input-port <input-port>
  (static-field <input-port> 'inLocation))
(define-alias-parameter current-output-port <output-port>
  (static-field <output-port> 'outLocation))
(define-alias-parameter current-error-port <output-port>
  (static-field <output-port> 'errLocation))

(define-constant current-path ::gnu.mapping.LocationProc
  (gnu.mapping.LocationProc:makeNamed
   'current-path (object (gnu.mapping.ThreadLocation)
                         ((*init*)
                          (invoke-special gnu.mapping.ThreadLocation (this)
                                          '*init* 'current-path gnu.kawa.io.Path:pathLocation))
                         ((set new-value)::void
                          (invoke-special gnu.mapping.ThreadLocation (this)
                                          'set
                                          (gnu.kawa.io.Path:valueOf new-value))))))
	 
(define (write-char (ch ::character) #!optional
		    (port ::java.lang.Appendable
			  (invoke-static  <output-port> 'outDefault)))
  :: <void>
  (gnu.text.Char:append (as int ch) port))

(define (write-string (str ::string)
                      #!optional
                      (port ::java.lang.Appendable (current-output-port))
                      (start ::int 0)
                      (end ::int (str:length)))
  ::void
  (port:append str start end))

(define (write-u8 byte::int #!optional (port (current-output-port))) ::void
  ((gnu.kawa.io.BinaryOutPort:asOutputStream port):write byte))

(define (write-bytevector (bytes ::bytevector)
                          #!optional
                          (port (current-output-port))
                          (start ::int 0)
                          (end ::int (bytes:size)))
  ::void
  (bytes:writeTo start (- end start)
                 (gnu.kawa.io.BinaryOutPort:asOutputStream port)))

;; SRFI-6
(define (open-input-string (str ::string)) ::string-input-port
  (gnu.kawa.io.CharArrayInPort:make str))

(define (open-output-string) :: <string-output-port>
  (<string-output-port>))

(define (get-output-string (output-port  <string-output-port>))
  (<gnu.lists.FString> (output-port:toCharArray)))

(define (open-input-bytevector (bvector ::bytevector))
  ::gnu.kawa.io.BinaryInPort
  (let ((p (gnu.kawa.io.BinaryInPort (bvector:getBuffer) (bvector:size)
                                     "<bytevector>")))
    (p:setCharset "ISO-8859-1")
    p))

(define (open-output-bytevector) ::gnu.kawa.io.BinaryOutPort
  (let* ((bo (java.io.ByteArrayOutputStream))
         (out (gnu.kawa.io.BinaryOutPort bo "<bytevector>")))
        out))

(define (get-output-bytevector port::gnu.kawa.io.BinaryOutPort) ::bytevector
  (let ((bo ::java.io.ByteArrayOutputStream (port:getOutputStream)))
    (gnu.lists.U8Vector (bo:toByteArray))))

(define (call-with-input-string (str :: <string>) (proc :: <procedure>))
  (let* ((port ::string-input-port (gnu.kawa.io.CharArrayInPort:make str))
	 (result (proc port)))
    (close-input-port port)
    result))

(define (call-with-output-string (proc :: <procedure>))
  (let ((port :: <gnu.kawa.io.CharArrayOutPort>
	      (make <gnu.kawa.io.CharArrayOutPort>)))
    (proc port)
    (let ((chars :: <char[]> (invoke port 'toCharArray)))
      (invoke port 'close)
      (make <gnu.lists.FString> chars))))

(define (flush-output-port #!optional (port (current-output-port))) ::void
  (if (java.io.OutputStream? port)
      ((as java.io.OutputStream port):flush)
      ((as java.io.Writer port):flush)))

(define (force-output #!optional (port (current-output-port)))
  (if (java.io.OutputStream? port)
      ((as java.io.OutputStream port):flush)
      ((as java.io.Writer port):flush)))

(define (newline #!optional (port (current-output-port)))
  ((primitive-virtual-method <output-port> "println"
			     <void> ())
   port))

(define (eof-object? obj)
  validate-apply: "kawa.lib.compile_misc:isEofValidateApply"
  (eq? (gnu.mapping.Promise:force obj) #!eof))

(define (eof-object)
  #!eof)

(define (char-ready? #!optional (port (current-input-port)))
  (invoke-static <kawa.standard.char_ready_p> 'ready port))

(define (read-char #!optional (port (current-input-port)))
  ::character-or-eof
  (if (java.io.InputStream? port)
      (as character-or-eof
          (invoke (->java.io.InputStream port) 'read))
      (as character-or-eof
          (invoke (->gnu.kawa.io.InPort port) 'readCodePoint))))

(define (peek-char #!optional (port (current-input-port)))
  ::character-or-eof
  (if (java.io.InputStream? port)
      (let ((is ::java.io.InputStream port))
        (is:mark 1)
        (let ((ch (is:read)))
          (is:reset)
          (as character-or-eof ch)))
      (as character-or-eof
          (invoke (->gnu.kawa.io.InPort port) 'peekCodePoint))))

(define (read-string (k ::int)
                     #!optional (port ::input-port (current-input-port)))
  (let* ((arr (char[] length: k)))
    (let loop ((seen ::int 0))
      (let* ((m ::int (- k seen))
             (n (port:read arr seen m)))
        (cond ((< n 0)
               (if (> seen 0)
                   (gnu.lists.FString arr 0 seen)
                   #!eof))
              ((= n m) (gnu.lists.FString arr)) ;; resuse arr
              (else (loop (+ seen n))))))))

(define (read-u8 #!optional (port (current-input-port)))
  (let ((b ::int (if (gnu.kawa.io.BinaryInPort? port)
                     ((as gnu.kawa.io.BinaryInPort port):readByte)
                     ((as java.io.InputStream port):read))))
    (if (< b 0) #!eof b)))

(define (peek-u8 #!optional (port (current-input-port)))
  (let ((b ::int (if (gnu.kawa.io.BinaryInPort? port)
                     ((as gnu.kawa.io.BinaryInPort port):peekByte)
                     (let ((ins ::java.io.InputStream port))
                       (ins:mark 1)
                       (let ((b ::int (ins:read)))
                         (ins:reset)
                         b)))))
    (if (< b 0) #!eof b)))

(define (u8-ready? #!optional (port (current-input-port)))
  (if (gnu.kawa.io.BinaryInPort? port)
      ((as gnu.kawa.io.BinaryInPort port):ready)
      (> ((as java.io.InputStream port):available) 0)))
  
(define (read-bytevector (k ::int)
                     #!optional (port (current-input-port)))
  (let ((arr (byte[] length: k)))
    (let loop ((seen ::int 0))
      (let* ((m ::int (- k seen))
             (n ::int (if (gnu.kawa.io.BinaryInPort? port)
                          ((as gnu.kawa.io.BinaryInPort port):readBytes
                           arr seen m)
                          ((as java.io.InputStream port):read arr seen m))))
        (cond ((< n 0)
               (if (> seen 0)
                   (gnu.lists.U8Vector arr 0 seen) ;; copy seen bytes of arr
                   #!eof))
              ((= n m) (gnu.lists.U8Vector arr)) ;; reuse arr
              (else
               (loop (+ seen n))))))))

(define (read-bytevector! (bv ::bytevector)
                          #!optional
                          (port (current-input-port))
                          (start ::int 0)
                          (end ::int (bv:size)))
  (let loop ((seen ::int 0))
    (let* ((want ::int (- end start seen))
           (is ::java.io.InputStream
               (if (gnu.kawa.io.BinaryInPort? port)
                   ((as gnu.kawa.io.BinaryInPort port):getInputStream)
                   port))
           (n ::int (bv:readFrom (+ start seen) want is)))
      (cond ((< n 0)
             (if (> seen 0)
                 seen
                 #!eof))
            ((= n want) (+ seen n))
            (else
             (loop (+ seen n)))))))

(define (write-simple value #!optional (out ::output-port (current-output-port))) ::void
  (gnu.kawa.functions.DisplayFormat:schemeWriteSimpleFormat:format
   value out))

(define-private (%write-shared%
                 fmt::gnu.kawa.functions.DisplayFormat
                 value out::output-port) ::void
  (let ((pretty-out (out:getPrettyWriter)))
    (pretty-out:initialiseIDHash)
    (pretty-out:setSharing #t)
    (try-finally
     (fmt:format value out)
     (pretty-out:setSharing #f))
    (pretty-out:finishIDHash)))

(define (write
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          (if (eqv? *print-circle* #t)
              gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
              gnu.kawa.functions.DisplayFormat:schemeWriteFormat)
          value out))

(define (write-shared
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
          value out))

(define (write-with-shared-structure 
	 value #!optional (out ::output-port (current-output-port))) ::void
         (%write-shared%
          gnu.kawa.functions.DisplayFormat:schemeWriteSharedFormat
          value out))
    
(define (display value #!optional (out (current-output-port))) :: <void>
  (*:format gnu.kawa.functions.DisplayFormat:schemeDisplayFormat value out))

(define (input-port-read-state port)
  ((primitive-virtual-method <input-port> "getReadState" <char> ())
   port))

(define (set-port-line! (port ::input-port) (line ::int)) ::void
  (port:setLineNumber line))

(define-procedure port-line
  setter: set-port-line!
  (begin
    (define (port-line (port :: input-port))
      (invoke port 'getLineNumber))
    port-line))

(define (set-input-port-line-number! port num)
  (set-port-line! port (- num 1)))

(define-procedure input-port-line-number
  setter: set-input-port-line-number!
  (begin
    (define (input-port-line-number (port ::input-port))
      (+ 1 (port-line port)))
    input-port-line-number))

(define (port-column (port ::input-port)) ::int
  (port:getColumnNumber))

(define (input-port-column-number port)
  (+ 1 (port-column port)))

(define (default-prompter port)
  (let ((state (input-port-read-state port)))
    (if (char=? state #\Newline)
	""
	(string-append (if (char=? state #\Space)
			   "#|kawa:"
			   (string-append "#|" (make-string 1 state) "---:"))
		       (number->string (input-port-line-number port))
		       "|# "))))

(define (set-input-port-prompter!
	 (port :: <gnu.kawa.io.TtyInPort>) (prompter :: <procedure>))
  (invoke port 'setPrompter prompter))

(define-procedure input-port-prompter
  setter: set-input-port-prompter!
  (begin
    (define (input-port-prompter (port :: <gnu.kawa.io.TtyInPort>))
      (invoke port 'getPrompter))
    input-port-prompter))

(define (close-port (port ::java.io.Closeable))
  (port:close))

(define (close-input-port (port ::java.io.Reader))
  (port:close))

(define (close-output-port (port ::java.io.Writer))
  (port:close))

(define (read #!optional (port :: <input-port> (current-input-port)))
  (let ((lexer (gnu.kawa.lispexpr.LispReader:new port)))
    (lexer:setReturnMutablePairs #t)
    (try-catch
     (let ((result (lexer:readObject)))
       (if (lexer:seenErrors)
	   (primitive-throw
	    (gnu.text.SyntaxException:new (lexer:getMessages))))
       result)
     (ex <gnu.text.SyntaxException>
	 (ex:setHeader "syntax error in read:")
	 (primitive-throw ex)))))

(define (read-line #!optional
		   (port ::input-port (current-input-port))
		   (handling :: <symbol> 'trim))
  (kawa.standard.read_line:apply port handling))

(define (transcript-on filename) :: <void>
  (invoke-static <output-port> 'setLogFile (invoke filename 'toString)))

(define (transcript-off)
  (invoke-static <output-port> 'closeLogFile))
