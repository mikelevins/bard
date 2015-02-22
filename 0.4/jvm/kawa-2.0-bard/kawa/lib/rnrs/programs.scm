(module-name <kawa.lib.rnrs.programs>)
(module-export command-line exit emergency-exit)
(require <kawa.lib.prim_syntax>)

(define (command-line) :: list
  (let* ((rest ; Actual arguments, not including "argv[0]" equivalent.
          (gnu.lists.LList:makeList
           gnu.expr.ApplicationMainSupport:commandLineArgArray 0))
         (command-name ;; Explicitly specified "argv[0]" equivalent
          ::java.lang.String
          (gnu.expr.ApplicationMainSupport:commandName:get #!null))
         (arg0
          (if (eq? command-name #!null)
              (let ((command
                     ::java.lang.String
                     (try-catch
                      ;; No explicitly passed-in argv[0].
                      ;; Try looking at entire command line.
                      (let* ((raw1 (java.lang.System:getProperty
                                    "kawa.command.line"))
                             (raw (if (eq? raw1 #!null)
                                      (let ((raw2
                                             (java.lang.System:getProperty
                                              "sun.java.command")))
                                        (if (eq? raw2 #!null) #!null
                                            ("java ":concat raw2)))
                                      raw1)))
                        (if (eq? raw #!null) #!null
                            ;; Strip off the tail of the property value that
                            ;; duplicates the rest value.
                            (let* ((frest (format #f "~{ ~a~}" rest))
                                   (rlen (raw:length))
                                   (flen (frest:length))
                                   (alen (- rlen flen)))
                              (cond ((= flen 0) raw)
                                    ;; Sanity check
                                    ((and (>= alen 0)
                                          ((raw:substring alen):equals frest))
                                     (raw:substring 0 alen))
                                    (else
                                     #!null)))))
                      (exp java.lang.Throwable #!null))))
                (if (eq? command #!null) "kawa" command))
              command-name)))
    (cons arg0 rest)))

(define (exit #!optional (code 0)) :: void
  (let ((status :: int
		(cond ((integer? code) code)
		      (code 0)
		      (else -1))))
    (gnu.kawa.util.ExitCalled:doExit status)))

(define (emergency-exit #!optional (code 0)) :: void
  (let ((status :: int
		(cond ((integer? code) code)
		      (code 0)
		      (else -1))))
    ((java.lang.Runtime:getRuntime):halt status)))
