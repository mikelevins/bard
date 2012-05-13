;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          error.scm
;;;; Project:       Bard
;;;; Purpose:       displaying errors
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (error->string err)
  (cond
   ((error-exception? err) (error-exception-message err))
   ((heap-overflow-exception? err) "Heap overflow")
   ((stack-overflow-exception? err) "Stack overflow")
   ((os-exception? err) (string-append "OS exception: " (object->string (os-exception-code err))))
   ((unbound-os-environment-variable-exception? err)
    (string-append "Unbound OS environment variable; "
                   "procedure: " (object->string (unbound-os-environment-variable-exception-procedure err))
                   " arguments: " (object->string (unbound-os-environment-variable-exception-arguments err))))
   ((no-such-file-or-directory-exception? err) 
    (let* ((args (no-such-file-or-directory-exception-arguments err))
           (path (if (null? args) "<Unknown path>" (car args))))
      (string-append "No such file or directory: " (object->string path))))
   ((scheduler-exception? err) "Scheduler exception")
   ((deadlock-exception? err) "Deadlock exception")
   ((abandoned-mutex-exception? err) "Abandoned mutex")
   ((join-timeout-exception? err) "Join timeout")
   ((started-thread-exception? err) "Thread already started")
   ((terminated-thread-exception? err) "Thread has already terminated")
   ((uncaught-exception? err) "Uncaught exception")
   ((cfun-conversion-exception? err) "error converting a foreign value")
   ((sfun-conversion-exception? err) "error converting to a foreign value")
   ((multiple-c-return-exception? err) "error returning from a foreign procedure")
   ((datum-parsing-exception? err) 
    (string-append "error parsing a datum: " (object->string (datum-parsing-exception-kind err)) " "
                   (object->string (datum-parsing-exception-parameters err))))
   ((expression-parsing-exception? err) 
    (string-append "parse error parsing an expression: " (object->string (expression-parsing-exception-kind err)) " "
                   (object->string (expression-parsing-exception-parameters err))))
   ((unbound-global-exception? err) (string-append "unbound global variable: "
                                                   (object->string (unbound-global-exception-variable err))))
   ((type-exception? err)
    (string-append "type error parsing an expression: " (object->string (type-exception-procedure err)) " "
                   (object->string (type-exception-arguments err))))
   ((range-exception? err) "argument out of range")
   ((divide-by-zero-exception? err) "divide by zero")
   ((improper-length-list-exception? err) "list is the wrong length")
   ((wrong-number-of-arguments-exception? err) "wrong number of arguments")
   ((number-of-arguments-limit-exception? err) "too many arguments")
   ((nonprocedure-operator-exception? err) "operator is not a procedure")
   ((unknown-keyword-argument-exception? err) "unknown keyword argument")
   ((keyword-expected-exception? err) "keyword argument expected")
   ((noncontinuable-exception? err) "noncontinuable exception")
   (else "unrecognized error")))

(define (display-error err)
  (let ((msg (error->string err)))
    (display (string-append (string #\newline) "ERROR: " msg " " (string #\newline)))))

