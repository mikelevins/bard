;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          print.scm
;;;; Project:       Bard
;;;; Purpose:       printing Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "values/function-macros.scm")
(##include "values/type-macros.scm")


(define as-string (%make-function name: 'as-string))

(define (%defprinter type printer)
  (%function-add-method! as-string `(,type) printer))

(define (%as-string thing)
  (%apply as-string (list thing)))

(%defprinter Anything (lambda (val)(object->string val)))
(%defprinter <undefined> (lambda (val) "undefined"))
(%defprinter <null> (lambda (val) "nothing"))
(%defprinter <character> (lambda (val) (string-append "\\" (string val))))
(%defprinter <boolean> (lambda (val) (if val "true" "false")))
(%defprinter <keyword> (lambda (val) (string-append (keyword->string val) ":")))

(%defprinter <primitive-procedure>
             (lambda (val)
               (string-append "#<primitive-procedure "
                              (number->string (object->serial-number val)) ">")))

(%defprinter <input-stream>
             (lambda (val)
               (string-append "#<input-stream "
                              (number->string (object->serial-number val)) ">")))

(%defprinter <output-stream>
             (lambda (val)
               (string-append "#<output-stream "
                              (number->string (object->serial-number val)) ">")))

(%defprinter <frame>
             (lambda (val)
               (let loop ((keys (bard:keys val))
                          (outstr "{"))
                 (if (null? keys)
                     (string-append outstr "}")
                     (loop (cdr keys)
                           (let* ((val (%frame-get val (car keys))))
                             (string-append outstr " " (%as-string (car keys)) " " (%as-string val))))))))

(%defprinter <function>
             (lambda (val)
               (let ((fname (%function-name val)))
                 (if fname
                     (string-append "#<function "
                                    (object->string fname)
                                    " "
                                    (number->string (object->serial-number val))
                                    ">")
                     (string-append "#<an anonymous function " (number->string (object->serial-number val)) ">")))))

(%defprinter <method>
             (lambda (val)
               (let ((mname (%method-name val)))
                 (if mname
                     (string-append "#<method "
                                    (object->string mname)
                                    " "
                                    (number->string (object->serial-number val))
                                    ">")
                     (string-append "#<an anonymous method " (number->string (object->serial-number val)) ">")))))

(%defprinter <protocol>
             (lambda (val)
               (object->string (%protocol-name val))))

(%defprinter <singleton>
             (lambda (val)
               (string-append "#<singleton "
                              (%as-string (%singleton-value val))
                               ">")))

(%defprinter <type>
             (lambda (val)
               (cond
                ((%singleton? val) (string-append "#<singleton " (%as-string (%singleton-value val)) ">"))
                ((%primitive-type? val) (string-append "#<primitive-type " (%as-string (%primitive-type-name val)) ">"))
                ((%structure-type? val)  (string-append "#<structure-type " (%as-string (%structure-type-name val)) ">"))
                ((%protocol? val) (string-append "#<protocol " (%as-string (%protocol-name val)) ">"))
                (else (error "not a type" val)))))

(define (bard:print object #!optional (out (current-output-port)))
  (print port: out (%as-string object)))

(define (show x)
  (newline)
  (display (%as-string x))
  (newline))

;;; (show 4)
;;; (show (bard:undefined))
;;; (show '())
;;; (show #\C)
;;; (show (bard:true))
;;; (show (bard:false))
;;; (show 'Foo)
;;; (show FooBar:)
;;; (show +)
;;; (show *)
;;; (show (current-input-port))
;;; (show (current-output-port))
;;; (show (%make-frame '()))
;;; (show (%make-frame '(name: "Fred" age: 101)))
;;; (show as-string)
;;; (show (%make-function))
;;; (show Anything)
;;; (show (%singleton 5))


