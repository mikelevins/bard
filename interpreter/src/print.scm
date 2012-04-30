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

(##include "values/type-macros.scm")
(##include "values/function-macros.scm")


(define as-string (%make-function name: 'as-string))

(define (%defprinter type printer)
  (%function-add-method! as-string `(,type) printer))

(define (%as-string thing)
  (%apply as-string (list thing)))

(%defprinter Anything (%primitive-method (val) (object->string val)))
(%defprinter <undefined> (%primitive-method (ignored) "undefined"))
(%defprinter <null> (%primitive-method (ignored) "nothing"))
(%defprinter <character> (%primitive-method (val)(object->string val)))
(%defprinter <boolean> (%primitive-method (val) (if val "true" "false")))
(%defprinter <keyword> (%primitive-method (val) (string-append (keyword->string val) ":")))

(%defprinter <primitive-procedure> 
             (%primitive-method (x) 
                                (string-append "#<primitive-procedure "
                                               (number->string (object->serial-number x)) ">")))

(%defprinter <input-stream> 
             (%primitive-method (x) 
                                (string-append "#<input-stream "
                                               (number->string (object->serial-number x)) ">")))

(%defprinter <output-stream> 
             (%primitive-method (x) 
                                (string-append "#<output-stream "
                                               (number->string (object->serial-number x)) ">")))

(%defprinter <cons>
             (%primitive-method (x) 
                                (let loop ((items x)
                                           (outstr "("))
                                  (if (null? items)
                                      (string-append outstr ")")
                                      (if (equal? items x)
                                          (loop (cdr items)(string-append outstr (%as-string (car items))))
                                          (loop (cdr items)(string-append outstr " " (%as-string (car items)))))))))

(%defprinter <frame>
             (%primitive-method (x) 
                                (let ((all-keys (%keys x)))
                                  (let loop ((keys all-keys)
                                             (outstr "{"))
                                    (if (null? keys)
                                        (string-append outstr "}")
                                        (if (equal? keys all-keys)
                                            (loop (cdr keys)
                                                  (let* ((x (%frame-get x (car keys))))
                                                    (string-append outstr (%as-string (car keys)) " " (%as-string x))))
                                            (loop (cdr keys)
                                                  (let* ((x (%frame-get x (car keys))))
                                                    (string-append outstr " " (%as-string (car keys)) " " (%as-string x))))))))))

(%defprinter <function>
             (%primitive-method (x) 
                                (let ((fname (%function-name x)))
                                  (if fname
                                      (string-append "#<function "
                                                     (object->string fname)
                                                     " "
                                                     (number->string (object->serial-number x))
                                                     ">")
                                      (string-append "#<an anonymous function " (number->string (object->serial-number x)) ">")))))

(%defprinter <method>
             (%primitive-method (x) 
                                (let ((mname (%method-name x)))
                                  (if mname
                                      (string-append "#<method " (object->string mname)
                                                     " " (number->string (object->serial-number x)) ">")
                                      (string-append "#<an anonymous method " (number->string (object->serial-number x)) ">")))))


(%defprinter <protocol> (%primitive-method (val)(object->string val)))

(%defprinter <singleton>
             (%primitive-method (x) 
                                (string-append "#<singleton "
                                               (%as-string (%singleton-value x))
                                               ">")))

(%defprinter <type>
             (%primitive-method (x) 
                                (cond
                                 ((%singleton? x) (string-append "#<singleton " (%as-string (%singleton-value x)) ">"))
                                 ((%primitive-type? x) (string-append "#<primitive-type " (%as-string (%primitive-type-name x)) ">"))
                                 ((%structure-type? x)  (string-append "#<structure-type " (%as-string (%structure-type-name x)) ">"))
                                 ((%protocol? x) (string-append "#<protocol " (%as-string (%protocol-name x)) ">"))
                                 (else (error (string-append "not a type" (object->string x)))))))


(define (bard:print object #!optional (out (current-output-port)))
  (print port: out (%as-string object)))

(define (show x)
  (newline)
  (display (%as-string x))
  (newline))

;;; (show 4)
;;; (show (%undefined))
;;; (show '())
;;; (show #\C)
;;; (show (%true))
;;; (show (%false))
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


