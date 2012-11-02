;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          print.scm
;;;; Project:       Bard
;;;; Purpose:       bard printer
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; the bootstrapping strategy for the printer is to first define
;;; versions of the printing procedures that work for the fixed
;;; set of built-in value types, then load the type and generic function
;;; systems, then replace the printing procedures wth generic 
;;; versions.

(define (%list->string val)
  (str "[" (apply string-append (interpose " " (map %as-string val))) "]"))

(define (%alist-table->string val)
  (let ((%entry-as-string (lambda (e) (str (%as-string (car entry)) " " (%as-string (cdr entry))))))
    (str "{" (apply string-append (interpose " " (map %entry-as-string val))) "}")))

(define (%as-string val)
  (cond
   ((%simple-character? val)(object->string val))
   ((eqv? val (%false)) "false")
   ((eqv? val (%true)) "true")
   ((%keyword? val)(object->string val))
   ((%nothing? val) "nothing")
   ((%pair? val)(%list->string val))
   ;;((%method? val)(%method->string val))
   ((%primitive-procedure? val)(object->string val))
   ((%big-integer? val)(object->string val))
   ((%fixnum? val)(object->string val))
   ((%flonum? val)(object->string val))
   ((%ratnum? val)(object->string val))
   ;;((%series? val)(%series->string val))
   ((%symbol? val)(object->string val))
   ((%alist-table? val)(%alist-table->string val))
   ((%ascii-string? val)(object->string val))
   ((%primitive-input-stream? val)(object->string val))
   ((%primitive-output-stream? val)(object->string val))
   ((%undefined? val) "undefined")
   (else (str "#<unrecognized value [" (object->string val) "]>"))))

(define (%print-value val #!optional port)
  (let ((port (or port (current-output-port))))
    (display (%as-string val) port)))

(define (show val #!optional port)
  (let ((port (or port (current-output-port))))
    (newline)
    (%print-value val port)
    (newline)))

;;; (show (bard:read-from-string "#\\C"))
;;; (show (bard:read-from-string "#\\space"))
;;; (show (bard:read-from-string "#\\u0041"))
;;; (show (bard:read-from-string "false"))
;;; (show (bard:read-from-string "true"))
;;; (show (bard:read-from-string "Foo:"))
;;; (show (bard:read-from-string "nothing"))
;;; (show (bard:read-from-string "()"))
;;; (show (bard:read-from-string "[]"))
;;; (show (bard:read-from-string "{}"))
;;; (show (bard:read-from-string "\"\""))
;;; (show (bard:read-from-string "[0 1 2 3]"))
;;; (show (bard:read-from-string "(^ (x)(* x x))"))
;;; (show (bard:read-from-string "99999999999999999999999999999999999999999999999999999999999"))
;;; (show (bard:read-from-string "999"))
;;; (show (bard:read-from-string "9.99"))
;;; (show (bard:read-from-string "1.3e+12"))
;;; (show (bard:read-from-string "2/3"))
;;; (show (bard:read-from-string "foo"))
;;; (show (bard:read-from-string "Bar"))
;;; (show (bard:read-from-string "|Foo Bar|"))
;;; (show (bard:read-from-string "{}"))
;;; (show (bard:read-from-string "{a: 1 b: 2}"))
;;; (show (bard:read-from-string "(~ x in: [1 2])"))
;;; (show (bard:read-from-string "(~ x in: NATURAL where: (odd? x))"))
;;; (show (bard:read-from-string "(~ with: [[x 0] [y 1]] yield: [x y] then: [y (+ y 1)])"))
;;; (show (bard:read-from-string "\"Foo bar baz\""))
;;; (show (bard:read-from-string "undefined"))