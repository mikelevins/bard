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

;;; we bootstrap the printer by:
;;; 1. defining a non-extensible printer for the built-in
;;;    base types
;;; 2. after the type system is built, redefining
;;;    the printer to be polymorphic and extensible

(define (bard:list->string val)
  (let ((valstrs (map bard:value->string val)))
    (append-strings (list "[" (append-strings (interpose " " valstrs)) "]"))))

(define (table? val) #f)
(define (bard:table->string val) "{}")

(define (bard:value->string val)
  (cond
   ((null? val) "nothing")
   ((eq? #!unbound val) "undefined")
   ((eq? #t val) "true")
   ((eq? #f val) "false")
   ((symbol? val) (symbol->string val))
   ((pair? val) (bard:list->string val))
   ((table? val) (bard:table->string val))
   (else (object->string val))))

(define (bard:print-object obj #!optional port)
  (let ((port (or port (current-output-port))))
    (display (bard:value->string obj) port)))

(define (show val)
  (newline)
  (bard:print-object val)
  (newline))


;;; (show (bard:read-from-string "undefined"))
;;; (show (bard:read-from-string "nothing"))
;;; (show (bard:read-from-string "true"))
;;; (show (bard:read-from-string "false"))
;;; (show (bard:read-from-string "5"))
;;; (show (bard:read-from-string "5.4"))
;;; (show (bard:read-from-string "5/4"))
;;; (show (bard:read-from-string "888888888"))
;;; (show (bard:read-from-string "#\\C"))
;;; (show (bard:read-from-string "#\\space"))
;;; (show (bard:read-from-string "#\\u0041"))
;;; (show (bard:read-from-string "#\\u03BB"))
;;; (show (bard:read-from-string "\"Fred and Barney\""))
;;; (show (bard:read-from-string "Fred"))
;;; (show (bard:read-from-string "|Fred and Barney|"))
;;; (show (bard:read-from-string "name:"))
;;; (show (bard:read-from-string "(list 0 1 2 3)"))
;;; (show (bard:read-from-string "[0 1 2 3]"))
