;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.scm
;;;; Project:       bard
;;;; Purpose:       bard's printer
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define print:$object-printers (make-table))

(define (print:type-for-printing obj)
  (cond
   ((eqv? obj #!void) 'undefined)
   ((null? obj) 'nothing)
   ((eqv? obj #t) 'true)
   ((eqv? obj #f) 'false)
   (else (error "Unrecognized object type when printing" obj))))

(define (bard:defprinter print-type printfn)
  (table-set! print:$object-printers print-type printfn))

(define (print:lookup-printer obj)
  (let ((print-type (print:type-for-printing obj)))
    (table-ref print:$object-printers print-type #f)))

(bard:defprinter 'undefined (lambda (x) "undefined"))
(bard:defprinter 'nothing (lambda (x) "nothing"))
(bard:defprinter 'true (lambda (x) "true"))
(bard:defprinter 'false (lambda (x) "false"))

(define (bard:print-object obj #!optional (port #f))
  (let* ((printer (print:lookup-printer obj))
         (port (or port (current-output-port)))
         (object-string (printer obj)))
    (display object-string port)))