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

(define $bard-printer-table (make-table test: equal?))

(define (%defprinter type printer-function)
  (table-set! $bard-printer-table type printer-function))

(%defprinter Anything (lambda (val)(object->string val)))
(%defprinter <undefined> (lambda (val) "undefined"))
(%defprinter <null> (lambda (val) "nothing"))
(%defprinter <character> (lambda (val) (string-append "\\" (string val))))

(define (%as-string thing)
  (let* ((value (if (%singleton? thing)
                    (%singleton-value thing)
                    thing))
         (type (%object->bard-type value))
         (found-printer (table-ref $bard-printer-table type #f))
         (default-printer (table-ref $bard-printer-table Anything))
         (thing-string (if found-printer
                           (found-printer value)
                           (default-printer value))))
    (if (%singleton? thing)
        (string-append "#< singleton (" thing-string ")>")
        thing-string)))

(define (bard:print object #!optional (out (current-output-port)))
  (print port: out (%as-string thing)))

(define (show x)
  (newline)
  (display (%as-string x))
  (newline))

;;; (show 4)
;;; (show (bard:undefined))
;;; (show '())
;;; (show #\C)