;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          read.scm
;;;; Project:       Bard
;;;; Purpose:       bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "readtable.scm")

;;;---------------------------------------------------------------------
;;; the Bard readtable
;;;---------------------------------------------------------------------

(define (bard:make-readtable)
  (let ((rt (##make-standard-readtable)))
    (readtable-keywords-allowed?-set rt #t)
    (macro-readtable-bracket-keyword-set! rt 'list)
    (macro-readtable-brace-keyword-set! rt 'frame)
    rt))

(define +bard-readtable+ (bard:make-readtable))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define (%read-value->bard-value val)
  (cond
   ((eq? 'undefined val)(%undefined))
   ((eq? 'nothing val)(%nothing))
   ((eq? 'true val)(%true))
   ((eq? 'false val)(%false))
   ((list? val)(map %read-value->bard-value val))
   (else val)))

(define (bard:read #!optional port)
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port +bard-readtable+))
        (lambda ()(let ((port (or port (current-input-port))))
                    (%read-value->bard-value (read port))))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

(define (bard:read-from-string s)
  (call-with-input-string s (lambda (in)(bard:read in))))

;;; (bard:read-from-string "")
;;; (car (bard:read-from-string "'x"))
;;; (car (bard:read-from-string ",x"))
;;; (car (bard:read-from-string ",@x"))
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
;;; (show (bard:read-from-string "\"Fred and Barney\""))
;;; (show (bard:read-from-string "Fred"))
;;; (show (bard:read-from-string "name:"))
;;; (show (bard:read-from-string "(0 1 2 3)"))
;;; (show (bard:read-from-string "[0 1 2 3]"))
;;; (show (bard:read-from-string "{0 1 2 3}"))

