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

;;;; extracted from Gambit's lib/_io#.scm so it can be used and modified
;;;; without the large overhead of including all of gambit#.scm

;;;----------------------------------------------------------------------------

;;; Representation of readtables.

(define-type readtable
  id: bebee95d-0da2-401d-a33a-c1afc75b9e43
  type-exhibitor: macro-type-readtable
  constructor: macro-make-readtable
  implementer: implement-type-readtable
  macros:
  prefix: macro-
  opaque:

  (case-conversion?               unprintable: read-write:)
  (keywords-allowed?              unprintable: read-write:)
  (escaped-char-table             unprintable: read-write:)
  (named-char-table               unprintable: read-write:)
  (sharp-bang-table               unprintable: read-write:)
  (char-delimiter?-table          unprintable: read-write:)
  (char-handler-table             unprintable: read-write:)
  (char-sharp-handler-table       unprintable: read-write:)
  (max-unescaped-char             unprintable: read-write:)
  (escape-ctrl-chars?             unprintable: read-write:)
  (sharing-allowed?               unprintable: read-write:)
  (eval-allowed?                  unprintable: read-write:)
  (write-extended-read-macros?    unprintable: read-write:)
  (write-cdr-read-macros?         unprintable: read-write:)
  (max-write-level                unprintable: read-write:)
  (max-write-length               unprintable: read-write:)
  (pretty-print-formats           unprintable: read-write:)
  (quote-keyword                  unprintable: read-write:)
  (quasiquote-keyword             unprintable: read-write:)
  (unquote-keyword                unprintable: read-write:)
  (unquote-splicing-keyword       unprintable: read-write:)
  (sharp-quote-keyword            unprintable: read-write:)
  (sharp-quasiquote-keyword       unprintable: read-write:)
  (sharp-unquote-keyword          unprintable: read-write:)
  (sharp-unquote-splicing-keyword unprintable: read-write:)
  (sharp-num-keyword              unprintable: read-write:)
  (sharp-seq-keyword              unprintable: read-write:)
  (paren-keyword                  unprintable: read-write:)
  (bracket-keyword                unprintable: read-write:)
  (brace-keyword                  unprintable: read-write:)
  (angle-keyword                  unprintable: read-write:)
  (start-syntax                   unprintable: read-write:)
  (six-type?                      unprintable: read-write:)
  (r6rs-compatible-read?          unprintable: read-write:)
  (r6rs-compatible-write?         unprintable: read-write:)
  (here-strings-allowed?          unprintable: read-write:)
)
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
   ((list? val)(%cons->ralist (map %read-value->bard-value val)))
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
;;; (bard:read-from-string "'x")
;;; (bard:read-from-string ",x")
;;; (bard:read-from-string ",@x")
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

