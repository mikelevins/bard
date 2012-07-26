;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          reader.scm
;;;; Project:       Bard VM
;;;; Purpose:       the Bard reader
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

(define (%make-readtable)
  (let* ((rt (##make-standard-readtable)))
    (macro-readtable-brace-keyword-set! rt 'frame)
    (macro-readtable-bracket-keyword-set! rt 'list)
    (readtable-keywords-allowed?-set rt 'prefix)))

(define +bard-readtable+ (%make-readtable))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define (%read-cons val)
  (cond
   ((null? val) (%nothing))
   ((eq? 'list (car val)) (%cons 'list (%read-cons (cdr val))))
   ((eq? 'frame (car val)) (%cons 'frame (%read-cons (cdr val))))
   (else (let loop ((items val)
                    (ls %nil))
           (if (null? items)
               ls
               (loop (cdr items)
                     (%append ls (%list (%read-value->bard-value (car items))))))))))

(define (%read-value->bard-value val)
  (cond
   ((null? val)(%nothing))
   ((eq? 'undefined val)(%undefined))
   ((eq? 'nothing val)(%nothing))
   ((eq? 'true val)(%true))
   ((eq? 'false val)(%false))
   ((pair? val)(%read-cons val))
   (else val)))

(define (%read #!optional port)
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port +bard-readtable+))
        (lambda ()(let ((port (or port (current-input-port))))
                    (%read-value->bard-value (read port))))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

(define (%read-from-string s)
  (call-with-input-string s (lambda (in)(%read in))))

;;; (%primitive-print (%read-from-string "undefined"))
;;; (%primitive-print (%read-from-string "nothing"))
;;; (%primitive-print (%read-from-string "true"))
;;; (%primitive-print (%read-from-string "false"))
;;; (%primitive-print (%read-from-string "5"))
;;; (%primitive-print (%read-from-string "5.4"))
;;; (%primitive-print (%read-from-string "5/4"))
;;; (%primitive-print (%read-from-string "888888888"))
;;; (%primitive-print (%read-from-string "#\\C"))
;;; (%primitive-print (%read-from-string "#\\space"))
;;; (%primitive-print (%read-from-string "#\\u0041"))
;;; (%primitive-print (%read-from-string "\"Fred and Barney\""))
;;; (%primitive-print (%read-from-string "Fred"))
;;; (%primitive-print (%read-from-string ":name"))
;;; (%primitive-print (%read-from-string "(list 0 1 2 3)"))
;;; (%primitive-print (%read-from-string "[0 1 2 3]"))
;;; (%primitive-print (%read-from-string "{a: 1 b: 2 c: [1 2 3]}"))
;;; (%primitive-print (%read-from-string "{a: 1 b: [1 2 3]}"))


;;; (%primitive-print (%read-from-string "{0 1 2 3 4 {a: 1 b: 2}}"))
;;; (%primitive-print (%read-from-string "{0 1 2 3 4 [01 2 3] 5 {a: 1 b: 2}}"))
;;; (%primitive-print (%read-from-string "{name: 'test age: 101 friends: ['a 'b 'c]}"))
;;; (%primitive-print (%eval (%read-from-string "{name: 'test age: 101 friends: ['a 'b 'c]}")))
;;; (%primitive-print (%frame-get (%eval (%read-from-string "{name: 'test age: 101 friends: ['a 'b 'c]}")) friends:))
;;; (%primitive-print (%eval (%read-from-string "(list 0 1 2 3 4 5)")))
