;;;; ***********************************************************************
;;;;
;;;; Name:          read.scm
;;;; Project:       Bard
;;;; Purpose:       bard reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;;; extracted from Gambit's lib/_io#.scm so it can be used and modified
;;;; without the large overhead of including all of gambit#.scm

;;;----------------------------------------------------------------------------

;;; Representation of readtables from the Gambit sources

(##include "~~lib/_gambit#.scm")
(##include "~~lib/_io#.scm")

;;;---------------------------------------------------------------------
;;; the Bard readtable
;;;---------------------------------------------------------------------

(define (bard:make-readtable)
  (let ((rt (##make-standard-readtable)))
    ;;(macro-readtable-bracket-keyword-set! rt 'vector)
    (macro-readtable-brace-keyword-set! rt 'dict)
    ;; make gambit read [] as a vector, not a list
    (macro-readtable-r6rs-compatible-read?-set! rt #f)
    rt))

(define +bard-readtable+ (bard:make-readtable))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------


(define (%bard-read-uri re c)
  (let ((start-pos (##readenv-current-filepos re)))
    (macro-read-next-char-or-eof re) ;; skip #\@
    (macro-read-next-char-or-eof re) ;; skip #\"
    (macro-readenv-filepos-set! re start-pos) ;; set pos to start of datum
    (let ((str (##build-escaped-string-up-to re #\")))
      (macro-readenv-wrap re `(uri ,str)))))

;;; redefine Gambit's #: reader
(define (%bard-read-sharp-colon re next start-pos)
  (begin
    (macro-read-next-char-or-eof re) ;; skip char after #\#
    (let ((type-identifier (##read-datum-or-label-or-none re))
          (expr (bard:read (macro-readenv-port re))))
      `(as ,type-identifier ,expr))))

(##readtable-char-class-set! +bard-readtable+ #\@ #t %bard-read-uri)
(##readtable-char-sharp-handler-set! +bard-readtable+ #\: %bard-read-sharp-colon)
(macro-readtable-keywords-allowed?-set! +bard-readtable+ #t) ; keyword:
;;(macro-readtable-keywords-allowed?-set! +bard-readtable+ 'prefix) ; :keyword

(define (%read-cons val)
  (cond
   ((null? val) '())
   ((eq? 'list (car val)) (cons 'list (%read-cons (cdr val))))
   ((eq? 'dict (car val)) (cons 'dict (%read-cons (cdr val))))
   (else (let ((items val))
           (if (null? items)
               '()
               (cons (%read-value->bard-value (car items))
                     (%read-value->bard-value (cdr items))))))))

(define (%read-value->bard-value val)
  (cond
   ((null? val) '())
   ((eq? 'undefined val) #!unbound)
   ((eq? 'nothing val) '())
   ((eq? 'true val) #t)
   ((eq? 'false val) #f)
   ((pair? val)(%read-cons val))
   (else val)))

(define (bard:read #!optional (port #f))
  (let* ((port (or port (current-input-port)))
         (original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port +bard-readtable+))
        (lambda ()(let ((port (or port (current-input-port))))
                    (%read-value->bard-value (read port))))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

(define (bard:read-from-string s)
  (call-with-input-string s (lambda (in)(bard:read in))))



