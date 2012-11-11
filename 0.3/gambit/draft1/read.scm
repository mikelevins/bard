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
    (macro-readtable-paren-keyword-set! rt 'app)
    (macro-readtable-bracket-keyword-set! rt 'list)
    (macro-readtable-brace-keyword-set! rt 'table)
    rt))

(define +bard-readtable+ (bard:make-readtable))

;;; ----------------------------------------------------------------------
;;; the reader
;;; ----------------------------------------------------------------------

(define (%read-cons val)
  (cond
   ((null? val) '(syntax:nothing ()))
   ((eq? 'app (car val)) (cond 
                          ((null? (cdr val)) '(syntax:empty-app ()))
                          ((eq? '~ (cadr val))
                           (if (null? (cddr val))
                               '(syntax:empty-series '())
                               (list 'syntax:series (map %read-value->bard-syntax (cddr val)))))
                          ((member (cadr val) '(^ lambda method))
                           (list 'syntax:method 
                                 (cons (list 'syntax:lambda-list
                                             (map %read-value->bard-syntax (drop 1 (caddr val))))
                                       (map %read-value->bard-syntax (cdddr val)))))
                          (else (list 'syntax:app (map %read-value->bard-syntax (cdr val))))))
   ((eq? 'list (car val)) (if (null? (cdr val)) 
                              '(syntax:empty-list ())
                              (list 'syntax:list (map %read-value->bard-syntax (cdr val)))))
   ((eq? 'table (car val)) (if (null? (cdr val)) 
                               '(syntax:empty-table '())
                               (list 'syntax:table (map %read-value->bard-syntax (cdr val)))))
   (else (map %read-value->bard-syntax val))))

(define (%read-value->bard-syntax val)
  (cond
   ((null? val) '(syntax:nothing ()))
   ((eq? 'undefined val) '(syntax:undefined #!unbound))
   ((eq? 'nothing val) '(syntax:nothing ()))
   ((eq? 'true val) '(syntax:true #t))
   ((eq? 'false val) '(syntax:false #f))
   ((pair? val)(%read-cons val))
   ((char? val) `(syntax:character ,val))
   ((keyword? val) `(syntax:keyword ,val))
   ((number? val) (cond
                   ((##bignum? val) `(syntax:bignum ,val))
                   ((##fixnum? val) `(syntax:fixnum ,val))
                   ((##flonum? val) `(syntax:flonum ,val))
                   ((##ratnum? val) `(syntax:ratnum ,val))
                   (else (error (str "Unsupported number type: " val)))))
   ((symbol? val) `(syntax:symbol ,val))
   ((string? val) (if (string=? "" val)
                      '(syntax:empty-string "")
                      `(syntax:text ,val)))
   (else `(syntax:self-evaluating ,val))))

(define (bard:read #!optional port)
  (let ((original-readtable (input-port-readtable port)))
    (dynamic-wind
        (lambda ()(input-port-readtable-set! port +bard-readtable+))
        (lambda ()(let ((port (or port (current-input-port))))
                    (%read-value->bard-syntax (read port))))
        (lambda ()(input-port-readtable-set! port original-readtable)))))

(define (bard:read-from-string s)
  (call-with-input-string s (lambda (in)(bard:read in))))


;;; (bard:read-from-string "#\\C")
;;; (bard:read-from-string "#\\space")
;;; (bard:read-from-string "#\\u0041")
;;; (bard:read-from-string "false")
;;; (bard:read-from-string "true")
;;; (bard:read-from-string "Foo:")
;;; (bard:read-from-string "[]")
;;; (bard:read-from-string "[0 1 2 3]")
;;; (bard:read-from-string "(^ (x)(* x x))")
;;; (bard:read-from-string "nothing")
;;; (bard:read-from-string "()")
;;; (bard:read-from-string "[]")
;;; (bard:read-from-string "{}")
;;; (bard:read-from-string "\"\"")
;;; (bard:read-from-string "0")
;;; (bard:read-from-string "1.2")
;;; (bard:read-from-string "2/3")
;;; (bard:read-from-string "1.3e+12")
;;; (bard:read-from-string "2+3i")
;;; (bard:read-from-string "(~ x in: [1 2])")
;;; (bard:read-from-string "(~ x in: NATURAL where: (odd? x))")
;;; (bard:read-from-string "(~ with: [[x 0] [y 1]] yield: [x y] then: [y (+ y 1)])")
;;; (bard:read-from-string "foo")
;;; (bard:read-from-string "Bar")
;;; (bard:read-from-string "|Foo Bar|")
;;; (bard:read-from-string "{}")
;;; (bard:read-from-string "{a: 1 b: 2}")
;;; (bard:read-from-string "\"Foo bar baz\"")
;;; (bard:read-from-string "undefined")





