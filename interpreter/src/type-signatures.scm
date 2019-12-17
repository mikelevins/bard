;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          type-signatures.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard type signatures
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define-type signature
  constructor: make-signature
  input-types
  restarg
  output-types)

(define (signature-equal? s1 s2)
  (and (equal? (signature-input-types s1)
               (signature-input-types s2))
       (equal? (signature-output-types s1)
               (signature-output-types s2))
       (or (and (not (signature-restarg s1))
                (not (signature-restarg s2)))
           (and (symbol? (signature-restarg s1))
                (symbol? (signature-restarg s2)))
           (and (alist-table-instance? (signature-restarg s1))
                (alist-table-instance? (signature-restarg s2))))))

(define (signature-congruent? s1 s2)
  (and (= (length (signature-input-types s1))
          (length (signature-input-types s2)))
       (or (and (not (signature-restarg s1))
                (not (signature-restarg s2)))
           (and (signature-restarg s1)
                (signature-restarg s2)))))

(define (signature->string sig #!key (name #f))
  (let ((fname (if name (str name " ") "")))
    (str "(" fname
         (string-join " " (map %as-string (signature-input-types sig)))
         (let ((restarg (signature-restarg sig)))
           (cond
            ((not restarg) "")
            ((symbol? restarg) (str "& '" (%as-string restarg)))
            ((alist-table-instance? restarg) (str "& " (%as-string restarg)))))
         " -> "
         (string-join " " (map %as-string (signature-output-types sig)))
         ")")))


