;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.rkt
;;;; Project:       bard 0.4
;;;; Purpose:       bard module reader
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************
#lang racket

;;; ---------------------------------------------------------------------
;;; infrastructure from racket
;;; ---------------------------------------------------------------------

(require racket/generator)

;;; ---------------------------------------------------------------------
;;; module-level syntax
;;; ---------------------------------------------------------------------

(provide make-bard-readtable
         (except-out (all-from-out racket) -> define lambda let let*)
         (except-out (all-from-out racket/generator) generator)
         (rename-out (lambda ^)
                     (define %define)
                     (generator generate)
                     (let %let)
                     (let* %let*)))

;;; ---------------------------------------------------------------------
;;; reader changes
;;; ---------------------------------------------------------------------

;;; bard character literals
 
(define (unicode-name? datum)
  (and (symbol? datum)
       (let ((s (symbol->string datum)))
         (and (char-ci=? #\u (string-ref s 0))
              (char=? #\+ (string-ref s 1))))))
 
(define (unicode-name->datum-string datum)
 (string-append "u" (substring (symbol->string datum) 2)))
 
(define (bard-read-char-name in)
  (let* ((datum (read in))
         (datumstr (cond
                     ((unicode-name? datum)(unicode-name->datum-string datum))
                     ((symbol? datum)(symbol->string datum))
                     ((number? datum)(number->string datum))
                     (else (error "Invalid character syntax: " datum)))))
    (string-append "#\\" datumstr)))
 
(define bard-read-char
  (case-lambda
    [(ch in)
     (call-with-input-string (bard-read-char-name in)
                             (lambda (instr)(read instr)))]
    [(ch in src line col pos)
     (call-with-input-string (bard-read-char-name in)
                             (lambda (instr)(read-syntax src instr)))]))
 
(define bard-read-list-literal
  (case-lambda
    ((ch in)
     (let ((ls (read/recursive in ch #f #t)))
       (cons 'list ls)))
    ((ch in src line col pos)
     (let ((ls (read-syntax/recursive src in ch #f #t)))
       (cons 'list ls)))))
 
(define bard-read-table-literal
  (case-lambda
    ((ch in)
     (let ((ls (read/recursive in ch #f #t)))
       (cons 'table ls)))
    ((ch in src line col pos)
     (let ((ls (read-syntax/recursive src in ch #f #t)))
       (cons 'table ls)))))
 
(define (make-bard-readtable)
  (make-readtable (current-readtable)
                  #\\ 'terminating-macro bard-read-char
                  #\[ 'terminating-macro bard-read-list-literal
                  #\{ 'terminating-macro bard-read-table-literal))
 
;;;(parameterize ([current-readtable (make-bard-readtable)])
;;;    (call-with-input-string "{0 1 2 {3 4}}" (^ (in)(read in))))
;;; (current-readtable (make-bard-readtable))
