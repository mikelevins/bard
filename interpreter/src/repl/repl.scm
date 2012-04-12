;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          repl.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%initial-bard-environment)
  (%extend-environment 
   (%null-environment)
   `(;; generic comparison
     = ,bard:=
     ;; numbers
     + ,prim:+
     - ,prim:-
     * ,prim:*
     / ,prim:/
     > ,prim:>
     < ,prim:<
     >= ,prim:>=
     <= ,prim:<=
     )))


(define *bard-banner* "Bard 0.1")
(define *bard-prompt* "bard> ")

(define (bard:repl)
  (set! $bard-toplevel-environment (%initial-bard-environment))
  (newline)
  (display *bard-banner*)
  (newline)
  (read-line)
  (let loop ()
    (newline)
    (display *bard-prompt*)
    (let* ((input (read-line))
           (expr (bard:read-from-string input)))
      (if (or (eq? expr quit:)
              (eq? expr q:))
          (begin
            (newline)
            (display "Bard terminated")
            (newline))
          (if (eq? expr #!eof)
              (loop)
              (let* ((val (%eval expr $bard-toplevel-environment))
                     (valstr (%as-string val)))
                (newline)
                (display valstr)
                (loop)))))))


(define (bard:repl)
  (set! $bard-toplevel-environment (%initial-bard-environment))
  (newline)
  (display *bard-banner*)
  (read-line)
  (let loop ()
    (newline)
    (display *bard-prompt*)
    (let* ((line-reader (lambda ()(read-line)))
           (reader-handler (lambda (err)
                             (newline)
                             (display "ERROR: reader error ")
                             (display err)
                             (newline)
                             (loop)))
           (eval-handler (lambda (err)
                           (newline)
                           (display "ERROR: eval error ")
                           (display err)
                           (newline)
                           (loop)))
           (printer-handler (lambda (err)
                              (newline)
                              (display "ERROR: print error ")
                              (display err)
                              (newline)
                              (loop)))
           (input (with-exception-catcher reader-handler line-reader))
           (expr-reader (lambda ()(bard:read-from-string input)))
           (expr (with-exception-catcher reader-handler expr-reader))
           (evaluator (lambda ()(%eval expr $bard-toplevel-environment))))
      (if (or (eq? expr quit:)
              (eq? expr q:))
          (begin
            (newline)
            (display "Bard terminated")
            (newline))
          (if (eq? expr #!eof)
              (loop)
              (let* ((val (with-exception-catcher eval-handler evaluator))
                     (printer (lambda ()(%as-string val)))
                     (valstr (with-exception-catcher printer-handler printer)))
                (newline)
                (display valstr)
                (newline)
                (loop)))))))