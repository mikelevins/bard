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

(define *bard-prompt* "bard> ")

(define (bard:repl)
  (gc-report-set! #f)
  (newline)
  (display $bard-version-string)
  (newline)
  (let loop ()
    (newline)
    (display *bard-prompt*)
    (let ((error-handler (lambda (err)
                           (display-error err)
                           (loop)))
          (rep (lambda ()
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
                           (let* ((val (%eval expr (%null-environment)))
                                  (valstr (%as-string val)))
                             (newline)
                             (display valstr)
                             (loop))))))))
      (with-exception-catcher error-handler rep))))