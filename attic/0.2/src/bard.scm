;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.scm
;;;; Project:       Bard
;;;; Purpose:       a read-eval-print loop for Bard
;;;;                this file is also compiled last by Gambit, giving its
;;;;                name ("bard")the library loader for the built library
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
  (%init-bard)
  (let loop ()
    (newline)
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
                           (let* ((val (%eval expr (%null-environment))))
                             (if (%defined? val)
                                 (display (%as-string val)))
                             (loop))))))))
      (with-exception-catcher error-handler rep))))

