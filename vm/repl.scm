;;;; ***********************************************************************
;;;;
;;;; Name:          repl.scm
;;;; Project:       Bard
;;;; Purpose:       the vm's interactive toplevel
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(define *bardvm-prompt* "bardvm> ")

(define (bardvm:repl #!key (debug #f))
  (gc-report-set! #f)
  (newline)
  (display $bardvm-version-string)
  (newline)
  (%init-bardvm)
  (call/cc
   (lambda (exit-bardvm)
     (let loop ()
       (newline)
       (newline)
       (display *bardvm-prompt*)
       (let ((rep (lambda ()
                    (let* ((expr (bardvm:read)))
                      (if (or (eq? expr quit:)
                              (eq? expr q:))
                          (begin
                            (newline)
                            (display "Quitting bardvm...")
                            (newline)
                            (exit-bardvm))
                          (if (eq? expr #!eof)
                              (loop)
                              (begin (newline)
                                     (display expr)
                                     (loop))))))))
         (rep))))))
