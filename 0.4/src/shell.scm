;;;; ***********************************************************************
;;;;
;;;; Name:          shell.scm
;;;; Project:       Bard
;;;; Purpose:       the kernel shell
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an interactive shell for the kernel

(define (kernel:shell)
  (globals:init)
  (let loop ((halt #f))
    (if halt
        'ok
        (begin (newline)
               (display "kernel> ")
               (let ((line (trim-whitespace (read-line))))
                 (if (or (match-prefix? ":quit ")
                         (match-prefix? ":q "))
                     (loop #t)
                     (begin (kernel:print (kernel:eval (kernel:read line)))
                            (loop #f))))))))

