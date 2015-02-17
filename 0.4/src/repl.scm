;;;; ***********************************************************************
;;;;
;;;; Name:          repl.scm
;;;; Project:       Bard
;;;; Purpose:       the bard repl
;;;; Author:        mikel evins
;;;; Copyright:     2015 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; an interactive read-eval-print-loop for the bard language

(define (bard:repl)
  (globals:init)
  (let loop ((halt #f))
    (if halt
        'ok
        (with-exception-catcher
         (lambda (err)
           (display (string-append "ERROR: "
                                   (object->string (error-exception-message err))
                                   " "
                                   (object->string (error-exception-parameters err))))
           (newline)
           (loop #f))
         (lambda ()
           (begin (newline)
                  (display "bard> ")
                  (let ((line (trim-whitespace (read-line))))
                    (if (or (match-prefix? ":quit" line)
                            (match-prefix? ":q" line))
                        (loop #t)
                        (begin (kernel:print (kernel:eval (bard:compile (kernel:read line))))
                               (loop #f))))))))))
