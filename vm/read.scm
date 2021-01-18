;;;; ***********************************************************************
;;;;
;;;; Name:          read.scm
;;;; Project:       Bard
;;;; Purpose:       the vm's reader
;;;; Author:        mikel evins
;;;; Copyright:     2021 by mikel evins
;;;;
;;;; ***********************************************************************

(define (bardvm:read #!optional (port #f))
  (let* ((port (or port (current-input-port)))
         (original-readtable (input-port-readtable port)))
    (dynamic-wind
        ;; customize reader environment here
        (lambda () #f)
        (lambda ()(let ((port (or port (current-input-port))))
                    (read port)))
        ;; restore default reader environment here
        (lambda () #f))))
