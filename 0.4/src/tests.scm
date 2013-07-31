;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Bard
;;;; Purpose:       tests of VM subsystems
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; instruction tests
;;; ----------------------------------------------------------------------

(define (test-show)
  (let* ((code (asm (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (showvm state)))

;;; (test-show)

(define (test-halt)
  (let* ((code (asm (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-halt)

(define (test-const)
  (let* ((code (asm (instruction 'CONST 5)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-const)

