;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          test.lisp
;;;; Project:       Bard
;;;; Purpose:       test code for the Bard interpreter
;;;; Author:        mikel evins
;;;; Copyright:     2011 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

(defun test-read-eval ()
  (let ((test-exprs 
         '(("undefined" . "undefined")
           ("nothing" . "nothing")
           ("true" . "true")
           ("false" . "false")
           ("integer 0" . "0")
           ("integer -1" . "-1")
           ("integer 1234567890" . "1234567890")
           ("float -0.0" . "-0.0")
           ("float 0.00001" . "0.00001")
           ("float 0.123456789" . "0.123456789")
           ("character c" . "\\c")
           ("character space" . "\\space")
           ("variable *version*" . "*version*")
           ("variable bard.core:*module*" . "bard.core:*module*")
           ("literal sequence []" . "[]")
           ("literal sequence [0 1 2 3]" . "[0 1 2 3]")
           ("literal sequence [0 [1 2] 3]" . "[0 [1 2] 3]")
           ("literal map {}" . "{}")
           ("literal map {0 1 2 3}" . "{0 1 2 3}")
           ("literal map {:name \"Fred\" :age 101}" . "{:name \"Fred\" :age 101}")
           ("function application ()" . "()")
           ("function application (bard-version)" . "(bard-version)")
           ("function application (+ 2 3)" . "(+ 2 3)")
           ("function application (* (+ 1 2)(- 3 1))" . "(* (+ 1 2)(- 3 1))"))))
    (format t "~%bard read-eval test")
    (force-output)
    (dolist (test-pair test-exprs)
      (let* ((bard (init-bard))
             (env (standard-environment))
             (label (car test-pair))
             (e (cdr test-pair))
             (expr (handler-case (read-expr e bard)
                     (condition (err)(format nil "ERROR in read: ~A" err))))
             (val (handler-case (eval expr env bard)
                    (condition (err)(format nil "ERROR in eval: ~A" err)))))
        (format t "~%")
        (format t "~%~A:" label)
        (format t "~%       input text:~S" e)
        (format t "~%  read expression: ~S" expr)
        (format t "~%     output value: ~S" val)
        (force-output)))))

;;; (test-read-eval)
;;; (eval (read-expr "*version*" nil) (standard-environment) (init-bard))
;;; (read-expr "*version*" nil)