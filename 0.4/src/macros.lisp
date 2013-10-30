;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          macros.lisp
;;;; Project:       Bard
;;;; Purpose:       implementation of macros and definition of built-in macros
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; macro support
;;; ---------------------------------------------------------------------

(defparameter *bard-macroexpanders* (make-hash-table))

(defmethod bard-macro? (x)
  (declare (ignore x))
  nil)

(defmethod bard-macro? ((x symbol))
  (gethash x *bard-macroexpanders*))

(defmacro def-bard-macro (name parmlist &body body)
  `(setf (gethash ',name *bard-macroexpanders*)
         #'(lambda ,parmlist .,body)))

(defun bard-macroexpand (x)
  "Macro-expand this Bard expression."
  (if (and (listp x) (bard-macro? (first x)))
      (bard-macroexpand
        (apply (bard-macro? (first x)) (rest x)))
      x))

;;; built-in macros
;;; ---------------------------------------------------------------------

(def-bard-macro bard-symbols::|def| (name val-form)
  `(bard-symbols::|set!| ,name ,val-form))

(def-bard-macro bard-symbols::|and| (&rest args)
  (if (null args)
      *true*
      (let* ((test (first args))
             (more (rest args))
             (then-clause `(bard-symbols::|and| ,@more)))
        (if (null more)
            test
            `(bard-symbols::|if| ,test
                            ,then-clause
                            ,*false*)))))

(def-bard-macro bard-symbols::|or| (&rest args)
  (if (null args)
      *false*
      (let* ((test (first args))
             (more (rest args)))
        (if (null more)
            test
            `((bard-symbols::|method| (result) 
                             (bard-symbols::|if| result
                                            result
                                            (bard-symbols::|or| ,@more))) 
              ,test)))))
