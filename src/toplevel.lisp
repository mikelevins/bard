;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; toplevel.lisp
;;;; the bard toplevel
;;;; ---------------------------------------------------------------------

(in-package :bardvm)


;;;; ---------------------------------------------------------------------
;;;; the bard toplevel
;;;; ---------------------------------------------------------------------


(defun init-bard-comp ()
  "Initialize values (including call/cc) for the Bard compiler."
  (set-global-var! 'name! #'name!)
  (set-global-var! 'exit
                   (new-method :name 'exit :args '(val) :code '((HALT))))
  (set-global-var! 'call/cc
                   (new-method :name 'call/cc :args '(f)
                           :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
                                   (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-methods*)
    (setf (get (prim-symbol prim) 'global-val)
          (new-method :env nil :name (prim-symbol prim)
                  :code (seq (gen 'PRIM (prim-symbol prim))
                             (gen 'RETURN))))))

;;; ==============================

(defparameter bard-top-level
  '(begin (define (bard)
            (newline)
            (display "bard> ")
            (write ((compiler (read))))
            (bard))
          (bard)))

(defun bard ()
  "A compiled Bard read-eval-print loop"
  (init-bard-comp)
  (machine (compiler bard-top-level)))

