;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          bard.lisp
;;;; Project:       Bard
;;;; Purpose:       bard 0.5 toplevel
;;;; Author:        mikel evins
;;;; Copyright:     2014 mikel evins
;;;;                Incrementally derived from Peter Norvig's Scheme compiler
;;;;                Code from Paradigms of Artificial Intelligence Programming
;;;;                Copyright (c) 1991 Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ==============================

(defun init-bard-comp ()
  "Initialize values (including call/cc) for the Bard compiler."
  ;; base singletons
  (set-global-var! 'undefined (undefined))
  (set-global-var! 'nothing (nothing))
  (set-global-var! 'true (true))
  (set-global-var! 'false (false))
  (set-global-var! 'end (end))
  ;; base types
  (set-global-var! 'bits +bits+)
  (set-global-var! 'procedure +procedure+)
  (set-global-var! 'record +record+)
  (set-global-var! 'list +list+)
  (set-global-var! 'values +values+)
  ;; built-in methods
  (set-global-var! 'exit 
                   (new-procedure :type 'method :name 'exit :args '(val) :code '((HALT))))
  (set-global-var! 'call/cc
                   (new-procedure :type 'method :name 'call/cc :args '(f)
                           :code '((ARGS 1) (CC) (LVAR 0 0 ";" f)
                                   (CALLJ 1)))) ; *** Bug fix, gat, 11/9/92
  (dolist (prim *primitive-fns*)
    (setf (get (prim-symbol prim) 'global-val)
          (new-procedure :type 'method :env nil :name (prim-symbol prim)
                         :code (seq (gen 'PRIM (prim-symbol prim))
                                    (gen 'RETURN))))))

;;; ==============================

(defparameter bard-top-level
  '(begin (define (bard)
           (newline)
           (display "bard> ")
           (display ((compiler (read))))
           (bard))
    (bard)))

(defun bard ()
  "A compiled Bard read-eval-print loop"
  (init-bard-comp)
  (machine (compiler bard-top-level)))
