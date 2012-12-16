;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       representation of lexical environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************



(define (%null-env) '())

(define (%lfind env vname)
  (assq vname env))

(define (%lref env vname)
  (let ((entry (lfind env vname)))
    (if entry
        (cdr entry)
        #!unbound)))

(define (%lset! env vname val)
  (let ((entry (%find-lvar env vname)))
    (if entry
        (set-cdr! entry val)
        (error (str "Undefined lexical variable " vname)))))

(define (%ladd env vname val)
  (cons (cons vname val)
        env))

