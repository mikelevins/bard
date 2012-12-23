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

(define (%env-find env vname)
  (assq vname env))

(define (%env-ref env vname)
  (let ((entry (%env-find env vname)))
    (if entry
        (cdr entry)
        #!unbound)))

(define (%env-set! env vname val)
  (let ((entry (%env-find env vname)))
    (if entry
        (set-cdr! entry val)
        (error (str "Undefined lexical variable " vname)))))

(define (%env-add env vname val)
  (cons (cons vname val)
        env))


(define (%merge-environments . envs)
  (if (null? envs)
      (%null-env)
      (let ((env1 (car envs)))
        (if (null? (cdr envs))
            env1
            (let ((env2 (cadr envs))
                  (more (cddr envs)))
              (apply %merge-environments
                     (append env1 env2)
                     more))))))
