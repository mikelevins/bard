;;; -*- Mode: Lisp -*-

;;; substitutions.lisp
;;; General CL structures unifier.
;;; Substitution definitions.

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.

;;;---------------------------------------------------------------------------
;;; Substitution application.

;;; apply-substitution --
;;;
;;; EXCLUDE-VARS are variables that will just pass through (a list for
;;; the time being).

(defgeneric apply-substitution (substitution item &optional exclude-vars))


(defmethod apply-substitution ((s environment) (n number) &optional exclude-vars)
  (declare (ignore exclude-vars))
  n)


(defmethod apply-substitution ((substitution environment) (s symbol)
                               &optional (exclude-vars ()))
  (declare (type list exclude-vars))
  (cond ((variable-any-p s) s)
        ((variablep s)
         (if (member s exclude-vars :test #'eq)
             s
             (multiple-value-bind (val foundp)
                 (find-variable-value s substitution)
               (cond (foundp (apply-substitution substitution val exclude-vars))
                     (t (warn "~S is a free variable in the current environment."
                              s)
                        s))))
         )
        (t s)))


(defmethod apply-substitution ((substitution environment) (l cons)
                               &optional (exclude-vars ()))
  (declare (type list exclude-vars))
  (cons (apply-substitution substitution (first l) exclude-vars)
        (apply-substitution substitution (rest l) exclude-vars)))


(defmethod apply-substitution ((substitution environment) (l null)
                               &optional exclude-vars)
  (declare (ignore exclude-vars))
  '())


;;; compose-substitions --
;;; The definition is a direct translation of TPL's definition at page 318.
;;; Usually these are done by directly composing and currying
;;; functions in ML/Haskell derivatives, but that is just being "lazy".
;;; The current definition may be too "eager", but the "correct"
;;; semantics should be preserved.

(defun compose-substitutions (env2 env1) ; note the order.
  (declare (type environment env2 env1))

  (loop for env1-frame in (environment-frames env1)
        collect
        (loop for (var . term) in (frame-bindings env1-frame)
              collect (make-binding var (apply-substitution env2 term))
              into result-bindings
              finally (return (make-frame result-bindings)))
        into frames
        finally (return (make-environment :frames frames))))
                      
        


;;; ground-term --

(defun ground-term (term &optional (substitution (make-empty-environment)))
  (apply-substitution substitution term))


;;; end of file -- apply-substitutions.lisp --
