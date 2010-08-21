;;;; -*- Mode: Lisp -*-

;;;; substitutions.lisp --
;;;; General CL structures unifier.
;;;; Substitution definitions.  Mostly a rehash of the usual SICP stuff.

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.

;;;---------------------------------------------------------------------------
;;; Bindings.

(deftype binding () 'cons)

(deftype bindings () 'list) ; An A-LIST.

(defun make-binding (variable value)
  (cons variable value))

(defun extend-bindings (variable value bindings)
  (acons variable value bindings))


(defun binding-variable (b)
  (declare (type binding b))
  (car b))

(defun (setf binding-variable) (v b)
  (declare (type binding b))
  (setf (car b) v))


(defun binding-value (b)
  (declare (type binding b))
  (cdr b))


(defun (setf binding-value) (v b)
  (declare (type binding b))
  (setf (cdr b) v))


(defun bindings-values (bindings) (mapcar #'cdr bindings))

(defun bindings-keys (bindings) (mapcar #'car bindings))



(define-condition unification-variable-unbound (unbound-variable)
  ()
  )

(define-condition unification-failure (simple-error)
  ())


;;;---------------------------------------------------------------------------
;;; Frames.

(defstruct (frame (:constructor make-frame (&optional bindings)))
  (bindings () :type bindings))

(defun empty-frame-p (f)
  (declare (type frame f))
  (null (frame-bindings f)))


(defun find-variable-binding-in-frame (v f)
  (declare (type frame f))
  (assoc v (frame-bindings f)))


(defun find-variable-value-in-frame (v f)
  (declare (type frame f))
  (let ((b (find-variable-binding-in-frame v f)))
    (declare (type (or null binding) b))
    (if b
        (values (cdr b) t)
        (values nil nil))))

(defun frame-variables (frame)
  (mapcar 'binding-variable (frame-bindings frame)))


(defun frame-values (frame)
  (mapcar 'binding-value (frame-bindings frame)))


;;;---------------------------------------------------------------------------
;;; Environments.

(defstruct (environment (:print-object print-environment)
                        (:copier nil))
  (frames () :type list))

(defun print-environment (env stream)
  (if *print-readably*
      (format stream "#S(ENVIRONMENT FRAMES ~S)"
              (environment-frames env))
      (print-unreadable-object (env stream :type nil :identity t)
        (format stream "~:[~;EMPTY ~]UNIFY ENVIRONMENT: ~D frame~:P"
                (empty-environment-p env)
                (list-length (environment-frames env))))))

(deftype substitution () 'environment)

(defun substitution-p (x) (environment-p x))

(defun first-frame (env)
  (first (environment-frames env)))


(defun make-empty-environment ()
  (make-environment :frames (list (make-frame))))

(defun copy-environment (env)
  (make-environment :frames (copy-list (environment-frames env))))

(defun make-shared-environment (env)
  (make-environment :frames (environment-frames env)))

(defun empty-environment-p (env)
  (declare (type environment env))
  (let ((env-frames (environment-frames env)))
    (declare (type list env-frames))
    (and (= 1 (list-length env-frames))
         (empty-frame-p (first env-frames)))))

(defparameter *null-environment* (make-empty-environment))


(defun find-variable-value (variable &optional (env *null-environment*) errorp)
  (declare (type environment env))
  (labels ((find-var-value (frames)
             (cond (frames
                    (multiple-value-bind (val foundp)
                        (find-variable-value-in-frame variable (first frames))
                      (if foundp
                          (values val t)
                          (find-var-value (rest frames)))))
                   (errorp
                    (error 'unification-variable-unbound :variable variable))
                   (t (values nil nil))))
           )
    (find-var-value (environment-frames env))))



(defun extend-environment (var pat &optional (env (make-empty-environment)))
  (let ((first-frame (first-frame env)))
    (setf (frame-bindings first-frame)
          (extend-bindings var pat (frame-bindings first-frame)))
    env))


(defun fill-environment (vars pats &optional (env (make-empty-environment)))
  (map nil (lambda (v p) (extend-environment v p env)) vars pats)
  env)


(defun fill-environment* (vars-pats &optional (env (make-empty-environment)))
  (loop for (v . p) in vars-pats do (extend-environment v p env))
  env)


(declaim (inline v?))
(declaim (ftype (function (symbol environment &optional boolean)
                          (values t boolean))
                find-variable-value
                v?))

(defun v? (s env &optional (plain-symbol-p nil))
  (find-variable-value (if plain-symbol-p
                           (make-var-name s)
                           s)
                       env))


(defun environment-variables (env)
  (mapcan #'frame-variables (environment-frames env)))

(defun environment-values (env)
  (mapcan #'frame-values (environment-frames env)))




;;;; end of file -- substitutions.lisp --
