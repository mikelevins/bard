;;;; ***********************************************************************
;;;;
;;;; Name:          primitives.lisp
;;;; Project:       the bard programming language
;;;; Purpose:       the primitive substrate
;;;; Author:        mikel evins
;;;; Copyright:     2026 by mikel evins
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; Primitives are monomorphic, fixed-arity, and named with a leading
;;; underscore to keep them out of the language's namespace. There is no
;;; single addition primitive; there are several, one per numeric
;;; representation, and the polymorphic + that dispatches among them is
;;; a prelude function, not one of these.
;;;
;;; Until the prelude exists, the language-level operators are bound
;;; directly to the fixnum primitives so that programs can be written.
;;; That is scaffolding and is expected to go away.

;;; ---------------------------------------------------------------------
;;; installing
;;; ---------------------------------------------------------------------

(defun install-primitive (name arity function)
  "Bind NAME to a primitive of ARITY implemented by FUNCTION."
  (let ((binding (global-binding name))
        (primitive (make-primitive name arity function)))
    (setf (binding-value binding) primitive
          (binding-bound? binding) t)
    primitive))

(defmacro define-primitive (name (&rest args) &body body)
  "Define and install a primitive. Arity is the length of ARGS."
  `(install-primitive ',name ,(length args) (lambda ,args ,@body)))

;;; ---------------------------------------------------------------------
;;; named values
;;; ---------------------------------------------------------------------
;;; NOTHING is false, the empty list, and the empty collection at once.
;;; These are permanent names for permanent values, not scaffolding.

(defun install-value (name value)
  (let ((binding (global-binding name)))
    (setf (binding-value binding) value
          (binding-bound? binding) t)
    value))

(install-value 'nothing *nothing*)
(install-value 'true t)

;;; ---------------------------------------------------------------------
;;; fixnum arithmetic
;;; ---------------------------------------------------------------------

;;; Fixnum overflow will eventually signal a condition that the
;;; prelude's + turns into promotion to bignum. For now these are the
;;; host's operations.

(define-primitive _fixnum-add (a b) (+ a b))
(define-primitive _fixnum-sub (a b) (- a b))
(define-primitive _fixnum-mul (a b) (* a b))
(define-primitive _fixnum-div (a b) (truncate a b))
(define-primitive _fixnum-mod (a b) (mod a b))
(define-primitive _fixnum-lt (a b) (if (< a b) t *nothing*))
(define-primitive _fixnum-gt (a b) (if (> a b) t *nothing*))
(define-primitive _fixnum-eq (a b) (if (= a b) t *nothing*))

;;; ---------------------------------------------------------------------
;;; identity and pairs
;;; ---------------------------------------------------------------------

(define-primitive _eq (a b) (if (eq a b) t *nothing*))
(define-primitive _cons (a b) (cons a b))
(define-primitive _car (p) (if (consp p) (car p) *nothing*))
(define-primitive _cdr (p) (if (consp p) (cdr p) *nothing*))

;;; ---------------------------------------------------------------------
;;; dynamic variables
;;; ---------------------------------------------------------------------
;;; dynamic-let and a defparameter analogue are prelude, built over
;;; these and over dynamic-wind so that a rebinding survives a non-local
;;; exit. The machine supplies only the flag, the per-thread list, and
;;; the branch that consults it.

(define-primitive _dynamic! (binding)
  (setf (binding-dynamic? binding) t)
  binding)

(define-primitive _push-rebinding! (binding value)
  (push (cons binding value) (thread-dynenv *current-thread*))
  value)

(define-primitive _pop-rebinding! ()
  (pop (thread-dynenv *current-thread*))
  *nothing*)

;;; ---------------------------------------------------------------------
;;; threads
;;; ---------------------------------------------------------------------

(define-primitive _spawn (fn)
  (spawn fn))

;;; ---------------------------------------------------------------------
;;; lists and symbols
;;; ---------------------------------------------------------------------

(define-primitive _list1 (a) (list a))
(define-primitive _append (a b) (append a b))
(define-primitive _null? (x) (if (null x) t *nothing*))
(define-primitive _pair? (x) (if (consp x) t *nothing*))
(define-primitive _symbol? (x) (if (and x (symbolp x)) t *nothing*))

(defvar *gensym-count* 0
  "Not *gensym-counter*: that name is Common Lisp's.")

(define-primitive _gensym (prefix)
  (make-symbol (format nil "~A~D" prefix (incf *gensym-count*))))

;;; ---------------------------------------------------------------------
;;; output
;;; ---------------------------------------------------------------------

(define-primitive _print (x)
  (format t "~&~A~%" x)
  *nothing*)

;;; ---------------------------------------------------------------------
;;; scaffolding: language-level names
;;; ---------------------------------------------------------------------

;;; These bind the operators directly to fixnum primitives so that
;;; programs can be written before the prelude exists. Every one of them
;;; is meant to become a polymorphic prelude function that calls the
;;; primitive; none of them should survive.

(defun install-scaffolding ()
  (loop for (name primitive) in '((+ _fixnum-add)
                                  (- _fixnum-sub)
                                  (* _fixnum-mul)
                                  (< _fixnum-lt)
                                  (> _fixnum-gt)
                                  (= _fixnum-eq)
                                  (print _print))
        do (let ((binding (global-binding name))
                 (source (global-binding primitive)))
             (setf (binding-value binding) (binding-value source)
                   (binding-bound? binding) t)))
  (values))

(install-scaffolding)

#+repl (install-scaffolding)                ; => no values
#+repl (binding-value (global-binding '+))  ; => #<PRIMITIVE _FIXNUM-ADD/2>
