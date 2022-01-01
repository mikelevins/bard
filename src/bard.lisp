;;;; bard.lisp

(IN-PACKAGE :bard.internal)

;;; ---------------------------------------------------------------------
;;; toplevel special forms
;;; ---------------------------------------------------------------------

(DEFMACRO ^ (&REST forms)
  `(LAMBDA ,@forms))

(DEFMACRO call (fn &REST args)
  `(FUNCALL ,fn ,@args))

(DEFMACRO begin (&REST forms)
  `(PROGN ,@forms))

(DEFMACRO bind (bindings &BODY body)
  (IF (NULL bindings)
      `(PROGN ,@body)
      (LET* ((first-binding (FIRST bindings))
             (rest-bindings (REST bindings))
             (binding-count (LENGTH first-binding))
             (vars (SUBSEQ first-binding 0 (1- binding-count)))
             (val-expr (ELT first-binding (1- binding-count))))
        `(MULTIPLE-VALUE-BIND (,@vars) ,val-expr
           (bind ,rest-bindings ,@body)))))

(DEFMACRO define (name val)
  `(DEFPARAMETER ,name ,val))

(DEFMACRO set! (place val)
  `(SETF ,place ,val))

