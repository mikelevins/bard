;;;; bard.lisp

(IN-PACKAGE :bard.internal)

;;; ---------------------------------------------------------------------
;;; special symbols
;;; ---------------------------------------------------------------------

(DEFINE-SYMBOL-MACRO true T)
(DEFINE-SYMBOL-MACRO false NIL)

;;; ---------------------------------------------------------------------
;;; toplevel special forms
;;; ---------------------------------------------------------------------

(DEFMACRO ^ (&REST forms)
  `(LAMBDA ,@forms))

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

(DEFMACRO call (fn &REST args)
  `(FUNCALL ,fn ,@args))

(DEFMACRO define (name val)
  `(DEFPARAMETER ,name ,val))

(DEFMACRO if (test &REST rest)
  `(IF ,test ,@rest))

(DEFMACRO set! (place val)
  `(SETF ,place ,val))

(DEFMACRO unless (test &REST rest)
  `(UNLESS ,test ,@rest))

(DEFMACRO when (test &REST rest)
  `(WHEN ,test ,@rest))

