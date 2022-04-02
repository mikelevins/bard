;;;; bard.lisp
;;;; special forms

(IN-PACKAGE :bard.internal)
(IN-READTABLE :MODERN)

;;; ---------------------------------------------------------------------
;;; special symbols
;;; ---------------------------------------------------------------------

(DEFINE-SYMBOL-MACRO true T)
(DEFINE-SYMBOL-MACRO false ())

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

(DEFMACRO define (name &REST args)
  (IF (= 1 (LENGTH args))
      `(DEFPARAMETER ,name ,(FIRST args))
      (ERROR "definition syntax error: ~s" `(define ,name ,@args))))

(DEFMACRO if (test conseq &OPTIONAL (alt NIL))
  `(IF ,test ,conseq ,alt))

(DEFMACRO set! (place val)
  `(SETF ,place ,val))

