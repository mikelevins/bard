;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Applicable.scm
;;;; Project:       Bard
;;;; Purpose:       generic equality 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Applicable)

;;; applicable?
;;; ---------------------------------------------------------------------

(define bard:applicable? (%make-function name: 'applicable?))
(%function-add-method! bard:applicable? `(,Anything)(lambda (y)(%false)))
(%function-add-method! bard:applicable? `(,<primitive-procedure>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<function>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<method>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<null>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<cons>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<string>)(lambda (y)(%true)))
(%function-add-method! bard:applicable? `(,<frame>)(lambda (y)(%true)))

;;; apply
;;; ---------------------------------------------------------------------

(define bard:apply (%make-function name: 'apply))

(define (%bard-apply app args . more)
  (let ((env (if (null? more)
                 $bard-toplevel-environment
                 (car more))))
    (%apply app args env)))

(%function-add-method! bard:apply `(,<primitive-procedure> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<function> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<method> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<null> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<cons> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<string> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<frame> & more) %bard-apply)

(%function-add-method! bard:apply `(,<frame> ,<null> & more) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<cons> & more) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<string> & more) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<frame> & more) %bard-apply)

