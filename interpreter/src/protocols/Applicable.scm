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
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Applicable)

;;; applicable?
;;; ---------------------------------------------------------------------

(define bard:applicable? (%make-function name: 'applicable?))
(%function-add-method! bard:applicable? `(,Anything)(%method (y) false))
(%function-add-method! bard:applicable? `(,<primitive-procedure>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<function>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<method>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<null>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<cons>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<string>)(%method (y) true))
(%function-add-method! bard:applicable? `(,<frame>)(%method (y) true))

;;; apply
;;; ---------------------------------------------------------------------

(define bard:apply (%make-function name: 'apply))

(define %bard-apply (%primitive-method (app args)(%apply app args)))

(%function-add-method! bard:apply `(,<primitive-procedure> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<primitive-procedure> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<function> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<function> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<method> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<method> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<null> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<null> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<cons> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<cons> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<string> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<string> ,<frame>) %bard-apply)

(%function-add-method! bard:apply `(,<frame> ,<null>) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<cons>) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<string>) %bard-apply)
(%function-add-method! bard:apply `(,<frame> ,<frame>) %bard-apply)
