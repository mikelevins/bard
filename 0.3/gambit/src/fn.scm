;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          fn.scm
;;;; Project:       Bard
;;;; Purpose:       primitive bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; callable objects
;;; ---------------------------------------------------------------------

(define-type %callable
  extender: %defcallable
  (name %fn-name))

(%defcallable primitive
              constructor: %make-primitive
              (required-arg-count %prim-required-arg-count)
              (restargs? %prim-restargs?)
              (function %prim-function))

(%defcallable continuation
              constructor: %make-continuation
              (vmstate %cc-vmstate)
              (stack %cc-stack))

(define (%makecc vm pc)
  (let ((vmstate (%vmstate (%fn vm)(%code vm) pc (%env vm))))
    (%private-make-continuation name vmstate (%stack vm))))

(%defcallable function
              constructor: %make-function
              (parameters %function-parameters)
              (dispatcher %function-dispatcher))

;;; ---------------------------------------------------------------------
;;; callables with bodies of Bard code
;;; ---------------------------------------------------------------------

(%defcallable method
              constructor: %private-make-method
              (parameters %method-parameters)
              (env %method-env)
              (code %method-code))

(define (%make-method parameters code #!key (env (%null-env))(name '|An anonymous method|))
  (%private-make-method name parameters env code))



