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


(define-type %fn
  constructor: %private-make-fn
  extender: %defn
  (name %fn-name))

(%defn method
       constructor: %private-make-method
       (parameters %fn-parameters)
       (env %fn-env)
       (code %method-code %set-method-code!))

(define (%make-method parameters code #!key (env (%null-env))(name '|An anonymous method|))
  (%private-make-method name parameters env code))

(%defn continuation
  constructor: %private-make-continuation
  (vmstate %cc-vmstate)
  (stack %cc-stack))

(define (%makecc vm pc)
  (let ((vmstate (%vmstate (%fn vm)(%code vm) pc (%env vm))))
    (%private-make-continuation name vmstate (%stack vm))))

(define (%fn-code f)
  (if (method? f)
      (%method-code f)
      (error (str "Don't know how to get the fn-code from this value: " f))))

