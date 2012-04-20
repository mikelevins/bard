;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of Bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (%function-param->formal-argument param)
  (cond
   ((symbol? param) param)
   ((list? param) (car param))
   (else (error "invalid parameter spec" param))))

(define (%function-param->signature-type param)
  (cond
   ((symbol? param) Anything)
   ((list? param) (let ((type-spec (cadr param)))
                    (if (%type? type-spec)
                        type-spec
                        (error "invalid type" type-spec))))
   (else (error "invalid parameter spec" param))))

(define (%function-param-list->formal-arguments params)
  (map %function-param->formal-argument params))

(define (%function-param-list->method-signature params)
  (let ((required-params (take-before (lambda (p)(eq? p '&))
                                      params))
        (tail (if (position-if (lambda (x) (eq? x '&)) params)
                  '(&)
                  '())))
    (append (map %function-param->signature-type required-params)
            tail)))


(define (%method-entry< cons1 cons2)
  (let loop ((e1 (car cons1))
             (e2 (car cons2)))
    (if (null? e2)
        #t
        (if (null? e1)
            #f
            (if (%subtype? e1 e2)
                #t
                (loop (cdr e1) (cdr e2)))))))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(include "~~lib/_gambit#.scm")
(##include "function-macros.scm")

;;; ---------------------------------------------------------------------
;;; method tables
;;; ---------------------------------------------------------------------

;;; a method table matches formal type signatures to methods
;;; a formal type signature is a list of one of the following
;;; two forms:
;;; 1. (type*)
;;; 2. (type* &)
;;; form 1 is a list of type objects. In order to match it, 
;;; a list of argument types must be the same length as the
;;; signature, and each argument type must be a subtype of
;;; the signature type.
;;; form 2 is a list of types followed by the ampersand.
;;; In order to match, a list of argtypes must have at
;;; least as many members as the signature, excluding the
;;; ampersand, and each of the argument types that appears
;;; before the position of the ampersand must be a
;;; subtype of the corresponding signature type.
;;; 
;;; the method signatures used by method tables should not be
;;; confused with the parameters that are stored on methods.
;;; those parameters are the formal parameters that are bound
;;; to input arguments, including the name of the rest argument
;;; that is bound to any optional args. by contrast, a
;;; method signature as used in the method table is not
;;; a list of formal parameters, but a list of the allowed 
;;; types of formal parameters, along with the presence or
;;; absence of the ampersand to indicate whether extra 
;;; arguments are allowed.

(define-type %method-table
  id: F7221DF7-EA48-413A-9897-527989D87B63
  constructor: %make-method-table
  (entries %method-table-entries %set-method-table-entries!))

(define (%method-table-find-entry mtable signature)
  (assoc signature (%method-table-entries mtable)))

(define (%method-table-set-entry! mtable signature method)
  (let ((entry (%method-table-find-entry mtable signature)))
    (if entry
        (set-cdr! entry method)
        (%set-method-table-entries! mtable
                                    (cons (cons (copy-tree signature) 
                                                method)
                                          (%method-table-entries mtable))))))

(define (%types-match-method-signature? argtypes msig)
  (let* ((ampersand? (lambda (x)(eq? x '&)))
         (rest? (any? ampersand? msig))
         (required-args (take-before ampersand? msig))
         (required-argcount (length required-args))
         (supplied-argcount (length argtypes)))
    (and
     (if rest?
         (>= supplied-argcount required-argcount)
         (= supplied-argcount required-argcount))
     (every? %subtype?
             argtypes
             required-args))))

(define (%method-entries-matching-types mtable argtypes)
  (filter (lambda (entry)
            (%types-match-method-signature? argtypes (car entry)))
          (%method-table-entries mtable)))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define (%validate-method-name name)
  (cond
   ((symbol? name) name)
   ((eq? name #f) #f)
   (else (error "invalid method name" name))))

(define (%validate-method-params params)
  (if (every? symbol? params)
      params
      (error "Invalid parameter list for method" params)))

(define (%make-method #!key (name #f)(params '())(body '()))
  (let ((valid-name (%validate-method-name name))
        (valid-params (%validate-method-params params)))
    (%private-make-method valid-name valid-params body)))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define (%validate-function-name name)
  (cond
   ((symbol? name) name)
   ((eq? name #f) #f)
   (else (error "invalid function name" name))))

(define (%make-function #!key (name #f))
  (let* ((valid-name (%validate-function-name name))
         (method-table (%make-method-table '())))
    (%private-make-function valid-name method-table)))

(define (%function-add-method! fn signature method)
  (let ((mtable (%function-method-table fn)))
    (%method-table-set-entry! mtable signature method)
    fn))

(define (%function-ordered-methods fn types)
  (let* ((mtable (%function-method-table fn))
         (entries (%method-entries-matching-types mtable types))
         (ordered-entries (sort entries %method-entry<)))
    (map cdr ordered-entries)))


