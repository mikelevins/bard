;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          functions.scm
;;;; Project:       Bard
;;;; Purpose:       representation of Bard functions
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(include "function-macros.scm")

;;; ---------------------------------------------------------------------
;;; utils
;;; ---------------------------------------------------------------------

(define (%ampersand? x)(eq? x '&))

(define (%function-param->formal-argument param)
  (cond
   ((symbol? param) param)
   ((list? param) (car param))
   (else (error "invalid parameter spec" param))))

(define (%function-param->signature-type param env)
  (cond
   ((symbol? param) Anything)
   ((list? param) (let* ((type-spec (cadr param))
                         (type (%eval type-spec env)))
                    (if (%type? type)
                        type
                        (error "invalid type" type))))
   (else (error "invalid parameter spec" param))))

(define (%function-param-list->formal-arguments params)
  (map %function-param->formal-argument params))

(define (%function-param-list->method-signature params env)
  (let ((required-params (take-before (lambda (p)(eq? p '&))
                                      params))
        (tail (if (position-if (lambda (x) (eq? x '&)) params)
                  '(&)
                  '()))
        (get-type (lambda (x)(%function-param->signature-type x env))))
    (append (map get-type required-params)
            tail)))


(define (%method-entry< e1 e2)
  (let loop ((types1 (car e1))
             (types2 (car e2)))
    (if (null? types1)
        #f
        (if (null? types2)
            #t
            (let ((t1 (car types1))
                  (t2 (car types2)))
              (if (%subtype? t2 t1)
                  #f
                  (if (%subtype? t1 t2)
                      #t
                      (loop (cdr types1)(cdr types2)))))))))



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

(define (%vals-match-method-signature? vals msig)
  (let* ((ampersand-pos (position-if %ampersand? msig))
         (required-argcount (or ampersand-pos (length msig)))
         (supplied-argcount (length vals))
         (matches? (lambda (val tp)
                     (if (equal? tp Anything)
                         #t
                         (if (%singleton? tp)
                             (equal? val (%singleton-value tp))
                             (%subtype? (%object->bard-type val) tp))))))
    (and
     (if ampersand-pos
         (>= supplied-argcount required-argcount)
         (= supplied-argcount required-argcount))
     (let loop ((vals vals)
                (types msig)
                (i 0))
       (if (>= i required-argcount)
           #t
           (if (matches? (car vals)(car types))
               (loop (cdr vals)(cdr types)(+ i 1))
               #f))))))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-type %method
  id: 86F8548C-056C-4369-ADF3-1657D7E83649
  constructor: %private-make-method
  (name %method-name)
  (environment %method-environment %set-method-environment!)
  (parameters %method-parameters)
  (required-count %method-required-count)
  (body %method-body))

(%define-structure-type <method> (##structure-type (%private-make-method #f '() '() 0 '())) %method?)

(define (%validate-method-name name)
  (cond
   ((symbol? name) name)
   ((eq? name #f) #f)
   (else (error "invalid method name" name))))

(define (%validate-method-params params)
  (if (every? symbol? params)
      params
      (error "Invalid parameter list for method" params)))

(define (%make-method #!key (name #f)(environment '())(params '())(body '()))
  (let* ((valid-name (%validate-method-name name))
         (valid-params (%validate-method-params params))
         (ampersand-pos (position-if %ampersand? valid-params))
         (required-count (or ampersand-pos (length valid-params))))
    (%private-make-method valid-name environment valid-params required-count body)))

(define (%with-environment env meth)
  (begin
    (%set-method-environment! meth env)
    meth))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-type %function
  id: C612A269-DA79-48F2-9FA0-F5F8F329EEBC
  constructor: %private-make-function
  (name %function-name)
  (method-table %function-method-table %set-function-method-table!))

(%define-structure-type <function> (##structure-type (%private-make-function #f #f)) %function?)

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

(define (%function-best-method fn vals)
  (let loop ((entries (%method-table-entries (%function-method-table fn)))
             (best #f))
    (if (null? entries)
        (if best (cdr best) #f)
        (let ((entry (car entries)))
          (if (%vals-match-method-signature? vals (car entry))
              (if best
                  (if (%method-entry< entry best)
                      (loop (cdr entries) entry)
                      (loop (cdr entries) best))
                  (loop (cdr entries) entry))
              (loop (cdr entries) best))))))


