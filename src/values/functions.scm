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

;;; ---------------------------------------------------------------------
;;; method signatures
;;; ---------------------------------------------------------------------

(define (%optional-argument-type? tp)
  (eq? tp &))

(define (%method-signature-rest-arg? sig)
  (and (> (%length sig) 0)
       (%optional-argument-type? (%last sig))))

(define (%method-signature-required-count sig)
  (if (%method-signature-rest-arg? sig)
      (- (%length sig) 1)
      (%length sig)))

(define (%method-signature-element sig i)
  (%list-ref sig i))

(define (%method-signature-matches? sig args)
  (let* ((required-count (%method-signature-required-count sig))
         (rest-arg? (%method-signature-rest-arg? sig))
         (test (if rest-arg? >= =)))
    (if (test (length args) required-count)
        (let loop ((i 0))
          (if (>= i required-count)
              #t
              (if (%subtype? (%object->bard-type (car args)) 
                             (%method-signature-element sig i))
                  (loop (+ i 1))
                  #f)))
        #f)))

(define (%type-more-specific? t1 t2)
  (if (%optional-argument-type? t1)
      (if (%optional-argument-type? t2)
          #t
          #f)
      (if (eq? t1 t2)
          #t
          (if (%singleton? t2)
              #f
              (if (%singleton? t1)
                  (%type-more-specific? (%object->bard-type (%singleton-value t1)) t2)
                  (if (eq? t2 Anything)
                      #t
                      #f))))))

(define (%method-signature-more-specific? sig1 sig2)
  (let loop ((sig1 sig1)
             (sig2 sig2))
    (if (%null? sig1)
        (if (%null? sig2)
            #t
            #f)
        (if (%type-more-specific? (%car sig1)(%car sig2))
            (loop (%cdr sig1)(%cdr sig2))
            #f))))

(define (%function-param->method-signature-type env param)
  (cond
   ((symbol? param) (if (eq? param '&) & Anything))
   ((%list? param) (%eval (%car (%cdr param)) env))
   (else (error (string-append "Invalid function parameter: " (object->string param))))))

(define (%validate-method-signature sig)
  (if (and (%list? sig)
           (%every? %type? sig)
           (let ((ampersand-pos (%position & sig)))
             (if ampersand-pos 
                 (= ampersand-pos (- (%length sig) 1))
                 #t)))
      sig
      (else (error (string-append "Invalid method signature: " (object->string sig))))))

(define (%function-param-list->method-signature params env)
  (%validate-method-signature (%map (partial %function-param->method-signature-type env)
                                    params)))

(define (%function-param->formal-argument param)
  (cond
   ((symbol? param) param)
   ((%list? param) (%car param))
   (else (error (string-append "Invalid function parameter: " (object->string param))))))

(define (%function-param-list->formal-arguments params)
  (%map %function-param->formal-argument params))

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-type %method
  id: 927A1AD2-762A-4DE6-9900-C22857D20E5A
  extender: %def-method-type
  read-only:
  (name %method-name)
  (environment %method-environment)
  (formals %method-formals))

(%def-method-type %primitive-method
                  constructor: %private-make-primitive-method
                  read-only:
                  (function %method-function))

(define (%make-primitive-method formals method-function #!key (environment '())(name #f))
  (%private-make-primitive-method name environment formals method-function))

(define <primitive-method>
  (%define-standard-type '<primitive-method> (##structure-type (%make-primitive-method '() #f))))

(%def-method-type %interpreted-method
                  constructor: %private-make-interpreted-method
                  read-only:
                  (body %method-body))

(define (%make-interpreted-method formals method-body  #!key (environment '())(name #f))
  (%private-make-interpreted-method name environment formals method-body))

(define <interpreted-method>
  (%define-standard-type '<interpreted-method> (##structure-type (%make-interpreted-method '() '()))))

(define (%method-name? x)
  (or (symbol? x)(string? x)))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-type %function
  id: 0E7FE105-F4B7-4EE4-AA16-9475976B003D
  constructor: %private-make-function
  read-only:
  (name %function-name)
  (signatures %function-method-signatures %set-function-method-signatures!)
  (formals %function-method-formals %set-function-method-formals!)
  (bodies %function-method-bodies %set-function-method-bodies!))

(define (%make-function #!key (name #f))
  (%private-make-function name (%list) (%list) (%list)))

(define <function> (%define-standard-type '<function> (##structure-type (%make-function))))

(define (%function-max-method-index fn)
  (- (%length (%function-method-signatures fn)) 1))

(define (%function-nth-method-signature i)
  (%list-ref (%function-method-signatures fn) i))

(define (%function-best-method fn args)
  (let ((last-method-index (%function-max-method-index fn)))
    (let loop ((i 0)
               (best #f))
      (if (>= i last-method-index)
          best
          (let ((sig (%function-nth-method-signature i)))
            (if (%method-signature-matches? sig args)
                (if best
                    (if (%method-signature-more-specific? sig best)
                        (loop (+ 1 i) sig)
                        (loop (+ 1 i) best))
                    (loop (+ 1 i) sig))
                (loop (+ 1 i) best)))))))
