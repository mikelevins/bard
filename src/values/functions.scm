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
    (if (test (%length args) required-count)
        (let loop ((i 0))
          (if (>= i required-count)
              #t
              (if (%subtype? (%object->bard-type (%list-ref args i)) 
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

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-type %method
  id: 927A1AD2-762A-4DE6-9900-C22857D20E5A
  extender: %def-method-type
  read-only:
  (name %method-name)
  (formals %method-formals)
  (restarg %method-restarg)
  (required-count %method-required-count))

(%def-method-type %primitive-method
                  constructor: %private-make-primitive-method
                  read-only:
                  (function %method-function))

(define (%make-primitive-method function  
                                #!key 
                                (parameters %nil)
                                (name #f)
                                (required-count 0)
                                (restarg #f))
  (%private-make-primitive-method name parameters restarg required-count function))

(define <primitive-method>
  (%define-standard-type '<primitive-method> (##structure-type (%make-primitive-method #f))))

(%def-method-type %interpreted-method
                  constructor: %private-make-interpreted-method
                  read-only:
                  (environment %method-environment)
                  (body %method-body))

(define (%make-interpreted-method parameters method-body  
                                  #!key 
                                  (environment '())
                                  (name #f)
                                  (required-count 0)
                                  (restarg #f))
  (%private-make-interpreted-method name parameters restarg required-count environment method-body))

(define <interpreted-method>
  (%define-standard-type '<interpreted-method> (##structure-type (%make-interpreted-method '() '()))))

(define (%method-name? x)(or (symbol? x)(string? x)))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-type %function
  id: 0E7FE105-F4B7-4EE4-AA16-9475976B003D
  constructor: %private-make-function
  read-only:
  (name %function-name)
  read-write:
  (signatures %function-method-signatures %set-function-method-signatures!)
  (formals %function-method-formals %set-function-method-formals!)
  (methods %function-methods %set-function-methods!))

(define (%make-function #!key (name #f))
  (%private-make-function name (%list) (%list) (%list)))

(define <function> (%define-standard-type '<function> (##structure-type (%make-function))))

(define (%function-max-method-index fn)
  (- (%length (%function-method-signatures fn)) 1))

(define (%function-nth-method-signature fn i)
  (%list-ref (%function-method-signatures fn) i))

(define (%function-nth-method fn i)
  (%list-ref (%function-methods fn) i))

(define (%function-best-method fn args)
  (let ((last-method-index (%function-max-method-index fn)))
    (let loop ((i 0)
               (m #f)
               (best #f))
      (if (> i last-method-index)
          (if best
              (values (%function-nth-method fn m) best)
              (values #f #f))
          (let ((sig (%function-nth-method-signature fn i)))
            (if (%method-signature-matches? sig args)
                (if best
                    (if (%method-signature-more-specific? sig best)
                        (loop (+ 1 i) i sig)
                        (loop (+ 1 i) m best))
                    (loop (+ 1 i) i sig))
                (loop (+ 1 i) m best)))))))

(define (%add-method! fn msig method)
  (let* ((found-pos (%position (lambda (s)(%every? equal? s msig)) 
                               (%function-method-signatures fn)))
         (sigs (if found-pos
                   (%function-method-signatures fn)
                   (%append (%function-method-signatures fn) (%list msig))))
         (methods (if found-pos
                      (ra:list-set (%function-methods fn) found-pos method)
                      (%append (%function-methods fn) (%list method)))))
    (%set-function-method-signatures! fn sigs)
    (%set-function-methods! fn methods)
    fn))

(define (%add-primitive-method! fn msig params method-function #!key (name #f))
  (let* ((method (%make-primitive-method method-function 
                                         name: name parameters: params 
                                         required-count: (%method-signature-required-count msig)
                                         restarg: (if (%method-signature-rest-arg? msig)
                                                      (%last params)
                                                      #f)))
         (found-pos (%position (lambda (s)(%every? equal? s msig)) 
                               (%function-method-signatures fn)))
         (sigs (if found-pos
                   (%function-method-signatures fn)
                   (%append (%function-method-signatures fn) (%list msig))))
         (methods (if found-pos
                      (ra:list-set (%function-methods fn) found-pos method)
                      (%append (%function-methods fn) (%list method)))))
    (%set-function-method-signatures! fn sigs)
    (%set-function-methods! fn methods)
    fn))
