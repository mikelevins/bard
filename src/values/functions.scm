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

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define-type %method
  id: 927A1AD2-762A-4DE6-9900-C22857D20E5A
  extender: %def-method-type
  read-only:
  (name %type-name)
  (formals %method-formals))

(%def-method-type %primitive-method
                  constructor: %make-primitive-method
                  read-only:
                  (function %method-function))

(%def-method-type %interpreted-method
                  constructor: %make-interpreted-method
                  read-only:
                  (environment %method-environment)
                  (body %method-body))

;;; ---------------------------------------------------------------------
;;; functions
;;; ---------------------------------------------------------------------

(define-type %function
  id: 0E7FE105-F4B7-4EE4-AA16-9475976B003D
  read-only:
  (name %function-name)
  (signatures %function-method-signatures)
  (formals %function-method-formals)
  (bodies %function-method-bodies))

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
