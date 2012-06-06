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

(define (%method-signature-matches? sig required-count rest-arg? args)
  (let* ((test (if rest-arg? >= =)))
    (if (test (%length args) required-count)
        (let loop ((i 0))
          (if (>= i required-count)
              #t
              (if (%instance-of? (%list-ref args i)
                                 (%list-ref sig i))
                  (loop (+ i 1))
                  #f)))
        #f)))

(define (%type-more-specific? t1 t2)
  (if (eq? t1 t2)
      #t
      (if (%singleton? t2)
          #f
          (if (%singleton? t1)
              (%type-more-specific? (%object->bard-type (%singleton-value t1)) t2)
              (if (eq? t2 Anything)
                  #t
                  #f)))))

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
          (let* ((sig (%function-nth-method-signature fn i))
                 (meth (%function-nth-method fn i))
                 (required-count (%method-required-count meth))
                 (rest? (%method-restarg meth)))
            (if (%method-signature-matches? sig required-count rest? args)
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
                      (%list-put (%function-methods fn) found-pos method)
                      (%append (%function-methods fn) (%list method)))))
    (%set-function-method-signatures! fn sigs)
    (%set-function-methods! fn methods)
    fn))

(define (%add-primitive-method! fn msig params method-function #!key (name #f))
  (let* ((method (%make-primitive-method method-function 
                                         name: name parameters: params 
                                         required-count: (%length params)))
         (found-pos (%position (lambda (s)(%every? equal? s msig)) 
                               (%function-method-signatures fn)))
         (sigs (if found-pos
                   (%function-method-signatures fn)
                   (%append (%function-method-signatures fn) (%list msig))))
         (methods (if found-pos
                      (%list-put (%function-methods fn) found-pos method)
                      (%append (%function-methods fn) (%list method)))))
    (%set-function-method-signatures! fn sigs)
    (%set-function-methods! fn methods)
    fn))
