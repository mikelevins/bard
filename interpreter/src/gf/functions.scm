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

(define-type bard:%function-metadata
  id: 9BAAB52D-2A7F-482D-8BED-1F5CD677A335
  constructor: bard:%make-function-metadata
  (debug-name bard:%function-debug-name)
  (signature bard:%function-signature)
  (method-table bard:%function-method-table))

(define-type bard:%method-metadata
  id: CDBFEF31-AD04-453F-A826-A92AEE8070A6
  constructor: bard:%make-method-metadata
  (debug-name bard:%method-debug-name)
  (signature bard:%method-signature))

(define (%function-metadata proc)
  (cond
   ((##interp-procedure? proc) (if (vector? (##interp-procedure-rte proc))
                                   (##vector-ref (##interp-procedure-rte proc) 1)))
   ((##closure? proc)(##closure-ref proc 1))
   (else (error "function expected"))))

(define (%dispatch-function args metadata)
  'function-dispatched)

(define (%validate-function-name name)
  (cond
   ((symbol? name) name)
   ((eq? name #f) #f)
   (else (error "invalid function name" name))))

(define (%validate-function-param p)
  (if (symbol? p)
      p
      (error "invalid function parameter" p)))

(define (%validate-function-signature signature)
  (and (every? %validate-function-param signature)
       signature))

(define (%validate-method-param p)
  (if (or (symbol? p)
          (and (list? p)
               (= 2 (length p))
               (symbol? (car p))
               (bard:type? (cadr p))))
      p
      (error "invalid method parameter p")))

(define (%validate-method-signature signature)
  (and (every? %validate-method-param signature)
       signature))

(define (%make-function #!key (name #f)(signature '()))
  (let* ((valid-name (%validate-function-name name))
         (valid-signature (%validate-function-signature signature))
         (method-table (make-table test: equal?))
         (meta (bard:%make-function-metadata valid-name valid-signature method-table)))
    (let ((metadata meta))
      (lambda args
        (%dispatch-function args metadata)))))

(define (%undefined-method-body . args)
  (error "No definition supplied for method with args" args))

(define (%apply-method args metadata method-function)
  (apply method-function args))

(define (%make-method #!key (name #f)(signature '())(method-function %undefined-method-body))
  (let* ((valid-name (%validate-function-name name))
         (valid-signature (%validate-method-signature signature))
         (meta (bard:%make-method-metadata valid-name valid-signature)))
    (let ((metadata meta))
      (lambda args
        (%apply-method args metadata method-function)))))

(define (%function? x)
  (bard:%function-metadata? (%function-metadata x)))

(define (%method? x)
  (bard:%method-metadata? (%function-metadata x)))

;;; (%function? (%make-function))
;;; (%method? (%make-method))
