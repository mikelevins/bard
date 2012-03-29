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
;;; representation of functions
;;; ---------------------------------------------------------------------

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

(define-type bard:%interpreted-method-metadata
  id: FE3CCB2B-8A69-41D1-807E-AE97E3DE5D2F
  constructor: bard:%make-interpreted-method-metadata
  (debug-name bard:%interpreted-method-debug-name)
  (signature bard:%interpreted-method-signature)
  (body bard:%interpreted-method-body))

(define (%function-metadata proc)
  (cond
   ((##interp-procedure? proc) (if (vector? (##interp-procedure-rte proc))
                                   (##vector-ref (##interp-procedure-rte proc) 1)))
   ((##closure? proc)(##closure-ref proc 1))
   (else (error "function expected"))))

(define (%make-matching-signatures argtypes)
  (if (null? argtypes)
      (list argtypes)
      (if (null? (cdr argtypes))
          (list argtypes
                (map (constantly Anything) argtypes))
          (let* ((argtype (car argtypes))
                 (alt Anything)
                 (tails (%make-matching-signatures (cdr argtypes)))
                 (argtails (map (lambda (tl)(cons argtype tl)) tails))
                 (alttails (map (lambda (tl)(cons alt tl)) tails)))
            (append argtails alttails)))))

(define (%type-order tp1 tp2)
  (if (eq? tp1 tp2)
      'equal
      (if (eq? tp2 Anything)
          'ascending
          (if (eq? tp1 Anything)
              'descending
              'equal))))

(define (%sig-more-specific? sig1 sig2)
  (if (null? sig2)
      #t
      (if (null? sig1)
          #f
          (case (%type-order (car sig1)(car sig2))
            ((ascending) #t)
            ((descending) #f)
            (else (%sig-more-specific? (cdr sig1) (cdr sig2)))))))

(define (%order-signatures sigs)
  (sort sigs %sig-more-specific?))

;;; pick the most specific method
(define (%find-best-method mtable argtypes)
  (let ((perfect-match (table-ref mtable argtypes #f)))
    (or perfect-match
        (let* ((signatures (%order-signatures (%make-matching-signatures argtypes)))
               (methods (filter identity (map (lambda (sig)(table-ref mtable sig #f)) signatures))))
          (if (null? methods)
              #f
              (car methods))))))

(define (%dispatch-function args metadata)
  (let ((f-argcount (length (bard:%function-signature metadata))))
    (if (= f-argcount (length args))
        (let* ((mtable (bard:%function-method-table metadata))
               (argtypes (map %object->bard-type args))
               (method (%find-best-method mtable argtypes)))
          (if method
              (apply method args)
              (let ((fname (or (bard:%function-debug-name metadata)
                               "an anonymous function")))
                (error "no applicable method for function with arguments" fname args))))
        (let ((fname (or (bard:%function-debug-name metadata)
                         "an anonymous function")))
          (error "wrong number of arguments to function" fname)))))

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
  (if (and (list? p)
           (= 2 (length p))
           (symbol? (car p))
           (bard:type? (cadr p)))
      p
      (error "invalid method parameter" p)))

(define (%validate-method-signature signature)
  (and (every? %validate-method-param signature)
       signature))

(define (%method-signature-matches? s1 s2)
  (if (null? s1)
      (if (null? s2)
          #t
          #f)
      (let ((a (car s1))
            (b (car s2)))
        (if (equal? b Anything) 
            (%method-signature-matches? (cdr s1) (cdr s2))
            (if (equal? a b)
                (%method-signature-matches? (cdr s1) (cdr s2))
                #f)))))

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

(define (%function-argcount f)
  (length (bard:%function-signature (%function-metadata f))))

(define (%method-argcount m)
  (length (bard:%method-signature (%function-metadata m))))

(define (%apply-method args metadata method-function)
  (if (= (length args)
         (length (bard:%method-signature metadata)))
      (apply method-function args)
      (error "wrong number of arguments to method")))

(define (%apply-interpreted-method args metadata method-body)
  (if (= (length args)
         (length (bard:%method-signature metadata)))
      (let* ((sig (bard:%method-signature metadata))
             (env (zip sig args)))
        (bard:eval method-body env))
      (error "wrong number of arguments to method")))

(define (%make-method #!key (name #f)(signature '())(method-function %undefined-method-body))
  (let* ((valid-name (%validate-function-name name))
         (valid-signature (%validate-method-signature signature))
         (meta (bard:%make-method-metadata valid-name valid-signature)))
    (let ((metadata meta))
      (lambda args
        (%apply-method args metadata method-function)))))

(define (%make-interpreted-method #!key (name #f)(signature '())(method-body '()))
  (let* ((valid-name (%validate-function-name name))
         (valid-signature (%validate-method-signature signature))
         (meta (bard:%make-interpreted-method-metadata valid-name valid-signature (cons 'begin method-body))))
    (let ((metadata meta))
      (lambda args
        (%apply-interpreted-method args metadata method-body)))))

(define (%function? x)
  (bard:%function-metadata? (%function-metadata x)))

(define (%method? x)
  (bard:%method-metadata? (%function-metadata x)))

;;; (define $m (%make-method signature: '(x y) method-function: (lambda (x y)(cons x y))))
;;; ($m 1 2)

(define (%method-param-type p)
  (cadr p))

(define (%add-method! fun meth)
  (let ((m-argcount (%method-argcount meth))
        (f-argcount (%function-argcount fun)))
    (if (= m-argcount f-argcount)
        (let* ((mtable (bard:%function-method-table (%function-metadata fun)))
               (mtypes (map %method-param-type (bard:%method-signature (%function-metadata meth)))))
          (table-set! mtable mtypes meth))
        (error "can't add method to function: mismatched argument lists"))))

;;; ---------------------------------------------------------------------
;;; type support
;;; ---------------------------------------------------------------------

