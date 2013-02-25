;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          types-function.scm
;;;; Project:       Bard
;;;; Purpose:       schema <function>
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;; =====================================================================
;;; base schemas
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <function>
;;; ----------------------------------------------------------------------

(define tags:$bard-function (%next-bard-type-number))
(define <function> (make-base-schema '<function> tags:$bard-function))

;;; constructor

(define (%add-method! fn argtypes method)
  (let ((method-tree (function-method-tree fn)))
    (if (null? argtypes)
        (set-function-thunk-method! fn method)
        (apply %singleton-tree-put! method method-tree argtypes))
    fn))

(define (%remove-method! fn argtypes)
  (let ((method-tree (function-method-tree fn)))
    (if (null? argtypes)
        fn
        (begin
          (apply %singleton-tree-remove! method-tree argtypes)
          fn))
    fn))

(define (%add-primitive-method! fn argtypes method-proc #!key (debug-name #f)(restarg #f))
  (let* ((required-count (length argtypes))
         (method (make-primitive debug-name: debug-name
                                 procedure: method-proc
                                 required-count: required-count
                                 restarg: restarg)))
    (%add-method! fn argtypes method)
    fn))

(define (%search-method-tree-for-value mtree val)
  (let* ((sing (%existing-singleton val))
         (found (if sing (%singleton-tree-ref mtree sing) #f)))
    (or found
        (let* ((tp (%value->schema val))
               (found (%singleton-tree-ref mtree tp)))
          (or found
              (%singleton-tree-ref mtree Anything))))))

(define (%search-method-tree-for-values mtree vals)
  (if (null? vals)
      #f
      (let ((found (%search-method-tree-for-value mtree (car vals))))
        (if found
            (if (null? (cdr vals))
                (if (%singleton-tree? found)
                    #f
                    found)
                (if (%singleton-tree? found)
                    (%search-method-tree-for-values found (cdr vals))
                    #f))
            #f))))

(define (%function-best-method fn vals)
  (let* ((sigs (function-signatures fn))
         (sig (car sigs))
         (input-count (length (signature-input-types sig)))
         (vals (if (signature-restarg sig)
                   (take input-count vals)
                   vals)))
    (if (null? vals)
        (function-thunk-method fn)
        (%search-method-tree-for-values (function-method-tree fn) vals))))

(define (make-function #!key 
                       (debug-name #f)
                       (signatures '()))
  (let* ((fn (make-function-instance <function> debug-name #f signatures #f (%singleton-tree)))
         (fn-proc (lambda args
                    (let ((best-method (%function-best-method fn args)))
                      (if best-method
                          (%apply best-method args)
                          (error (string-append "No applicable method for "
                                                (string #\newline) (%as-string fn) (string #\newline)
                                                "with arguments " 
                                                (string-join " " (map %as-string args)))))))))
    (set-function-proc! fn fn-proc)
    fn))

;;; accessors

(define function? function-instance?)
(define function-name function-instance-name)
(define function-proc function-instance-proc)
(define set-function-proc! function-instance-proc-set!)
(define function-thunk-method function-instance-thunk-method)
(define set-function-thunk-method! function-instance-thunk-method-set!)
(define function-method-tree function-instance-method-tree)
(define function-signatures function-instance-signatures)
(define set-function-signatures! function-instance-signatures-set!)

(define (function-add-signatures! fn sigs)
  (for-each (lambda (sig)
              (let ((fn-sigs (function-signatures fn)))
                (if (not (some? (partial signature-equal? sig) fn-sigs))
                    (set-function-signatures! fn (cons sig fn-sigs)))))
            sigs)
  fn)



