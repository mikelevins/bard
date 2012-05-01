;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          apply.scm
;;;; Project:       Bard
;;;; Purpose:       the apply function
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (%single-arg args)
  (if (= (length args) 1)
      (car args)
      (error (string-append "argument list for apply has the wrong number of arguments " 
                            (object->string args)))))

(define (%apply-string s args)
  (string-ref s (%single-arg args)))

(define (%apply-list ls args)
  (list-ref ls (%single-arg args)))

(define (%apply-frame fr args)
  (%frame-get fr (%single-arg args)))

(define (%method-lexical-environment meth args)
  (let* ((ampersand? (lambda (x)(eq? x '&)))
         (env (%method-environment meth))
         (formals (%method-parameters meth))
         (required-formals (take-before ampersand? formals))
         (required-count (length required-formals))
         (required-args (take required-count args))
         (rest-args? (any? ampersand? formals)))
    (if rest-args?
        (let* ((rest-formal (cadr (drop-before ampersand? formals)))
               (rest-args (drop required-count args))
               (ext-formals (append required-formals (list rest-formal)))
               (ext-args (append required-args (list rest-args)))
               (plist (interleave ext-formals ext-args)))
          (%extend-environment env plist))
        (let ((plist (interleave required-formals required-args)))
          (%extend-environment env plist)))))

(define (%apply-bard-method method args)
  (let* ((method-body (%method-body method))
         (method-env (%method-environment method))
         (lexical-env (%method-lexical-environment method args)))
    (%eval method-body lexical-env)))

(define (%no-applicable-method fn args)
  (error (string-append "No applicable method for " (%as-string fn) " with arguments " (%as-string args))))

(define (%apply-bard-function fn args)
  (let* ((methods (%function-ordered-methods fn args)))
    (if (null? methods)
        (%no-applicable-method fn args)
        (%apply-bard-method (car methods) args))))

(define (%apply applicable args)
  (cond
     ((null? applicable) (%nothing))
     ((string? applicable)(%apply-string applicable args))
     ((list? applicable)(%apply-list applicable args))
     ((%frame? applicable)(%apply-frame applicable args))
     ((procedure? applicable)(apply applicable args))
     ((%method? applicable)(%apply-bard-method applicable args))
     ((%function? applicable)(%apply-bard-function applicable args))
     (else (error "not an applicable object" applicable))))

;;; (%apply '() '())
;;; (%apply "Fred" '(3))
;;; (define $fr (->frame 'name "Fred" 'age 101 'shape 'square 'color "orange"))
;;; (%apply $fr '(name))
;;; (%apply $fr '(shape))
;;; (%apply $fr '(hypotenuse))
;;; (%apply + '())
;;; (%apply + '(1))
;;; (%apply + '(1 2))
;;; (%apply + '(1 2 3))

