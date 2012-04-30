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

(define (%apply-not-defined thing args)
  (error "not an applicable value" thing))

(define (%no-applicable-method fn args)
  (error "no applicable method" fn args))

(define (%extend-environment-with-formals-and-args env formals args)
  (let* ((ampersand? (lambda (x)(eq? x '&)))
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
  (if (procedure? method)
      (apply method args)
      (if (%method? method)
          (let* ((formals (%method-parameters method))
                 (env (%extend-environment-with-formals-and-args env formals args))
                 (body (%method-body method)))
            (%eval body env))
          (error "not a method" method))))

(define (%apply-bard-function fn args)
  (let* ((types (map %object->bard-type args))
         (methods (%function-ordered-methods fn types)))
    (if (null? methods)
        (%no-applicable-method fn args)
        (%apply-bard-method (car methods) args))))

(define (%apply applicable args)
  (let ((argcount (length args)))
    (cond
     ((null? applicable) (%nothing))
     ((string? applicable)(let ((i (if (= argcount 1)
                                       (car args)
                                       (error (string-append "wrong number of arguments applying a string " 
                                                             (object->string argcount))))))
                            (string-ref applicable i)))
     ((list? applicable)(let ((i (if (= argcount 1)
                                     (car args)
                                     (error (string-append "wrong number of arguments applying a list " 
                                                           (object->string argcount))))))
                          (list-ref applicable i)))
     ((%frame? applicable)(let ((k (if (= argcount 1)
                                       (car args)
                                       (error (string-append "wrong number of arguments applying a list " 
                                                             (object->string argcount))))))
                            (%frame-get applicable k)))
     ((procedure? applicable)(apply applicable args))
     ((%method? applicable)(%apply-bard-method applicable args))
     ((%function? applicable)(%apply-bard-function applicable args))
     (else (error "not an applicable object" applicable)))))

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
