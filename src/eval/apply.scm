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
  (let* ((env (%method-environment meth))
         (formals (%method-parameters meth))
         (required-count (%method-required-count meth)))
    (let loop ((formals formals)
               (args args)
               (i 0)
               (env env))
      (if (>= i required-count)
          (if (null? formals)
              env
              (%add-binding env (cadr formals) args))
          (loop (cdr formals)(cdr args)(+ i 1)(%add-binding env (car formals)(car args)))))))

(define (%apply-method method args)
  (let* ((method-body (%method-body method))
         (lexical-env (%method-lexical-environment method args)))
    (%eval method-body lexical-env)))

(define (%no-applicable-method fn args)
  (error (string-append "No applicable method for " (%as-string fn) " with arguments " (%as-string args))))

(define (%apply-function fn args)
  (let ((method (%function-best-method fn args)))
    (if method
        (%apply-method method args)
        (%no-applicable-method fn args))))

(define (%apply applicable args)
  (cond
   ((%function? applicable)(%apply-function applicable args))
   ((%method? applicable)(%apply-method applicable args))
   ((procedure? applicable)(apply applicable args))
   ((%frame? applicable)(%apply-frame applicable args))
   ((%list? applicable)(%apply-list applicable args))
   ((string? applicable)(%apply-string applicable args))
   ((%null? applicable) (%nothing))
   (else (error "not an applicable object" applicable))))

(define %funcall 
  (lambda (fn . args)
    (cond
     ((%function? fn)(%apply-bard-function fn args))
     ((%method? fn)(%apply-method fn args))
     ((procedure? fn)(apply fn args))
     (else (error "not an applicable object" fn)))))

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

