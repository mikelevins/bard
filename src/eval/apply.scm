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

(define (%apply-keyed-collection op args)
  (if (= 1 (%length args))
      (cond
       ((%null? op) %nil)
       ((string? op)(string-ref op (%car args)))
       ((%list? op)(%list-ref op (%car args)))
       ((%frame? op)(%frame-get op (%car args)))
       (else (error (string-append "Not an applicable object: " (%as-string op)))))
      (error (string-append "Too many arguments: " (%as-string args)))))

(define (%apply-primitive-method op args)
  (let* ((required-count (%method-required-count op))
         (argcount (%length args)))
    (if (< argcount required-count)
        (error (string-append "Not enough arguments: " (%as-string args)))
        (let ((rest? (%method-restarg op)))
          (if (and (not rest?)(> argcount required-count))
              (error (string-append "Too many arguments: " (%as-string args)))
              (let ((fn (%method-function op)))
                (case argcount
                  ((0)(fn))
                  ((1)(let ((a (%list-ref args 0)))
                        (fn a)))
                  ((2)(let ((a (%list-ref args 0))
                            (b (%list-ref args 1)))
                        (fn a b)))
                  ((3)(let ((a (%list-ref args 0))
                            (b (%list-ref args 1))
                            (c (%list-ref args 2)))
                        (fn a b c)))
                  ((4)(let ((a (%list-ref args 0))
                            (b (%list-ref args 1))
                            (c (%list-ref args 2))
                            (d (%list-ref args 3)))
                        (fn a b c d)))
                  (else (let ((parms (%ralist->cons args)))
                          (apply fn parms))))))))))

(define (%method-lexical-environment env params rest vals)
  (let loop ((env env)
             (formals params)
             (args vals))
    (if (%null? args)
        ;; out of args
        (if (%null? formals)
            env
            (error (string-append "Not enough arguments: " (%as-string vals))))
        ;; more args to process
        (if (%null? formals)
            (if (%true? rest)
                (%add-binding env rest args)
                (error (string-append "Too many arguments: " (%as-string vals))))
            (loop (%add-binding env (%car formals)(%car args))
                  (%cdr formals)
                  (%cdr args))))))

(define (%apply-interpreted-method op params)
  (let* ((required-count (%method-required-count op))
         (argcount (%length params)))
    (if (< argcount required-count)
        (error (string-append "Not enough arguments: " (%as-string params)))
        (let ((rest (%method-restarg op))
              (body (%method-body op))
              (env (%method-environment op))
              (formals (%method-formals op)))
          (let* ((env (%method-lexical-environment env formals rest params)))
            (%eval body env))))))

(define (%apply op args)
  (cond
   ((%keyed-collection? op)(%apply-keyed-collection op args))
   ((%primitive-method? op)(%apply-primitive-method op args))
   ((%interpreted-method? op)(%apply-interpreted-method op args))
   (else (error (string-append "not an applicable object: " (%as-string op) "; args: " (%as-string args))))))

(define %funcall (lambda (fn . args)(%apply fn (%cons->ralist args))))
