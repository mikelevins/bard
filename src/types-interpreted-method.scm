;;;; ***********************************************************************
;;;;
;;;; Name:          types-interpreted-method.scm
;;;; Project:       Bard
;;;; Purpose:       schema <interpreted-method>
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings))

;;; =====================================================================
;;; base schemas
;;; =====================================================================
;;; ----------------------------------------------------------------------
;;; <interpreted-method>
;;; ----------------------------------------------------------------------

(define tags:$bard-interpreted-method (%next-bard-type-number))
(define <interpreted-method> (make-base-schema '<interpreted-method> tags:$bard-interpreted-method))

;;; constructor

(define (%merge-symbol-restarg env rest args)
  (%add-binding env rest args))

(define (%merge-table-restarg env rest args)
  (let* ((restmap (merge-alists (alist-table-instance-slots rest)
                                (plist->alist args)))
         (param-map (map (lambda (slot)
                           (cons (string->symbol (keyword->string (car slot)))
                                 (cdr slot)))
                         restmap)))
    (let loop ((params param-map)
               (out-env env))
      (if (null? params)
          out-env
          (let ((param (car params)))
            (loop (cdr params)
                  (%add-binding out-env (car param)(cdr param))))))))

(define (%merge-restarg env rest args)
  (if (symbol? rest)
      (%merge-symbol-restarg env rest args)
      (%merge-table-restarg env rest args)))

(define (%method-lexical-environment env params rest vals)
  (let loop ((env env)
             (formals params)
             (args vals))
    (if (null? args)
        ;; out of args
        (if (null? formals)
            (if rest (%merge-restarg env rest args) env)
            (error (str "Not enough arguments: " vals)))
        ;; more args to process
        (if (null? formals)
            (if rest
                (%merge-restarg env rest args)
                (error (str "Too many arguments: " vals)))
            (loop (%add-binding env (car formals)(car args))
                  (cdr formals)
                  (cdr args))))))

(define (make-interpreted-method #!key
                                 (formal-parameters '())
                                 (restarg #f)
                                 (body '(begin))
                                 (debug-name 'an-anonymous-interpreted-method)
                                 (environment (%null-environment)))
  (let* ((required-count (length formal-parameters))
         (method (make-interpreted-method-instance 
                  <interpreted-method> debug-name #f formal-parameters restarg required-count environment body))
         (method-proc (lambda args
                        (let* ((argcount (length args)))
                          (if (< argcount required-count)
                              (error (str "Expected " required-count "arguments, but found " (length args)))
                              (let* ((env (%method-lexical-environment (interpreted-method-environment method)
                                                                       formal-parameters restarg args)))
                                (%eval body env)))))))
    (set-interpreted-method-proc! method method-proc)
    method))

;;; accessors

(define interpreted-method? interpreted-method-instance?)
(define interpreted-method-name interpreted-method-instance-name)
(define interpreted-method-proc interpreted-method-instance-proc)
(define set-interpreted-method-proc! interpreted-method-instance-proc-set!)
(define interpreted-method-formals interpreted-method-instance-formals)
(define interpreted-method-restarg interpreted-method-instance-restarg)
(define interpreted-method-required-count interpreted-method-instance-required-count)
(define interpreted-method-environment interpreted-method-instance-environment)
(define set-interpreted-method-environment! interpreted-method-instance-environment-set!)
(define interpreted-method-body interpreted-method-instance-body)

