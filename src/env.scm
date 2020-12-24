;;;; ***********************************************************************
;;;;
;;;; Name:          env.scm
;;;; Project:       Bard
;;;; Purpose:       variable-binding environments
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; global environment
;;; ---------------------------------------------------------------------

(define (%global-variables)
  (make-table test: eq?))

(define $bard-global-variables #f)

(define (%defglobal var val)
  (table-set! $bard-global-variables var val)
  var)

(define (%remglobal var)
  (table-set! $bard-global-variables var)
  var)

(define (%global-value var)
  (table-ref $bard-global-variables var #!unbound))

;;; ---------------------------------------------------------------------
;;; lexical environments
;;; ---------------------------------------------------------------------

(define (%null-environment) '())

(define (%add-binding env var val)
  (cons (cons var val) env))

(define (%eval-single-let-binding binding env)
  (%add-binding env
                (car binding)
                (%eval (cadr binding)
                       env)))

(define (%eval-multiple-let-binding binding env)
  (let* ((count (length binding))
         (vars (take (- count 1) binding))
         (vals-expr (car (drop (- count 1) binding))))
    (call-with-values (lambda ()(%eval vals-expr env))
      (lambda vals
        (let loop ((vars vars)
                   (vals vals)
                   (env env))
          (if (null? vars)
              env
              (let ((var (car vars))
                    (val (car vals)))
                (loop (cdr vars)
                      (cdr vals)
                      (%add-binding env var val)))))))))

(define (%eval-let-binding binding env)
  (cond
   ((= (length binding) 2) (%eval-single-let-binding binding env))
   ((> (length binding) 2) (%eval-multiple-let-binding binding env))
   (error (str "Malformed let binding: " (%as-string binding)))))

(define (%add-let-bindings env bindings)
  (if (null? bindings)
      env
      (let ((binding (car bindings)))
        (%add-let-bindings (%eval-let-binding binding env)
                           (cdr bindings)))))

(define (%extend-environment env plist)
  (if (null? plist)
      env
      (if (null? (cdr plist))
          (error "Odd number of arguments to extend-environment" plist)
          (let* ((var (car plist))
                 (val (cadr plist)))
            (%extend-environment (%add-binding env var val) (cddr plist))))))

(define (%merge-environments env1 env2)
  (append env2 env1))

(define (%copy-environment env)
  (if (null? env)
      '()
      (let ((binding (car env)))
        (cons (cons (car binding)
                      (cdr binding))
            (%copy-environment (cdr env))))))

(define (%lookup-variable-value env var)
  (let ((binding (assq var env)))
    (if binding (cdr binding) #!unbound)))

(define (%set-variable! var val env)
  (let ((binding (assq var env)))
    (if binding
        (begin
          (set-cdr! binding val)
          val)
        (let ((global-val (%global-value var)))
          (if (%defined? global-val)
              (begin
                (%defglobal var val)
                val)
              (error (string-append "Undefined variable: " (symbol->string var))))))))

;;; ---------------------------------------------------------------------
;;; variable utils
;;; ---------------------------------------------------------------------

(define (%globally-bound? varname)
  (let* ((not-found (gensym))
         (val (table-ref $bard-global-variables varname not-found)))
    (if (eq? not-found val)
        #f
        #t)))

(define (%bound? varname #!optional (env (%null-environment)))
  (if (assq varname env)
      #t
      (%globally-bound? varname)))


