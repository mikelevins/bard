(module bard-syntax racket
  (require racket/generator)
  (require "bard-semantics.rkt")
  (provide (except-out (all-from-out racket) define let)
           (rename-out (def define))
           (rename-out (bind let))
           ^ function loop method series unless when
           (all-from-out "bard-semantics.rkt"))
  
  ;;; define
  (define-syntax def
    (syntax-rules (class macro method protocol -> record variable vector)
      ((def class classname)
       (define classname (make-bard-class)))
      ((def macro mname expander)
       (display '(bard-define-macroexpander mname expander)))
      ((def method (fname (arg type) ...) expr ...)
       (display '(bard-add-method! fname (list type ...) (lambda (arg ...) expr ...))))
      ((def protocol pname [(fname pclass ...) -> (rclass ...)] ...)
       (display '(define pname (make-bard-protocol (list `(fname . ,(make-bard-function (list pclass ...)(list rclass ...))) ...)))))
      ((def record rname () sname ...)
       (display '(define rname (bard-make-record-schema '() (list (make-slot sname) ...)))))
      ((def record rname (include ...) sname ...)
       (display '(define rname (bard-make-record-schema (list include ...) (list (make-slot sname) ...)))))
      ((def variable vname expr)
       (define vname expr))
      ))
  
  ;;; let
  (define-syntax bind 
    (syntax-rules ()
      ((bind ((v val) ...) expr ...)
       (let* ((v val) ...) expr ...))
      ((bind ((a b ... val)) expr ...)
       (let*-values (((a b ...) val)) expr ...))))
  
  (define-syntax function
    (syntax-rules (->)
      ((function (pclass ...) -> (rclass ...))
       (make-bard-function (list pclass ...) (list rclass ...)))))
  
  ;;; loop
  (define-syntax loop
    (syntax-rules ()
      ((loop loopname ((x v) ...) body ...)
       (let loopname ((x v) ...) body ...))))
  
  ;;; method, λ, ^, lambda
  (define-syntax ^
    (syntax-rules ()
      ((^ lambda-list body ...)
       (lambda lambda-list body ...))))
  (define-syntax method
    (syntax-rules ()
      ((method lambda-list body ...)
       (lambda lambda-list body ...))))
  
  ;;; series
  (define-syntax series
    (syntax-rules (in where yield then)
      ((series x in vals)(make-bard-series (display '(generate-values vals))))
      ((series x in vals where expr)(make-bard-series (display '(generate-filtered-values (lambda (x) expr) vals))))
      ((series ((x xval) ...) yield: valexpr then: (nextx ...))
       (make-bard-series (display '(bard-generator ((x xval) ...)
                                                   (yield valexpr)
                                                   (then nextx ...)))))))

  
  
  ;;; unless
  (define-syntax unless
    (syntax-rules ()
      ((unless test exp ...)
       (if test (void) (begin exp ...)))))
  
  ;;; when
  (define-syntax when
    (syntax-rules ()
      ((unless test exp ...)
       (if test (begin exp ...)(void)))))
  
  ;;; with-exit
  (define-syntax with-exit
    (syntax-rules ()
      ((with-exit (x) body ...)
       (call/cc (lambda (x) body ...) )))))

