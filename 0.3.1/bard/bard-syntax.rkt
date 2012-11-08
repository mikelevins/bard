(module bard-syntax racket
  (require racket/generator)
  (require "bard-semantics.rkt")
  (provide (except-out (all-from-out racket) define let)
           (rename-out (def define))
           (rename-out (bind let))
           ^ loop method unless when
           (all-from-out "bard-semantics.rkt"))
  
  ;;; define
  ;;; TODO: add:
  ;;;  macro
  ;;;  vector
  (define-syntax def
    (syntax-rules (class macro method protocol -> record variable vector)
      ((def class classname)
       (define classname (make-bard-class)))
      ((def method (fname (arg type) ...) expr ...)
       (display '(bard-add-method! fname (list type ...) (lambda (arg ...) expr ...))))
      ((def protocol pname [(fname pclass ...) -> (rclass ...)] ...)
       (display '(define pname (make-protocol (list (make-function fname (list pclass ...)(list rclass ...)) ...)))))
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
  (define-syntax ~
    (syntax-rules (in where yield then)
      ((~ x in vals)(make-bard-series (generator () 
                                                 (let loop ((items vals)) 
                                                   (if (null? items)
                                                       (begin
                                                         (yield (car vals))
                                                         (loop (cdr vals)))
                                                       (begin
                                                         (yield (car items))
                                                         (loop (cdr items))))))))
      ))
  
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

