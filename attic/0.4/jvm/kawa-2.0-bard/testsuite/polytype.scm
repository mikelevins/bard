;;; POLYTYPE -- A polymorphic type inferencer for Scheme.
(test-begin "polytype" 1)

;------------------------------------------------------------------------------
;
;                     A polymorphic type inferencer for Scheme
;
;                              Marc Feeley (05/16/88)
;
; Modified by Per Bothner (Febrary 2012) to fit into SRFI-64 test framework
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
; Error handling.

(define (fail msg) (error "error:" msg))

;------------------------------------------------------------------------------
;
; An environment is an object that associates variables to values.  They are
; represented by association lists.

(define (env-empty) ; returns an empty environment
  '())

(define (env-update var val env) ; returns a copy of 'env' but with 'var'
  (cons (cons var val) env))     ; associated to 'val'

(define (env-join env1 env2) ; return a new environment containing the bindings
  (append env1 env2))        ; in env1 and env2 (env1 has precedence)

(define (env-bound? var env) ; returns true if the variable is bound in 'env'
  (assq var env))

(define (env-value var env) ; returns the value associated to 'var' in 'env'
  (cdr (assq var env)))

;------------------------------------------------------------------------------
;
; Variables are symbols starting with a "?" (e.g. ?a)

(define variable-count #f)

(define (new-variable)
  (set! variable-count (+ variable-count 1))
  (string->symbol (string-append "?" (number->string variable-count))))

(define (variable? x)
  (and (symbol? x) (char=? (string-ref (symbol->string x) 0) #\?)))

(define (variable<? x y)
  (string<? (symbol->string x) (symbol->string y)))

;------------------------------------------------------------------------------
;
; The unifier.

(define (unify x y initial-env) ; return the values that must be substituted
  (define (uni x y env)         ; for the variables so that x and y unify
    (let ((x* (deref x env))
          (y* (deref y env)))
      (cond ((eq? x* y*)
             env)
            ((and (variable? x*) (or (not (variable? y*)) (variable<? y* x*)))
             (env-update x* y* env))
            ((variable? y*)
             (env-update y* x* env))
            ((and (pair? x*) (pair? y*))
             (uni (cdr x*) (cdr y*) (uni (car x*) (car y*) env)))
            (else
             (fail "can not unify the two structures")))))
  (uni x y initial-env))

(define (deref var env) ; get the value of a variable
  (if (variable? var)
    (if (env-bound? var env)
      (deref (env-value var env) env)
      var)
    var))

(define (substitute x env) ; return x, where each variable is substituted for
  (cond ((variable? x)     ; the value it is associated to in env
         (let ((y (deref x env)))
           (if (variable? y) y (substitute y env))))
         ((pair? x)
          (cons (substitute (car x) env) (substitute (cdr x) env)))
         (else
          x)))

;------------------------------------------------------------------------------

(define (instance x prefix)   ; generate an instance of x with new variables
  (define (new x env success) ; in place of the generic variables in prefix
    (cond ((and (variable? x) (generic? x prefix))
           (if (env-bound? x env)
             (success (env-value x env) env)
             (let ((var (new-variable)))
               (success var (env-update x var env)))))
           ((pair? x)
            (new (car x) env
              (lambda (a env)
                (new (cdr x) env
                  (lambda (b env)
                    (success (cons a b) env))))))
           (else
            (success x env))))
  (new x (env-empty) (lambda (a env) a)))

(define (generic? var prefix) ; is the variable 'var' generic in the 'prefix'
  (cond ((null? prefix)
         #t)
        ((and (eq? (cadar prefix) var) (not (eq? (caar prefix) 'let)))
         #f)
        (else
         (generic? var (cdr prefix)))))

;------------------------------------------------------------------------------

(define global-var-types
  '(
     (+        . (-> num num num))
     (-        . (-> num num num))
     (*        . (-> num num num))
     (/        . (-> num num num))
     (<        . (-> num num bool))
     (<=       . (-> num num bool))
     (=        . (-> num num bool))
     (>=       . (-> num num bool))
     (char>?   . (-> char char bool))
     (char<?   . (-> char char bool))
     (char<=?  . (-> char char bool))
     (char=?   . (-> char char bool))
     (char>=?  . (-> char char bool))
     (char>?   . (-> char char bool))
     (cons     . (-> ?a (list ?a) (list ?a)))
     (car      . (-> (list ?a) ?a))
     (cdr      . (-> (list ?a) (list ?a)))
     (set-car! . (-> (list ?a) ?a ()))
     (set-cdr! . (-> (list ?a) (list ?a) ()))
     (null?    . (-> (list ?a) bool))
     (length   . (-> (list ?a) num))
     (append   . (-> (list ?a) (list ?a) (list ?a)))
     (reverse  . (-> (list ?a) (list ?a)))
     (map      . (-> (-> ?a ?b) (list ?a) (list ?b)))
     (not      . (-> bool bool))
     (call/cc  . (-> (-> (-> ?a ?b) ?c) ?a))
     (apply    . (-> (-> ?a ?b) (list ?a) ?b)) ; works for monadic procs only
     (write    . (-> ?a ()))
   )
)

(define (constant-type x)
  (cond ((number? x)                'num)
        ((char? x)                  'char)
        ((or (eq? x #t) (eq? x #f)) 'bool)
        ((null? x)                  '(list ?a))
        ((pair? x)
         (let ((element-type (constant-type (car x))))
           (for-each (lambda (y)
                       (if (not (equal? (constant-type y) element-type))
                         (fail "list is not homogenous")))
                     (cdr x))
           (list 'list element-type)))
        (else (fail "unknown constant type"))))

(define (quoted-constant-type x)
  (if (eq? x '())
    '(list ?a) ; patch because #f = ()
    (constant-type x)))

(define (map-left-right f lst)
  (let loop ((lst lst))
    (if (pair? lst)
      (let ((x (f (car lst))))
        (cons x (loop (cdr lst))))
      '())))

(define (map-left-right2 f lst1 lst2)
  (let loop ((lst1 lst1) (lst2 lst2))
    (if (pair? lst1)
      (let ((x (f (car lst1) (car lst2))))
        (cons x (loop (cdr lst1) (cdr lst2))))
      '())))

(define (type f) ; return the type of expression 'f'
  (define e (env-empty))
  (define (j p f) ; algorithm 'j' from Robin Milner's paper
    (cond ((symbol? f)
           (if (env-bound? f p)
             (let ((x (env-value f p)))
               (let ((kind (car x))
                     (type (cadr x)))
                 (if (eq? kind 'let) (instance type p) type)))
             (instance (cdr (assq f global-var-types)) (env-empty))))
          ((not (pair? f))
           (instance (constant-type f) (env-empty)))
          ((eq? (car f) 'quote)
           (instance (quoted-constant-type (cadr f)) (env-empty)))
          ((eq? (car f) 'if)
           (let* ((pre (j p (cadr f)))
                  (con (j p (caddr f)))
                  (alt (j p (cadddr f))))
             (set! e (unify con alt (unify pre 'bool e)))
             con))
          ((eq? (car f) 'lambda)
           (let ((parms (map-left-right (lambda (x) (new-variable)) (cadr f))))
             (let ((body
                    (j (env-join (map-left-right2
                                  (lambda (x y) (list x 'lambda y))
                                  (cadr f)
                                  parms)
                                 p)
                       (caddr f))))
               (cons '-> (append parms (list body))))))
          ((eq? (car f) 'let)
           (j (env-join (map-left-right
                         (lambda (x) (list (car x) 'let (j p (cadr x))))
                         (cadr f))
                        p)
              (caddr f)))
          ((eq? (car f) 'letrec)
           (let ((p* (env-join (map-left-right
                                (lambda (x)
                                  (list (car x) 'letrec (new-variable)))
                                (cadr f))
                               p)))
             (for-each
               (lambda (x)
                 (let ((val (j p* (cadr x))))
                   (set! e (unify (cadr (env-value (car x) p*)) val e))))
               (cadr f))
             (j p* (caddr f))))
          ((and (pair? (car f)) (eq? (caar f) 'lambda))
           (j p (list 'let
                      (map-left-right2 list (cadar f) (cdr f))
                      (caddar f))))
          (else
           (let* ((result (new-variable))
                  (oper (j p (car f)))
                  (args (map-left-right (lambda (x) (j p x)) (cdr f))))
             (set! e (unify oper (cons '-> (append args (list result))) e))
             result))))
  (let ((t (j (env-empty) f)))
    (substitute t e)))

;------------------------------------------------------------------------------
;
; Some examples:

(define example1
  '(cdr '(1 2 3)))

(define example2
  '(lambda (x y) (if (car x) (car y) (length y))))

(define example3
  '(letrec ((fact (lambda (x) (if (< x 2) 1 (* x (fact (- x 1)))))))
     fact))

(define example4
  '(let ((ident (lambda (x) x))) (ident ident)))

(define example5
  '(letrec ((length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l)))))))
     length))

(define example6
  '(letrec ((map (lambda (f l)
                   (if (null? l) '() (cons (f (car l)) (map f (cdr l)))))))
     map))

(define example7
  '(lambda (x)
     (+ 4 (call/cc (lambda (exit)
                     (if (null? x) (exit 0) (length x)))))))

(define (run)

  (set! variable-count 0)

  (let* ((e1 (type example1))
         (e2 (type example2))
         (e3 (type example3))
         (e4 (type example4))
         (e5 (type example5))
         (e6 (type example6))
         (e7 (type example7)))
    (list e1 e2 e3 e4 e5 e6 e7)))

(define expected
  '((list num)
    (-> (list bool) (list num) num)
    (-> num num)
    (-> ?20 ?20)
    (-> (list ?24) num)
    (-> (-> ?33 ?34) (list ?33) (list ?34))
    (-> (list ?51) num)))

;(define (main . args)
;  (run-benchmark
;    "polytype"
;    polytype-iters
;    (lambda () (run))
;    (lambda (result) (equal? result expected))))

(test-equal expected (run))

(test-end)
