#lang racket


; Auxiliary functions:

; update : (a -> b) a b -> (a -> b)
(define (update f x y)
  (λ (x*)
    (if (equal? x x*)
        y
        (f x*))))

; Assertion:
; if x != x'
; then (f x) == ((update f x' y) x)

; Assertion:
; ((update f x y) x) == y

; update* : (a -> b) [a] [b] -> (a -> b)
(define (update* f xs ys)
  (match* (xs ys)
    [['() '()]    f]
    [[(cons x xs*) (cons y ys*)]
     (update* (update f x y) xs* ys*)]))



; A-Normal Form:

; <lam> ::= (λ (<var> ...) <exp>)

; <aexp> ::= <integer>
;         |  <var>
;         |  #t | #f
;         |  (<prim> <aexp> ...)

; <cexp> ::= (set! <var> <aexp>)
;         |  (call/cc <aexp>)
;         |  (if <aexp> <exp> <exp>)
;         |  (letrec ([<var> <aexp>] ...) <exp>)
;         |  (<aexp> <aexp> ...)

; <exp> ::= <cexp>
;        |  <aexp>
;        |  (let ([<var> <exp>]) <exp>)


; atom? : exp -> boolean?
(define (atom? exp)
  (match exp
    [`(λ ,_ ,_)   #t]
    [(? symbol?)    #t]
    [(? boolean?)   #t]
    [(? integer?)   #t]
    [(cons (? prim?) _)  #t]
    [else           #f]))

; prim? : symbol? -> boolean?
(define (prim? exp)
  (case exp
    [(+ - * = void) #t]
    [else      #f]))


; CESK machine state:
(struct state {control        ; exp
               environment    ; env
               store          ; store
               continuation}) ; kont

; ρ : env = symbol -> addr

; σ : store = addr -> value

; value = integer + boolean + clo + cont

; clo ::= (clo <lam> <env>)

; κ : kont ::= (letk <var> <exp> <env> <kont>)
;           |  halt

; cont ::= (cont <kont>)

; addr = a set of unique addresses;
;        for this machine, symbols work;
;        gensym can create fresh addresses



; apply-kont : kont value store -> (state + answer)
(define (apply-kont κ value σ)
  (match κ
    ; Apply the halt continuation:
    ['halt
     `(answer ,value ,σ)]
    
    ; Resume execution:
    [`(letk ,v ,e ,ρ ,κ)
     
     (define a* (gensym 'a)) ; fresh address
     
     (state e (update ρ v a*) (update σ a* value) κ)]))

; prim->proc : symbol? -> procedure?
(define (prim->proc prim)
  (match prim
    ['+    +]
    ['-    -]
    ['*    *]
    ['=    =]
    ['void void]))
                   
; eval-atom : aexp env store -> value
(define (eval-atom atom ρ σ)
  (match atom
    [(? symbol?)    (σ (ρ atom))]
    [(? boolean?)   atom]
    [(? integer?)   atom]
    
    [(cons (and prim (? prim?)) rest)
     ; =>
     (let ([args (map (eval-atom/curry ρ σ) rest)])
       (apply (prim->proc prim) args))]
       
    [`(λ ,_ ,_) 
     ; =>
     `(clo ,atom ,ρ)]
    
    [else           (error "unknown atom")]))

; eval-atom/curry : env store -> aexp -> value
(define (eval-atom/curry ρ σ)
  (λ (atom) 
    (eval-atom atom ρ σ)))


; env0 is the initial (empty) environment
(define env0 (λ (v) (error (format "unbound variable: ~a" v))))

; store0 is the initial (empty) store
(define store0 (λ (addr) (error (format "unbound address: ~a" addr))))

; inject : exp -> state
(define (inject exp)
  (state exp env0 store0 'halt))

; apply-proc : value [value] store kont -> (state + answer)
(define (apply-proc proc args σ κ)
  (match proc
    
    ; apply a closure:
    [`(clo (λ ,vars ,body) ,ρ)
     ; =>
     ; allocate fresh addresses:
     (define addrs (map gensym vars)) 
     
     ; update the environment:
     (define ρ* (update* ρ vars addrs))
     
     ; update the store:
     (define σ* (update* σ addrs args))
     
     (state body ρ* σ* κ)]
    
    ; apply a continuation:
    [`(cont ,κ*)
     (apply-kont κ* (car args) σ)]))
  
; step : state -> (state + answer)
(define (step ς)
  (match ς
    
    ; return:
    [(state (and atom (? atom?)) ρ σ κ)
     ; =>
     ; evaluate the return value:
     (define return-value (eval-atom atom ρ σ))
       
     (apply-kont κ return-value σ)]
        
    ; conditional evaluation:
    [(state `(if ,cond ,cons ,alt) ρ σ κ)
     ; =>
     (if (eval-atom cond ρ σ)
         (state cons ρ σ κ)
         (state alt ρ σ κ))]
      
    ; call with current continuation:
    [(state `(call/cc ,f) ρ σ κ)
     ; =>
     ; look up the procedure to call:
     (define proc (eval-atom f ρ σ))
     
     ; capture the current continuation:
     (define current-continuation `(cont ,κ))
     
     (apply-proc proc (list current-continuation) σ κ)]
    
    ; mutation:
    [(state `(set! ,v ,aexp) ρ σ κ)
     ; =>
     (define value (eval-atom aexp ρ σ))
     
     (define σ* (update σ (ρ v) value))
     
     (apply-kont κ (void) σ*)]
    
    [(state `(letrec ([,vars ,aexps] ...) ,body) ρ σ κ)
     ; =>
     ; allocate fresh addresses:
     (define addrs (map gensym vars)) 

     ; update the environment:
     (define ρ* (update* ρ vars addrs)) 
     
     ; evaluate the expressions with the *new* env:
     (define values (map (eval-atom/curry ρ* σ) aexps))
     
     ; update the store:
     (define σ* (update* σ addrs values))
     
     (state body ρ* σ* κ)]
    
    ; let-binding:
    [(state `(let ([,v ,exp]) ,body) ρ σ κ)
     ; =>
     (state exp ρ σ `(letk ,v ,body ,ρ ,κ))]
    
    ; function application:
    [(state `(,f . ,es) ρ σ κ)
     ; =>
     ; evaluate the procedure:
     (define proc (eval-atom f ρ σ))
     
     ; evaluate the arguments:
     (define args (map (λ (x) (eval-atom x ρ σ)) es))

     (apply-proc proc args σ κ)]))
    


; step* : state -> answer
(define (step* ς)
  (if (state? ς)
      (step* (step ς))
      ς))

; run : exp -> answer
(define (run exp)
  (define ς0 (inject exp))
  (step* ς0))


; example
(define fact-prog
  '(letrec ([f (λ (n)
                 (if (= n 0)
                     1
                     (let ([n-1! (f (- n 1))])
                       (* n n-1!))))])
     (f 5)))

(run fact-prog)
  