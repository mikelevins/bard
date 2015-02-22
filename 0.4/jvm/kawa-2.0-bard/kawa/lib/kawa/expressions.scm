;; Experimental API for manipulating and validating expressions.

(module-name (kawa expressions))
(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)

(define (->exp obj) ::gnu.expr.Expression
  (cond ((gnu.expr.Expression? obj)
         obj)
        ((gnu.expr.Declaration? obj)
         (gnu.expr.ReferenceExp (->gnu.expr.Declaration obj)))
        (else
         (gnu.expr.QuoteExp:getInstance obj))))

(define (get-visitor) ::gnu.expr.InlineCalls
  (gnu.expr.InlineCalls:currentVisitor:get))

(define (get-compilation) ::gnu.expr.Compilation
  ((get-visitor):getCompilation))

(define (visit-exp exp::gnu.expr.Expression
                   #!optional (required ::gnu.bytecode.Type #!null))
  ::gnu.expr.Expression
  (gnu.expr.ExpVisitor:visit (get-visitor) exp required))

(define-syntax syntax-as-exp
  (lambda (form)
    (syntax-case form ()
      ((_ expr)
       (syntax->expression (syntax expr))))))

(define (apply-exp func . args)
  (gnu.expr.ApplyExp (->exp func)
                     @(map ->exp args)))

(define (begin-exp . args)
  (gnu.expr.BeginExp @(map ->exp args)))

(define (if-exp a b #!optional (c #!null))
  (gnu.expr.IfExp (->exp a) (->exp b) (if (eq? c #!null) c (->exp c))))

(define (set-exp (var::gnu.expr.Declaration) val)
  (let ((se (gnu.expr.SetExp var (->exp val))))
    (se:setContextDecl var)
    (var:setCanWrite #t)
    (se:setBinding var)
    (var:noteValueFromSet se)
    se))

(define-syntax define-validate
  (syntax-rules ()
    ((_ name (exp req proc) clauses ...)
     (define (name
              exp::gnu.expr.ApplyExp
              visitor::gnu.expr.InlineCalls
              required::gnu.bytecode.Type
              proc::gnu.mapping.Procedure) ::gnu.expr.Expression
              (let ((ex ::gnu.expr.Expression
                        (cond clauses ... (else #!null))))
                (if (eq? ex #!null) #!null
                    (gnu.expr.ExpVisitor:visit visitor (ex:maybeSetLine exp)
                                               required)))))))
