(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.lists>)

(define-namespace <CaseClause> <gnu.expr.CaseExp>:CaseClause)
(define-namespace <CaseExp> <gnu.expr.CaseExp>)
(define-namespace <Expression> <gnu.expr.Expression>)
(define-namespace <QuoteExp> <gnu.expr.QuoteExp>)

;; Needed when the case is inside a macro
(define (syntax-form->datum obj)
  (if (kawa.lang.SyntaxForm? obj)
      ((as kawa.lang.SyntaxForm obj):getDatum)
      obj))

;; Converts a list of datums to a list of QuoteExp
(define (clause-datums->exps datums)
  (if (null? datums) ()
      (let* ((tr ::kawa.lang.Translator (gnu.expr.Compilation:getCurrent))
             (datum (tr:rewrite_car datums #f)))
        (cons 
         (<QuoteExp>:getInstance 
          (syntax-form->datum (car datums)) 
          datum)
         (clause-datums->exps (cdr datums))))))

;; Creates the case clauses to be passed to the CaseExp
(define (syntax->case-clauses s-clauses key)
    (if (not (null? s-clauses))
        (let* ((clause (car s-clauses))
               (datums (car clause))
               (rest (cdr s-clauses)))
          (cons
           (<CaseClause>
            (<Expression>[] @(clause-datums->exps datums))
            (case-clause->expression clause key))
           (syntax->case-clauses rest key)))
        ()))

;; Creates a begin expression from the expressions included in a clause
(define (case-clause->expression s-clause key)
  (if (or (null? s-clause) (null? (cdr s-clause)))
      (report-syntax-error s-clause "too few expressions for a 'case' clause")
      (let* ((exp (cdr s-clause))
             (exp (if (eq? (car exp) '=>)
                      (list (append (cdr exp) (list key)))
                      exp)))
        (syntax->expression (cons 'begin exp)))))

(define-syntax case (syntax-rules ()
    ((case key clauses ...)
     (let ((tmp key))
         (%case tmp clauses ...)))))

(define-rewrite-syntax %case
  (lambda (x)
    (syntax-case x (else =>)
      ((_ case-key case-clause ... (else case-else ...))
       (<CaseExp>
         (syntax->expression (syntax case-key))
         (<CaseClause>[]
                @(syntax->case-clauses (syntax (case-clause ...))
                                       (syntax case-key)))
         (<CaseClause>
           (case-clause->expression (syntax (else case-else ...))
                                    (syntax case-key)))))
      ((_ case-key case-clause ...)
       (<CaseExp>
         (syntax->expression (syntax case-key))
         (<CaseClause>[]
                @(syntax->case-clauses (syntax (case-clause ...))
                                       (syntax case-key))))))))
