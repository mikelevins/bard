;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          compiler.scm
;;;; Project:       bard
;;;; Purpose:       the bard compiler

(define (comp:self-evaluating? x)
  (cond
   ((<syntax-atom>:syntax-atom? x)(let ((stype (<frame>:get seq syntax-type:)))
                                    (if (memq stype '(eof undefined nothing boolean character
                                                          integer float ratio text))
                                        #t
                                        #f)))
   ((<syntax-sequence>:syntax-atom? x)(let ((stype (<frame>:get seq syntax-type:)))
                                        (if (memq stype '(sequence frame))
                                            #t
                                            #f)))
   (else (error "unrecognized syntax" x))))

(define (comp:compile-self-evaluating exp)
  (comp:gen PUSH (comp:syntax->value exp)))

(define (comp:variable? exp)
  (eq? 'name (<frame>:get seq syntax-type:)))

(define (comp:compile-variable exp env)
  (if (comp:variable-in-environment? exp env)
      (comp:gen LVAR exp env)
      (comp:gen GVAR exp env)))

(define (comp:application? exp)
  (eq? 'application (<frame>:get seq syntax-type:)))

(define (comp:application-op exp)
  )

(define (comp:prim? op)
  )

(define (bard:compile-prim-call exp env)
  )

(define (comp:macro? op)
  )

(define (bard:compile-macro-call exp env)
  )

(define (comp:special-form? op)
  )

(define (bard:compile-special-form-call exp env)
  )

(define (bard:compile-apply exp env)
  )

(define (bard:compile-application exp env)
  (let ((op (comp:application-op exp)))
    (cond
     ((comp:prim? op)(bard:compile-prim-call exp env))
     ((comp:macro? op)(bard:compile-macro-call exp env))
     ((comp:special-form? op)(bard:compile-special-form-call exp env))
     (else (bard:compile-apply exp env)))))

(define (comp:sequence? exp)
  (eq? 'sequence (<frame>:get seq syntax-type:)))

(define (bard:compile-sequence exp env)
  )

(define (comp:frame? exp)
  (eq? 'frame (<frame>:get seq syntax-type:)))

(define (bard:compile-frame exp env)
  )

(define (bard:compile exp env)
  (cond
   ((comp:self-evaluating? exp)(comp:compile-self-evaluating exp))
   ((comp:variable? exp)(comp:compile-variable exp env))
   ((comp:application? exp)(bard:compile-application exp env))
   ((comp:sequence? exp)(bard:compile-sequence exp env))
   ((comp:frame? exp)(bard:compile-frame exp env))
   (else "invalid expression" exp)))