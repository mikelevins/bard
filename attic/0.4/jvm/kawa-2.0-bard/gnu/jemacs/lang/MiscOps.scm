;; FIXME
(define (buffer-disable-undo buf) '())

;; FIXME
(define (sit-for n) (sleep n))

;; FIXME
(define (input-pending-p) '())

;; FIXME
(define (message msg . args) (format #t msg))

;; FIXME
(define (provide name) '())

(define (purecopy obj) obj)

(define (minibuffer-depth) 0)  ;; FIXME

;; The 'if' primitive in version takes an arbitary number of 'else'
;; expressions, in contrast to the Scheme and CommonLisp definitions.

(define-rewrite-syntax if
  (lambda (x)
    (syntax-case x ()
		 ((_ test then)
		  (make <gnu.expr.IfExp>
		    (syntax->expression (syntax test))
		    (syntax->expression (syntax then))
		    (static-field <gnu.commonlisp.lang.Lisp2> 'nilExpr)))
		 ((_ test then else ...)
		  (make <gnu.expr.IfExp>
		    (syntax->expression (syntax test))
		    (syntax->expression (syntax then))
		    (syntax-body->expression (syntax (begin else ...)))))
		 ((_ . rest)
		  (report-syntax-error (syntax rest)
				"too few expressions for 'if'")))))

(define-syntax catch
  (syntax-rules ()
		((catch tag body ...)
		 (try-catch (begin body ...)
			    (ex <gnu.jemacs.lang.CatchableException>
				(invoke ex 'match tag))))))
(define-syntax condition-case
  (syntax-rules ()
		((_ var body (handler-name . handler-body) ...)
                 (try-catch body
                            (ex gnu.jemacs.lang.CatchableException
                                (cond ((ex:match 'handler-name) . handler-body)
                                      ...
                                      (else (primitive-throw ex))))))))

(define (throw tag value) :: <never-returns>
  (primitive-throw
   (make <gnu.jemacs.lang.CatchableException> tag value)))

;; Should use setf, not setq, and guard against duplicate evaluation.  FIXME.
(define-syntax push
  (syntax-rules ()
		((push x place)
		 (setq place (cons x place)))))

(define (format fmt #!rest (args :: <Object[]>))
  (invoke-static 'gnu.kawa.functions.Format 'formatToFString #\% fmt args))

(define (quit-char) #\bel)

(define (emacs:read #!optional (port (current-input-port)))
  ((<gnu.kawa.lispexpr.LispReader> port):readObject))

(define (princ value #!optional (out (current-output-port))) :: <void>
  (gnu.jemacs.lang.ELisp:displayFormat:format value out))

(define (prin1 value #!optional (out (current-output-port))) :: <void>
  (gnu.jemacs.lang.ELisp:writeFormat:format value out))

(define (prefix-numeric-value arg)
  ;; FIXME
  arg)
