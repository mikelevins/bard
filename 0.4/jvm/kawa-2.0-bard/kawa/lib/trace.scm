(require <kawa.lib.prim_syntax>)

(define-syntax %do-trace
  (syntax-rules ()
		((%do-trace proc flag)
		 (set! proc
		       (invoke-static
			 <kawa.standard.TracedProcedure> 'doTrace
			 proc flag)))))

(define-syntax trace
  (syntax-rules ()
		((trace proc ...)
		 (begin (%do-trace proc #t) ...))))

(define-syntax untrace
  (syntax-rules ()
		((untrace proc ...)
		 (begin (%do-trace proc #f) ...))))

(define (disassemble (proc :: procedure))
  (gnu.expr.PrimProcedure:disassemble proc))
