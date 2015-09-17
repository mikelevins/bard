(require <kawa.lib.prim_syntax>)

(define-syntax (provide form)
  (syntax-case form ()
    ((provide 'feature)
     #`(define-constant
         #,(datum->syntax-object
           form
           (string->symbol
            (string-append "%provide%"
                           (symbol->string
                            (syntax-object->datum (syntax feature))))))
         ;; The value doesn't matter.
         ::int 123))
    ((_ . rest)
     (report-syntax-error form "provide requires a quoted feature-name"))))

(define-syntax test-begin
  (syntax-rules ()
    ((test-begin suite-name)
     (begin
       (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
       (%test-begin suite-name #f)))
    ((test-begin suite-name count)
     (begin
       (cond-expand (srfi-64 #!void) (else (require 'srfi-64)))
       (%test-begin suite-name count)))))

(define-syntax module-uri
  (lambda (form)
    (syntax-case form ()
      ((_)
       (gnu.kawa.functions.GetModuleClass:getModuleClassURI
	(gnu.expr.Compilation:getCurrent))))))

(define-syntax resource-url
  (syntax-rules ()
    ((resource-url uri)
     (gnu.kawa.io.URLPath:valueOf
      (((((module-uri):resolve uri):toURL):openConnection):getURL)))))

#|
(define-syntax source-file
  (lambda (x)
    (syntax-case x ()
		 ((_ form)
		  (let ((form (syntax-object->datum (syntax (form)))))
		    (if (instance? form <gnu.lists.PairWithPosition>)
			(list (quote quote)
			      (datum->syntax-object form (gnu.lists.PairWithPosition:getFileName form)))
			#f))))))
|#
