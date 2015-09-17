;;; For now these are written in Scheme.
;;; They should be re-written in Common Lisp, but there are still some
;;; limitations in the Common Lisp support making that difficult.

;; SYMBOLS

(define (boundp symbol) :: |clisp:boolean|
  ((primitive-static-method <gnu.commonlisp.lang.Symbols> "isBound"
                            <boolean> (<object>))
   symbol))

(define (symbolp x) :: |clisp:boolean|
  (invoke-static <gnu.commonlisp.lang.Symbols> 'isSymbol x))

(define (symbol-name symbol)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'getPrintName symbol))

;(define (make-symbol NAME) ...)

; (define (intern name #!optional obarray)  ...)

; (define (intern-soft NAME #!optonal obarray) ..)

;; obarray
;; mapatoms
;; unintern

(define (symbol-plist symbol)
 (gnu.mapping.PropertyLocation:getPropertyList symbol))

(define (setplist symbol plist)
  (gnu.mapping.PropertyLocation:setPropertyList symbol plist)
  plist)

(define (plist-get plist prop #!optional default)
  (gnu.mapping.PropertyLocation:plistGet plist prop default))

(define (plist-put plist prop value)
  (gnu.mapping.PropertyLocation:plistPut plist prop value))

(define (plist-remprop plist prop)
  (gnu.mapping.PropertyLocation:plistRemove plist prop))

(define (plist-member plist prop)
  (if (eq?
       (gnu.mapping.PropertyLocation:plistGet plist prop #!void)
       #!void)
      '() 't))

(define (get (symbol :: <gnu.mapping.Symbol>) property #!optional (default '()))
  (gnu.mapping.PropertyLocation:getProperty symbol property default))

(define (put symbol property value)
  (gnu.mapping.PropertyLocation:putProperty symbol property value))

;; VARIABLES

;;A `void-variable' error is signaled if SYMBOL has neither a local
;;     binding nor a global value.

(define (symbol-value sym)
  (invoke (gnu.mapping.Environment:getCurrent) 'get (invoke-static <gnu.commonlisp.lang.Symbols> 'getSymbol sym)))

;; setq

;(define (make-symbol NAME)  ...)

(define (set symbol value)
  ((gnu.mapping.Environment:getCurrent):put
   (<gnu.commonlisp.lang.Symbols>:getSymbol symbol)
   value))

#|
(define (add-to-list symbol value)
  (let ((old (symbol-value symbol)))
    (or (elisp::member value old) ;; FIXME
	(set symbol (cons value (symbol-value symbol))))))
|#

;; FUNCTIONS

;; This returns the object in the function cell of SYMBOL.  If the
;; symbol's function cell is void, a `void-function' error is signaled.

(define (symbol-function symbol)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'getFunctionBinding
		 symbol))

;(define (fboundp x) ..)

;(fmakunboud symbol)

(define (fset symbol object)
  (invoke-static <gnu.commonlisp.lang.Symbols> 'setFunctionBinding
		 (invoke-static <gnu.mapping.Environment> 'getCurrent)
		 symbol object))

;;; ARRAYS

(define (length (x :: <gnu.lists.Sequence>))
  (invoke x 'size))

(define (arrayp x) |clisp:boolean|
  (instance? x <gnu.lists.SimpleVector>))

(define (aref
         (array <gnu.lists.SimpleVector>)
         (k <int>))
  (invoke array 'get k))

(define (aset (array <gnu.lists.SimpleVector>)
	      (k <int>)
	      obj)
  (invoke array 'set k obj)
  obj)

(define (fillarray (array <gnu.lists.SimpleVector>) obj)
  (invoke array 'fill obj)
  obj)

;;; STRINGS

(define (stringp x) |clisp:boolean|
  (instance? x <string>))

(define (make-string (count :: <int>) ch)
  (make <gnu.lists.FString> count (invoke-static <gnu.commonlisp.lang.CommonLisp> 'asChar ch)))

(define (substring (str :: <string>) from #!optional (to '()))
  (if (eq? to '())
      (set! to (string-length str)))
  (if (< to 0)
      (set! to (- (string-length str) to)))
  (if (< from 0)
      (set! from (- (string-length str) from)))
  (make <gnu.lists.FString> str (as <int> from) (as <int> (- to from))))

(define (char-to-string ch)
  (make <gnu.lists.FString> 1 (invoke-static <gnu.commonlisp.lang.CommonLisp> 'asChar ch)))

(define (functionp x) |clisp:boolean|
  (instance? x <function>))

(define (princ value #!optional (out (current-output-port))) :: <void>
  (gnu.commonlisp.lang.CommonLisp:displayFormat:format value out))

(define (prin1 value #!optional (out (current-output-port))) :: <void>
  (gnu.commonlisp.lang.CommonLisp:writeFormat:format value out))
