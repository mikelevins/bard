;; A macro to define enums.
;; Jamison Hope

(module-static #t)
(module-export define-enum)

(define (make-field-desc (t-name :: symbol)
                         (e-name :: symbol)
                         (e-val :: int))
  ;; "Produces a field description for the constant E-NAME in
  ;;  enumeration T-NAME with value E-VAL."
  `(,e-name :: ,t-name
            allocation: 'static access: '(enum final)
            init: (,t-name ,(symbol->string e-name) ,e-val)))

(define (make-init)
  ;; "Returns a list in the form of an enum constructor."
  `((*init* (str :: String) (num :: int))
    access: 'private
    (invoke-special java.lang.Enum (this) '*init* str num)))

(define (make-values (t-arr :: symbol)
                     (e-names :: list))
  ;; "Returns a list in the form of an values method which will return
  ;;  an array of type T-ARR containing the enumeration constants
  ;;  E-NAMES."
  `((values) :: ,t-arr allocation: 'static (,t-arr ,@e-names)))

(define-syntax (define-enum form)
  ;; "(define-enum TNAME option-pair ... (E0 ... EN) OTHER-DEF ...)
  ;;  creates a Java enumeration class. This only supports "simple"
  ;;  enums which do not require a custom constructor.
  (syntax-case form ()
    ((_ "findkeywords" tname (k ...) keyword value e ...) (identifier? (syntax keyword))
     #`(define-enum "findkeywords" tname (keyword value k ...) e ...))
    ((_ "findkeywords" tname (k ...) e ...)
     #`(%define-enum tname (k ...) e ...))
    ((_) (report-syntax-error form "no enum type name given"))
    ((_ tname) (report-syntax-error form "no enum constants given"))
    ((_ tname e ...)
     #`(define-enum "findkeywords" tname () e ...))))

(define (map-names (t-name ::symbol) (e-names ::list) (i ::int))::list
  (if (null? e-names) '()
      (cons (make-field-desc t-name (car e-names) i)
	    (map-names t-name (cdr e-names) (+ i 1)))))

(define-syntax (%define-enum form)
  (syntax-case form ()
    ((_ typename option-pairs (e ...) other-def ...)
     (let* ((t-name :: symbol (syntax typename))
            (t-arr :: symbol (string->symbol (string-append
                                              (symbol->string t-name) "[]")))
            (e-names :: list (syntax (e ...)))
            (n :: int (length e-names))
            (field-descs :: list (map-names t-name e-names 0))
            (init :: list (make-init))
            (values-method :: list (make-values t-arr e-names))
            (opts :: list (syntax option-pairs))
            (other-defs :: list (syntax (other-def ...))))
       #`(define-simple-class #,(datum->syntax-object form t-name)
           (java.lang.Enum) access: '(enum final)
           #,@(datum->syntax-object form opts)
           #,(datum->syntax-object form init)
           #,(datum->syntax-object form values-method)
	   ((valueOf s::String)::#,(datum->syntax-object form t-name)
	    allocation: 'static
	    (java.lang.Enum:valueOf #,(datum->syntax-object form t-name) s))
           #,@(datum->syntax-object form field-descs)
           #,@(datum->syntax-object form other-defs))))))
