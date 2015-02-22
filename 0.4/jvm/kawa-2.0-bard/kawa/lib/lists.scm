(module-export pair? cons null? set-car! set-cdr! car cdr
	       caar cadr cdar cddr
	       caaar caadr cadar caddr cdaar cdadr cddar cdddr
	       caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	       cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	       length reverse list-tail list-ref list-set!
               list? make-list reverse!
	       memq memv member assq assv assoc list-copy)

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.reflection>)

(define (pair? x) :: <boolean>
  (<pair>:instance? (gnu.mapping.Promise:force x)))

(define (cons car cdr) :: <pair>
  (<pair>:new car cdr))

(define (null? x) :: <boolean>
  (eq? (gnu.mapping.Promise:force x) '()))

(define (set-car! (p <pair>) x)
  (set! p:car x))

(define (set-cdr! (p <pair>) x)
  (set! p:cdr x))

(define-procedure car
  setter: set-car!
  (lambda (x :: <pair>) name: 'car
	  x:car))

(define-procedure cdr
  setter: set-cdr!
  (lambda (x :: <pair>) name: 'cdr
	  x:cdr))

(define-syntax define-cxr
  (lambda (form)
    (syntax-case form ()
       ((_ fname slots)
       #`(define-procedure fname
	   setter: (lambda (arg value)
		     #,(syntax-case #'slots ()
			((first1 . rest1)
			 #`(set!
			    (field
                             (as <pair>
                                 #,(let loop ((f #'rest1))
                                     (syntax-case f ()
                                       (() #'arg)
                                       ((first . rest)
                                        #`(field (as <pair> #,(loop #'rest))
                                                 '#,(syntax-object->datum #'first))))))
                             'first1)
			    value))))
	   (lambda (arg) name: 'fname
		   #,(let loop ((f #'slots))
		      (syntax-case f ()
			(() #'arg)
			((first . rest)
			 #`(field (as <pair> #,(loop #'rest))
				  '#,(syntax-object->datum #'first)))))))))))

(define-cxr caar (car car))
(define-cxr cadr (car cdr))
(define-cxr cdar (cdr car))
(define-cxr cddr (cdr cdr))
(define-cxr caaar (car car car))
(define-cxr caadr (car car cdr))
(define-cxr cadar (car cdr car))
(define-cxr caddr (car cdr cdr))
(define-cxr cdaar (cdr car car))
(define-cxr cdadr (cdr car cdr))
(define-cxr cddar (cdr cdr car))
(define-cxr cdddr (cdr cdr cdr))
(define-cxr caaaar (car car car car))
(define-cxr caaadr (car car car cdr))
(define-cxr caadar (car car cdr car))
(define-cxr caaddr (car car cdr cdr))
(define-cxr cadaar (car cdr car car))
(define-cxr cadadr (car cdr car cdr))
(define-cxr caddar (car cdr cdr car))
(define-cxr cadddr (car cdr cdr cdr))
(define-cxr cdaaar (cdr car car car))
(define-cxr cdaadr (cdr car car cdr))
(define-cxr cdadar (cdr car cdr car))
(define-cxr cdaddr (cdr car cdr cdr))
(define-cxr cddaar (cdr cdr car car))
(define-cxr cddadr (cdr cdr car cdr))
(define-cxr cdddar (cdr cdr cdr car))
(define-cxr cddddr (cdr cdr cdr cdr))

(define (length list :: <list>) :: <int>
  (invoke-static <list> 'length list))

(define (reverse (list :: <list>)) :: <list>
  (let loop ((arg list) (result '()))
    (if (null? arg) result
	(let ((pair :: <pair> arg))
	  (loop (cdr pair) (cons (car pair) result))))))

(define (list-tail list (count :: <int>))
  (let loop ((lst list))
    (set! count (- count 1))
    (cond ((< count 0)
           lst)
          (else
           (let ((flst (gnu.mapping.Promise:force lst)))
             (if (gnu.lists.Pair? flst)
                 (loop ((as gnu.lists.Pair flst):getCdr))
                 (primitive-throw (java.lang.IndexOutOfBoundsException "List is too short."))))))))

(define (list-set! list index::int obj)::void
  (set-car! (list-tail list index) obj))

(define-procedure list-ref
  setter: list-set!
  (lambda (list index::int) name: 'list-ref
          (car (list-tail list index))))

(define (list? obj) :: <boolean>
  (>= (invoke-static <list> 'listLength obj #f) 0))

(define (make-list k::int #!optional (fill #!null)) ::list
  (let loop ((result ::list '())
	     (i ::int k))
    (if (> i 0)
	(loop (cons fill result) (- i 1))
	result)))

;; Not in R5RS, but is in Guile (with extra mystery argument).
(define (reverse! (list :: <list>)) :: <list>
  (invoke-static <list> 'reverseInPlace list))

(define (memq x list)
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (let ((p :: pair lst))
	   (if (eq? x p:car) lst
	       (lp p:cdr))))))

(define (memv x list)
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (let ((p :: pair lst))
	   (if (eqv? x p:car) lst
	       (lp p:cdr))))))

;;;  The optional test argument is an srfi-1 extension.
(define (member x list #!optional (test :: <procedure> equal?))
  (let lp ((lst list))
    (and (instance? lst <pair>)
	 (let ((p :: pair lst))
	   (if (test x p:car) lst
	       (lp p:cdr))))))

(define (assq x list)
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair> (car list)))
	  (if (eq? pair:car x)
	      pair
	      (lp (cdr list)))))))

(define (assv x list)
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair> (car list)))
	  (if (eqv? pair:car x)
	      pair
	      (lp (cdr list)))))))

;;;  The optional test argument is an srfi-1 extension.
(define (assoc key list #!optional (test :: <procedure> equal?))
  (let lp ((list list))
    (if (eq? list '())
	 #f
	(let ((pair :: <pair> (car list)))
	  (if (test key pair:car) pair
	      (lp (cdr list)))))))

(define (list-copy obj)
  (let ((result obj)
        (prev ::pair #!null))
    (let recur ((x obj))
      (if (pair? x)
	  (let* ((pold ::pair x)
                 (pnew (cons (pold:getCar) #!null)))
	    (if (eq? prev #!null)
		(set! result pnew)
		(set-cdr! prev pnew))
	    (set! prev pnew)
	    (recur (pold:getCdr)))
           (if (not (eq? prev #!null))
               (set-cdr! prev x))))
    result))

