(defun car (x)
  (if (null x)
      nil
      (invoke (the pair x) '|getCar|)))

(defun first (x)
  (car x))

(defun cdr (x)
  (if (null x)
      nil
      (invoke (the pair x) '|getCdr|)))

(defun rest (x)
  (cdr x))

(defun second (x)
  (first (rest x)))

(defun third (x)
  (first (rest (rest x))))

(defun nthcdr (n lst)
  (declare (int n))
  (do ((i n (1- i))
       (result lst (cdr result)))
      ((= i 0) result)))

(defun nth (n x)
  (first (nthcdr n x)))

(defun 1- (x) (- x 1))
(defun 1+ (x) (+ x 1))

(defun acons (key datum alist)
  (cons (cons key datum) alist))

(defun listp (obj)
  (typep obj 'list))

(defun numberp (obj)
  (typep obj 'number))

(defun atom (obj)
  (not (consp obj)))

(defun eql (x y)
  (eqv? x y))

(defun complement (pred)
  (lambda (&rest arguments)
    (not (apply pred arguments))))

(defun member-with-test (x lst test key)
  (declare (list lst))
  (cond ((null lst) nil)
	((funcall test x (funcall key (car lst))) lst)
	(t (member-with-test x (cdr lst) test key))))

(defun member-with-key (x lst key)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (funcall key (car lst))) lst)
	(t (member-with-key x (cdr lst) key))))

(defun member-plain (x lst)
  (declare (list lst))
  (cond ((null lst) nil)
	((eql x (car lst)) lst)
	(t (member-plain x (cdr lst)))))

(defun member (x lst &key key
		       (test nil test-supplied)
		       (test-not nil test-not-supplied))
  (declare (list lst))
  (cond (test-supplied
	 (member-with-test x lst test key))
	(test-not-supplied
	 (member-with-test x lst (complement test-not) key))
	(key
	 (member-with-key x lst key))
	(t
	 (member-plain x lst))))

(defun apply (func &rest args)
  (invoke (the |function|
	       (if (symbolp func)
		   (symbol-function func)
		   func))
	  '|applyN|
	  (invoke-static |gnu.kawa.functions.Apply|
			 '|getArguments|
			 args
			 0 #'apply)))

(defun funcall (func &rest args)
  (apply func args))

(defun minusp (x)
  (< x 0))

(defun plusp (x)
  (> x 0))

;; ANSI: This should be inclosed in "an implicit block whose name is
;; the function block name of the function-name or name, as
;; appropriate." But we don't have support for CL blocks yet.
(define-syntax flet
  (syntax-rules ()
    ((_ ((fname parameters body ...) ...)
	e ...)
     (%flet ((fname (lambda parameters body ...)) ...)
	    e ...))))

(define-syntax labels
  (syntax-rules ()
    ((labels ((fname parameters body ...) ...) e ...)
     (flet ((fname parameters #!void) ...)
        (set! #'fname (lambda parameters body ...)) ...
	e ...))))

;; This is a hack. The calling conventions of Kawa will need to
;; be adjusted to make this conform to ANSI CL.
;; To wit (t-woo)
;; (multiple-value-bind (x) (values 1 2 3) (list x)) ;=> <error>. In
;; fact, passing an improper number of arguments to a continuation
;; implicitly accepting a single value is undefined in Scheme.
;;
;; The workaround of course is to provide superfluous arguments,
;; (multiple-value-bind (x y z) (values 1 2 3) (list x)) ;=> (1)
(define-syntax multiple-value-bind
  (syntax-rules ()
    ((_ parameters producer body ...)
     (call-with-values (lambda () producer)
       (lambda parameters body ...)))))

(defun floor (number &optional (divisor 1))
  (values (div number divisor) (remainder number divisor)))
