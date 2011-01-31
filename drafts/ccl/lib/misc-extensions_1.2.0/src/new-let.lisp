;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: New-Let -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package :new-let)

;;; This code implements a new LET macro with expanded syntax and semantics,
;;; a generalization of LET, LET*, and MULTIPLE-VALUE-BIND.  Some examples:
;;;
;;;   (let ((a (foo))
;;;         ((b (bar a))))
;;;     ...)
;;;
;;; This example illustrates that clause nesting depth is used to indicate
;;; ordering of evaluation and binding.  B is bound after A, and its initial
;;; value expression refers to A.
;;;
;;;   (let ((a b c (zot))
;;;         ((d (quux a c))
;;;          ((e f (mumble b d))
;;;           (g (mung a))))
;;;         ((h (frobozz c))
;;;          ((i (xyzzy h))))
;;;         (*print-level* 3))
;;;     ...)
;;;
;;; A, B, and C are bound to the first three values of (ZOT), and in parallel,
;;; *PRINT-LEVEL* is bound to 3; then D and H are bound; then E, F, G, and I
;;; are bound.
;;;
;;; As this example illustrates, all bindings at a given nesting level are
;;; done in parallel, with all bindings at a deeper level following.
;;; 
;;; Since I like to use multiple values, I find this syntax for binding them
;;; very handy, and I think many will agree.  (Those familiar with Dylan
;;; will think that I have borrowed the idea from it, but I wrote the first
;;; version of this macro in 1980.)  The value of using nesting to indicate
;;; sequencing will perhaps be less clear.  The additional flexibility
;;; provided, compared to LET*, is admittedly rarely of importance in terms
;;; of expressing an idea in fewer keystrokes.  Personally, though, I like
;;; being able to indicate clearly the data flow dependences among the
;;; various variables I may be binding in one LET; and I have written LET
;;; expressions of complexity comparable to the second example above.  (I
;;; should emphasize that the breaking up of the clauses into groups, as in
;;; that second example, to emphasize their data dependence relationships
;;; is strictly for clarity; in fact, the initial value expression for G,
;;; for instance, is within the scope of H.)
;;;
;;; This code also implements an extension to COND.  It is simply this: that
;;; if the predicate expression of a COND clause is a LET form, the scope of
;;; all variables bound by the LET is extended to include the consequent
;;; expressions of the clause.  (However, it does not include subsequent
;;; clauses.)  This simplifies the writing of somewhat Prolog-like code that
;;; simultaneously tests that an object has a certain structure and binds
;;; variables to parts of that structure in order to do something else.
;;; (In order to be recognized as such, the predicate expression must be
;;; written as a LET form, not a macro invocation that expands to a LET form.
;;; I think this is a feature, but am open to being persuaded otherwise.)
;;;
;;; To use these macros, you must shadow the standard definitions in your
;;; package.  This can be done by including the following option clause in
;;; your DEFPACKAGE form:
;;;
;;;    (:shadowing-import-from "NEW-LET" "LET" "COND")
;;;
;;; If for some reason you don't want to shadow these, you can access this
;;; version of LET as NLET, and this version of COND as BCOND (the "B" is
;;; for "binding"), by using the following DEFPACKAGE option instead:
;;;
;;;    (:import-from "NEW-LET" "NLET" "BCOND")
;;;
;;; Enjoy!
;;; Scott L. Burson   2/18/2005


(defmacro let (clauses &body body)
  "A generalization of CL:LET that better supports nested bindings and multiple
values.  Syntax: (let (<clause>*) <body>).  The <clause> syntax is more general
than for CL:LET:
  <clause>  ::=   <symbol>                 ; binds to NIL
                | ( <symbol> )             ; likewise
		| <clause1>
  <clause1> ::=   ( <symbol>+ <form> )     ; binding
                | ( <clause1>+ )           ; nesting
When a clause begins with more than one variable name, they are to be bound to
successive values of the form.  The nesting of clauses indicates sequencing of
bindings; more deeply nested clauses may reference bindings of shallower clauses.
All bindings at a given depth are done in parallel.  This allows arbitrary
combinations of parallel and sequential binding.  Standard declarations at the
head of BODY are handled correctly, though nonstandard ones may not be.  If two
variables of the same name are bound at different levels, any declaration
applies to the inner one."
  (multiple-value-bind (decls body)
      (analyze-decls clauses body)
    (car (expand-new-let clauses body decls))))

;;; Alternative name for the above.  I could have this one expand into that
;;; one, or conversely, but I'd want to duplicate the doc string anyway, and
;;; that's most of the code.
(defmacro nlet (clauses &body body)
  "A generalization of CL:LET that better supports nested bindings and multiple
values.  Syntax: (let (<clause>*) <body>).  The <clause> syntax is more general
than for CL:LET:
  <clause>  ::=   <symbol>                 ; binds to NIL
                | ( <symbol> )             ; likewise
		| <clause1>
  <clause1> ::=   ( <symbol>+ <form> )     ; binding
                | ( <clause1>+ )           ; nesting
When a clause begins with more than one variable name, they are to be bound to
successive values of the form.  The nesting of clauses indicates sequencing of
bindings; more deeply nested clauses may reference bindings of shallower clauses.
All bindings at a given depth are done in parallel.  This allows arbitrary
combinations of parallel and sequential binding.  Standard declarations at the
head of BODY are handled correctly, though nonstandard ones may not be.  If two
variables of the same name are bound at different levels, any declaration
applies to the inner one."
  (multiple-value-bind (decls body)
      (analyze-decls clauses body)
    (car (expand-new-let clauses body decls))))

(defun expand-new-let (clauses body decls)
  (labels ((expand-1 (this-level-single this-level-multiple next-level body decls)
	     (cl:cond ((and this-level-multiple
			    (null (cdr this-level-multiple))
			    (null this-level-single))
			 (cl:let ((vars (butlast (car this-level-multiple))))
			   (multiple-value-bind (body decls)
			       (expand-1 nil nil next-level body decls)
			     (values `((multiple-value-bind ,vars
					   ,(car (last (car this-level-multiple)))
					 ,@(bound-decls decls vars)
					 ,@(and (null next-level)
						(mapcar #'(lambda (d) `(declare ,d))
							(cdr decls)))
					 . ,body))
				     (prune-decls decls vars)))))
		      (this-level-multiple
		       (let* ((vars (butlast (car this-level-multiple)))
			      (gensyms (mapcar #'(lambda (x)
						   (declare (ignore x))
						   (gensym))
					       vars)))
			 (multiple-value-bind (body decls)
			     (expand-1 (append (mapcar #'list vars gensyms)
					       this-level-single)
				       (cdr this-level-multiple) next-level body decls)
			   (values `((multiple-value-bind ,gensyms
					 ,(car (last (car this-level-multiple)))
				       ,@(bound-decls decls vars)
				       ,@(and (null next-level)
					      (mapcar #'(lambda (d) `(declare ,d))
						      (cdr decls)))
				       . ,body))
				   (prune-decls decls vars)))))
		      (this-level-single
		       (cl:let ((vars (mapcar #'(lambda (x) (if (consp x) (car x) x))
					      this-level-single)))
			 (multiple-value-bind (body decls)
			     (expand-1 nil nil next-level body decls)
			   (values `((cl:let ,this-level-single
				       ,@(bound-decls decls vars)
				       ,@(and (null next-level)
					      (mapcar #'(lambda (d) `(declare ,d))
						      (cdr decls)))
				       . ,body))
				   (prune-decls decls vars)))))
		      (next-level
		       (expand-new-let next-level body decls))
		      ((cdr decls)
		       (values `((locally ,@(mapcar #'(lambda (d) `(declare ,d))
						    (cdr decls))
				   . ,body))
			       nil))
		      (t (values body decls)))))
    (multiple-value-bind (this-level-single this-level-multiple next-level)
	(split-level clauses nil nil nil)
      (expand-1 this-level-single this-level-multiple next-level body decls))))

(defun split-level (clauses this-level-single this-level-multiple next-level)
  (if (null clauses)
      (values (reverse this-level-single) (reverse this-level-multiple)
	      next-level)
    (cl:let ((clause (car clauses)))
      (cl:cond ((and (listp clause) (listp (car clause)))
		  (split-level (cdr clauses) this-level-single this-level-multiple
			       (append next-level clause)))
	       ((and (listp clause) (cddr clause))
		(split-level (cdr clauses) this-level-single
			     (cons clause this-level-multiple) next-level))
	       (t
		(split-level (cdr clauses) (cons clause this-level-single)
			     this-level-multiple next-level))))))

(defun bound-decls (decls vars)
  (let* ((bd-alist (car decls))
	 (prs (remove-if-not #'(lambda (pr) (member (car pr) vars))
			     bd-alist)))
    (and prs `((declare . ,(mapcar #'(lambda (pr)
				       (if (listp (cdr pr))
					   `(,@(cdr pr) ,(car pr))
					 `(,(cdr pr) ,(car pr))))
				   prs))))))

(defun prune-decls (decls vars)
  (cl:let ((bd-alist (car decls)))
    (cons (remove-if #'(lambda (pr) (member (car pr) vars))
		     bd-alist)
	  (cdr decls))))

(defun analyze-decls (clauses body)
  "Returns two values. The first value is a cons of: (a) for the bound declarations
at the head of `body', an alist from variable name to a list of declarations
affecting that variable; (b) a list of the remaining (free) declarations.  The
second value is `body' with the declarations stripped off."
  (labels ((process-declares (body bd-alist free vars)
	     (if (or (null body) (not (consp (car body)))
		     (not (eq (caar body) 'declare)))
		 (values bd-alist free body)
	       (multiple-value-bind (bd-alist free)
		   (process-decls (cdar body) bd-alist free vars)
		 (process-declares (cdr body) bd-alist free vars))))
	   (process-decls (decls bd-alist free vars)
	     (if (null decls)
		 (values bd-alist free)
	       (multiple-value-bind (bd-alist free)
		   (process-decl (car decls) bd-alist free vars)
		 (process-decls (cdr decls) bd-alist free vars))))
	   (process-decl (decl bd-alist free vars)
	     (cl:cond
	       ((not (consp decl))	; defensive programming
		(values bd-alist (cons decl free)))
	       ((member (car decl) '(ignore ignorable))
		;; These are always bound.
		(values (append (mapcar #'(lambda (x) (cons x (car decl)))
					(cdr decl))
				bd-alist)
			free))
	       ((type-specifier-name? (car decl))
		(process-vars (cdr decl) (list 'type (car decl)) bd-alist free vars))
	       ((eq (car decl) 'type)
		(process-vars (cddr decl) (list 'type (cadr decl)) bd-alist free vars))
	       ((eq (car decl) 'special)
		(process-vars (cdr decl) (car decl) bd-alist free vars))
	       (t (values bd-alist (cons decl free)))))
	   (process-vars (decl-vars decl-name bd-alist free vars)
	     (if (null decl-vars)
		 (values bd-alist free)
	       (multiple-value-bind (bd-alist free)
		   (process-vars (cdr decl-vars) decl-name bd-alist free vars)
		 (if (member (car decl-vars) vars)
		     (values (cons (cons (car decl-vars) decl-name)
				   bd-alist)
			     free)
		   (values bd-alist
			   (cons (list decl-name (car decl-vars))
				 free)))))))
    (multiple-value-bind (bd-alist free body)
	(process-declares body nil nil (new-let-bound-vars clauses))
      (values (cons bd-alist free) body))))

(defun new-let-bound-vars (clauses)
  (and clauses
       (append (cl:let ((clause (car clauses)))
		 (cl:cond ((symbolp clause) (cons clause nil))
			    ((symbolp (car clause)) (butlast clause))
			    (t (new-let-bound-vars clause))))
	       (new-let-bound-vars (cdr clauses)))))

(defun type-specifier-name? (x)
  (or (member x '(array atom bignum bit bit-vector character compiled-function
		  complex cons double-float extended-char fixnum float function
		  hash-table integer keyword list long-float nil null number
		  package pathname random-state ratio rational real readtable
		  sequence short-float simple-array simple-bit-vector
		  simple-string simple-vector single-float standard-char stream
		  string base-char symbol t vector))
      (find-class x nil)))


(defmacro cond (&rest clauses)
  "A generalization of CL:COND that makes it convenient to compute a value in
the predicate expression of a clause and then use that value in the consequent.
If the predicate expression is a LET form, then the scope of the variables bound
by the LET is extended to include the consequent expressions.  For example:

  (cond ((let ((x (foo)))
           (bar x))
         (baz x)))

Here the X in (BAZ X) is the one bound to the result of (FOO)."
  (cl:let ((block-nm (gensym)))
    `(block ,block-nm
       . ,(mapcar #'(lambda (c) (bcond-clause c block-nm)) clauses))))

;;; Cause our version of COND to be indented correctly in Zmacs.  (The variable is
;;; in the Genera 8.2 Zmacs; I'm guessing it's in the LMITI system too -- if it
;;; ever matters :-)
#+LispM (push 'cond zwei:*not-lone-function-superiors*)

(defmacro bcond (&rest clauses)
  "A generalization of CL:COND that makes it convenient to compute a value in
the predicate expression of a clause and then use that value in the consequent.
If the predicate expression is a LET form, then the scope of the variables bound
by the LET is extended to include the consequent expressions.  For example:

  (cond ((let ((x (foo)))
           (bar x))
         (baz x)))

Here the X in (BAZ X) is the one bound to the result of (FOO)."
  (cl:let ((block-nm (gensym)))
    `(block ,block-nm
       . ,(mapcar #'(lambda (c) (bcond-clause c block-nm)) clauses))))

(defun bcond-clause (clause block-nm)
  (cl:cond ((not (listp clause))
	      (error "COND clause is not a list: ~S" clause))
	     ((and (listp (car clause))
		   ;; Allow NLET and CL:LET in case the user hasn't chosen
		   ;; to shadow LET.
		   (member (caar clause) '(let nlet cl:let)))
	      (bcond-build-clause (caar clause) (cadar clause)
				  `(progn . ,(cddar clause))
				  (cdr clause) block-nm))
	     (t
	      (bcond-build-clause nil nil (car clause) (cdr clause) block-nm))))

(defun bcond-build-clause (let-sym let-clauses pred consequents block-nm)
  (cl:let ((body (if consequents
		     `(if ,pred (return-from ,block-nm (progn . ,consequents)))
		   (cl:let ((temp-var (gensym)))
		     `(cl:let ((,temp-var ,pred))
			(if ,temp-var (return-from ,block-nm ,temp-var)))))))
    (if let-clauses
	`(,let-sym ,let-clauses ,body)
      body)))



