;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: Lexical-Contexts -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

;;; For documentation (such as it is) see `contexts.text'.

(in-package :lexical-contexts)

(defstruct (context
	     (:constructor make-context (name parameter-list constructor
					      &optional bind-vars bind-fns
					      all-vars all-fns macros)))
  name			; just for show
  parameter-list	; just for show
  constructor		; a symbol
  bind-vars		; list ((var slot) ...)
  bind-fns		; list ((var slot temp) ...)
  all-vars		; list ((var slot) ...)
  all-fns		; list ((var slot temp) ...)
  macros)		; list for `macrolet'

(defmethod make-load-form ((ctxt context) &optional environment)
  (declare (ignore environment))
  `(make-context ',(context-name ctxt) ',(context-parameter-list ctxt)
                 ',(context-constructor ctxt)
		 ',(context-bind-vars ctxt) ',(context-bind-fns ctxt)
		 ',(context-all-vars ctxt) ',(context-all-fns ctxt)
		 ',(context-macros ctxt)))

(defconstant context-instance-default-element
  '|Unbound context slot.  Recompile your `with-context' form.|
  "An attempt to give the user a clue if they run into this.")


(defmacro defcontext (context-name parameter-list &body body)
  (let* ((ctxt (or (get context-name 'context)
		   (make-context context-name parameter-list
				 (gensym (format nil "~A-~A-"
						 'defcontext context-name)))))
	;; To make it possible to recompile a `defcontext' without recompiling
	;; all `in-context's that use it, we keep track of all names we've
	;; ever used, as well as those currently in use.
	(all-vars (context-all-vars ctxt))
	(all-fns (context-all-fns ctxt))
	(vars nil)
	(fns nil)
	(ctor-params nil)
	(&opt? nil)
	(&key? nil)
	(ctxt-var (gensym "CTXT-"))
	(instance-var (gensym "CTXT-INSTANCE-")))
    (flet ((get-var-rec (var)
	     (let ((var-rec (or (assoc var all-vars)
				(let ((var-rec (list var (+ (length all-vars)
							    (length all-fns)))))
				  (push var-rec all-vars)
				  var-rec))))
	       (when (assoc var vars)
		 (error "~A ~S: variable bound more than once: ~S"
			'defcontext context-name var))
	       (push var-rec vars)
	       var-rec))
	   (get-fn-rec (fn)
	     (let ((fn-rec (or (assoc fn all-fns)
			       (let ((fn-rec (list fn (+ (length all-vars)
							 (length all-fns))
						   (gensym (format nil "~A-" fn)))))
				 (push fn-rec all-fns)
				 fn-rec))))
	       (when (assoc fn fns)
		 (error "~A ~S: function bound more than once: ~S"
			'defcontext context-name fns))
	       (push fn-rec fns)
	       fn-rec)))
      (dolist (p parameter-list)
	(cond ((eq p '&optional)
	       (when &opt?
		 (error "~A ~S: ~A supplied after a previous ~A or ~A"
			'defcontext context-name '&optional '&optional '&key))
	       (setq &opt? t)
	       (push p ctor-params))
	      ((eq p '&key)
	       (when &key?
		 (error "~A ~S: ~A supplied after a previous ~A"
			'defcontext context-name '&key '&key))
	       (setq &opt? t &key? t)
	       (push p ctor-params))
	      ;; &&& Might add `&rest'.
	      ((and (symbolp p)
		    (< 0 (length (symbol-name p)))
		    (eql #\& (char (symbol-name p) 0)))
	       (error "~A ~S: Lambda-list keyword ~A not supported"
		      'defcontext context-name p))
	      ((symbolp p)
	       (push p ctor-params)
	       (get-var-rec p))
	      ((not (listp p))
	       (error "~A ~S: strange thing found in parameter list: ~S"
		      'defcontext context-name p))
	      ((and &opt? (eq (car p) 'function))
	       (error "~A ~S: optional functional parameters must have a default ~
		       (or, you tried to use `~A' as a parameter name)"
		      'defcontext context-name 'function))
	      ((or (eq (car p) 'function)
		   (and (listp (car p)) (eq (caar p) 'function)))
	       (let ((pp (if (eq (car p) 'function) p (car p))))
		 (unless (and (= (length pp) 2) (symbolp (cadr pp)))
		   (error "~A ~S: Function parameter not supplied or not symbol: ~S"
			  'defcontext context-name pp))
		 (unless (or (eq (car p) 'function)
			     (<= 2 (length p) 3))
		   (error "~A ~S: Malformed optional parameter spec: ~S"
			  'defcontext context-name p))
		 (let* ((fn-rec (get-fn-rec (cadr pp)))
			(temp (caddr fn-rec)))
		   (push (if (eq (car p) 'function) temp (cons temp (cdr p)))
			 ctor-params))))
	      ((not &opt?)
	       (error "~A ~S: Required parameter may not have default: ~S"
		      'defcontext context-name p))
	      (t
	       (unless (and (symbolp (car p))
			    (<= 2 (length p) 3))
		 (error "~A ~S: Malformed optional parameter spec: ~S"
			'defcontext context-name p))
	       (push p ctor-params)
	       (get-var-rec (car p)))))
      (when (stringp (car body))
	(pop body))				; doc string
      (let ((deflex? nil)
	    (defun? nil)
	    (imports nil)
	    (macros nil)
	    (deflexes nil)
	    (defuns nil)
	    (fn-params fns))
	(dolist (form body)
	  (cond ((and (listp form) (eq (car form) 'import-context))
		 (when (or deflex? defun?)
		   (error "~A ~S: ~A ~S form follows a ~A form"
			  'defcontext context-name 'import-context (cadr form)
			  (if defun? 'defun 'deflex)))
		 (push (cadr form) imports))
		((and (listp form) (eq (car form) 'deflex))
		 (when defun?
		   (error "~A ~S: ~A ~S form follows a ~A form"
			  'defcontext context-name 'deflex (cadr form) 'defun))
		 (unless (and (symbolp (cadr form))
			      (<= 3 (length form) 4))
		   (error "~A ~S: invalid ~A form: ~S"
			  'defcontext context-name 'deflex form))
		 (push form deflexes)
		 (setq deflex? t))
		;; `defmacro' forms have the same constraints as `deflex' forms
		;; and can be mixed with them.
		((and (listp form) (eq (car form) 'defmacro))
		 (when defun?
		   (error "~A ~S: ~A ~S form follows a ~A form"
			  'defcontext context-name 'defmacro (cadr form) 'defun))
		 (unless (and (symbolp (cadr form))
			      (<= 3 (length form) 4))
		   (error "~A ~S: invalid ~A form: ~S"
			  'defcontext context-name 'deflex form))
		 (push (cdr form) macros)
		 (setq deflex? t))
		((and (listp form) (eq (car form) 'defun))
		 (unless (and (symbolp (cadr form))
			      (<= 4 (length form)))
		   (error "~A ~S: invalid ~A form: ~S"
			  'defcontext context-name 'defun form))
		 (push form defuns)
		 (setq defun? t))
		(t
		 (error "~A ~S: unknown body form: ~S"
			'defcontext context-name form))))
	(let* ((imports (reverse imports))		; Some of these are just
	       (deflexes (reverse deflexes))		; cosmetic for those
	       (macros (reverse macros)) 		; reading the expansions.
	       (defuns (reverse defuns))
	       (ctor-params (reverse ctor-params))
	       ;; Side-effects `vars', so let's be sure it's done first.
	       (deflex-bindings (mapcar (lambda (form)
					  (get-var-rec (cadr form))
					  `(,(cadr form) ,(caddr form)))
					deflexes))
	       ;; Similarly for `fns'.
	       (defun-bindings (mapcar (lambda (form)
					 (get-fn-rec (cadr form))
					 (cdr form))
				       defuns)))
	  ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
	  #+clisp (setf (context-constructor ctxt)
			(intern (symbol-name (context-constructor ctxt))
				:lexical-contexts))
	  `(eval-when (:compile-toplevel :load-toplevel)
	     (defun ,(context-constructor ctxt) ,ctor-params
	       (flet ,(mapcar (lambda (fn-rec)
				`(,(car fn-rec) (&rest args)
				  (apply ,(caddr fn-rec) args)))
			      fn-params)
		 ;; I hope most compilers will do this inlining; consing the &rest
		 ;; list would suck.  But I can't make them macros because then
		 ;; they can't be function-ified.
		 (declare (inline . ,(mapcar #'car fn-params)))
		 (let ((,instance-var
			 (make-array ,(+ (length all-vars) (length all-fns))
				     :initial-element context-instance-default-element)))
		   ;; We bind the user's variable names in a `let*' to get the right
		   ;; scoping effect (each init-form is in a scope that includes
		   ;; all the prior deflexes, and none that follow).  Then we get
		   ;; the values into the instance vector and shadow the names with
		   ;; `symbol-macrolet'.
		   (with-contexts ,imports
		     (macrolet ,macros
		       (let* ,deflex-bindings
			 ,@(mapcar (lambda (var-rec)
				      `(setf (svref ,instance-var ,(cadr var-rec))
					     ,(car var-rec)))
				    vars)
			 (symbol-macrolet ,(mapcar (lambda (var-rec)
						     `(,(car var-rec)
						       (svref ,instance-var
							      ,(cadr var-rec))))
						   vars)
			   (labels ,defun-bindings
			     ,@(mapcar (lambda (fn-rec)
					 `(setf (svref ,instance-var ,(cadr fn-rec))
						#',(car fn-rec)))
				       fns)
			     ,instance-var))))))))
	    (let ((,ctxt-var ,(make-load-form ctxt)))
	      (setf (context-parameter-list ,ctxt) ',parameter-list)
	      (setf (context-bind-vars ,ctxt-var) ',vars)
	      (setf (context-bind-fns ,ctxt-var) ',fns)
	      (setf (context-all-vars ,ctxt-var) ',all-vars)
	      (setf (context-all-fns ,ctxt-var) ',all-fns)
	      (setf (context-macros ,ctxt-var) ',macros)
	      (setf (get ',context-name 'context) ,ctxt-var)
	      ',context-name)))))))


(defmacro with-context (context-invocation &body body)
  (let* ((context-name (car context-invocation))
	 (args (cdr context-invocation))
	 (ctxt (get context-name 'context))
	 (instance-var (gensym "CTXT-INSTANCE-")))
    (unless ctxt
      (error "~A: undefined context name: ~S"
	     'with-context context-name))
    ;; Must re-get the context object at runtime to make sure we have the
    ;; right copy of the uninterned constructor symbol even after fasl-loading.
    `(let ((,instance-var (funcall (context-constructor (get ',context-name 'context))
			    . ,args)))
       (declare (ignorable ,instance-var))
       (symbol-macrolet ,(mapcar (lambda (var-rec)
				   `(,(car var-rec)
				     (svref ,instance-var ,(cadr var-rec))))
				 (context-bind-vars ctxt))
	 (macrolet ,(context-macros ctxt)
	   (flet ,(mapcar (lambda (fn-rec)
			    `(,(car fn-rec) (&rest args)
			       (apply (svref ,instance-var ,(cadr fn-rec)) args)))
			  (context-bind-fns ctxt))
	     (declare (inline . ,(mapcar #'car (context-bind-fns ctxt))))
	     . ,body))))))


(defmacro with-contexts (context-invocations &body body)
  (if (null context-invocations)
      `(progn . ,body)
    `(with-context ,(car context-invocations)
       (with-contexts ,(cdr context-invocations)
	 . ,body))))


;;; This is actually unrelated to the above except that I've used the name for
;;; lexical variables within contexts, so for benefit of those who might want
;;; global lexicals for general use also (since you're going to want to import
;;; `deflex' from this package), here's a macro definition.  Inspired by Rob
;;; Warnock's version, but simplified.
(defmacro deflex (var &optional (val nil val?) doc)
  "Declares `var' as a global lexical variable, and if `val' is supplied and
`var' is not already bound, initializes it to `val'.  `doc', if supplied,
is taken as a documentation string.  In some implementations (e.g. Scieneer),
locally rebinding the same name is not permitted; in most, it is permitted
but creates a new lexical variable, with no effect on the global one."
  ; Scieneer has this built in (will give an error if you try to bind the var)
  #+scl
  `(ext:defgvar ,var ,@(and val? `(,val)) ,@(and doc `(,doc)))
  #-scl
  `(progn
     (eval-when (:load-toplevel :execute)
       ,@(and doc `((setf (documentation ',var 'variable) ',doc)))
       ,@(and val? `((unless (boundp ',var)
		       (setf (symbol-value ',var) ,val)))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,var (symbol-value ',var)))))

;;; This one reinitializes the variable even if it is already bound.
(defmacro deflex-reinit (var val &optional doc)
  "Declares `var' as a global lexical variable, and initializes it to `val'.
`doc', if supplied, is taken as a documentation string.  In some
implementations (e.g. Scieneer), locally rebinding the same name is not
permitted; in most, it is permitted but creates a new lexical variable,
with no effect on the global one."
  ; Scieneer has this built in (will give an error if you try to bind the var)
  #+scl
  `(progn
     (ext:defgvar ,var nil ,@(and doc `(,doc)))
     (setq ,var ,val))
  #-scl
  `(progn
     (eval-when (:load-toplevel :execute)
       ,@(and doc `((setf (documentation ',var 'variable) ',doc)))
       (setf (symbol-value ',var) ,val))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (define-symbol-macro ,var (symbol-value ',var)))))

;;; Also related only tangentially... but I want to put it somewhere.
(defmacro isetq (var val)
  "\"Interactive `setq'\": can be used on previously-undeclared variables
without getting ugly warnings or causing the variable to be declared special.
Not to be used in code!!!"
  (unless (symbolp var)
    (error "~A requires a symbol, not: ~S" 'isetq var))
  `(eval-when (:execute)
     (when (eq (macroexpand-1 ',var) ',var)
       (define-symbol-macro ,var (symbol-value ',var)))
     (setq ,var ,val)))
