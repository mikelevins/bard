;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: GMap -*-

;;; This file is in the public domain.  It is provided with ABSOLUTELY 
;;; NO WARRANTY.

(in-package gmap)

;
; GMAP, version 3.3, by Scott L. Burson
;
; This file is in the public domain.
;
; The GMAP macro provides a new kind of iteration facility in LISP.
; Basically, it is intended for when you would like to use MAPCAR, but
; can't because the things you want to map over aren't in lists, or you
; need to collect the results of the mapping into something other than a
; list.  That is, GMAP is probably the right thing to use when you are
; using iteration to perform the same computation on each element of
; some collection, as opposed to changing your state in some complicated
; way on every iteration of a loop.
; In fact, it's conceptually reasonable to imagine all the iterations of a
; GMAP as happening in parallel, just as you might with MAPCAR.  The
; difference is that with GMAP you explicitly say, via keywords, what kinds
; of collections the elements come in and what kind of collection to make
; from the result.  For example, the following two expressions are equivalent:
; (mapcar #'foo this-list that-list)	and
; (gmap :list #'foo (:list this-list) (:list that-list))
; The first :list keyword indicates that GMAP is to build a list; the other
; two tell it that this-list and that-list are in fact lists of elements over
; which foo is to be mapped.  Other keywords exist besides :list; for
; example, :string, if used as an argument keyword, causes its argument
; to be viewed as a string; the values it "generates" for the function being
; mapped are the successive characters of the string.
; Perhaps the best feature of GMAP is its facility for defining one's own
; keywords.  Thus you can adapt it to other kinds of data structures over
; which you would like to iterate.
;
; The overall syntax of GMAP is:
;   (gmap <result-spec> <fn>
;	  <arg-spec-0>
;	  <arg-spec-1>
;	  ... )
; where <fn> is the function being mapped, just like the first argument
; to MAPCAR.  The <result-spec> and the <arg-spec-i> are lists, whose first
; element is a keyword indicating the type of result constructor or argument
; generator, and the interpretation of whose subsequent elements depends on
; that type.  For example, in:
;   (gmap :list #'+
;	  (:list '(14 32 51))
;	  (:index 3))
; #'+ is the function to be mapped;
; the result-type of :list specifies that a list is to be constructed containing
; the results;
; the first arg-spec specifies that the first argument to the function
; being mapped will be successive elements of the list '(14 32 51);
; and the second arg-spec says that the second argument will be successive
; integers starting with 3.
; The result, of course, is (17 36 56).
;
; **** Argument generators ****
; Each generator is given one variable in which to maintain its state.  We have
; to tell GMAP explicitly how to get from the current value of the state variable
; to a)the value to be generated and b)the next value of the state variable.
;
; The keyword, the first element, of each argument spec tells what kind of
; generator to use.  NIL as a keyword specifies that one is defining a generator
; for this instance of GMAP only instead of using one of the predefined ones.
; A NIL-type arg-spec has the following syntax:
;   (nil <init> &optional <exitp> <argfn> <nextfn> <let-specs>)
; where <init> is the initial value of the generator's state variable;
; <exitp>, if non-nil, is a function of one argument; when it becomes true of
;  [i.e., returns a non-nil value when applied to] the state variable, the
;  iteration terminates.  If it is absent or nil, this generator has no exit-test.
;  If more than one arg-spec supplies an exitp, then the
;  first one to finish terminates the entire iteration [just like mapcar, which
;  stops when any list runs out].
; <argfn>, if non-nil, is a function of one argument which is applied to the
;  current value of the state variable to get the value the generator actually
;  returns on this iteration.  UPDATE: as of 3.3, this function may return
;  multiple values.  If this item is of the form `(:values <n> <argfn>)', <n>
;  specifies how many of its values to use.  See predefined arg-type `:alist'
;  below for an example.  The values are passed to the function being mapped as
;  multiple arguments, somewhat analogously to `multiple-value-call' except that
;  how many arguments are supplied by each arg-spec is declared statically
;  rather than being the number of values actually returned at runtime (ugh).
; <nextfn>, if non-nil, is a function of one argument which takes the current
;  value of the state variable and returns the next.
; <let-specs> facilitates arbitrary hair and is explained below.
; For example, an arg-spec of (:list foo) is equivalent to
; (nil foo #'null #'car #'cdr)
; where foo is the initial value of the list; #'null is the predicate that says
; when the list has run out; #'car, the argfn, is what is done to the list to
; get the current element; and #'cdr, the nextfn, is what is done to the list
; to get the next list.
;
; An argument generator described this way is conceptually equivalent to
; (let `(state-var ,@<let-specs>)
;   #'(lambda ()
;	(if (<exitp> state-var)
;	    (*throw 'exit-iteration nil)
;	    (prog1 (<argfn> state-var)
;		   (setq state-var (<nextfn> state-var))))))
;
; Note that if only (nil <init>) is specified, the argument is a constant <init>;
; no more circular-list'ing!
;
; Other predefined argument types include:
;  (:constant <value>)
;    A more readable version of `(nil <value>)'.
;  (:list <list>)
;    As shown in examples above: supplies successive elements of <list>.
;  (:index <start> &optional <stop> <incr>)
;    Provides numbers beginning at <start> and going to (but not including) <stop>
;    incrementing by <incr> each time.  If <stop> is missing or nil, this generates
;    numbers indefinitely.  <incr> may be positive or negative and defaults to 1.
;  (:index-inc <start> <stop> &optional <incr>)
;    "Index, INClusive": like :index, but the numbers generated include <stop>.
;  (:vector <vector>)
;    Generates successive elements of <vector>.
;  (:simple-vector <vector>)
;    Generates successive elements of <vector> (which must be simple).
;  (:string <string>)
;    Generates successive characters of <string>.
;  (:simple-string <string>)
;    Generates successive characters of <string> (which must be simple).
;  (:exp <initial-value> <base>)
;    Generates an exponential sequence whose first value is <initial-value>, and
;    whose value is multiplied by <base> on each iteration.
;
; **** Result Constructors ****
; Like arg-specs, result-specs begin with a keyword saying what kind of
; constructor to use, i.e., how to put together the results of the function
; being mapped.  And again, a keyword of NIL means that no predefined
; constructor is being used.  A NIL-type result-spec looks like:
;  (nil <init> <resfn> &optional <cleanup> <filterp> <let-specs>)
; where
; <init> is the initial value of the constructor's state variable;
; <resfn> is a function of two arguments, the current value of the state variable
;  and the current value returned by the function being mapped; it gives the next
;  value of the state variable.  UPDATE: as of 3.3, this function can now take
;  more than two arguments.  If this item is of the form `(:consume <n> <resfn>)',
;  <n> specifies how many of the values of the function being mapped are to be
;  passed to <resfn> (which then takes <n> + 1 arguments).  See result-type
;  `:alist' below for an example.
; <cleanup>, if present and non-nil, is a function of one argument that
;  translates the final value of the state variable into the value that the GMAP
;  actually returns.
; <filterp>, if present and non-nil, is a predicate of one argument; when it is false
;  of the current value of the function being mapped, <resfn> is not called on that
;  iteration, and the value of the state variable is unchanged.
; <let-specs>, as before, is hairy; I'll get back to it below.
; For example, a res-spec of (:list) is equivalent to
; (nil nil #'(lambda (a b) (cons b a)) #'nreverse)
; -- the state variable starts at nil, gets successive values consed onto it, and
;  gets nreversed before being returned.
;
; A result-spec that supplies no arguments may be written without the parens; so
; (:list) and :list are equivalent.
;
; Other predefined result types include:
;  :list
;    Generates a list, like mapcar, of the values.
;  :and
;    Returns the first NIL, or the last value if none are NIL.
;  :or
;    Returns the first non-NIL, or NIL if all values are NIL.
;  :sum
;    Returns the sum of the values.  E.g., to get sum of products, use
;    (gmap :sum #'* ...)
;  (:array <initial-array>)
;    Generates an array of the values.	You supply the initial array; the values
;    are stored starting with element 0.  If the array has a fill pointer, it is
;    set upon exit to the number of elements stored.  The array itself is returned.
;  (:string &optional <length-guess>)
;    Generates a string from the values.  <length-guess> is the initially allocated
;    string size; it defaults to 20.  #'array-push-extend is used to append each
;    character.
;  (:values &rest <result-specs>)
;    The function being mapped is expected to return as many values as there are
;    result-specs; each value is accumulated separately according to its respective
;    result-spec, and finally, all the result values are returned.
;
; **** User-defined argument and result types ****
; A useful feature of GMAP is the provision for the user to define his/her own
; argument generators and result constructors.	For example, if in some program you
; commonly iterate over words in a sentence, or lines in an editor buffer, or users
; currently logged on, then define an argument type SENTENCE, or LINES, or USERS.
; And similarly with result-types.  The way this is done [which I'm not yet sure is
; entirely satisfactory] is with the two special forms DEF-GMAP-ARG-TYPE and
; DEF-GMAP-RES-TYPE.  These have syntax like DEFUN:
; (def-gmap-foo-type <name> (<args>)
;   <body>)
; When <name> is seen as the keyword of an arg- or result-spec, and has
; been defined with the appropriate special form, then the function
; #'(lambda (<args>) <body>) is applied to the cdr of the spec; that is,
; the keyword itself has been stripped off.  Whatever this returns is interpreted
; as a nil-type spec, except, again, without the keyword "nil".  For example, the
; arg-type :list is actually defined by
;   (def-gmap-arg-type :list (initial-list)
;     `(,initial-list			; init
;	#'null #'car #'cdr))		; exitp, argfn, and resfn
;
; Lists of what arg- and result-types are defined can be found in the variables
; *GMAP-ARG-TYPE-LIST* and *GMAP-RES-TYPE-LIST*.
;
; Now for the promised explanation about let-specs.  Sometimes [indeed, fairly
; often] a user-defined type will want to compute values and bind variables
; other than those automatically provided by the iteration.  For example, the
; index type goes to some trouble to evaluate its parameters only once.  It does
; this by providing a list of specs, i.e., (<var> <value>) pairs, which go into
; a LET that surrounds the entire iteration.  Except, that is, for the following
; hack: if you want several dependent initializations, e.g., you want foo to be
; something hairy and bar to be the cdr of foo, you can indicate the dependence
; by the nesting in list structure of the specs:
; ((foo (something-hairy))
;  ((bar (cdr foo))))
; This will cause a gmap that uses this type to expand into
; (let ((foo (something-hairy)))
;   (let ((bar (cdr foo)))
;     ... [iteration] ...))
; For details, see the NLET macro at the end of this file.  For examples,
; see some of the types defined herein.

; Remaining tidbits:
; Many arg- and result-specs take optional parameters, which are defined to do
;  something only if both present and non-nil.	By "non-nil" here I mean non-nil
;  *at expansion time*.
; The function being mapped can itself be nil, subject of course to the above
;  considerations; in which case the identity function of the first argument is
;  used, and other arguments are ignored.
; No guarantees are offered as to the order in which the forms generated by
;  the arg-specs and result-specs are evaluated, except that the `exitp'
;  and `argfn' functions are called at the beginning of each iteration, before
;  the function being mapped; and the `nextfn' and `filterp' functions are called
;  after the function being mapped.  Do not depend on the order of side-effects
;  across clauses!  (The whole purpose of GMap is to better support a highly
;  functional style.)

; Bugs:
;
; Purists will object to the use of symbols in the keyword package rather than
; the `lisp' package for the arg- and result-types.  It would make sense for
; these symbols to come from the package providing the types they refer to;
; among other advantages, this would prevent name collisions (which is, after
; all, the whole point of the package system).  Against this very reasonable
; argument is my desire to have it immediately apparent to someone seeing a
; `gmap' form, perhaps for the first time, that it is a macro with somewhat
; unusual syntax; the use of ordinary Lisp symbols (`list', `vector', etc.)
; would tend to disguise this fact.  Anyway, there's nothing requiring the arg-
; and result-type names to be in the keyword package; anyone who strongly
; dislikes this is welcome to define names in some other package.

;;; CHANGES:
;;;
;;; Version 3.3, 2007-07-04:
;;;
;;; () Revamped arg types `:index' and `:index-inc' (incompatibly).
;;; () Added doc strings, including on the predefined arg- and result-types,
;;;    and arranged for them to be placed on the symbol-plist, where there's
;;;    an outside chance someone might find them.
;;; () Revamped result types `:vector' and `:string' (incompatibly).
;;; () Removed deprecated arg- and result-types `:array'.
;;; () Changed most optional arguments to arg- and result-types to keyword
;;;    arguments.
;;; () Substantially expanded the multiple-value capability.  Now it is possible
;;;    for an arg-spec to generate any number of arguments to the function being
;;;    mapped, and for a single result-spec to consume more than one value
;;;    returned by that function.
;;; () Using the new multiple-value capabilities, added `:alist' and `:plist'
;;;    as both arg- and result-types.
;;;
;;; Thanks to Joerg Hoehle for some useful suggestions.

;;; The top-level macro.
(defmacro gmap (res-spec fn &rest arg-specs)
  "A generalized mapping macro.  Applies `fn' to the successive values generated
by the `arg-specs', analagously to `mapcar'; that is, on each iteration, each
arg-spec yields one value, and `fn' is called with these values as arguments.
The values returned by `fn' are accumulated into a result according to
`res-spec'.  The `res-spec' is either a list whose car is a predefined result
type, or a list whose car is `nil' and whose cdr has the same form as the value
of a result type expander (see `def-gmap-res-type'); or, the `res-spec' can be
a symbol, which is shorthand for a list of that symbol; or, it can be a list
whose car is `:values' and whose cdr is a list of result-specs.  Similarly,
each of the `arg-specs' is either a list whose car is a predefined arg type,
or a list whose car is `nil' and whose cdr has the same form as the value of
an arg type expander (see `def-gmap-arg-type')."
  (unless arg-specs
    (error "At least one argument spec is required."))
  (gmap>expand fn
	       (gmap>res-spec-lookup res-spec)
	       (mapcar #'gmap>arg-spec-lookup arg-specs)))

;;; This does the real work.
(defun gmap>expand (fn res-specs arg-specs)
  (let ((param-list
	  (mapcar #'gmap>param arg-specs))
	(result-list (gmap>res>init-clauses res-specs))
	(let-specs (gmap>let-specs arg-specs res-specs)))
    (let ((one-value-p (null (cdr result-list)))
	  (multi-vars (mapcar #'gmap>param>multi-vars arg-specs))
	  (fnval-vars (mapcan #'(lambda (res-spec)
				  (and res-spec
				       (let ((resfn (second res-spec)))
					 (if (and (consp resfn)
						  (eq (first resfn) ':consume))
					     (let ((vars nil))
					       (dotimes (i (second resfn))
						 (push (gensym "VAR-") vars))
					       (nreverse vars))
					   (list (gensym "VAR-"))))))
			      res-specs)))
      `(let ,let-specs
	 (do (,@param-list
	      ,@result-list)
	     ((or ,@(apply #'append (mapcar #'gmap>param>exit-test	; exit test
					    param-list arg-specs)))
	      ,(gmap>res>cleanup res-specs result-list one-value-p))
	   (let ,(reduce #'append
			 (mapcar #'gmap>param>multi-let-specs
				 param-list arg-specs multi-vars))
	     ,(if (null fnval-vars)
		  ;; Null result spec -- just call the function for effect.
		  (apply #'gmap>funcall fn
			 (reduce #'append
				 (mapcar #'gmap>param>arg
					 param-list arg-specs multi-vars)))
		`(let ((,@fnval-vars
			,(apply #'gmap>funcall fn
				(reduce #'append
					(mapcar #'gmap>param>arg
						param-list arg-specs multi-vars)))))
		   . ,(let ((setqs nil))
			(do ((res-specs res-specs (cdr res-specs))
			     (result-list result-list (cdr result-list)))
			    ((null res-specs))
			  (let ((next-exp fnvs
				  (gmap>res>next (car res-specs) (caar result-list)
						 fnval-vars)))
			    (setq fnval-vars fnvs)
			    (push `(setq ,(caar result-list) ,next-exp) setqs)))
			(nreverse setqs))))))))))


;;; extract the let-specs.
(defun gmap>let-specs (arg-specs res-specs)
  (nconc (mapcan #'fifth arg-specs) (mapcan #'fifth res-specs)))

;;; generate the do-variable spec for each argument.
(defun gmap>param (arg-spec)
  (let ((param-name (gensym "VAR-"))
	(init (first arg-spec))
	(nextfn (fourth arg-spec)))
    `(,param-name
      ,init
      ,@(if nextfn
	    `(,(gmap>funcall nextfn param-name))
	    nil))))

;;; get the argument to the function being mapped from the do-variable.
(defun gmap>param>arg (param arg-spec multi-vars)
  (let ((param-name (first param))
	(argfn (third arg-spec)))
    (or multi-vars
	`(,(gmap>funcall argfn param-name)))))

;;; get the exit test for the variable.
(defun gmap>param>exit-test (param arg-spec)
  (let ((param-name (first param))
	(exitp (second arg-spec)))
    (if exitp
	`(,(gmap>funcall exitp param-name))
	nil)))

(defun gmap>param>multi-vars (arg-spec)
  (let ((argfn (third arg-spec)))
    (and (consp argfn) (eq (first argfn) ':values)
	 ;; (gmap :list (lambda (i) (gensym))
	 ;;       (:index 0 (cadr argfn)))
	 (let ((vars nil))
	   (dotimes (i (second argfn))
	     (push (gensym "VAR-") vars))
	   (nreverse vars)))))

(defun gmap>param>multi-let-specs (param arg-spec multi-vars)
  (let ((argfn (third arg-spec)))
    (and multi-vars
	 `((,@multi-vars ,(gmap>funcall (third argfn) (first param)))))))

;;; get the initial value of the result.
(defun gmap>res>init-clauses (res-specs)
  (mapcan #'(lambda (res-spec)
	      (and res-spec (cons (list (gensym "VAR-") (first res-spec))
				  nil)))
	  res-specs))

;;; compute the next value of the result from the current one and the
;;; current value of the function.
(defun gmap>res>next (res-spec result fnvals)
  (let ((resfn (second res-spec))
	(filterp (fourth res-spec))
	((n-fnvals resfn
	   (if (and (consp resfn) (eq (first resfn) ':consume))
	       (values (second resfn) (third resfn))
	     (values 1 resfn)))
	 ((my-fnvals (subseq fnvals 0 n-fnvals)))))
    (values (if filterp
		`(if ,(apply #'gmap>funcall filterp my-fnvals)
		     ,(apply #'gmap>funcall resfn result my-fnvals)
		   ,result)
	      (apply #'gmap>funcall resfn result my-fnvals))
	    (subseq fnvals n-fnvals))))

;;; call the cleanup function on exit.
(defun gmap>res>cleanup (res-specs result-list one-value-p)
  (if one-value-p
      (gmap>funcall (third (car res-specs)) (caar result-list))
    `(values . ,(mapcar #'(lambda (res-spec result-pair)
			    (gmap>funcall (third res-spec) (car result-pair)))
			res-specs result-list))))

;;; For some reason, some compilers don't convert, e.g., (funcall #'car foo)
;;; to (car foo); thus we lose some efficiency for functions that would normally
;;; open-code, like car.  Hence this function to perform the optimization.  Also
;;; cleans up the expansion a bit.
(defun gmap>funcall (function &rest args)
  (let ((args (copy-list args)))
    (cond ((or (null function) (eq function ':id))
	   `(values . ,args))
	  ((and (listp function)
		(eq (car function) 'function))
	   `(,(cadr function) . ,args))
	  ((and (listp function)
		(eq (car function) 'lambda))
	   `(,function . ,args))
	  (t `(funcall ,function . ,args)))))


(eval-when (:execute :compile-toplevel :load-toplevel)
  (defvar *gmap-arg-type-list* nil
    "A list of all GMAP arg types that have been defined.")
  (defvar *gmap-res-type-list* nil
    "A list of all GMAP result types that have been defined."))


(defmacro def-gmap-arg-type (name args &body body)
  "Defines a GMap arg-type.  Syntax is identical to `defun'.  The body should
return a list of 1 to 5 elements: (0, \"init\") the initial value of the
state variable; (1, \"exitp\"), if non-nil, a function of one argument which
is called on the state variable, a true result causing the iteration to
exit; (2, \"argfn\"), if non-nil, a function of one argument which is called
on the state variable to get the value to be used on this iteration; (3,
\"nextfn\"), if non-nil, a function of one argument which is called on the
state variable to get the new value of same; and (4, \"let-specs\") a list of
clauses for an `nlet' that will wrapped around the entire expansion.

It is also possible for an arg-type to generate multiple arguments.  If
element 2, \"argfn\", is of the form `(:values N FN)', FN should be a function
returning N values, which will be passed as separate arguments to the function
being mapped."
  (let ((fn-name (gensym "GMAP-ARG-SPEC-EXPANDER-")))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name ':gmap-arg-spec-expander) ',fn-name)
	 ,@(and doc-string
		`((setf (get ',name ':gmap-arg-spec-expander-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-arg-type-list*)))))


(defmacro def-gmap-res-type (name args &body body)
  "Defines a GMap result-type.  Syntax is identical to `defun'.  The body should
return a list of 2 to 5 elements: (0, \"init\") the initial value of the state
variable; (1, \"resfn\") a function of two arguments which is called on the
state variable and the current value of the function being mapped, returning
the new value of the state variable; (2, \"cleanup\"), if non-nil, a function
of one argument which is called on the final value of the state variable to
get the value of the `gmap' form; (3, \"filterp\"), if non-nil, a predicate
of one argument which is called on the current value of the function being
mapped, a false value causing \"resfn\" not to be called on this iteration (and
the state variable to be unchanged); and (4, \"let-specs\") a list of
clauses for an `nlet' that will wrapped around the entire expansion.

It is also possible for a result-type to consume more than one value of the
function being mapped.  If element 1, \"resfn\", is of the form `(:consume N
FN)', FN should be a function of N + 1 arguments, and will receive N values
from the function being mapped."
  (let ((fn-name (gensym "GMAP-RES-SPEC-EXPANDER-")))
    ;; CLISP doesn't seem to preserve EQ-ness on fasl-dumped uninterned symbols.
    #+clisp (setq fn-name (intern (symbol-name fn-name) :gmap))
    (let ((doc-string body
	    (if (stringp (car body)) (values (car body) (cdr body))
	      (values nil body))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
	 (defun ,fn-name ,args . ,body)
	 (setf (get ',name ':gmap-res-spec-expander) ',fn-name)
	 ,@(and doc-string
		`((setf (get ',name ':gmap-res-spec-expander-doc-string) ,doc-string)))
	 (pushnew ',name *gmap-res-type-list*)))))

;;; look up an arg type.
(defun gmap>arg-spec-lookup (raw-arg-spec)
  (let ((type (car raw-arg-spec)))
    (if (null type)
	(cdr raw-arg-spec)
	(let ((generator (get type ':gmap-arg-spec-expander)))
	  (if generator
	      (apply generator (cdr raw-arg-spec))
	    (error "Argument spec, ~S, to gmap is of unknown type
  (Do you have the package right?)"
		   raw-arg-spec))))))

;;; look up a result type.
(defun gmap>res-spec-lookup (raw-res-spec)
  (if (and (listp raw-res-spec)
	   (eq (car raw-res-spec) ':values))
      (mapcar #'gmap>res-spec-lookup-1 (cdr raw-res-spec))
    (cons (gmap>res-spec-lookup-1 raw-res-spec) nil)))
(defun gmap>res-spec-lookup-1 (raw-res-spec)
  (let ((type (if (listp raw-res-spec) (car raw-res-spec)
		raw-res-spec)))
    (if (null type)
	(cdr raw-res-spec)
      (let ((generator (get type ':gmap-res-spec-expander)))
	(if generator
	    (apply generator (and (listp raw-res-spec) (cdr raw-res-spec)))
	  (error "Result spec, ~S, to gmap is of unknown type
  (Do you have the package right?)"
		 raw-res-spec))))))



;;; ******** Predefined argument types ********
;;; See above for documentation.

(def-gmap-arg-type :constant (value)
  "Yields an unbounded sequence of `value'."
  `(,value))

(def-gmap-arg-type :list (list)
  "Yields the successive elements of `list'."
  `(,list
    #'endp #'car #'cdr))

(def-gmap-arg-type :alist (alist)
  "Yields, as two values, the successive pairs of `alist'."
  `(,alist
    #'endp
    (:values 2 #'(lambda (alist) (values (caar alist) (cdar alist))))
    #'cdr))

(def-gmap-arg-type :plist (plist)
  "Yields, as two values, the successive pairs of elements of `plist'; that is,
there is one iteration for each two elements."
  `(,plist
    #'endp
    (:values 2 #'(lambda (plist) (values (car plist) (cadr plist))))
    #'cddr))

;;; If `incr' is +1 or -1, then swapping `start' and `stop' and negating `incr'
;;; generates the same sequence in reverse order.  This isn't true, though, in
;;; general.  Should it be?
(def-gmap-arg-type :index (&optional (start 0) stop &key (incr 1) (fixnums? t))
  "Yields integers in the interval [`start', `stop') if `incr' (which defaults
to 1) is positive; or in the interval [`stop', `start') if `incr' is negative.
Specifically, in the upward case, the values begin with `start' and increase by
`incr' until >= `stop'; in the downward case, the values begin with
`start' - `incr' and decrease by `incr' until < `stop'.  All values are
assumed to be fixnums unless `fixnums?' is a literal `nil'.  `stop' can be
omitted or a literal `nil' to indicate an unbounded sequence.  `start' can be
omitted to start at 0."
  (let ((incr-temp (gensym "INCR-"))
	(stop-temp (gensym "STOP-")))
    ;; Aargh, have to handle the constant vs. variable cases of `incr' separately
    ;; to avoid unreachable-code warnings from Python.  (Suggested heuristic:
    ;; if code becomes unreachable because a variable is bound to a compile-time
    ;; constant, and the name of the variable is an uninterned symbol, suppress
    ;; the warning on the grounds that a macro generated the code in question.)
    `(,(if (numberp incr)			; init
	   (if (minusp incr)
	       (if fixnums? `(+ (the fixnum ,start) ,incr)
		 `(+ ,start ,incr))
	     start)
	 `(if (minusp ,incr-temp)
	      ,(if fixnums? `(+ (the fixnum ,start) (the fixnum ,incr-temp))
		   `(+ ,start ,incr-temp))
	   ,start))
      ,(and stop				; exitp
	    (if (numberp incr)
		(if fixnums?
		    `#'(lambda (val)
			 (,(if (minusp incr) '< '>=)
			   (the fixnum val) (the fixnum ,stop-temp)))
		  `#'(lambda (val)
		       (,(if (minusp incr) '< '>=) val ,stop-temp)))
	      `#'(lambda (val)
		   ,@(and fixnums? `((declare (type fixnum val ,incr-temp ,stop-temp))))
		   (if (minusp ,incr-temp)
		       (< val ,stop-temp)
		     (>= val ,stop-temp)))))
      nil					; no argfn
      #'(lambda (val)				; nextfn
	  ,(if fixnums?
	       `(the fixnum (+ (the fixnum val)
			       (the fixnum ,(if (numberp incr) incr incr-temp))))
	     `(+ val ,(if (numberp incr) incr incr-temp))))
      (,@(and (not (numberp incr))		; let-specs
	      `((,incr-temp ,incr)))
       ,@(and stop
	      `((,stop-temp ,stop)))))))

(def-gmap-arg-type :index-inc (start stop &key (incr 1) (fixnums? t))
  "Yields integers in the interval [`start', `stop'].  Specifically, in the
upward case (`incr' > 0), the values begin with `start' and increase by
`incr' until > `stop'; in the downward case, the values begin with `start'
and decrease by `incr' until < `stop'.  All values are assumed to be fixnums
unless `fixnums?' is a literal `nil'.  `stop' can be a literal `nil' to
indicate an unbounded sequence."
  (let ((incr-temp (gensym "INCR-"))
	(stop-temp (gensym "STOP-")))
    ;; Aargh, have to handle the constant vs. variable cases of `incr' separately
    ;; to avoid unreachable-code warnings from Python.  (Suggested heuristic:
    ;; if code becomes unreachable because a variable is bound to a compile-time
    ;; constant, and the name of the variable is an uninterned symbol, suppress
    ;; the warning on the grounds that a macro generated the code in question.)
    `(,start
      ,(and stop				; exitp
	    (if (numberp incr)
		(if fixnums?
		    `#'(lambda (val)
			 (,(if (minusp incr) '< '>)
			   (the fixnum val) (the fixnum ,stop-temp)))
		  `#'(lambda (val)
		       (,(if (minusp incr) '< '>) val ,stop-temp)))
	      `#'(lambda (val)
		   ,@(and fixnums? `((declare (type fixnum val ,incr-temp ,stop-temp))))
		   (if (minusp ,incr-temp)
		       (< val ,stop-temp)
		     (> val ,stop-temp)))))
      nil					; no argfn
      #'(lambda (val)				; nextfn
	  ,(if fixnums?
	       `(the fixnum (+ (the fixnum val)
			       (the fixnum ,(if (numberp incr) incr incr-temp))))
	     `(+ val ,(if (numberp incr) incr incr-temp))))
      (,@(and (not (numberp incr))		; let-specs
	      `((,incr-temp ,incr)))
       ,@(and stop
	      `((,stop-temp ,stop)))))))

(def-gmap-arg-type :exp (initial-value base)
  "Yields an unbounded exponential sequence starting with `initial-value'
and multiplying by `base' after each iteration."
  (let ((base-temp (gensym "BASE-")))
    `(,initial-value
      nil
      nil
      #'(lambda (x) (* x ,base-temp))
      ((,base-temp ,base)))))

(def-gmap-arg-type :vector (vec &key start stop incr)
  "Yields elements of vector `vec'.  `start' and `stop' may be supplied to select
a subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc.  For performance, you may prefer `:simple-vector'."
  (let ((vec-temp (gensym "VEC-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= i ,stop-temp))
       #'(lambda (i) (aref ,vec-temp i))
       #'(lambda (x) (+ x ,(or incr-temp 1)))
       ((,vec-temp ,vec)
	,@(and incr `((,incr-temp ,incr)))
	((,stop-temp ,(or stop `(length ,vec-temp))))))))

(def-gmap-arg-type :simple-vector (vec &key start stop incr)
  "Yields elements of vector `vec', which is assumed to be simple, and whose size
is assumed to be a fixnum.  `start' and `stop' may be supplied to select a
subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc."
  (let ((vec-temp (gensym "VEC-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= (the fixnum i) ,stop-temp))
       #'(lambda (i) (svref ,vec-temp (the fixnum i)))
       #'(lambda (i) (the fixnum (+ (the fixnum i) ,(or incr-temp 1))))
       ((,vec-temp ,vec)
	,@(and incr `((,incr-temp (the fixnum ,incr))))
	((,stop-temp (the fixnum ,(or stop `(length ,vec-temp)))))))))

(def-gmap-arg-type :string (str &key start stop incr)
  "Yields elements of string `str'.  `start' and `stop' may be supplied to select
a subsequence of `vec'; `incr' may be supplied (it must be positive) to select
every second element etc.  For performance, you may prefer `:simple-string'."
  (let ((str-temp (gensym "STR-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= i ,stop-temp))
       #'(lambda (i) (char ,str-temp i))
       #'(lambda (i) (+ i ,(or incr-temp 1)))
       ((,str-temp (string ,str))
	,@(and incr `((,incr-temp ,incr)))
	((,stop-temp ,(or stop `(length ,str-temp))))))))

(def-gmap-arg-type :simple-string (str &key start stop incr)
  "Yields elements of string `str', which is assumed to be simple, and whose size
is assumed to be a fixnum.  `start' and `stop' may be supplied to select a
subsequence of `str'; `incr' may be supplied (it must be positive) to select
every second element etc."
  (let ((str-temp (gensym "STR-"))
	(incr-temp (and incr (gensym "INCR-")))
	(stop-temp (gensym "STOP-")))
    `(,(or start 0)
       #'(lambda (i) (>= (the fixnum i) ,stop-temp))
       #'(lambda (i) (schar ,str-temp (the fixnum i)))
       #'(lambda (i) (+ (the fixnum i) ,(or incr-temp 1)))
       ((,str-temp ,str)
	,@(and incr `((,incr-temp (the fixnum ,incr))))
	((,stop-temp (the fixnum ,(or stop `(length ,str-temp)))))))))


;;; ******** Predefined result types ********

(def-gmap-res-type :list (&key filterp)
  "Returns a list of the values, optionally filtered by `filterp'."
  `(nil #'(lambda (x y) (cons y x)) #'nreverse ,filterp))

(def-gmap-res-type :alist (&key filterp)
  "Consumes two values from the mapped function; returns an alist of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (res x y) (cons (cons x y) res))) #'nreverse ,filterp))

(def-gmap-res-type :plist (&key filterp)
  "Consumes two values from the mapped function; returns a plist of the
pairs.  Note that `filterp', if supplied, must take two arguments."
  `(nil (:consume 2 #'(lambda (res x y) (cons y (cons x res)))) #'nreverse ,filterp))

(def-gmap-res-type :append (&key filterp)
  "Returns the result of `append'ing the values, optionally filtered by
`filterp'."
  `(nil
    #'(lambda (old new) (revappend new old))
    #'nreverse
    ,filterp))

(def-gmap-res-type :nconc (&key filterp)
  "Returns the result of `nconc'ing the values, optionally filtered by
`filterp'."
  (let ((result-var (gensym "RESULT-")))	; have to use our own, sigh.
    `(nil					; init
      #'(lambda (tail-loc new)			; nextfn
	  (if tail-loc (rplacd tail-loc new)
	    (setq ,result-var new))
	  (if new (last new) tail-loc))
      #'(lambda (ignore)
	  (declare (ignore ignore))
	  ,result-var)
      ,filterp
      ((,result-var nil)))))

(def-gmap-res-type :and ()
  "If one of the values is false, terminates the iteration and returns false.
Does not work as an operand of `:values'."
  '(t #'(lambda (ignore new)
	  (declare (ignore ignore))
	  (if new new (return nil)))))

(def-gmap-res-type :or ()
  "If one of the values is true, terminates the iteration and returns it.
Does not work as an operand of `:values'."
  '(nil #'(lambda (ignore new)
	    (declare (ignore ignore))
	    (if new (return new) nil))))

(def-gmap-res-type :sum (&key filterp)
  "Returns the sum of the values, optionally filtered by `filterp'."
  `(0 #'+ nil ,filterp))

(def-gmap-res-type :count-if ()
  "Returns the number of true values."
  '(0 #'(lambda (n new)
	  (if new (1+ n) n))))

(def-gmap-res-type :max (&key filterp)
  "Returns the maximum of the values, optionally filtered by `filterp'; or `nil'
if there are none."
  `(nil
    #'(lambda (old new) (if (null old) new (max old new)))
    nil
    ,filterp))

(def-gmap-res-type :min (&key filterp)
  "Returns the minimum of the values, optionally filtered by `filterp'; or `nil'
if there are none."
  `(nil
    #'(lambda (old new) (if (null old) new (min old new)))
    nil
    ,filterp))

(def-gmap-res-type :vector (&key use-vector length fill-pointer adjustable filterp)
  "Constructs a vector containing the results.  If `use-vector' is supplied,
the argument will be filled with the results and returned; if `fill-pointer'
is true and `adjustable' is true, it must have a fill pointer and be adjustable,
and values will be appended to it with `vector-push-extend'; if `fill-pointer'
is true and `adjustable' is false, it must have a fill pointer, and values will
be appended to it with `vector-push'; otherwise, the vector is assumed to be
simple and must be large enough to hold the results.  (Recall that `vector-push'
has no effect if the vector is full.)

If `use-vector' is not supplied, a vector will be constructed and returned;
if `length' is supplied, returns a simple vector of the specified length (which
must be sufficient to hold the results); otherwise, returns a simple vector of
the correct length (but to do this, it must cons a temporary list).

In any case, if `filterp' is supplied, it is a predicate of one argument,
the value of the function being mapped, that says whether to include it in
the result."
  (cond ((and use-vector fill-pointer)
	 `(,use-vector
	   #'(lambda (vec next-elt)
	       (,(if adjustable
		     'vector-push-extend
		   'vector-push)
		next-elt vec)
	       vec)
	   nil
	   ,filterp))
	((and use-vector length)
	 (error "Makes no sense to supply both `:use-vector' and `:length'"))
	(use-vector
	 (let ((index-temp (gensym "INDEX-")))
	   `(,use-vector
	     #'(lambda (vec next-elt)
		 (setf (svref vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(length
	 (let ((index-temp (gensym "INDEX-")))
	   `((make-array ,length)
	     #'(lambda (vec next-elt)
		 (setf (svref vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(t
	 (let ((len-temp (gensym "LEN-")))
	   `(nil
	     #'(lambda (list next-elt)
		 (incf (the fixnum ,len-temp))
		 (cons next-elt list))
	     #'(lambda (list)
		 (let ((vec (make-array (the fixnum ,len-temp))))
		   (dolist (x list)
		     (setf (svref vec (decf (the fixnum ,len-temp))) x))
		   vec))
	     ,filterp
	     ((,len-temp 0)))))))

(def-gmap-res-type :string (&key use-string length fill-pointer adjustable filterp)
  "Constructs a string containing the results.  If `use-string' is supplied,
the argument will be filled with the results and returned; if `fill-pointer'
is true and `adjustable' is true, it must have a fill pointer and be adjustable,
and values will be appended to it with `vector-push-extend'; if `fill-pointer'
is true and `adjustable' is false, it must have a fill pointer, and values will
be appended to it with `vector-push'; otherwise, the vector is assumed to be
simple and must be large enough to hold the results.  (Recall that `vector-push'
has no effect if the vector is full.)

If `use-string' is not supplied, a string will be constructed and returned;
if `length' is supplied, returns a simple string of the specified length (which
must be sufficient to hold the results); otherwise, returns a simple string of
the correct length (but to do this, it must cons a temporary list).

In any case, if `filterp' is supplied, it is a predicate of one argument,
the value of the function being mapped, that says whether to include it in
the result."
  (cond ((and use-string fill-pointer)
	 `(,use-string
	   #'(lambda (str next-elt)
	       (,(if adjustable
		     'vector-push-extend
		   'vector-push)
		next-elt str)
	       str)
	   nil
	   ,filterp))
	(use-string
	 (let ((index-temp (gensym "INDEX-")))
	   `(,use-string
	     #'(lambda (vec next-elt)
		 (setf (schar vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(length
	 (let ((index-temp (gensym "INDEX-")))
	   `((make-string ,length)
	     #'(lambda (vec next-elt)
		 (setf (schar vec (the fixnum ,index-temp)) next-elt)
		 (incf (the fixnum ,index-temp))
		 vec)
	     nil
	     ,filterp
	     ((,index-temp 0)))))
	(t
	 (let ((len-temp (gensym "LEN-")))
	   `(nil				; init
	     #'(lambda (list next-elt)		; nextfn
		 (incf (the fixnum ,len-temp))
		 (cons next-elt list))
	     #'(lambda (list)			; cleanup
		 (let ((str (make-string (the fixnum ,len-temp))))
		   (dolist (x list)
		     (setf (schar str (decf (the fixnum ,len-temp))) x))
		   str))
	     ,filterp
	     ((,len-temp 0)))))))


; End of gmap.lisp
