;;;; -*- Mode: Lisp -*-

;;;; match-block.lisp --
;;;; Various macros built on top of the unifier: MATCH, MATCHING and MATCH-CASE.

(in-package "UNIFY")

(defun clean-unify-var-name (v)
  (assert (variablep v))
  (intern (subseq (symbol-name v) 1)
          (symbol-package v)))


(defmacro match ((template object
                           &key
                           (match-named nil)
                           (substitution '(make-empty-environment))
                           (errorp t)
                           (error-value nil))
                 &body forms)
  "Sets up a lexical environment to evaluate FORMS after an unification.

MATCH unifies a TEMPLATE and an OBJECT and then sets up a lexical
environment where the variables present in the template are bound
lexically.  Note that both variable names '?FOO' and 'FOO' are bound
for convenience.

The MATCH form returns the values returned by the evaluation of the
last of the FORMS.

If ERRORP is non-NIL (the default) then the form raises a
UNIFICATION-FAILURE, otherwise the result of evaluating ERROR-VALUE,
whose default is NIL is returned. (Note that UNIFICATION-FAILUREs
raising from the evaluation of FORMS will also be caught and handled
according to ERRORP settings.)

If MATCH-NAMED is not NIL, then a surrounding BLOCK named MATCH-NAMED
is set up around the matching code.
"
  (let ((template-vars (collect-template-vars template))
        (env-var (gensym "UNIFICATION-ENV-"))
        (template (if (variablep template)
                      `',template ; Logical variables are special-cased.
                      template))
        )
    (flet ((generate-var-bindings ()
             (loop for v in template-vars
                   nconc (list `(,v (find-variable-value ',v
                                                         ,env-var))
                               `(,(clean-unify-var-name v) ,v))))
           )
      `(block ,match-named
         (handler-case
             (let* ((,env-var (unify ,template ,object ,substitution))
                    ,@(generate-var-bindings)
                    )
	       (declare (ignorable ,@(mapcar #'first
                                             (generate-var-bindings))))
               ,@forms)
           
           ;; Yes.  The above is sligthly wasteful.

           (unification-failure (uf)
                                (if ,errorp
                                    (error uf)
                                    ,error-value))
           )))))


(defmacro matchf ((template object
                            &key
                            (match-named nil)
                            (substitution '(make-empty-environment))
                            (errorp t)
                            (error-value nil))
                  &body forms)
  "Sets up a lexical environment to evaluate FORMS after an unification.

MATCHF unifies a TEMPLATE and an OBJECT and then sets up a lexical
environment where the variables present in the template are bound
lexically.  Note that both variable names '?FOO' and 'FOO' are bound
for convenience.

MATCHF does not 'evaluate' TEMPLATE (note that using the #T syntax will
generate a template at read-time).

The MATCHF form returns the values returned by the evaluation of the
last of the FORMS.

If ERRORP is non-NIL (the default) then the form raises a
UNIFICATION-FAILURE, otherwise the result of evaluating ERROR-VALUE,
whose default is NIL is returned. (Note that UNIFICATION-FAILUREs
raising from the evaluation of FORMS will also be caught and handled
according to ERRORP settings.)

If MATCH-NAMED is not NIL, then a surrounding BLOCK named MATCH-NAMED
is set up around the matching code.
"
  (let ((template-vars (collect-template-vars template))
        (env-var (gensym "UNIFICATION-ENV-"))
        (template (cond ((variablep template)
                         `',template) ; Logical variables are special-cased.
                        ((listp template) ; Same for lists.
                         (make-instance 'list-template
                                        :spec (cons 'list template)))
                        ;`',template)
                        (t
                         template)))
        )
    ;; Logical variables and lists are special cased for convenience.
    ;; Lists are especially inteded as abbreviation for destructuring.
    (flet ((generate-var-bindings ()
             (loop for v in template-vars
                   nconc (list `(,v (find-variable-value ',v
                                                         ,env-var))
                               `(,(clean-unify-var-name v) ,v))))
           )
      `(block ,match-named
         (handler-case
             (let* ((,env-var (unify ,template ,object ,substitution))
                    ,@(generate-var-bindings)
                    )
	       (declare (ignorable ,@(mapcar #'first
                                             (generate-var-bindings))))
               ,@forms)
           
           ;; Yes.  The above is sligthly wasteful.

           (unification-failure (uf)
                                (if ,errorp
                                    (error uf)
                                    ,error-value))
           )))))



(define-condition unification-non-exhaustive (unification-failure)
  ())


(defmacro matching ((&key errorp
                          (default-substitution
                           (make-empty-environment))
                          (matching-named nil))
                    &rest match-clauses)
  "MATCHING sets up a COND-like environment for multiple template matching clauses.

The syntax of MATCHING comprises a number of clauses of the form

  <clause> ::= <regular-clause> | <default-clause>
  <regular-clause> ::= ((<template> <form>) &body <forms>)
  <default-clause> ::= (t &body <forms>)
                   |   (otherwise &body <forms>)
<form> and <forms> are regular Common Lisp forms.
<template> is a unification template.

The full syntax of MATCHING is

  matching (&key errorp default-substitution) <clauses>

Each clause evaluates its forms in an environment where the variables
present in the template are bound lexically.  Note that both variable
names '?FOO' and 'FOO' are bound for convenience.

The values returned by the MATCHING form are those of the last form in
the first clause that satisfies the match test.

If ERRORP is non-NIL then if none of the regular clauses matches, then
an error of type UNIFICATION-NON-EXAUSTIVE is signalled, regardless of
any default clause.  Otherwise, the default clause behaves as a
standard COND default clause.  The default value of ERRORP is NIL.
"
  (declare (ignore default-substitution)) ; For the time being.
  (labels ((%%match%% (clause-var template object forms substitution)
             (let ((template-vars (collect-template-vars template))
                   (template (if (variablep template)
                                 `',template ; Logical variables are
                                             ; special-cased.
                                 template)) 
                   )
               (flet ((generate-var-bindings ()
                        (loop for v in template-vars
                              nconc (list `(,v (find-variable-value
						',v
						,clause-var))
                                          `(,(clean-unify-var-name v) ,v))))
                      )
                 `((setf ,clause-var
                         (ignore-errors (unify ,template
					       ,object
					       ,substitution)))
                   (let* (,@(generate-var-bindings))
                     ,@forms))
                 )))

           (build-match-clause (match-clause match-env-var)
             (destructuring-bind ((template object) &body forms)
                 match-clause
               (%%match%% match-env-var
			  template
			  object
			  forms
			  '(make-empty-environment))))
           )
    (when (or (and (find t match-clauses :key #'first)
		   (find 'otherwise match-clauses :key #'first))
	      (> (count t match-clauses :key #'first) 1)
              (> (count 'otherwise match-clauses :key #'first) 1))
      (error 'program-error))
    (let* ((default-clause (or (find t match-clauses
                                     :key #'first)
                               (find 'otherwise match-clauses
                                     :key #'first)))
           (match-clauses (delete default-clause match-clauses)) ; EQL
                                                                 ; test
                                                                 ; suffices.
           (match-clauses-env-vars (mapcar (lambda (mc)
                                             (declare (ignore mc))
                                             (gensym "UNIFICATION-ENV-")
                                             )
                                           match-clauses))
           )

      `(block ,matching-named
         (let ,match-clauses-env-vars
	   (declare (dynamic-extent ,@match-clauses-env-vars))
           (cond ,@(mapcar (lambda (match-clause match-clause-env-var)
                             (build-match-clause match-clause
                                                 match-clause-env-var))
                           match-clauses
                           match-clauses-env-vars)
                 (,errorp
                  (error 'unification-non-exhaustive
                         :format-control "Non exhaustive matching."))
                 ,@(when default-clause (list default-clause))))))
    ))


;;; match-case --
;;; Implementation provided by Peter Scott.
;;;
;;; Notes:
;;;
;;; [MA 20071109]
;;; The construction of the inner MATCH clauses could be done
;;; more intelligently by supplying :ERRORP NIL, thus avoiding the
;;; HANDLER-CASEs, which are quite expensive.  Any takers?

(defmacro match-case ((object &key errorp default-substitution match-case-named)
                      &rest clauses)
  "MATCH-CASE sets up a CASE-like environment for multiple template matching clauses.

The syntax of MATCH-CASE comprises a number of clauses of the form

  <clause> ::= <regular-clause> | <default-clause>
  <regular-clause> ::= (<template> &body <forms>)
  <default-clause> ::= (t &body <forms>)
                   |   (otherwise &body <forms>)
<form> and <forms> are regular Common Lisp forms.
<template> is a unification template.

The full syntax of MATCH-CASE is

  match-case <object> (&key errorp default-substitution) <clauses>

Each clause evaluates its forms in an environment where the variables
present in the template are bound lexically.  Note that both variable
names '?FOO' and 'FOO' are bound for convenience.

The values returned by the MATCH-CASE form are those of the last form in
the first clause that satisfies the match test.

If ERRORP is non-NIL then if none of the regular clauses matches, then
an error of type UNIFICATION-NON-EXAUSTIVE is signalled, regardless of
any default clause.  Otherwise, the default clause behaves as a
standard CASE default clause.  The default value of ERRORP is NIL.
"
  (declare (ignore default-substitution)) ; For the time being.
  (let* ((object-var (gensym "OBJECT-VAR-"))
         (otherwise-clause-present-p
          (member (caar (last clauses)) '(t otherwise)))
	 (non-otherwise-clauses
          (if otherwise-clause-present-p
              (butlast clauses)
              clauses))
	 (otherwise-clause
          (if otherwise-clause-present-p
              (first (last clauses))
              (when errorp
                `(t (error 'unification-non-exhaustive
                           :format-control "Non exhaustive matching.")))))
         )
    (labels ((generate-matchers (clauses)
	       (if (null clauses)
		   `(progn ,@(rest otherwise-clause))
		   (destructuring-bind (pattern &rest body)
		       (car clauses)
		     `(handler-case (match (,pattern ,object-var)
				      ,@body)
		        (unification-failure ()
			  ,(generate-matchers (cdr clauses))))))))
      `(block ,match-case-named
         (let ((,object-var ,object))
           ,(generate-matchers non-otherwise-clauses))))))


(defmacro matchf-case ((object &key errorp default-substitution match-case-named)
                      &rest clauses)
  "MATCHF-CASE sets up a CASE-like environment for multiple template matching clauses.

The syntax of MATCHF-CASE comprises a number of clauses of the form

  <clause> ::= <regular-clause> | <default-clause>
  <regular-clause> ::= (<template> &body <forms>)
  <default-clause> ::= (t &body <forms>)
                   |   (otherwise &body <forms>)
<form> and <forms> are regular Common Lisp forms.
<template> is a unification template.

The full syntax of MATCHF-CASE is

  matchf-case <object> (&key errorp default-substitution) <clauses>

Each clause evaluates its forms in an environment where the variables
present in the template are bound lexically.  Note that both variable
names '?FOO' and 'FOO' are bound for convenience.

The values returned by the MATCH-CASE form are those of the last form in
the first clause that satisfies the match test.

If ERRORP is non-NIL then if none of the regular clauses matches, then
an error of type UNIFICATION-NON-EXAUSTIVE is signalled, regardless of
any default clause.  Otherwise, the default clause behaves as a
standard CASE default clause.  The default value of ERRORP is NIL.

MATCHF-CASE behaves like MATCH-CASE, but the patterns are not
evaluated (i.e., it relies on MATCHF instead of MATCH to construct the
macro expansion.
"
  (declare (ignore default-substitution)) ; For the time being.
  (let* ((object-var (gensym "OBJECT-VAR-"))
         (otherwise-clause-present-p
          (member (caar (last clauses)) '(t otherwise)))
	 (non-otherwise-clauses
          (if otherwise-clause-present-p
              (butlast clauses)
              clauses))
	 (otherwise-clause
          (if otherwise-clause-present-p
              (first (last clauses))
              (when errorp
                `(t (error 'unification-non-exhaustive
                           :format-control "Non exhaustive matching.")))))
         )
    (labels ((generate-matchers (clauses)
	       (if (null clauses)
		   `(progn ,@(rest otherwise-clause))
		   (destructuring-bind (pattern &rest body)
		       (car clauses)
		     `(handler-case (matchf (,pattern ,object-var)
				      ,@body)
		        (unification-failure ()
			  ,(generate-matchers (cdr clauses))))))))
      `(block ,match-case-named
         (let ((,object-var ,object))
           ,(generate-matchers non-otherwise-clauses))))))

;;;;---------------------------------------------------------------------------
;;;; Testing.

#| Tests

(let ((n 42))
  (matching ()
            ((0 n) 1)
            ((?x n) (* x (1- x)))))


(let ((n 42))
  (match-case (n)
              (0 1)
              (?x (* x (1- x)))))


(let ((n 42))
  (match-case (n)
              (0 1)
              (otherwise (* n (1- n)))))

(defun fatt (x)
  (match-case (x :errorp t)
              (0 1)
              (#T(number ?n) (* ?n (fatt (1- n))))
              ))

|#

;;;; end of file -- math-blocks.lisp --
