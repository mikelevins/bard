;;; -*- Mode: Lisp -*-

;;; unifier.lisp
;;; General CL structures unifier.

(in-package "CL.EXT.DACF.UNIFICATION") ; DACF = Data And Control Flow.

(defgeneric unify (a b &optional env &key &allow-other-keys)
  (:documentation
   "Unifies two objects A and B given a substitution ENV.
A is a Common Lisp object and B is either a Common Lisp object or a
\"template\", A and B can be commuted.

The unification rules are rather complex. Each method of the generic
function implements a specific rule of unification.

The generic function returns a `substitution' upon success or it
signals a UNIFICATION-FAILURE condition upon failure."))



;;;===========================================================================
;;; Simple, non template methods.

(defmethod unify ((a symbol) (b list)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  "Unifies a symbol A and a list B in an environment ENV.
If A is not a variable then an error of type UNIFICATION-FAILURE is
signaled. If A is a unification variable, then the environment ENV is
extended with a binding for A to B, unless the occurrence check is
called and fails, in which case an error is signaled."
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
                  :format-control "Cannot unify a symbol with a list: ~S ~S."
                  :format-arguments (list a b)))))


(defmethod unify ((b list) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  "Unifies a symbol B and a list A in an environment ENV.
If A is not a variable then an error of type UNIFICATION-FAILURE is
signaled. If A is a unification variable, then the environment ENV is
extended with a binding for A to B, unless the occurrence check is
called and fails, in which case an error is signaled."
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
                  :format-control "Cannot unify a list with a symbol: ~S ~S."
                  :format-arguments (list b a)))))


(defmethod unify ((a list) (b list)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  "Unifies a list A and a list B in an environment ENV.
The unification procedure proceedes recursively on each element of
both lists. If two elements cannot be unified then an error of type
UNIFICATION-FAILURE is signaled.  Otherwise a possibly extended
environment is returned."
  (unify (rest a) (rest b) (unify (first a) (first b) env)))



(defmethod unify ((a number) (b number)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  "Unifies two numbers A and B.
Two numbers unify only if and only if they are equal as per the function #'=, in
which case an unmodified envirironment ENV is returned.
Otherwise an error of type UNIFICATION-FAILURE is signalled.
Of course, asking for unification of two floating point numbers may
not yield the expected result."
  (if (= a b)
      env
      (error 'unification-failure
             :format-control "Cannot unify two different numbers: ~S ~S."
             :format-arguments (list a b))))


(defparameter *unify-string-case-sensitive-p* t)

(defmethod unify ((a character) (b character)
                  &optional (env (make-empty-environment))
                  &key
                  (case-sensitive *unify-string-case-sensitive-p*)
                  &allow-other-keys)
  "Unifies two strings A and B.
Two CHARACTERs A and B unify if and only if they satisfy either #'CHAR= or
#'CHAR-EQUAL. The choice of which of test to perform (#'CHAR= or #'CHAR-EQUAL)
is made according to the value of the variable
*UNIFY-STRING-CASE-INSENSITIVE-P*, which defaults to NIL.
If A and B unify then an unmodified environment ENV is returned,
otherwise an error of type UNIFICATION-FAILURE is signaled."
  (cond ((and case-sensitive (char= a b))
         env)
        ((char-equal a b)
         env)
        (t
         (error 'unification-failure
                :format-control "Connot unify two different characters: ~S ~S."
                :format-arguments (list a b)))))


(defmethod unify ((a string) (b string)
                  &optional (env (make-empty-environment))
                  &key
                  (case-sensitive *unify-string-case-sensitive-p*)
                  &allow-other-keys)
  "Unifies two strings A and B.
Two strings A and B unify if and only if they satisfy either #'STRING= or
#'STRING-EQUAL. The choice of which of test to perform (#'STRING= or #'STRING-EQUAL)
is made according to the value of the variable
*UNIFY-STRING-CASE-INSENSITIVE-P*, which defaults to NIL.
If A and B unify then an unmodified environment ENV is returned,
otherwise an error of type UNIFICATION-FAILURE is signaled."
  (cond ((and case-sensitive (string= a b))
         env)
        ((string-equal a b)
         env)
        (t
         (error 'unification-failure
                :format-control "Connot unify two different strings: ~S ~S."
                :format-arguments (list a b)))))


(defmethod unify ((a symbol) (b string)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
		  :format-control "Cannot unify a symbol with a string: ~S ~S."
		  :format-arguments (list a b)))))


(defmethod unify ((b string) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
                  :format-control "Cannot unify a string with a symbol: ~S ~S."
                  :format-arguments (list b a)))))


(defmethod unify ((a symbol) (b symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        ((variable-any-p b) env)
        ((variablep b) (var-unify b a env))
        ((eq a b) env)
        (t (error 'unification-failure
                  :format-control "Cannot unify two different symbols: ~S ~S."
                  :format-arguments (list a b)))))


(defmethod unify ((a symbol) (b t)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (call-next-method))))


(defmethod unify ((b t) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (call-next-method))))


(defmethod unify ((a symbol) (b array)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
                  :format-control "Cannot unify a symbol with ~
                                   an array or vector: ~S and ~S."
                  :format-arguments (list a b)))))


(defmethod unify ((b array) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (error 'unification-failure
                  :format-control "Cannot unify an array or vector with a symbol: ~S and ~S."
                  :format-arguments (list a b)))))


(defmethod unify ((as vector) (bs vector)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unless (= (length as) (length bs))
    (error 'unification-failure
           :format-control "Cannot unify two vectors of different length: ~D and ~D."
           :format-arguments (list (length as) (length bs))))
  (loop for a across as
        for b across bs
        for mgu = (unify a b env) then (unify a b mgu)
        finally (return mgu)))


(defmethod unify ((s1 sequence) (s2 sequence)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unless (= (length s1) (length s2))
    (error 'unification-failure
           :format-control "Cannot unify two sequences of different length: ~D and ~D."
           :format-arguments (list (length s1) (length s2))))
  (loop for i from 0 below (length s1)
        for j from 0 below (length s2)
        for mgu = (unify (elt s1 i) (elt s2 j) env) then (unify (elt s1 i) (elt s2 j) mgu)
        finally (return mgu)))



(defgeneric untyped-unify (a b &optional env))

(defmethod untyped-unify ((as list) (bs vector)
                          &optional (env (make-empty-environment)))
  (loop for a in as
        for b across bs
        for mgu = (unify a b env) then (unify a b mgu)
        finally (return mgu)))


(defmethod untyped-unify ((as vector) (bs list)
                          &optional (env (make-empty-environment)))
  (untyped-unify bs as env))

(defmethod untyped-unify ((a t) (b t) &optional (env (make-empty-environment)))
  (unify a b env))


(defmethod unify ((as array) (bs array)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unless (= (array-total-size as) (array-total-size bs))
    (error 'unification-failure
           :format-control "Cannot unify two arrays of different total size: ~D and ~D."
           :format-arguments (list (array-total-size as) (array-total-size bs))))
  (loop for ai from 0 below (array-total-size as)
        for bi from 0 below (array-total-size bs)
        for mgu = (unify (row-major-aref as ai) (row-major-aref bs bi) env)
        then (unify (row-major-aref as ai) (row-major-aref bs bi) mgu)
        finally (return mgu)))


;;; Catch all method.

(defmethod unify ((a t) (b t)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (if (equalp a b)
      env
      (error 'unification-failure
             :format-control "Cannot unify a ~S and a ~S: ~S ~S."
             :format-arguments (list (type-of a) (type-of b) a b))))


;;;===========================================================================
;;; Templates methods.


;;; valid-template-p --
;;; Useful later.  Tests whether the object X can be considered a template.
;;; This should probably become a generic function.

(defun valid-template-p (x)
  (or (symbolp x)
      (consp x)
      (numberp x)
      (arrayp x)
      (typep (class-of x) 'structure-class)
      (typep (class-of x) 'standard-class)
      (typep (class-of x) 'built-in-class)
      (template-p x)))


;;; Special catch all method.

(defmethod unify ((x template) (y template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
           :format-control "Unification of two templates of type ~A and ~A ~
                            has not been yet implemented."
           :format-arguments (list (class-name (class-of x))
                                   (class-name (class-of y)))))


;;;---------------------------------------------------------------------------
;;; NIL special unification methods.

(defmethod unify ((x null) (y null)
		  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  env)


(defmethod unify ((x null) (nt nil-template)
		  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  env)


(defmethod unify ((nt nil-template) (x null)
		  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  env)


(defmethod unify ((nt1 nil-template) (nt2 nil-template)
		  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  env)


;;;---------------------------------------------------------------------------
;;; Symbol methods.

(defmethod unify ((a symbol) (b symbol-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (cond ((variable-any-p a) env)
        ((variablep a) (var-unify a b env))
        (t (unify a (symbol-template-symbol b) env))))


(defmethod unify ((b symbol-template) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


(defmethod unify ((a symbol) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify symbol ~S with template ~S."
         :format-arguments (list a b)))


(defmethod unify ((b template) (a symbol)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))



;;;---------------------------------------------------------------------------
;;; Number template methods.

(defmethod unify ((a number) (b number-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a (number-template-number b) env))


(defmethod unify ((b number-template) (a number)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))

(defmethod unify ((a number) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify the number ~S with template ~S."
         :format-arguments (list a b)))

(defmethod unify ((b template) (a number)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Sequence (List) template methods

(defmethod unify ((a sequence) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify a sequence with a non sequence ~
                         or non sequence access template: ~S and ~S."
         :format-arguments (list a b)))


(defmethod unify ((b template) (a sequence)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


#| Needs to be fixed.
(defmethod unify ((a list) (b lambda-template) &optional (env (make-empty-environment)))
  (unify a (template-spec b) env))


(defmethod unify ((b lambda-template) (a list) &optional (env (make-empty-environment)))
  (unify (template-spec b) a env))
|#


;;; The next is incomplete and does not signal appropriate errors.

(defmethod unify ((a list) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify a list with a non-list template: ~S ~S."
         :format-arguments (list a b)))


(defmethod unify ((a list) (b sequence-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (let ((template-lambda-list (sequence-template-lambda-list b))
        (ll (list-length a))
        )
    (multiple-value-bind (vars optionals keys rest)
        (parse-extended-ordinary-lambda-list template-lambda-list
                                             :ordinary-variable-test #'valid-template-p
                                             :optional-variable-test #'valid-template-p
                                             :key-variable-test #'valid-template-p
                                             :rest-variable-test #'valid-template-p
                                             )
      
      (let* ((n-vars (list-length vars))
             (n-optionals (list-length optionals))
             (env (unify (subseq a 0 (min ll (list-length vars)))
			 vars
			 env))
             )
        (when (and optionals (>= ll (+ n-vars n-optionals)))
          (setf env (unify (subseq a n-vars (+ n-vars n-optionals)) optionals env)))
        (when (and rest (>= ll (+ n-vars n-optionals)))
          (setf env (unify (subseq a (+ n-vars n-optionals)) (first rest) env)))
        (when keys (warn "Sorry matching of keywords ~S not yet implemented." keys))
        env
        ))))



(defmethod unify ((b template) (a list)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))



;;;---------------------------------------------------------------------------
;;; Vector template methods.

(defmethod unify ((a vector) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify a vector with a non-vector template: ~S ~S."
         :format-arguments (list a b)))


(defmethod unify ((a vector) (b vector-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (let ((template-lambda-list (sequence-template-lambda-list b))
        (vl (length a))
        )
    (multiple-value-bind (vars optionals keys rest)
        (parse-extended-ordinary-lambda-list template-lambda-list
                                             :ordinary-variable-test #'valid-template-p
                                             :optional-variable-test #'valid-template-p
                                             :key-variable-test #'valid-template-p
                                             :rest-variable-test #'valid-template-p
                                             )
      
      (let ((n-vars (list-length vars))
            (n-optionals (list-length optionals))
            )
        (loop for v in vars
              for e across (subseq a 0 (list-length vars))
              for mgu = (unify v e env) then (unify v e mgu)
              finally (setf env mgu))
        (when (and optionals (>= vl (+ n-vars n-optionals)))
          (loop for v in optionals
                for e across (subseq a n-vars (+ n-vars n-optionals))
                for mgu = (unify v e env) then (unify v e mgu)
                finally (setf env mgu)))
        (when (and rest (>= vl (+ n-vars n-optionals)))
          (setf env (unify (subseq a (+ n-vars n-optionals)) (first rest) env)))
        (when keys (warn "Sorry matching of keywords ~S not yet implemented." keys))
        env
        ))))


(defmethod unify ((b template) (a vector)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Array template methods.

(defmethod unify ((a array) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify an array with a non array ~
                          or non array access template: ~S and ~S."
         :format-arguments (list a b)))

(defmethod unify ((b template) (a array)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


(defun unify-array-row (array dims shape-template indexes env)
  (cond ((null dims) env)
        ((= (list-length dims) 1)
         ;; Unify the row with the shape-template.
         (let ((row (make-array (first dims)
                               :displaced-to array
                               :displaced-index-offset
                               (apply #'array-row-major-index
                                      array
                                      (append indexes (list 0))))))
           (declare (dynamic-extent row)
                    (type array row))
           (untyped-unify row shape-template env)))
        (t
         (loop for i from 0 below (first dims)
               for row-template in shape-template
               do (unify-array-row array
                                   (rest dims)
                                   row-template
                                   (append indexes (list i))
                                   env)
               finally (return env)))
        ))


(defun unify-array-rows (array shape-template env)
  (unify-array-row array (array-dimensions array) shape-template () env))


(defmethod unify ((a array) (b array-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (let ((template-spec (template-spec b)))
    (if (= 2 (length template-spec))

        ;; Template is (<array type specifier> <shape-template>)
        (destructuring-bind (array-type-spec shape-template)
            template-spec
          (declare (ignore array-type-spec))
          ;; Missing check for type-spec.
          (unify-array-rows a shape-template env))

        ;; Template is (array (['*' | <element type>] [<dimension spec>]) <shape template>)
        (destructuring-bind (array-kwd type-spec shape-template)
            template-spec
          (declare (ignore array-kwd type-spec))
          ;; Missing check for type-spec.
          (unify-array-rows a shape-template env))
        )))



;;;---------------------------------------------------------------------------
;;; Standard object template methods.

(defmethod unify ((a standard-object) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify a standard object with a ~
                          non standard object template: ~S and ~S."
         :format-arguments (list a b)))

#|| Old version with heavy syntax
(defmethod unify ((a standard-object) (b standard-object-template)
                  &optional (env (make-empty-environment)))
  (destructuring-bind (class &rest template-slot-specs)
      (template-spec b)
    (unless (typep a class)
      (error 'unification-failure
             :format-control "Cannot unify an instance of ~S with a template for class ~S."
             :format-arguments (list (class-of a) class)))
    (flet ((slot-spec-unify (accessor-spec reader value-template mgu)
             (ecase accessor-spec
               (slot-value
                (unify (slot-value a reader) value-template mgu))
               (slot-accessor
                (unify (funcall reader a) value-template mgu))))
           )
      (if template-slot-specs
          (loop for (accessor-spec reader value-template) in template-slot-specs
                for mgu = (slot-spec-unify accessor-spec reader value-template env)
                then (slot-spec-unify accessor-spec reader value-template mgu)
                finally (return mgu))
          env))))
||#


(defmethod unify ((a standard-object) (b standard-object-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (class &rest template-slot-specs)
      (template-spec b)
    (unless (typep a class)
      (error 'unification-failure
             :format-control "Cannot unify an instance of ~S with a template for class ~S."
             :format-arguments (list (class-of a) class)))
    (flet ((slot-spec-unify (reader value-template mgu)
             (etypecase reader
               (list
                (assert (eq (first reader) 'slot-value))
                (unify (slot-value a (second reader)) value-template mgu))
               ((or function symbol)
                (unify (funcall reader a) value-template mgu))))
           )
      (if template-slot-specs
          (loop for (reader value-template) on template-slot-specs by #'cddr
                for mgu = (slot-spec-unify reader value-template env)
                then (slot-spec-unify reader value-template mgu)
                finally (return mgu))
          env))))


(defmethod unify ((b template) (a standard-object)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Structure object template methods.

(defmethod unify ((a structure-object) (b template)
                  &optional (env)
                  &key &allow-other-keys)
  (declare (ignore env))
  (error 'unification-failure
         :format-control "Cannot unify a structure object with ~
                          a non structure object template: ~S and ~S."
         :format-arguments (list a b)))


(defmethod unify ((a structure-object) (b structure-object-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (class &rest template-slot-specs)
      (template-spec b)
    (unless (typep a class)
      (error 'unification-failure
             :format-control "Cannot unify an instance of ~S with a ~
                              template for structure ~S."
             :format-arguments (list (class-of a) class)))
    (if template-slot-specs
        (loop for (reader value-template) on template-slot-specs by #'cddr
              for mgu = (unify (funcall reader a) value-template env)
              then (unify (funcall reader a) value-template mgu)
              finally (return mgu))
        env)))


(defmethod unify ((b template) (a structure-object)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Expression template SUBSEQ methods.

;;; SEQUENCE
;;; For non LIST and non VECTOR possible SEQUENCE types.

(defmethod unify ((a sequence) (b subseq-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (subseq-kwd from to &rest spec)
      (template-spec b)
    (declare (ignore subseq-kwd))
    (let* ((seq-type (type-of a))
           (seq-template-kind (if (symbolp seq-type)
                                  seq-type
                                  (first seq-type))) ; Stupid FTTB.
           )
      (unify (subseq a from to)
             (make-template seq-template-kind `(,seq-template-kind ,@spec))
             env))))


;;; LIST

(defmethod unify ((a list) (b subseq-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (subseq-kwd from to &rest spec)
      (template-spec b)
    (declare (ignore subseq-kwd))
    (unify (subseq a from to)
	   (make-template 'list `(list ,@spec))
	   env)))


;;; VECTOR

(defmethod unify ((a vector) (b subseq-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (subseq-kwd from to &rest spec)
      (template-spec b)
    (declare (ignore subseq-kwd))
    (let ((seq-type (type-of a)))
      (unify (subseq a from to)
             (make-template seq-type `(,seq-type ,@spec))
             env))))


(defmethod unify ((b subseq-template) (a sequence)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Expression templates

;;; AREF methods.

(defmethod unify ((a array) (b aref-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (aref-kwd indexes value-template)
      (template-spec b)
    (declare (ignore aref-kwd))
    ;; Missing check on index spec.
    (unless (consp indexes)
      (setf indexes (list indexes)))
    (unify (apply #'aref a indexes) value-template env)))


;;; Necessary due to standard method sorting.

(defmethod unify ((a vector) (b aref-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (aref-kwd indexes value-template)
      (template-spec b)
    (declare (ignore aref-kwd))
    ;; Missing check on index spec.
    (when (and (consp indexes) (> (length indexes) 1))
      (error 'unification-failure
             :format-control "Cannot unify a vector with an element ~
                              too many dimensions down~@
                              (AREF #(...)~{ ~S~})."
             :format-arguments (list indexes)
             ))
    (unless (consp indexes)
      (setf indexes (list indexes)))
    (unify (apply #'aref a indexes) value-template env)))


(defmethod unify ((b aref-template) (a array)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;; ELT methods.
;;; LIST and VECTOR methods must be specified separatedly because of
;;; the UNIFY (VECTOR TEMPLATE) methods above.  It is a snag, but a
;;; relatively small one.  Besides, they are more efficient.
;;; The (SEQUENCE ELT-TEMPLATE) ELT-TEMPLATE method is left for those
;;; sequences which, according to the ANSI spec may exist and not be
;;; either VECTOR or LIST.

(defmethod unify ((a sequence) (b elt-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (elt-kwd index value-template)
      (template-spec b)
    (declare (ignore elt-kwd)
             (type fixnum index))
    ;; Missing index check.
    (unify (elt a index) value-template env)))


(defmethod unify ((a vector) (b elt-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (elt-kwd index value-template)
      (template-spec b)
    (declare (ignore elt-kwd)
             (type fixnum index))
    ;; Missing index check.
    (unify (aref a index) value-template env)))


(defmethod unify ((a list) (b elt-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (elt-kwd index value-template)
      (template-spec b)
    (declare (ignore elt-kwd)
             (type fixnum index))
    ;; Missing index check.
    (unify (nth index a) value-template env)))


(defmethod unify ((b elt-template) (a sequence)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;; NTH methods.

(defmethod unify ((a list) (b nth-template)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (destructuring-bind (nth-kwd index value-template)
      (template-spec b)
    (declare (ignore nth-kwd))
    ;; Missing index check.
    (unify (nth index a) value-template env)))

(defmethod unify ((b nth-template) (a list)
                  &optional (env (make-empty-environment))
                  &key &allow-other-keys)
  (unify a b env))


;;;---------------------------------------------------------------------------
;;; Utilities.

(defun unify* (a b &optional (env (make-empty-environment)))
  (ignore-errors (unify a b env)))


(defun unify-equations (eqns &optional (env (make-empty-environment)))
  (loop for (a b) in eqns
        for result-env = (unify a b env) then (unify a b result-env)
        finally (return result-env)))


(defun unify-equations* (lhss rhss &optional (env (make-empty-environment)))
  (loop for a in lhss
        for b in rhss
        for result-env = (unify a b env) then (unify a b result-env)
        finally (return result-env)))


;;;---------------------------------------------------------------------------
;;; VAR-UNIFY

(defparameter *occurrence-check-p* t)

(defgeneric occurs-in-p (var pat env))

(defun var-unify (var pat env)
  (if (eq var pat)
      env
      (multiple-value-bind (value foundp)
          (find-variable-value var env)
        (cond (foundp
               (unify value pat env))
              ((and *occurrence-check-p*
                    (occurs-in-p var pat env))
               (error 'unification-failure
                      :format-control "Variable ~S occurs in ~S."
                      :format-arguments (list var pat)))
              (t
               (extend-environment var pat env))))))



#||
(defmethod occurs-in-p ((var symbol) pat env)
  (cond ((variablep pat)
         (or (eq var pat)
             (multiple-value-bind (value foundp)
                 (find-variable-value pat env)
               (when foundp
                 (occurs-in-p var value env)))
             ))
        ((atom pat) nil)
        ((consp pat)
         (or (occurs-in-p var (first pat) env)
             (occurs-in-p var (rest pat) env)))
        (t
         (error "unimplemented"))))
||#


(defmethod occurs-in-p ((var symbol) (pat symbol) env)
  (when (variablep pat)
    (or (eq var pat)
        (multiple-value-bind (value foundp)
            (find-variable-value pat env)
          (when foundp
            (occurs-in-p var value env)))
        )))


(defmethod occurs-in-p ((var symbol) (pat list) env)
  (or (occurs-in-p var (first pat) env)
      (occurs-in-p var (rest pat) env)))


(defmethod occurs-in-p ((var symbol) (pat null) env)
  ;; This is needed because of different precedence rules among lisps
  ;; in COMPUTE-APPLICABLE-METHODS when NIL has to matched against
  ;; SYMBOL and LIST.
  
  ;; We know (assume) that VAR is not NIL.
  nil)


(defmethod occurs-in-p ((var symbol) (pat array) env)
  (loop for i from 0 below (array-total-size pat)
        thereis (occurs-in-p var (row-major-aref pat i) env)))


(defmethod occurs-in-p ((var symbol) (pat vector) env) ; This may be faster than the above.
  (some #'(lambda (x) (occurs-in-p var x env)) pat))


(defmethod occurs-in-p ((var symbol) (pat string) env) ; This is useless, but it's here for completeness.
  (declare (ignore env))
  nil)


(defmethod occurs-in-p ((var symbol) (pat number) env)
  (declare (ignore env))
  nil)


(defmethod occurs-in-p ((var symbol) (pat character) env)
  (declare (ignore env))
  nil)


(defmethod occurs-in-p ((var symbol) (pat t) env)
  (declare (ignore env))
  (warn "Occurrence test unimplemented for pattern ~S of type ~S in variable ~S;~@
         returning false."
        pat
        (type-of pat)
        var)
  nil)


(defmethod occurs-in-p ((var t) (pat t) env)
  (declare (ignore env))
  (error "Occurrence test called on a non symbol ~S. Major problem."
         var))

;;; end of file -- unifier.lisp --
