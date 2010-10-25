;;;; -*- Mode: Lisp -*-

;;;; unification-tests.lisp --
;;;; CL-UNIFICATION test suite.  Requires Franz's util.test package.

(in-package "UNIFY.TESTS")

(use-package "UNIFY")
(use-package "UTIL.TEST")

(with-tests (:name "basic constant unification")
  (test t (unify:environment-p (unify 42 42)))

  (test-error (unify 42 12) :condition-type 'unification-failure)

  (test-error (unify 42 'a) :condition-type 'unification-failure)

  (test t (unify:environment-p (unify 'a 'a)))

  (test t (unify:environment-p (unify '(a s d) '(a s d))))

  (test t (unify:environment-p (unify '(a (s 42) d) '(a (s 42) d))))

  (test-error (unify '(a (s forty-two) d) '(a (s 42) z))
	      :condition-type 'unification-failure)

  (test t (unify:environment-p (unify #(a s d) #(a s d))))

  (test t (unify:environment-p (unify #2a((a s d) (a s d))
				      #2a((a s d) (a s d)))))

  (test-error (unify #2a((a s d) (a s d))
                     #2a((a s d) (a 42 d)))
              :condition-type 'unification-failure)

  (test t (unify:environment-p (unify "I am a string" "I am a string")))

  (test-error (unify "I am a string" "I am A string")
	      :condition-type 'unification-failure)

  (test t (let ((*unify-string-case-insensitive-p* t))
            (unify:environment-p (unify "I am a string" "I am A string"))))

  )


(with-tests (:name "variables unification")
  (test '(42 T) (find-variable-value '?x (unify 42 '?x))
	:multiple-values t)
  (test '(NIL NIL) (find-variable-value '?y (unify 42 '?x))
	:multiple-values t)

  (test '(42 T) (find-variable-value '?x (unify '?x 42))
	:multiple-values t)

  (test '(s T) (v? '?x (unify '(a (?x 42) d) '(a (s 42) d)))
	:multiple-values t)
  (test '(s T) (v? '?x (unify '(a (?x 42) d) '(a (s 42) d)))
	:multiple-values t)

  (test '((?x 42) T) (v? '?z (unify '(a (?x 42) d) '(a ?z d)))
	:multiple-values t :test 'equal)

  (test '(NIL T) (v? '?x (unify '(a (?x 42) d) '(a (() 42) d)))
	:multiple-values t)

  (test '(NIL NIL) (v? '?variable (unify '(a (() 42) d) '(a (?x 42) d)))
	:multiple-values t)

  (test t (unify:environment-p (unify '_ '(1 2 3))))

  (test t (unify:environment-p (unify '_ '(1 _ 3))))

  (test t (unify:environment-p (unify '(1 2 _) '(1 _ 3))))

  (test t (unify:environment-p (unify '(1 2 _) '(1 _ 3))))

  (test '(2 T) (v? '?x (unify #(1 2 _) #(1 ?x 3)))
	:multiple-values t)

  (test-error (unify '(1 2 _) #(1 _ 3))
              :condition-type 'unification-failure
              :known-failure t
              :fail-info "Unification on SEQUENCEs does not discriminate type.")
  )


(with-tests (:name "basic templates unification")

  (with-tests (:name "number templates unification")
    (test t (unify:environment-p (unify #T(number 42) 42)))
    (test t (unify:environment-p (unify 42 #T(number 42))))
    (test t (unify:environment-p (unify 42 #T(integer 42))))
    (test t (unify:environment-p (unify 42 #T(fixnum 42))))

    (test t (unify:environment-p (unify 42.0 #T(real 42))))
    (test t (unify:environment-p (unify #C(0 1) #T(complex #C(0 1)))))

    (test '(42 T) (v? '?x (unify #T(number ?x) 42)) :multiple-values t)

    (test-error (unify 42 #T(float 42.0))
                :condition-type 'unification-failure
                :known-failure t
                :fail-info "Check rules for unification on numeric tower.")
    )
  )


(defclass test1 ()
  ((a :initarg :a :accessor a)
   (b :initarg :b :accessor b)))

(defstruct s-root a)
(defstruct (s-child (:include s-root)) b)

(with-tests (:name "advanced templates unification")

  (test '(a T) (v? '?x (unify #2A((1 #T(symbol ?x) 3) (_ _ _))
			      #2A((1 a 3) (q w e))))
          :multiple-values t)

  (test '(#\Space T) (ignore-errors (v? '?x (unify "This is a string!" #T(elt 4 ?x))))
        :multiple-values t)

  (test '(42 T) (ignore-errors (v? '?x (unify '(0 1 42 3 4 5) #T(nth 2 ?x))))
        :multiple-values t)

  (test '(42 T) (ignore-errors (v? '?x (unify '(0 1 42 3 4 5) #T(elt 2 ?x))))
        :multiple-values t)

  (test '(42 T) (ignore-errors (v? '?x (unify #(0 1 42 3 4 5) #T(aref 2 ?x))))
        :multiple-values t)

  (test '(42 T) (ignore-errors (v? '?x (unify #(0 1 42 3 4 5) #T(elt 2 ?x))))
        :multiple-values t)

  (test '(42 T) (v? '?x (unify #2a((0 1 42 3 4 5)) #T(aref (0 2) ?x)))
        :multiple-values t)

  (test '(42 T) (v? '?x (unify #T(aref (0 2) 42) #2a((0 1 ?x 3 4 5))))
        :multiple-values t)

  (test '(42 T) (v? '?x (unify #2a((0 1 ?x 3 4 5)) #T(aref (0 2) 42)))
        :multiple-values t)

  (test-error (unify #(0 1 42 3 4 5) #T(nth 2 ?x))
              :condition-type 'unification-failure
              :announce t)

  (test '(foo (1) (2) (3)) (let ((result-env (unify '(0 1 #T(list foo _ &rest ?z) 42)
                                                    '(0 1 (?y bar (1) (2) (3)) 42)))
                                 )
                             (cons (v? '?y result-env)
                                   (v? '?z result-env)))
        :test #'equal)

  (test '(2 T) (v? '?x (unify #T(test1 a #T(list 1 ?x 3 &rest) b "woot")
                              (make-instance 'test1 :a '(1 2 3) :b "woot")))
        :multiple-values t)

  (test-error (unify #T(s-root s-root-a '(1 ?x 3 4))
                     (make-s-root :a '(1 2 3 4)))
              :condition-type 'unification-failure
              :announce t
              ;; #T reader non evaluating sub forms.
              )

  (test '(2 T) (v? '?x (unify #T(s-root s-root-a #T(list 1 ?x 3 4))
                              (make-s-root :a '(1 2 3 4))))
        :multiple-values t)

  (test '(2 T) (v? '?x (unify #T(s-root s-root-a (1 ?x 3 4))
                              (make-s-root :a '(1 2 3 4))))
        :multiple-values t)

  (test '(2 T) (v? '?x (unify #T(s-root s-root-a #T(list 1 ?x 3 &rest))
                              (make-s-root :a '(1 2 3 4))))
        :multiple-values t)

  (test '(2 T) (v? '?x (unify #T(s-root s-root-a #(1 ?x 3 4))
                              (make-s-root :a #(1 2 3 4))))
        :multiple-values t)

  (test '(2 T) (v? '?x (unify #T(s-root s-root-a #T(vector 1 ?x 3 &rest))
                              (make-s-root :a #(1 2 3 4))))
        :multiple-values t)

  )


(defun nested-match-cases (input)
 (match-case (input)
   ('(:a ?a :b #T(list &rest ?bs))
    (loop for b in ?bs
	   collect (match-case (b)
		     ('(:c ?c)   ?c)
		     ('(:d ?d)   ?d)
		     (otherwise (error "error-inner")))))
   (otherwise (error "error-outer"))))

(with-tests (:name "control flow")
  (test-error (nested-match-cases '(:a 42 :b 33)) :announce t)

  (test-error (nested-match-cases '(:a 42 :b (33 42))) :announce t)

  (test '(42 43 44) (nested-match-cases '(:a 42 :b ((:d 42) (:c 43) (:c 44))))
        :test #'equal)
  )


;;;; end of file -- unification-tests.lisp --
