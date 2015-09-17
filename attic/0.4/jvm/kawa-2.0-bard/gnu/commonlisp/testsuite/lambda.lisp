(test-init "Lambda tests" 19)

;; These tests were adapted from P. F. Dietz's ANSI compilance test
;; suite (destructuring-bind.lsp) available from:
;; http://common-lisp.net/project/ansi-test/

(test 'a 'lambda.1
      ((lambda (x) x) 'a))

(test 'a 'lambda.2
      ((lambda () 'a)))

(test 'z 'lambda.4
      ((lambda (x) (declare (type symbol x)) x) 'z))

;; BUGS!
;; (deftest lambda.5
;;   ((lambda (&aux (x 'a)) x))
;;   a)

;; (deftest lambda.6
;;   ((lambda (&aux (x 'a)) (declare (type symbol x)) x))
;;   a)

(test "foo" 'lambda.7
      ((lambda () "foo")))

(test "bar" 'lambda.8
      ((lambda () "foo" "bar")))

(test '(1 2 nil) 'lambda.11
      ((lambda (x &optional y z) (list x y z)) 1 2))

(test '(1 nil c) 'lambda.12
      ((lambda (&optional (x 'a) (y 'b) (z 'c)) (list x y z)) 1 nil))

(test '(1 nil c t t nil) 'lambda.13
      ((lambda (&optional (x 'a x-p) (y 'b y-p) (z 'c z-p))
         (list x y z x-p y-p z-p)) 1 nil))

(test 2 'lambda.14
      (let ((x 1))
        ((lambda (&optional (x (+ x 1))) x))))

(test '(10 11) 'lambda.15
      ((lambda (y &optional (x (+ y 1))) (list y x)) 10))

(test '(10 14) 'lambda.16
      ((lambda (y &optional (x (+ y 1))) (list y x)) 10 14))

(test '(1 2 3) 'lambda.17
      ((lambda (&rest x) x) 1 2 3))

;; BUGS!
;; (test '(3 7) 'lambda.18
;;       (let ((b 10))
;;         ((lambda (&optional (a b) (b (+ a 1))) (list a b)) 3 7)))

;; (test '(3 4) 'lambda.19
;;       (let ((b 10))
;;         ((lambda (&optional (a b) (b (1+ a))) (list a b)) 3)))

(test nil 'lambda.23
      ((lambda (&key a) a)))

(test '(nil nil nil) 'lambda.24
      ((lambda (&key a b c) (list a b c))))

(test '(1 2 3) 'lambda.25
      ((lambda (&key (a 1) (b 2) (c 3)) (list a b c))))

;; BUG! This is technically allowed by ANSI, but due to how CL lambda
;; expressions are currently expanded into Scheme forms, it violates
;; the Scheme standard by having no body.
;; (test nil 'lambda.26
;;      ((lambda (&key))))

(test nil 'lambda.31
      ((lambda (&rest x &key) x)))

(test '(0 t 2 nil 5 t) 'lambda.34
      ((lambda (&key (a 1 a-p) (b 2 b-p) (c 3 c-p)) (list a a-p b b-p c c-p))
       :c 5 :a 0))

;; BUG!
;; (test '(nil t 2 nil 5 t) 'lambda.35
;;       ((lambda (&key (a 1 a-p) (b 2 b-p) (c 3 c-p)) (list a a-p b b-p c c-p))
;;        :c 5 :a nil :a 17 :c 100))

(test '(1 x) 'lambda.37
      (let ((b 1))
        ((lambda (&key (a b) b) (list a b)) :b 'x)))

(test '(nil x) 'lambda.38
      (let ((b 1))
        ((lambda (&key (a b) b) (list a b)) :b 'x :a nil)))
