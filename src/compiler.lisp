(in-package :bard.internal)
(in-readtable :standard)

(defun gen-constant (c)
  `(constant ',c))

(defun gen-method (params body env)
  `(make-method ',params ',body ,env))

(defun gen-variable-ref (v env)
  `(variable-reference ',v ,env))

(defun gen-variable-set! (var val env)
  `(variable-set! ',var ,val ,env))

(defun bard-macro? (x) nil)

(defun ensure-arg-count (form min &optional (max min))
  (let ((n-args (length (rest form))))
    (assert (<= min n-args max) (form)
            "Wrong number of arguments for ~a in ~a:
       ~d supplied, ~d~@[ to ~d~] expected"
            (first form) form n-args min (if (/= min max) max))))

(defun compile-begin (xs env)
  (let ((val-exprs (mapcar (lambda (x)(compile x env)) xs)))
    `(begin ,@val-exprs)))

(defun compile-constant (val)
  (case val
    (|nothing| (gen-constant nil))
    (|true| (gen-constant (|true|)))
    (|false| (gen-constant (|false|)))
    (|undefined| (gen-constant (|undefined|)))
    (t (gen-constant val))))

;;; (compile-constant '|true|)
;;; (compile-constant '|false|)
;;; (compile-constant '|undefined|)
;;; (compile-constant '|nothing|)
;;; (compile-constant 5)
;;; (compile-constant "Foobar")

(defun compile-funcall (fn args env)
  `(apply ,(compile fn env)
          ,(mapcar (lambda (arg)(compile arg env)) args)))

(defun compile-if (test then else env)
  `(if (true? ,(compile test env))
       ,(compile then env)
       ,(compile else env)))

(defun compile-method (parameters body env)
  (gen-method parameters body env))

(defun compile-set! (var val env)
  (gen-variable-set! var (compile val env) env))

(defun compile-variable (var env)
  (gen-variable-ref var env))

;;; (compile-variable 'x nil)

(defun compile (x env)
  (cond
    ((member x '(|true| |false| |nothing| |undefined|)) (compile-constant x))
    ((symbolp x) (compile-variable x env))
    ((atom x) (compile-constant x))
    ((bard-macro? (first x)) (compile (bard-macro-expand x) env))
    ((case (first x)
       (|quote|  (ensure-arg-count x 1)
                 (compile-constant (second x)))
       (|begin|  (compile-begin (rest x) env))
       (|set!|   (ensure-arg-count x 2)
                 (compile-set! (second x)(third x) env))
       (|if|     (ensure-arg-count x 3)
                 (compile-if (second x) (third x) (fourth x) env))
       (|method| (compile-method (second x) (cddr x) env))
       (t      (compile-funcall (first x) (rest x) env))))))

;;; (compile '|true| nil)
;;; (compile '|false| nil)
;;; (compile '|undefined| nil)
;;; (compile '|nothing| nil)
;;; (compile 5 nil)
;;; (compile "Foobar" nil)
;;; (compile 'x nil)
;;; (compile '(|quote| |x|) nil)
;;; (compile '(|begin| 1 2 (* 2 3)) nil)
;;; (compile '(|set!| x (+ 2 3)) nil)
;;; (compile '(|if| x (+ a b)(- a b)) nil)
;;; (compile '(|method| (x y) (/ x y)) nil)
