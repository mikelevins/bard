;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; ---------------------------------------------------------------------
;;;; bardvm
;;;; A VM implementation based on Norvig's Scheme compiler from PAIP
;;;; prims.lisp
;;;; defining and handling primitive functions
;;;; ---------------------------------------------------------------------

(in-package :bardvm)

;;;; ---------------------------------------------------------------------
;;;; primitive functions
;;;; ---------------------------------------------------------------------

(defstruct (prim (:type list))
  symbol n-args opcode always side-effects)

(defmethod %%first ((s sequence)) (elt s 0))
(defmethod %%first ((s fset:wb-seq)) (fset:@ s 0))

(defmethod %%rest ((s sequence)) (subseq s 1))
(defmethod %%rest ((s fset:wb-seq)) (fset:subseq s 1))

(defun %%list1 (x) (list x))
(defun %%list2 (x y) (list x y))
(defun %%list3 (x y z) (list x y z))

;;; bard map makes a <map>
;;; the name of bard's version of mapcar is each
(defun map (&rest args)
  (fset:convert 'fset:wb-map
                (loop for tail on args by #'cddr
                   collect (cons (first tail)
                                 (second tail)))))
(defun %%map0 ()(fset:wb-map))

(defun %%newline () (terpri))

;;; prim: (prim-name n-args opcode-name always side-effects?)
;;; n-args is the number of args require to compile as a primitive;
;;;        if the actual number of args is different then we compile
;;;        as a call out to the Lisp function named in opcode-name
;;; always is true if the return value is always non-nil, false
;;;        if it's always nil, and nil otherwise
(defparameter *primitive-methods*
  '((+ 2 + true nil) (- 2 - true nil) (* 2 * true nil) (/ 2 / true nil)
    (< 2 < nil nil) (> 2 > nil nil) (<= 2 <= nil nil) (>= 2 >= nil nil)
    (/= 2 /= nil nil) (= 2 = nil nil)
    (eq? 2 eq nil nil) (equal? 2 equal nil nil) (eqv? 2 eql nil nil)
    (not 1 not nil nil) (null? 1 not nil nil)
    (cons 2 cons true nil)
    (map 0 %%map0 true nil)
    (car 1 car nil nil) (cdr 1 cdr nil nil) (cadr 1 cadr nil nil)
    (first 1 %%first nil nil)(rest 1 %%rest nil nil)
    (list 1 %%list1 true nil) (list 2 %%list2 true nil) (list 3 %%list3 true nil)
    (read 0 %%read nil t) (eof-object? 1 eof-object? nil) ;***
    (write 1 %%write nil t) (display 1 %%display nil t)
    (newline 0 %%newline nil t)
    (compiler 1 compiler t nil)
    (name! 2 name! true t) (random 1 random true nil)))

(defun primitive-p (f env n-args)
  "F is a primitive if it is in the table, and is not shadowed
  by something in the environment, and has the right number of args."
  (and (not (in-env-p f env))
       (find f *primitive-methods*
             :test #'(lambda (f prim)
                       (and (eq f (prim-symbol prim))
                            (= n-args (prim-n-args prim)))))))

