;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

;;;; Implementation-Specific Details

(in-package :bard)

(eval-when (eval compile load)
  #+sbcl
  (progn
    (sb-ext:unlock-package '#:common-lisp)
    (sb-ext:unlock-package '#:common-lisp-user)))

;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)

(eval-when (load eval compile)
  (defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list .,variables))
	   (progn .,body)
	   (list 'let
	         ,`(list ,@(mapcar #'(lambda (tmp var)
			               `(list ',tmp ,var))
			           temps variables))
	         (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
		               variables temps)
	           .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(starts-with exp 'function)
	(and (starts-with exp 'the)
	     (side-effect-free? (third exp)))))

  (defmacro funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
  )

;;; ---------------------------------------------------------------------
;;; Auxiliary Functions
;;; ---------------------------------------------------------------------

(defun ->symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (format nil "~{~a~}" args)))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

;;; ==============================

;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;;; ==============================

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))


;;; ---------------------------------------------------------------------
;;; Other:
;;; ---------------------------------------------------------------------

(defun sort* (seq pred &key key)
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;; ==============================

(defun length=1 (x)
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

;;; ==============================

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

;;; ==============================

(defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(->symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(->symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(loop for element in elements
             for i from 0
             collect `(defparameter ,element ,i))))

;;; ==============================

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; ==============================

;;;; CLtL2 and ANSI CL Compatibility

(unless (fboundp 'defmethod)
(defmacro defmethod (name args &rest body)
  `(defun ',name ',args ,@body))
)

(unless (fboundp 'map-into)
(defun map-into (result-sequence function &rest sequences)
  "Destructively set elements of RESULT-SEQUENCE to the results
  of applying FUNCTION to respective elements of SEQUENCES."
  (let ((arglist (make-list (length sequences)))
        (n (if (listp result-sequence)
               most-positive-fixnum
               (array-dimension result-sequence 0))))
    ;; arglist is made into a list of args for each call
    ;; n is the length of the longest vector
    (when sequences
      (setf n (min n (loop for seq in sequences
                           minimize (length seq)))))
    ;; Define some shared functions:
    (flet
      ((do-one-call (i)
         (loop for seq on sequences
               for arg on arglist
               do (if (listp (first seq))
                      (setf (first arg)
                            (pop (first seq)))
                      (setf (first arg)
                            (aref (first seq) i))))
         (apply function arglist))
       (do-result (i)
         (if (and (vectorp result-sequence)
                  (array-has-fill-pointer-p result-sequence))
             (setf (fill-pointer result-sequence)
                   (max i (fill-pointer result-sequence))))))
      (declare (inline do-one-call))
      ;; Decide if the result is a list or vector,
      ;; and loop through each element
      (if (listp result-sequence)
          (loop for i from 0 to (- n 1)
                for r on result-sequence
                do (setf (first r)
                         (do-one-call i))
                finally (do-result i))
          (loop for i from 0 to (- n 1)
                do (setf (aref result-sequence i)
                         (do-one-call i))
                finally (do-result i))))
      result-sequence))

)

(unless (fboundp 'complement)
(defun complement (fn)
  "If FN returns y, then (complement FN) returns (not y)."
  #'(lambda (&rest args) (not (apply fn args))))
)

(unless (fboundp 'with-compilation-unit)
(defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; That way, undefined function warnings that are really
  ;; just forward references will not be printed at all.
  ;; This is defined in Common Lisp the Language, 2nd ed.
  (declare (ignore options))
  `(,(read-time-case
       #+Lispm 'compiler:compiler-warnings-context-bind
       #+Lucid 'with-deferred-warnings
               'progn)
    .,body))
)

;;;; Reduce

(when nil ;; Change this to T if you need REDUCE with :key keyword.

(defun reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq) #'reduce-list #'reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

(defun reduce (function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

(defun reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (funcall-if key (aref seq start)))
           (funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (funcall-if key (aref seq start))
                         (funcall-if key (aref seq (+ start 1)))))))
             (loop for i from (+ start (if init-p 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (funcall-if key (aref seq (- end 2)))
                         (funcall-if key (aref seq (- end 1)))))))
             (loop for i from (- end (if init-p 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (funcall-if key (aref seq i))
                              result)))
             result)))))

(defun reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) t start end
                      key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (funcall-if key (first seq)))
             (funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (funcall-if key (pop seq)))
                       (funcall
                         fn
                         (funcall-if key (pop seq))
                         (funcall-if key (pop seq))))))
             (if end
                 (loop repeat (- end (if init-p 1 2)) while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq)))))
                 (loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (funcall-if key (pop seq))))))
             result))))
)

