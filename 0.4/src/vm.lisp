;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard vm
;;;; Author:        mikel evins
;;;; Copyright:     2013 mikel evins
;;;;                Portions copyright 1991 by Peter Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; vm class
;;; ---------------------------------------------------------------------

(defclass <vm> ()
  ((method :accessor vm-method :initform nil :initarg :method)
   (code :accessor vm-code :initform nil :initarg :code)
   (pc :accessor vm-pc :initform nil :initarg :pc)
   (env :accessor vm-env :initform nil :initarg :env)
   (globals :accessor vm-globals :initform (make-hash-table))
   (stack :accessor vm-stack :initform nil :initarg :stack)
   (n-args :accessor vm-n-args :initform nil :initarg :n-args)
   (instr :accessor vm-instr :initform nil :initarg :instr)))

(defmethod initialize-instance :after ((vm <vm>) &rest initargs
                                       &key (method nil) &allow-other-keys)
  (declare (ignore initargs))
  (setf (vm-method vm) method)
  (setf (vm-code vm) (if method (method-code method) nil))
  (setf (vm-pc vm) 0)
  (setf (vm-env vm) (null-environment))
  (setf (vm-stack vm) nil)
  (setf (vm-n-args vm) 0)
  (setf (vm-instr vm) nil)

  ;; built-in globals
  ;; ----------------------------------------

  (def-global! vm 'bard-symbols::|Anything| (anything))
  (def-global! vm 'bard-symbols::|<alist>| *alist-structure*)
  (def-global! vm 'bard-symbols::|<bignum>| *bignum-structure*)
  (def-global! vm 'bard-symbols::|<boolean>| *boolean-structure*)
  (def-global! vm 'bard-symbols::|<character>| *character-structure*)
  (def-global! vm 'bard-symbols::|<cons>| *cons-structure*)
  (def-global! vm 'bard-symbols::|<eof>| *eof-structure*)
  (def-global! vm 'bard-symbols::|<fixnum>| *fixnum-structure*)
  (def-global! vm 'bard-symbols::|<file-stream>| *file-stream-structure*)
  (def-global! vm 'bard-symbols::|<flonum>| *flonum-structure*)
  (def-global! vm 'bard-symbols::|<function>| *function-structure*)
  (def-global! vm 'bard-symbols::|<keyword>| *keyword-structure*)
  (def-global! vm 'bard-symbols::|<method>| *method-structure*)
  (def-global! vm 'bard-symbols::|<null>| *null-structure*)
  (def-global! vm 'bard-symbols::|<ratnum>| *ratnum-structure*)
  (def-global! vm 'bard-symbols::|<stream>| *stream-structure*)
  (def-global! vm 'bard-symbols::|<string>| *string-structure*)
  (def-global! vm 'bard-symbols::|<symbol>| *symbol-structure*)
  (def-global! vm 'bard-symbols::|<undefined>| *undefined-structure*)
  (def-global! vm 'bard-symbols::|<url>| *url-structure*)

  ;; built-in methods
  ;; ----------------------------------------

  ;; exit
  (def-global! vm 'bard-symbols::|exit|
               (make-instance '<method> :name '|exit|
                              :args '() :code (assemble '((HALT))))))

;;; ---------------------------------------------------------------------
;;; return address class
;;; ---------------------------------------------------------------------

(defclass <return-address> () 
  ((function :accessor return-address-function :initform nil :initarg :function)
   (pc :accessor return-address-pc :initform nil :initarg :pc)
   (env :accessor return-address-env :initform nil :initarg :env)))

(defun make-return-address (&key pc function env)
  (make-instance '<return-address> :pc pc :function function :env env))

;;; ---------------------------------------------------------------------
;;; system tools
;;; ---------------------------------------------------------------------

(defparameter *vm-timer-start* nil)

(defun start-timer ()
  (setf *vm-timer-start* (get-internal-run-time))
  (format t "~%START-TIMER: timer started with value ~a" *vm-timer-start*)
  (force-output)
  (values))

(defun bard-error (msg)
  (format t "~%ERROR: ~A~%~%" msg))

(defun report-time ()
  (when *vm-timer-start*
    (let* ((end-time (get-internal-run-time))
           (elapsed (- end-time *vm-timer-start*))
           (secs (float (/ elapsed internal-time-units-per-second))))
      (format t "~%REPORT-TIME: timer stopped with value: ~a~%" end-time)
      (format t "  elapsed: ~a~%" elapsed)
      (format t "  (~a seconds)~%~%" secs)
      (force-output)
      (setf *vm-timer-start* nil)
      (values))))

;;; ---------------------------------------------------------------------
;;; vm accessors and utils
;;; ---------------------------------------------------------------------

(defmethod vm-stack-top ((vm <vm>))
  (first (vm-stack vm)))

(defmethod mutable? ((var symbol)(vm <vm>))
  t)

(defmethod def-global! ((vm <vm>)(var symbol) val)
  (setf (gethash var (vm-globals vm)) val))

(defmethod get-global ((vm <vm>)(var symbol))
  (gethash var (vm-globals vm) *undefined*))

(defmethod set-global! ((vm <vm>)(var symbol) val)
  (multiple-value-bind (val present?)(gethash var (vm-globals vm))
    (if present?
        (if (mutable? var vm)
            (setf (gethash var (vm-globals vm)) val)
            (error "Can't set an immutable variable: ~s" var))
        (error "Undefined variable ~s" var))))

;;; ---------------------------------------------------------------------
;;; instruction execution
;;; ---------------------------------------------------------------------

(defmethod vmexec ((vm <vm>) op args)
  (declare (ignore vm args))
  (error "Unknown opcode: ~a" op))

;;; Variable/stack-manipulation instructions:
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'LREF)) args)
  (declare (ignore op))
  (push (env-ref (vm-env vm) (first args))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'LSET)) args)
  (declare (ignore op))
  (env-set! (vm-env vm) (first args)
            (vm-stack-top vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'GDEF)) args)
  (declare (ignore op))
  (def-global! vm (first args) (vm-stack-top vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'GREF)) args)
  (declare (ignore op))
  (push (get-global vm (first args))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'GSET)) args)
  (declare (ignore op))
  (set-global! vm (first args) (vm-stack-top vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'POP)) args)
  (declare (ignore op args))
  (pop (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'CONST)) args)
  (declare (ignore op))
  (push (first args) (vm-stack vm)))

;;; Branching instructions:
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'GO)) args)
  (declare (ignore op))
  (setf (vm-pc vm) (first args)))

(defmethod vmexec ((vm <vm>) (op (eql 'FGO)) args)
  (declare (ignore op))
  (when (false? (pop (vm-stack vm)))
    (setf (vm-pc vm) (first args))))

(defmethod vmexec ((vm <vm>) (op (eql 'TGO)) args)
  (declare (ignore op))
  (when (true? (pop (vm-stack vm)))
    (setf (vm-pc vm) (first args))))

;;; Function call/return instructions:
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'SAVE)) args)
  (declare (ignore op))
  (push (make-return-address :pc (first args)
                             :function (vm-method vm)
                             :env (vm-env vm))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'RETURN)) args)
  (declare (ignore op args))
  ;; return value is top of stack; return-address is second
  (setf (vm-method vm) (return-address-function (second (vm-stack vm)))
        (vm-code vm) (method-code (vm-method vm))
        (vm-env vm) (return-address-env (second (vm-stack vm)))
        (vm-pc vm) (return-address-pc (second (vm-stack vm))))
  ;; Get rid of the return-address, but keep the value
  (setf (vm-stack vm) (cons (first (vm-stack vm))
                            (drop 2 (vm-stack vm)))))

(defun exec-method (vm method args)
  (let ((arg-count (length args)))
    (setf (vm-stack vm)(drop arg-count (vm-stack vm)))
    (setf (vm-method vm) method
          (vm-code vm) (method-code method)
          (vm-env vm) (make-call-env method (vm-env vm) args)
          (vm-pc vm) 0
          (vm-n-args vm) arg-count)))

(defun exec-function (vm function args)
  (let ((arg-count (length args))
        (method (most-specific-method function args)))
    (setf (vm-stack vm)(drop arg-count (vm-stack vm)))
    (setf (vm-method vm) method
          (vm-code vm) (method-code method)
          (vm-env vm) (make-call-env method (vm-env vm) args)
          (vm-pc vm) 0
          (vm-n-args vm) arg-count)))

(defmethod vmexec ((vm <vm>) (op (eql 'CALLJ)) args)
  (declare (ignore op))
  (let* ((proc (pop (vm-stack vm)))
         (found-arg-count (first args))
         (found-args (reverse
                      (subseq (vm-stack vm)
                              0 found-arg-count))))
    (cond
      ((method? proc)(exec-method vm proc found-args))
      ((function? proc)(exec-function vm proc found-args))
      (t (error "Unrecognized procedure type: ~s" proc)))))

;;; built-in constructors
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'METHOD)) args)
  (declare (ignore op))
  (push (make-instance '<method> 
                       :expression (method-expression (first args))
                       :code (method-code (first args))
                       :env (merge-environments (vm-env vm)(method-env (first args)))
                       :name (method-name (first args))
                       :args (method-args (first args)))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'FUNCTION)) args)
  (declare (ignore op))
  (push (make-instance '<function> 
                       :input-types (function-input-types (first args))
                       :output-types (function-output-types (first args)))
        (vm-stack vm)))


;;; Constants
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'TRUE)) args)
  (declare (ignore op args))
  (push (true) (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'FALSE)) args)
  (declare (ignore op args))
  (push (false) (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'UNDEFINED)) args)
  (declare (ignore op args))
  (push (undefined) (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'NOTHING)) args)
  (declare (ignore op args))
  (push (nothing) (vm-stack vm)))


;;; System instructions
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'START-TIMER)) args)
  (declare (ignore args))
  (funcall op))

(defmethod vmexec ((vm <vm>) (op (eql 'REPORT-TIME)) args)
  (declare (ignore args))
  (funcall op))

(defmethod vmexec ((vm <vm>) (op (eql 'HALT)) args)
  (declare (ignore op args))
  ;;(vm-stack-top vm)
  (values))

;;; primitive procedures
;;; -----------------------------------------

(defun exec-zero-argument-primitive (vm op)
  (push (funcall op) (vm-stack vm)))

(defun exec-one-argument-primitive (vm op)
  (push (funcall op (pop (vm-stack vm))) (vm-stack vm)))

(defun exec-two-argument-primitive (vm op)
  (setf (vm-stack vm)
        (cons (funcall op
                       (second (vm-stack vm))
                       (first (vm-stack vm)))
              (drop 2 (vm-stack vm)))))

(defun exec-three-argument-primitive (vm op)
  (setf (vm-stack vm)
        (cons (funcall op
                       (third (vm-stack vm))
                       (second (vm-stack vm))
                       (first (vm-stack vm)))
              (drop 3 (vm-stack vm)))))

(defmethod exec-list-construction-primitive (vm op)
  (declare (ignore vm))
  (error "Unrecognized list-contruction primitive ~S" op))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST0))) 
  (setf (vm-stack vm) (cons nil (vm-stack vm))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST1))) 
  (setf (vm-stack vm) 
        (cons (list (first (vm-stack vm)))
              (drop 1 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST2))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 2))
              (drop 2 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST3))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 3))
              (drop 3 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST4))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 4))
              (drop 4 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST5))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 5))
              (drop 5 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST6))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 6))
              (drop 6 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST7))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 7))
              (drop 7 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST8))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 8))
              (drop 8 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST9))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 9))
              (drop 9 (vm-stack vm)))))

(defmethod exec-list-construction-primitive ((vm <vm>) (op (eql 'LIST10))) 
  (setf (vm-stack vm) 
        (cons (reverse (subseq (vm-stack vm) 0 10))
              (drop 10 (vm-stack vm)))))

(defparameter $zero-argument-primitives
  '(BARD-READ NEWLINE STREAM.STANDARD-INPUT STREAM.STANDARD-OUTPUT STREAM.STANDARD-ERROR))

(defparameter $one-argument-primitives
  '(CONS.LEFT CONS.RIGHT CONS.FIRST CONS.REST CONS.LAST CONS.LENGTH
    BARD-NOT COMPILER DISPLAY BARD-WRITE
    AS-STRING AS-SYMBOL AS-KEYWORD AS-CONS 
    STRING.FIRST STRING.REST STRING.LAST ALIST? ALIST.KEYS ALIST.VALS AS-ALIST
    URL URL.SCHEME URL.HOST URL.PATH URL.PORT URL.QUERY AS-URL
    STREAM.LENGTH STREAM.READ-ALL-OCTETS STREAM.READ-ALL-CHARACTERS
    STREAM.READ-ALL-LINES STREAM.READ-ALL-OBJECTS
    GET-STRUCTURE SINGLETON BARD-ERROR))

(defparameter $two-argument-primitives
  '(+ - * / bard< bard> bard<= bard>= /= = 
    CONS NAME! IDENTICAL? EQUAL? STRING.APPEND CONS.APPEND CONS.DROP CONS.TAKE
    STRING.DROP STRING.TAKE ALIST.GET ALIST.MERGE
    STREAM.CREATE STREAM.READ-OCTET STREAM.READ-CHARACTER  STREAM.READ-LINE STREAM.READ-OBJECT))

(defparameter $three-argument-primitives
  '(STRING.SLICE CONS.SLICE ALIST.PUT 
    STREAM.READ-OCTETS STREAM.READ-CHARACTERS STREAM.READ-LINES STREAM.READ-OBJECTS
    STREAM.WRITE-OCTET STREAM.WRITE-OCTETS STREAM.WRITE-CHARACTER STREAM.WRITE-CHARACTERS
    STREAM.WRITE-LINE STREAM.WRITE-LINES  STREAM.WRITE-OBJECT STREAM.WRITE-OBJECTS
    ASSERT-METHOD!))

(defparameter $list-construction-primitives
  '(LIST0 LIST1 LIST2 LIST3 LIST4 LIST5 LIST6 LIST7 LIST8 LIST9 LIST10))

(defmethod vmexec ((vm <vm>) (op symbol) args)
  (declare (ignore args))
  (cond
    ((member op $zero-argument-primitives)(exec-zero-argument-primitive vm op))
    ((member op $one-argument-primitives)(exec-one-argument-primitive vm op))
    ((member op $two-argument-primitives)(exec-two-argument-primitive vm op))
    ((member op $three-argument-primitives)(exec-three-argument-primitive vm op))
    ((member op $list-construction-primitives)(exec-list-construction-primitive vm op))
    (t (error "Unknown opcode: ~a" op))))


;;; ---------------------------------------------------------------------
;;; execute one instruction
;;; ---------------------------------------------------------------------

(defmethod vmstep ((vm <vm>))
  (setf (vm-instr vm) (elt (vm-code vm) (vm-pc vm)))
  (incf (vm-pc vm))
  (let* ((instr (vm-instr vm))
         (op (opcode instr))
         (args (args instr)))
    (vmexec vm op args)))

;;; ---------------------------------------------------------------------
;;; run the vm
;;; ---------------------------------------------------------------------

(defmethod vmrun ((vm <vm>))
  (loop
     (if (< (vm-pc vm)
            (length (vm-code vm)))
         (vmstep vm)
         (return (values)))))



