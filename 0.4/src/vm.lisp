;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.lisp
;;;; Project:       Bard
;;;; Purpose:       the bard vm
;;;; Author:        mikel evins, after Norvig
;;;;
;;;; ***********************************************************************

(in-package :bard)

;;; ---------------------------------------------------------------------
;;; vm class
;;; ---------------------------------------------------------------------

(defclass <vm> ()
  ((mfn :accessor vm-mfn :initform nil :initarg :mfn)
   (code :accessor vm-code :initform nil :initarg :code)
   (pc :accessor vm-pc :initform nil :initarg :pc)
   (env :accessor vm-env :initform nil :initarg :env)
   (stack :accessor vm-stack :initform nil :initarg :stack)
   (n-args :accessor vm-n-args :initform nil :initarg :n-args)
   (instr :accessor vm-instr :initform nil :initarg :instr)))

(defmethod initialize-instance :after ((vm <vm>) &rest initargs
                                       &key (mfn nil) &allow-other-keys)
  (declare (ignore initargs))
  (setf (vm-mfn vm) mfn)
  (setf (vm-code vm) (if mfn (mfn-code mfn) nil))
  (setf (vm-pc vm) 0)
  (setf (vm-env vm) (null-environment))
  (setf (vm-stack vm) nil)
  (setf (vm-n-args vm) 0)
  (setf (vm-instr vm) nil))

;;; ---------------------------------------------------------------------
;;; return address class
;;; ---------------------------------------------------------------------

(defclass <return-address> () 
  ((fn :accessor return-address-fn :initform nil :initarg :fn)
   (pc :accessor return-address-pc :initform nil :initarg :pc)
   (env :accessor return-address-env :initform nil :initarg :env)))

(defun make-return-address (&key pc fn env)
  (make-instance '<return-address> :pc pc :fn fn :env env))

;;; ---------------------------------------------------------------------
;;; system tools
;;; ---------------------------------------------------------------------

(defparameter *vm-timer-start* nil)

(defun start-timer ()
  (setf *vm-timer-start* (get-internal-run-time))
  (format t "~%START-TIMER: timer started with value ~a" *vm-timer-start*)
  (force-output)
  (values))

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

(defmethod vmexec ((vm <vm>) (op (eql 'GREF)) args)
  (declare (ignore op))
  (push (get-global (first args))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'GSET)) args)
  (declare (ignore op))
  (set-global! (first args) (vm-stack-top vm)))

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
                             :fn (vm-mfn vm)
                             :env (vm-env vm))
        (vm-stack vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'RETURN)) args)
  (declare (ignore op args))
  ;; return value is top of stack; return-address is second
  (setf (vm-mfn vm) (return-address-fn (second (vm-stack vm)))
        (vm-code vm) (mfn-code (vm-mfn vm))
        (vm-env vm) (return-address-env (second (vm-stack vm)))
        (vm-pc vm) (return-address-pc (second (vm-stack vm))))
  ;; Get rid of the return-address, but keep the value
  (setf (vm-stack vm) (cons (first (vm-stack vm))
                            (drop 2 (vm-stack vm)))))

(defmethod vmexec ((vm <vm>) (op (eql 'CALLJ)) args)
  (declare (ignore op))
  (let* ((mfn (pop (vm-stack vm)))
         (found-arg-count (first args))
         (found-args (reverse
                      (subseq (vm-stack vm)
                              0 found-arg-count))))
    (setf (vm-stack vm)(drop found-arg-count (vm-stack vm)))
    (setf (vm-mfn vm) mfn
          (vm-code vm) (mfn-code mfn)
          (vm-env vm) (make-call-env mfn (vm-env vm) found-args)
          (vm-pc vm) 0
          (vm-n-args vm) found-arg-count)))

;;; built-in constructors
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'MFN)) args)
  (declare (ignore op))
  (push (make-instance '<mfn> 
                       :expression (mfn-expression (first args))
                       :code (mfn-code (first args))
                       :env (merge-environments (vm-env vm)(mfn-env (first args)))
                       :name (mfn-name (first args))
                       :args (mfn-args (first args)))
        (vm-stack vm)))


;;; Continuation instructions:
;;; -----------------------------------------

(defmethod vmexec ((vm <vm>) (op (eql 'SET-CC)) args)
  (declare (ignore op args))
  (setf (vm-stack vm) (vm-stack-top vm)))

(defmethod vmexec ((vm <vm>) (op (eql 'CC)) args)
  (declare (ignore op args))
  (push (make-instance '<mfn>
                       :name (gen-label 'continuation-)
                       :env (make-instance '<environment> 
                                           :frames (list (vector (vm-stack vm))))
                       :code (assemble
                              '((ARGS 1) (LREF 1 0 ";" stack) (SET-CC)
                                (LREF 0 0) (RETURN))))
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
  (vm-stack-top vm))

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
    STRING.FIRST STRING.REST STRING.LAST ALIST-MAP? ALIST.KEYS ALIST.VALS AS-ALIST-MAP
    URL URL.SCHEME URL.HOST URL.PATH URL.PORT URL.QUERY AS-URL
    STREAM.LENGTH STREAM.READ-ALL-OCTETS STREAM.READ-ALL-CHARACTERS
    STREAM.READ-ALL-LINES STREAM.READ-ALL-OBJECTS))

(defparameter $two-argument-primitives
  '(+ - * / bard< bard> bard<= bard>= /= = 
    CONS NAME! IDENTICAL? EQUAL? STRING.APPEND CONS.APPEND CONS.DROP CONS.TAKE
    STRING.DROP STRING.TAKE ALIST.GET ALIST.MERGE
    STREAM.CREATE STREAM.READ-OCTET STREAM.READ-CHARACTER  STREAM.READ-LINE STREAM.READ-OBJECT))

(defparameter $three-argument-primitives
  '(STRING.SLICE CONS.SLICE ALIST.PUT 
    STREAM.READ-OCTETS STREAM.READ-CHARACTERS STREAM.READ-LINES STREAM.READ-OBJECTS
    STREAM.WRITE-OCTET STREAM.WRITE-OCTETS STREAM.WRITE-CHARACTER STREAM.WRITE-CHARACTERS
    STREAM.WRITE-LINE STREAM.WRITE-LINES  STREAM.WRITE-OBJECT STREAM.WRITE-OBJECTS))

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
         (return (vm-stack-top vm)))))



