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
;;; executing the vm
;;; ---------------------------------------------------------------------


;;; execute one instruction
(defmethod vmstep ((vm <vm>))
  
  (setf (vm-instr vm) (elt (vm-code vm) (vm-pc vm)))
  (incf (vm-pc vm))

  (let ((instr (vm-instr vm)))
    (case (opcode instr)
      
      ;; Variable/stack manipulation instructions:
      ;; -----------------------------------------

      (LREF   (push (elt (elt (vm-env vm) (arg1 instr)) (arg2 instr))
                    (vm-stack vm)))
      (LSET   (setf (elt (elt (vm-env vm) (arg1 instr)) (arg2 instr))
                    (vm-stack-top vm)))
      (GREF   (let ((var (arg1 instr)))
                (push (get-global var)
                      (vm-stack vm))))
      (GSET   (let ((var (arg1 instr)))
                (set-global! var (vm-stack-top vm))))
      (POP    (pop (vm-stack vm)))
      (CONST  (push (arg1 instr) (vm-stack vm)))
      
      ;; Branching instructions:
      ;; -----------------------------------------

      (GO   (setf (vm-pc vm) (arg1 instr)))
      (FGO  (if (false? (pop (vm-stack vm))) (setf (vm-pc vm) (arg1 instr))))
      (TGO  (if (true? (pop (vm-stack vm))) (setf (vm-pc vm) (arg1 instr))))
      
      ;; Function call/return instructions:
      ;; -----------------------------------------

      (SAVE   (push (make-return-address :pc (arg1 instr)
                                   :fn (vm-mfn vm) :env (vm-env vm))
                    (vm-stack vm)))
      (RETURN ;; return value is top of stack; return-address is second
        (setf (vm-mfn vm) (return-address-fn (second (vm-stack vm)))
              (vm-code vm) (mfn-code (vm-mfn vm))
              (vm-env vm) (return-address-env (second (vm-stack vm)))
              (vm-pc vm) (return-address-pc (second (vm-stack vm))))
        ;; Get rid of the return-address, but keep the value
        (setf (vm-stack vm)
              (cons (first (vm-stack vm))
                    (drop 2 (vm-stack vm)))))
      (CALLJ  (pop (vm-env vm))                 ; discard the top frame
              (setf (vm-mfn vm) (pop (vm-stack vm))
                    (vm-code vm) (mfn-code (vm-mfn vm))
                    (vm-env vm) (mfn-env (vm-mfn vm))
                    (vm-pc vm) 0
                    (vm-n-args vm) (arg1 instr)))
      (ARGS   (assert (= (vm-n-args vm) (arg1 instr)) ()
                      "Wrong number of arguments: ~d expected, ~d supplied"
                      (arg1 instr) (vm-n-args vm))
              (push (make-array (arg1 instr)) (vm-env vm))
              (loop for i from (- (vm-n-args vm) 1) downto 0 do
                   (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))
      (ARGS.  (assert (>= (vm-n-args vm) (arg1 instr)) ()
                      "Wrong number of arguments: ~d or more expected, ~d supplied"
                      (arg1 instr) (vm-n-args vm))
              (push (make-array (+ 1 (arg1 instr))) (vm-env vm))
              (loop repeat (- (vm-n-args vm) (arg1 instr)) do
                   (push (pop (vm-stack vm)) (elt (first (vm-env vm)) (arg1 instr))))
              (loop for i from (- (arg1 instr) 1) downto 0 do
                   (setf (elt (first (vm-env vm)) i) (pop (vm-stack vm)))))

      ;; built-in constructors
      ;; -----------------------------------------

      (MFN     (push (make-instance '<mfn> 
                                    :expression (mfn-expression (arg1 instr))
                                    :code (mfn-code (arg1 instr))
                                    :env (vm-env vm)
                                    :name (mfn-name (arg1 instr))
                                    :args (mfn-args (arg1 instr)))
                     (vm-stack vm)))
      (PRIM   (push (apply (arg1 instr)
                           (loop with args = nil repeat (vm-n-args vm)
                              do (push (pop (vm-stack vm)) args)
                              finally (return args)))
                    (vm-stack vm)))
      
      ;; Continuation instructions:
      ;; -----------------------------------------

      (SET-CC (setf (vm-stack vm) (vm-stack-top vm)))
      (CC     (push (make-instance '<mfn>
                                   :name (gen-label 'continuation-)
                                   :env (list (vector (vm-stack vm)))
                                   :code (assemble
                                          '((ARGS 1) (LREF 1 0 ";" stack) (SET-CC)
                                            (LREF 0 0) (RETURN))))
                    (vm-stack vm)))
      

      ;; -----------------------------------------
      ;; primitive procedures
      ;; -----------------------------------------

      ;; zero arguments
      ;; -----------------------------------------

      ((BARD-READ NEWLINE STREAM.STANDARD-INPUT STREAM.STANDARD-OUTPUT STREAM.STANDARD-ERROR)
       (push (funcall (opcode instr)) (vm-stack vm)))
      
      ;; 1 argument
      ;; -----------------------------------------

      ((CONS.LEFT CONS.RIGHT CONS.FIRST CONS.REST CONS.LAST CONS.LENGTH
                  BARD-NOT COMPILER DISPLAY BARD-WRITE
                  AS-STRING AS-SYMBOL AS-KEYWORD AS-CONS 
                  STRING.FIRST STRING.REST STRING.LAST ALIST-MAP? ALIST.KEYS ALIST.VALS AS-ALIST-MAP
                  URL URL.SCHEME URL.HOST URL.PATH URL.PORT URL.QUERY AS-URL
                  STREAM.LENGTH STREAM.READ-ALL-OCTETS STREAM.READ-ALL-CHARACTERS
                  STREAM.READ-ALL-LINES STREAM.READ-ALL-OBJECTS)
       (push (funcall (opcode instr) (pop (vm-stack vm))) (vm-stack vm)))
      
      ;; 2 arguments
      ;; -----------------------------------------

      ((+ - * / bard< bard> bard<= bard>= /= = 
          CONS NAME! IDENTICAL? EQUAL? STRING.APPEND CONS.APPEND CONS.DROP CONS.TAKE
          STRING.DROP STRING.TAKE ALIST.GET ALIST.MERGE
          STREAM.CREATE STREAM.READ-OCTET STREAM.READ-CHARACTER  STREAM.READ-LINE STREAM.READ-OBJECT)
       (setf (vm-stack vm)
             (cons (funcall (opcode instr) (second (vm-stack vm))
                            (first (vm-stack vm)))
                   (drop 2 (vm-stack vm)))))
      
      ;; 3 arguments
      ;; -----------------------------------------

      ((STRING.SLICE CONS.SLICE ALIST.PUT 
                     STREAM.READ-OCTETS STREAM.READ-CHARACTERS STREAM.READ-LINES STREAM.READ-OBJECTS
                     STREAM.WRITE-OCTET STREAM.WRITE-OCTETS STREAM.WRITE-CHARACTER STREAM.WRITE-CHARACTERS
                     STREAM.WRITE-LINE STREAM.WRITE-LINES  STREAM.WRITE-OBJECT STREAM.WRITE-OBJECTS)
       (setf (vm-stack vm) (cons (funcall (opcode instr) (third (vm-stack vm))
                                          (second (vm-stack vm)) (first (vm-stack vm)))
                                 (drop 3 (vm-stack vm)))))

      
      ;; Primitive list constructors:
      ;; -----------------------------------------

      (LIST0 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) nil)
                         (vm-stack vm))))

      (LIST1 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) (first (vm-stack vm)))
                         (drop 1 (vm-stack vm)))))

      (LIST2 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) (second (vm-stack vm))
                                  (first (vm-stack vm)))
                         (drop 2 (vm-stack vm)))))

      (LIST3 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) (third (vm-stack vm))
                                  (second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 3 (vm-stack vm)))))

      (LIST4 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (fourth (vm-stack vm))(third (vm-stack vm))
                                  (second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 4 (vm-stack vm)))))

      (LIST5 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (fifth (vm-stack vm))(fourth (vm-stack vm))(third (vm-stack vm))
                                  (second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 5 (vm-stack vm)))))

      (LIST6 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (sixth (vm-stack vm))(fifth (vm-stack vm))(fourth (vm-stack vm))
                                  (third (vm-stack vm))(second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 6 (vm-stack vm)))))

      (LIST7 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (seventh (vm-stack vm))(sixth (vm-stack vm))(fifth (vm-stack vm))
                                  (fourth (vm-stack vm))(third (vm-stack vm))(second (vm-stack vm))
                                  (first (vm-stack vm)))
                         (drop 7 (vm-stack vm)))))

      (LIST8 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (eighth (vm-stack vm))(seventh (vm-stack vm))(sixth (vm-stack vm))
                                  (fifth (vm-stack vm))(fourth (vm-stack vm))(third (vm-stack vm))
                                  (second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 8 (vm-stack vm)))))

      (LIST9 (setf (vm-stack vm)
                   (cons (funcall (opcode instr) 
                                  (ninth (vm-stack vm))(eighth (vm-stack vm))(seventh (vm-stack vm))
                                  (sixth (vm-stack vm))(fifth (vm-stack vm))(fourth (vm-stack vm))
                                  (third (vm-stack vm))(second (vm-stack vm)) (first (vm-stack vm)))
                         (drop 9 (vm-stack vm)))))

      (LIST10 (setf (vm-stack vm)
                    (cons (funcall (opcode instr) 
                                   (tenth (vm-stack vm))(ninth (vm-stack vm))(eighth (vm-stack vm))
                                   (seventh (vm-stack vm))(sixth (vm-stack vm))(fifth (vm-stack vm))
                                   (fourth (vm-stack vm))(third (vm-stack vm))(second (vm-stack vm))
                                   (first (vm-stack vm)))
                          (drop 10 (vm-stack vm)))))

      ;; Push a constant
      ;; -----------------------------------------

      (TRUE (push (true) (vm-stack vm)))
      (FALSE (push (false) (vm-stack vm)))
      (UNDEFINED (push (undefined) (vm-stack vm)))
      (NOTHING (push (nothing) (vm-stack vm)))
      
      ;; Other instructions
      ;; -----------------------------------------
      
      ((START-TIMER REPORT-TIME) (funcall (opcode instr)))
      ((HALT) (vm-stack-top vm))
      (otherwise (error "Unknown opcode: ~a" instr)))))

;;; run the vm
(defmethod vmrun ((vm <vm>))
  (loop
     (if (< (vm-pc vm)
            (length (vm-code vm)))
         (vmstep vm)
         (return (vm-stack-top vm)))))



