;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm -- experimental variation 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; ABOUT
;;; ---------------------------------------------------------------------
;;; state vector
;;; the vm's state is represented as a variable-length vector of
;;; registers. the state vector has two logical parts: the constant
;;; segment, consisting of several registers that are always present
;;; and always in the same order, and the vals segment, consisting
;;; of a count followed by any number of val registers. the vals
;;; segment is used for passing argument values and for returning
;;; outputs.
;;;
;;; the state vector returned by a machine operation may or may not
;;; be EQ? to its input vector. each vm operation discards its input,
;;; and no subsequent operation should use a previous operation's input;
;;; operations always operate on the outputs of previous operations.
;;; each vm thread has its own state vector, and vm threads share no
;;; data, so no thread ever alters another thread's state vector.
;;; however, a vm operation may--and usually does--reuse its input 
;;; vector in order to avoid unnecessary consing. an operation
;;; returns a new vector only if it needs to change the size of
;;; state vector (i.e. if it needs more vals registers than the
;;; input vector supplies).
;;;
;;; globals
;;; the vm has two sets of globals:
;;; - read-only globals whose values may not be changed
;;; - read/write globals which are represented by Gambit threads
;;;   operations on globals include:
;;;   - GRO creates a new read-only global
;;;   - GRW creates a new read/write global
;;;   - GVAR reads a global
;;;     If the global is read-only, this simply means indexing
;;;     the vector of read-only globals. 
;;;     Read/write globals are implemented as Gambit threads. 
;;;     Reading one means asking the thread to send its current value
;;;     to the requesting thread, then blocking on receive.
;;;   - GSET writes a read/write global
;;;     Since read/write globals are represented by threads,
;;;     GSET means sending a message to the global's mailbox.


;;; ---------------------------------------------------------------------
;;; globals
;;; ---------------------------------------------------------------------

(define (make-globals) (vector #f #f))

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (null-env) '())

;;; ---------------------------------------------------------------------
;;; methods
;;; ---------------------------------------------------------------------

(define (null-method) '())
(define (null-code) '#())

;;; ---------------------------------------------------------------------
;;; the state vector and register operations
;;; ---------------------------------------------------------------------

(define (reg n sv)(vector-ref sv n))
(define (rset! n sv v)(vector-set! sv n v) sv)

(define reg:halt 0)
(define reg:pc 1)
(define reg:glob 2)
(define reg:env 3)
(define reg:meth 4)
(define reg:code 5)
(define reg:inst 6)
(define reg:valc 7)
(define (reg:valn n) (+ 1 reg:VALC n))

(define (state-vector-min-length)
  (+ reg:valc 1))

(define (new-state-vector #!optional (len 16))
  (make-vector len (void)))

(define (init-constant-set sv h p g e m c i)
    (vector-set! sv reg:halt h)
    (vector-set! sv reg:pc p)
    (vector-set! sv reg:glob g)
    (vector-set! sv reg:env e)
    (vector-set! sv reg:meth m)
    (vector-set! sv reg:code c)
    (vector-set! sv reg:inst i)
  sv)

(define (big-enough-for n)
  (- (expt 2 (integer-length n)) n))

(define (expand-state-vector sv) 
  (let ((vlen (vector-length sv)))
    (vector-append sv (new-state-vector (big-enough-for vlen)))))

;;; expand the state-vector only if there is no room for more vals
(define (maybe-expand-state-vector sv)
  (if (> (vector-length sv)
         (+ 2 reg:valc (reg reg:valc sv)))
      sv
      (expand-state-vector sv)))

(define (next-val-index sv)
  (+ 1 reg:valc (reg reg:valc sv)))

(define (inc-valc! sv)
  (rset! reg:valc sv (+ 1 (reg reg:valc sv))))

(define (push-val! v sv)
  (let ((sv (maybe-expand-state-vector sv))
        (n (next-val-index sv)))
    (rset! n sv v)
    (inc-valc! sv)
    sv))

(define (make-state-vector #!key 
                           (halt #f)
                           (pc 0)
                           (glob (make-globals))
                           (env (null-env))
                           (meth (null-method))
                           (code (null-code))
                           (inst #f)
                           (vals '()))
  (list->vector (append (list halt pc glob env meth code inst (length vals)) vals)))

;;; ---------------------------------------------------------------------
;;; vm diagnostic tools
;;; ---------------------------------------------------------------------

(define (showinst inst)
  (display "(")
  (display (opname (instruction:opcode inst)))
  (let ((argc (- (vector-length inst) 1)))
    (let loop ((i 1))
      (if (< i argc)
          (begin
            (display " ")
            (display (instruction:argn instr i))
            (loop (+ 1 i))))))
  (display ")"))

(define (showglobals globals) #f)
(define (showenv env) #f)
(define (showmeth env) #f)
(define (showcode env) #f)

(define (showvals sv)
  (let ((valc (reg reg:valc sv)))
    (let loop ((i 0))
      (if (< i valc)
          (begin
            (display " ")
            (display (reg (reg:valn i) sv)))))))

(define (showvm sv #!key (show-globals #f)(show-env #f)(show-method #f)(show-code #t)(show-vals #t))
  (newline)
  (display "halt: ")(display (reg reg:halt sv))(display " ")
  (display "pc: ")(display (reg reg:pc sv))(display " ")
  (display "inst: ")(showinst (reg reg:inst sv))(display " ")
  (if show-vals
      (let ((valc (reg reg:valc sv)))
        (newline)
        (if (> valc 0)
            (begin
              (display valc)(display " values:")
              (showvals sv))
            (display "0 values"))))
  (if show-code (begin (showcode (reg reg:code sv))))
  (if show-globals (begin (showglobals (reg reg:glob sv))))
  (if show-env (begin (showenv (reg reg:env sv))))
  (if show-method (begin (showmethod (reg reg:meth sv))))
  (newline)(newline))

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(define (instruction:opcode instr)
  (vector-ref instr 0))

(define (instruction:argn instr n)
  (vector-ref instr n))

;;; ---------------------------------------------------------------------
;;; vm operations
;;; ---------------------------------------------------------------------

(define $vm-operations (make-vector 255))
(define $vm-operation-names (make-vector 255))

(define-macro (make-op state proto body)
  (let* ((nm (car proto))
         (args (cdr proto))
         (bindings (let loop ((args args)
                              (i 1)
                              (blist '()))
                     (if (null? args)
                         (reverse blist)
                         (loop (cdr args)
                               (+ i 1)
                               (cons `(,(car args) (instruction:argn (reg reg:instr ,state) i))
                                     blist)))))
         (fn (gensym)))
    `(lambda (,state)(let ,bindings ,@body))))

(define-macro (defop num proto state . body)
  `(begin
     (define ,(car proto) ,num)
     (vector-set! $vm-operation-names ,num ',(car proto))
     (vector-set! $vm-operations ,num (make-op ,state ,proto ,body))))

(define (op n)(vector-ref $vm-operations n))
(define (opname n)(vector-ref $vm-operation-names n))

(defop 0 (HALT) vm (rset! reg:halt vm #t))

;;; ---------------------------------------------------------------------
;;; running the vm
;;; ---------------------------------------------------------------------

(define (fetch! sv)
  (rset! reg:inst sv (vector-ref (reg reg:code sv) (reg reg:pc sv))))

(define (exec! sv)
  ((op (instruction:opcode (reg reg:inst sv))) sv))

