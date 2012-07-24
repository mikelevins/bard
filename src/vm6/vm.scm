;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; constants
;;; ---------------------------------------------------------------------

(define $nothing '())
(define $true #t)
(define $false #f)

;;; ---------------------------------------------------------------------
;;; primitive predicates
;;; ---------------------------------------------------------------------

(define nothing? null?)
(define (true? x) (eqv? x #t))
(define (false? x) (eqv? x #f))

;;; ---------------------------------------------------------------------
;;; stretchy-vectors
;;; ---------------------------------------------------------------------

(define-type stretchy-vector
  constructor: %private-make-stretchy-vector
  read-only:
  extend-count
  read-write:
  fill-pointer
  elements)

(define (make-stretchy-vector count #!optional (initial-element #f)(fill-pointer #f)(extend-count #f))
  (let* ((fill-pointer (or fill-pointer 0))
         (v (make-vector count initial-element)))
    (%private-make-stretchy-vector extend-count fill-pointer v)))

(define (stretchy-vector-length sv)(stretchy-vector-fill-pointer sv))
(define (stretchy-vector-capacity sv)(vector-length (stretchy-vector-elements sv)))

(define (stretchy-vector-ref sv n)
  (if (< n (stretchy-vector-length sv))
      (vector-ref (stretchy-vector-elements sv) n)
      (error "index out of range")))

(define (stretchy-vector-set! sv index val)
  (let ((len (stretchy-vector-length sv)))
    (if (> index len)
        (error "index out of range")
        (vector-set! (stretchy-vector-elements sv) index val))))

(define (vector-push-extend! sv val)
  (let ((len (stretchy-vector-length sv))
        (cap (stretchy-vector-capacity sv)))
    (if (= len cap)
        (let ((oldelts (stretchy-vector-elements sv))
              (newelts (make-vector (or (stretchy-vector-extend-count sv) (* 2 cap))
                                    #f)))
          (let ((newlen (vector-length newelts)))
            (let loop ((i 0))
              (if (>= i len)
                  (begin
                    (stretchy-vector-elements-set! sv newelts)
                    (stretchy-vector-set! sv len val)
                    (stretchy-vector-fill-pointer-set! sv (+ len 1)))
                  (begin
                    (vector-set! newelts i (vector-ref oldelts i))
                    (loop (+ i 1)))))))
        (begin
          (stretchy-vector-set! sv len val)
          (stretchy-vector-fill-pointer-set! sv (+ len 1))))))

(define (vector-pop! sv)
  (let* ((newlen (- (stretchy-vector-length sv) 1))
         (val (vector-ref (stretchy-vector-elements sv)
                          newlen)))
    (stretchy-vector-fill-pointer-set! sv newlen)
    val))

;;; ---------------------------------------------------------------------
;;; opcodes
;;; ---------------------------------------------------------------------

(define HALT 0)
(define LVAR 1)
(define LSET 2)
(define MVAR 3)
(define MSET 4)
(define SLOT 5)
(define SSET 6)
(define JUMP 7)
(define TJUMP 8)
(define FJUMP 9)
(define ARGS 10)
(define METHOD 11)
(define DISP 12)
(define SAVE 13)
(define APPLY 14)
(define PRIM 15)
(define RESTORE 16)
(define CONST 17)
(define NOTHING 18)
(define TRUE 19)
(define FALSE 20)
(define ONE 21)
(define TWO 22)
(define NEG1 23)
(define NOP 24)

;;; ---------------------------------------------------------------------
;;; vm
;;; ---------------------------------------------------------------------

(define-type vm
  constructor: %private-make-vm
  optable
  code
  pc
  inst
  stack
  vals
  env
  globals)

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(define (instr opc . args)(list opc args))
(define (opc inst)(car inst))
(define (arg1 inst)(list-ref inst 1))
(define (arg2 inst)(list-ref inst 2))
(define (arg3 inst)(list-ref inst 3))
(define (args inst)(cdr inst))

(define (%halt vm) #f) ; dummy; the real %halt op ges initialized inside a vm's main loop
(define (%lvar vm i j) (lref (vm-env vm) i j))
(define (%lset vm i j v) (lset! (vm-env vm) i j v))
(define (%mvar vm i) (mref (vm-globals vm) i))
(define (%mset vm i v) (mset! (vm-globals vm) i v))
(define (%slot vm i j) (sref (vm-vals vm) i j))
(define (%sset vm i j v) (sset! (vm-vals vm) i j v))

(define (%const vm k) (vm-push-val! vm k))
(define (%nothing vm) (vm-push-val! vm $nothing))
(define (%true vm) (vm-push-val! vm $true))
(define (%false vm) (vm-push-val! vm $false))
(define (%one vm) (vm-push-val! vm 1))
(define (%two vm) (vm-push-val! vm 2))
(define (%neg1 vm) (vm-push-val! vm -1))

(define (%jump vm) #f)
(define (%tjump vm) #f)
(define (%fjump vm) #f)
(define (%args vm) #f)
(define (%method vm) #f)
(define (%disp vm) #f)
(define (%save vm) #f)
(define (%apply vm) #f)
(define (%prim vm) #f)
(define (%restore vm) #f)

(define (%nop vm) #f)

(define $optable
  (vector %halt %lvar %lset %mvar %mset %slot
          %sset %jump %tjump %fjump %args
          %method %disp %save %apply %prim %restore
          %const %nothing %true %false %one %two
          %neg1 %nop))

;;; ---------------------------------------------------------------------
;;; running
;;; ---------------------------------------------------------------------


;;; Notes:
;;; image:   generally, an image will be vm state deserialized from nonvolatile storage
;;; optable: the table of machine operations for this vm
;;;          initialized after the vm is created, because the runtime ops will contain
;;;          references to the vm itself. we prebind the vm parameter in each op,
;;;          making a lambda in which the vm parameter of the op is bound to the current vm
;;; code:   the starting program code, linked at load time by replacing opcodes with
;;;         the corresponding ops from the optable; if we did this statically, then
;;;         the vm wouldn't need an optable past startup, but then it wouldn't be able
;;;         to dynamically load code
;;; pc:     the index of the next instruction to exec
;;; instr:  the instruction currently being executed
;;; stack:  vm state is saved on the stack when calling a function, so that the
;;;         pre-call state can be restored on the function's return
;;; values: on function start, the inputs to the function; on function return, the outputs
;;;         from the function
;;; env     the current lexical environment
;;; globals the vm's global variables

(define (make-vm image)              
  (let ((vm (%private-make-vm
             #f                               ; optable
             (link (image-toplevel image) vm) ; code
             0                                ; pc 
             #f                               ; instr
             '()                              ; stack
             '()                              ; values
             (image-env image)       
             (image-globals image))))
    (vm-optable-set! (make-optable vm))
    vm))

