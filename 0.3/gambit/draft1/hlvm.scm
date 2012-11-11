;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          hlvm.scm
;;;; Project:       Bard
;;;; Purpose:       an experimental high-level vm
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; concurrency and distribution notes:

;;; each vm is single-threaded a bard process can run many vms, one
;;; thread per vm vms communnicate solely through mailboxes.  within a
;;; single process, vm threads are lightweight scheme threads; a bard
;;; process can support millions of them.

;;; a single bard process runs on a single CPU core. bard can take
;;; advantage of multiple cores by spawning additional bard processes.
;;; again, the vms running on the new process communicate with one
;;; another and with any other bard processes using mailboxes.

;;; a bard process can also communicate with remote bard processes, 
;;; again using mailboxes.

;;; within the bard language, each of these vms is represented as
;;; an actor. an actor is a function that runs concurrent with
;;; the function that spawns it, and which services a mailbox.
;;; in bard, the actor API creates these vms and their mailboxes
;;; and arranges to run them either in-process or in a spawned
;;; process.

;;; see Gambit's distr-comp example for examples of the low-level
;;; mechanisms that support this strategy.

;;; ---------------------------------------------------------------------
;;; machine parts
;;; ---------------------------------------------------------------------

;;; ---------------------------------------------------------------------
;;; stretchy vectors
;;; ---------------------------------------------------------------------
;;; used for flexible storage by vm registers and environments

(define-type stretchy-vector
  constructor: private-make-stretchy-vector
  extender: define-stretchy-vector
  (slots stretchy-vector-slots set-stretchy-vector-slots!)
  (fill-pointer stretchy-vector-fill-pointer set-stretchy-vector-fill-pointer!))

(define (make-stretchy-vector initial-size)
  (let* ((initial-capacity (* 8 (+ 1 (quotient initial-size 8))))
         (vec (make-vector initial-capacity #f)))
    (private-make-stretchy-vector vec 0)))

(define (stretchy-vector-length svec)
  (stretchy-vector-fill-pointer svec))

(define (stretchy-vector-capacity svec)
  (vector-length (stretchy-vector-slots svec)))

(define (stretchy-vector-check-range svec i)
  (if (or (< i 0)
          (>= i (stretchy-vector-length svec)))
      (error (str "Index out of range: " i))))

(define (stretchy-vector-ref svec i)
  (stretchy-vector-check-range svec i)
  (vector-ref (stretchy-vector-slots svec) i))

(define (stretchy-vector-set! svec i val)
  (stretchy-vector-check-range svec i)
  (vector-set! (stretchy-vector-slots svec) i val)
  svec)

(define (stretchy-vector-extend! svec i)
  (if (>= i (stretchy-vector-capacity svec))
      (let* ((old-slots (stretchy-vector-slots svec))
             (new-slots (make-vector (* 2 (vector-length old-slots)) #f)))
        (subvector-move! old-slots 0 (vector-length old-slots)
                         new-slots 0)
        (set-stretchy-vector-slots! svec new-slots)))
  svec)

(define (stretchy-vector-set-extend! svec i val)
  (let ((cap (stretchy-vector-capacity svec)))
    (if (>= i cap)(stretchy-vector-extend! svec i))
    (vector-set! (stretchy-vector-slots svec) i val)
    (set-stretchy-vector-fill-pointer! svec (+ i 1))
    svec))

(define (stretchy-vector-add! svec val)
  (let ((len (stretchy-vector-length svec)))
    (stretchy-vector-set-extend! svec len val)))

(define (stretchy-vector-clear! svec)
  (set-stretchy-vector-fill-pointer! svec 0)
  svec)

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $sv1 (make-stretchy-vector 0))
;;; (stretchy-vector-length $sv1)
;;; (stretchy-vector-capacity $sv1)
;;; (stretchy-vector-check-range $sv1 0)
;;; (stretchy-vector-add! $sv1 'zero)
;;; (stretchy-vector-ref $sv1 0)
;;; (stretchy-vector-set! $sv1 11 'eleven)
;;; (stretchy-vector-set-extend! $sv1 11 'eleven)
;;; (stretchy-vector-ref $sv1 11)
;;; (stretchy-vector-clear! $sv1)


;;; ---------------------------------------------------------------------
;;; register lists
;;; ---------------------------------------------------------------------
;;; using stretchy vectors to represent arbitrary numbers of machine registers

(define-stretchy-vector register-list
  constructor: private-make-register-list)

(define (make-register-list)
  (let* ((initial-capacity 8)
         (vec (make-vector initial-capacity #f)))
    (private-make-register-list vec 0)))

(define register-list-ref stretchy-vector-ref)
(define register-list-clear! stretchy-vector-clear!)

(define (register-list-set! rlist i v)
  (stretchy-vector-set-extend! rlist i v))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $rl1 (make-register-list))
;;; (register-list-set! $rl1 0 'zero)
;;; (register-list-ref $rl1 0)
;;; (register-list-set! $rl1 9 'nine)
;;; (register-list-ref $rl1 9)
;;; (register-list-clear! $rl1)

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------
;;; an environment is list of frames, so we can share common env structure
;;; a frame is a vector, so variables can be looked up in O(1) time
;;; a variable is a cons like this:
;;; (value . (name . setter-function))
;;; if the variable is immutable, the setter is #f, and fetching the 
;;; setter instead returns a function that signals an error

;;; ---------------------------------------------------------------------
;;; variables
;;; ---------------------------------------------------------------------

(define var-value car)
(define var-name cadr)
(define (set-var-name! v nm)(set-car! (cdr v) nm))
(define var-setter cddr)
(define (var-mutable? v)(and (var-setter v) #t))

(define (make-var vnm val mutable?)
  (let* ((v `(,val . (,vnm . #f))))
    (if mutable?
        (set-cdr! (cdr v)
                  (lambda (x)(set-car! v x))))
    v))

(define (var-set! v val)
  (let ((setter (var-setter v)))
    (or (and setter (setter val))
        (error (str "Tried to set an immutable variable: " (var-name v))))))

(define (lsetter v)
  (or (var-setter v)
      (lambda (val)(error (str "Tried to set an immutable variable: " (var-name v))))))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $lv1 (make-var 'x 0 #f))
;;; (var-value $lv1)
;;; (var-set! $lv1 1)
;;; (lsetter $lv1)
;;; ((lsetter $lv1) 1)
;;; (define $lv2 (make-var 'y 0 #t))
;;; (var-value $lv2)
;;; (var-set! $lv2 1)
;;; (lsetter $lv2)
;;; ((lsetter $lv2) 2)

;;; ---------------------------------------------------------------------
;;; environment frames
;;; ---------------------------------------------------------------------

(define (make-frame vars)
  (list->vector vars))

(define frame-ref vector-ref)
(define frame-set! vector-set!)

(define (frame-var-position fr vnm)
  (vector-position vnm fr 
                   test: (lambda (nm v)
                           (eq? nm (var-name v)))))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $fr1 (make-frame (list (make-var 'x 0 #f)(make-var 'y 1 #t))))
;;; (frame-var-position $fr1 'y)
;;; (frame-var-position $fr1 'z)

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (null-env) '())
(define (add-frame env fr)(cons fr env))
(define (drop-frame env)(cdr env))

(define (find-var-in-env vnm env)
  (let loop ((i 0)
             (frames env))
    (if (null? frames)
        #f
        (let* ((fr (car frames))
               (j (frame-var-position fr vnm)))
          (if j
              (cons i j)
              (loop (+ i 1)
                    (cdr frames)))))))

(define (lref env i j)
  (var-value (vector-ref (list-ref env i) j)))

(define (lset! env i j v)
  (var-set! (vector-ref (list-ref env i) j) v))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $env0 (null-env))
;;; (define $env1 (add-frame $env0 (make-frame (list (make-var 'x 0 #f)(make-var 'y 1 #t)))))
;;; (define $env2 (add-frame $env1 (make-frame (list (make-var 'a 10 #f)(make-var 'b 11 #t)))))
;;; (find-var-in-env 'b $env2)
;;; (let ((ref (find-var-in-env 'b $env2))) (lref $env2 (car ref)(cdr ref)))
;;; (let ((ref (find-var-in-env 'y $env2))) (lref $env2 (car ref)(cdr ref)))
;;; (find-var-in-env 'x $env2)
;;; (let ((ref (find-var-in-env 'x $env2))) (lref $env2 (car ref)(cdr ref)))
;;; (let ((ref (find-var-in-env 'x $env2))) (lset! $env2 (car ref)(cdr ref) 1001))
;;; (let ((ref (find-var-in-env 'x $env2))) (lref $env2 (car ref)(cdr ref)))
;;; (let ((ref (find-var-in-env 'y $env2))) (lset! $env2 (car ref)(cdr ref) 1001))
;;; (let ((ref (find-var-in-env 'y $env2))) (lref $env2 (car ref)(cdr ref)))


;;; ---------------------------------------------------------------------
;;; vm globals
;;; ---------------------------------------------------------------------
;;; globals are per-vm dynamic variables lexically visible from any scope

(define (make-globals)(make-table test: eq?))

(define (gref globals vnm)(table-ref globals vnm #!unbound))
(define (gset! globals vnm val)(table-set! globals vnm val))

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------
;;; the dynamic state of a running vm

(define-type vmstate
  constructor: private-make-vmstate
  code
  pc
  instr
  fn
  vals
  env
  globals
  halt)
