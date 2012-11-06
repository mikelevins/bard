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
;;; lexical variables
;;; ---------------------------------------------------------------------

(define lvar-value car)
(define lvar-name cadr)
(define (set-lvar-name! lvar nm)(set-car! (cdr lvar) nm))
(define lvar-setter cddr)
(define (lvar-mutable? lvar)(and (lvar-setter lvar) #t))

(define (make-lvar var val mutable?)
  (let* ((lvar `(,val . (,var . #f))))
    (if mutable?
        (set-cdr! (cdr lvar)
                  (lambda (x)(set-car! lvar x))))
    lvar))

(define (lvar-set! lvar val)
  (let ((setter (lvar-setter lvar)))
    (or (and setter (setter val))
        (error (str "Tried to set an immutable variable: " (lvar-name lvar))))))

(define (lsetter lvar)
  (or (lvar-setter lvar)
      (lambda (val)(error (str "Tried to set an immutable variable: " (lvar-name lvar))))))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $lv1 (make-lvar 'x 0 #f))
;;; (lvar-value $lv1)
;;; (lvar-set! $lv1 1)
;;; (lsetter $lv1)
;;; ((lsetter $lv1) 1)
;;; (define $lv2 (make-lvar 'y 0 #t))
;;; (lvar-value $lv2)
;;; (lvar-set! $lv2 1)
;;; (lsetter $lv2)
;;; ((lsetter $lv2) 2)

;;; ---------------------------------------------------------------------
;;; environment frames
;;; ---------------------------------------------------------------------

(define (make-frame lvars)
  (list->vector lvars))

(define frame-ref vector-ref)
(define frame-set! vector-set!)

(define (frame-lvar-position fr vnm)
  (vector-position vnm fr 
                   test: (lambda (nm lvar)
                           (eq? nm (lvar-name lvar)))))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $fr1 (make-frame (list (make-lvar 'x 0 #f)(make-lvar 'y 1 #t))))
;;; (frame-lvar-position $fr1 'y)
;;; (frame-lvar-position $fr1 'z)

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
               (j (frame-lvar-position fr vnm)))
          (if j
              (cons i j)
              (loop (+ i 1)
                    (cdr frames)))))))

(define (lref env i j)
  (lvar-value (vector-ref (list-ref env i) j)))

(define (lset! env i j v)
  (lvar-set! (vector-ref (list-ref env i) j) v))

;;; ---------------------------------------------------------------------
;;; tests
;;;
;;; (define $env0 (null-env))
;;; (define $env1 (add-frame $env0 (make-frame (list (make-lvar 'x 0 #f)(make-lvar 'y 1 #t)))))
;;; (define $env2 (add-frame $env1 (make-frame (list (make-lvar 'a 10 #f)(make-lvar 'b 11 #t)))))
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
