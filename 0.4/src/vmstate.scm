;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vmstate.scm
;;;; Project:       Bard
;;;; Purpose:       representation of the bard vm state (i.e. registers)
;;;; Author:        mikel evins
;;;; Copyright:     2013 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (inline)
         (inline-primitives))

;;; ----------------------------------------------------------------------
;;; vmstate structure
;;; ----------------------------------------------------------------------

(define-structure vmstate function pc nvals stack env globals haltfn)

;;; acessing and updating state

(define (vmstate-incpc! s)
  (vmstate-pc-set! s (1+ (vmstate-pc s))))

(define (vmstate-push! s v)
  (vmstate-stack-set! s (cons v (vmstate-stack s)))
  (vmstate-nvals-set! s (+ 1 (vmstate-nvals s))))

(define (vmstate-pushvals! s vals)
  (vmstate-stack-set! s (append vals (vmstate-stack s)))
  (vmstate-nvals-set! s (+ (length vals) (vmstate-nvals s))))

(define (vmstate-top s)
  (car (vmstate-stack s)))

(define (vmstate-pop! s)
  (let ((old-stack (vmstate-stack s)))
    (vmstate-stack-set! s (cdr old-stack))
    (vmstate-nvals-set! s (- (vmstate-nvals s) 1))
    (car old-stack)))

(define (vmstate-popn! s n)
  (let* ((stack (vmstate-stack s))
         (vals (take n stack))
         (stack* (drop n stack)))
    (vmstate-stack-set! s stack*)
    (vmstate-nvals-set! s (length stack*))
    vals))

(define (vmstate-pop-bottom! s)
  (let* ((stack (vmstate-stack s))
         (val (last stack))
         (stack* (butlast stack)))
    (vmstate-stack-set! state stack*)
    (vmstate-nvals-set! s (- (vmstate-nvals s) 1))
    val))

(define (vmstate-gref s var)
  (table-ref (vmstate-globals s) var +absent+))

(define (vmstate-gset! s var val)
  (table-set! (vmstate-globals s) var val))

(define (vmstate-lref s var)
  (let ((entry (assq var (vmstate-env s))))
    (or entry +absent+)))

(define (vmstate-lset! s var val)
  (let ((entry (assq (vmstate-env s) var)))
    (if entry
        (begin
          (set-cdr! entry val)
          val)
        (let ((entry (cons var val)))
          (vmstate-env-set! s (cons entry (vmstate-env s)))
          val))))

(define (make-parameters-environment params rest-param args)
  (let ((required-count (length params))
        (found-count (length args)))
    (cond
     ((< found-count required-count) 
      (let ((msg (string-append "not enough arguments to function; expected "
                                (number->string required-count)
                                "; found "
                                (number->string found-count))))
        (error msg args)))
     ((= found-count required-count) (map cons params args))
     (else (if rest-param
               (let* ((required (take required-count args))
                      (more (drop required-count args)))
                 (append (map params required)
                         (list (cons rest-param more))))
               (let ((msg (string-append "too many arguments to function; expected "
                                         (number->string required-count)
                                         "; found "
                                         (number->string found-count))))
                 (error msg args)))))))


(define (vmstate-apply! state)
#f)

(define (vmstate-return! state rr)
  #f)

(define (vmstate-setcc! state cc)
  #f)

