;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          make.scm
;;;; Project:       bard
;;;; Purpose:       the generic constructor
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define make:$init-functions (make-table))

(define (make:define-init-function proto initfun)
  (table-set! make:$init-functions proto initfun))

(define (make:init-function proto)
  (or (table-ref make:$init-functions
                 proto #f)
      (lambda (proto initframe)
        (error "No initializer defined for prototype" proto))))

(define make:%make-undefined
  (lambda (proto initframe)
    (bard:undefined)))

(define make:%make-nothing
  (lambda (proto initframe)
    (bard:nothing)))

(define make:%make-boolean
  (lambda (proto initframe)
    (let ((val (frame:get initframe value:)))
      (if (bard:defined? val)
          val
          (error "You must specify true or false when making a boolean value")))))

(define make:%make-number
  (lambda (proto initframe)
    (let ((val (frame:get initframe value:)))
      (if (bard:defined? val)
          val
          (error "You must specify a numeric value when making a number")))))

(define make:%make-text
  (lambda (proto initframe)
    (let ((val (frame:get initframe value:)))
      (if (bard:defined? val)
          (cond
           ((string? val) val)
           (else (error "Invalid initial value for text" val)))
          (error "You must specify some text when making a text value")))))

(define make:%make-sequence
  (lambda (proto initframe)
    (let ((elts (frame:get initframe elements:)))
      (if (bard:defined? elts)
          (cond
           ((string? elts) (apply seq:sequence (string->list elts)))
           ((list? elts) (apply seq:sequence elts))
           ((vector? elts) (apply seq:sequence (vector->list elts)))
           (else (error "Invalid initial elements for sequence" val)))
          (error "You must specify some the elements when making a sequence value")))))

(define make:%make-frame
  (lambda (proto initframe)
    (error "make:%make-frame not yet implemented")))

(define make:%make-process
  (lambda (proto initframe)
    (error "make:%make-process not yet implemented")))

(make:define-init-function <undefined> make:%make-undefined)
(make:define-init-function <nothing> make:%make-nothing)
(make:define-init-function <boolean> make:%make-boolean)
(make:define-init-function <number> make:%make-number)
(make:define-init-function <text> make:%make-text)
(make:define-init-function <sequence> make:%make-sequence)
(make:define-init-function <frame> make:%make-frame)
(make:define-init-function <process> make:%make-process)

(define (make proto . inits)
  (let* ((initframe (frame:plist->frame inits))
         (initfun (make:init-function proto)))
    (initfun proto initframe)))