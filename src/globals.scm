;;;; ***********************************************************************
;;;;
;;;; Name:          globals.scm
;;;; Project:       Bard
;;;; Purpose:       representation of bard's global variables
;;;; Author:        mikel evins
;;;; Copyright:     2016 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

(define (%make-globals)
  (make-table test: eq?))

(define $bard-global-variables #f)

(define (%init-globals . initargs)
  (set! $bard-global-variables (%make-globals))
  (map (lambda (arg)
         (%defglobal (car arg)
                     (cdr arg)))
       initargs))

(define (%init-standard-globals)
  (apply %init-globals '((PI . 3.1415926))))

(define (%defglobal var val)
  (table-set! $bard-global-variables var val)
  var)

(define (%remglobal var)
  (table-set! $bard-global-variables var)
  var)

(define (%global-value var)
  (table-ref $bard-global-variables var #!unbound))

(define (%globally-bound? varname)
  (let* ((not-found (gensym))
         (val (table-ref $bard-global-variables varname not-found)))
    (if (eq? not-found val)
        #f
        #t)))

;;; (%init-standard-globals)
;;; (%global-value 'PI)
