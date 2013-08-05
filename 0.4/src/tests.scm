;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          tests.scm
;;;; Project:       Bard
;;;; Purpose:       tests of VM subsystems
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; instruction tests
;;; ----------------------------------------------------------------------

;;; test-show
;;; ----------------------------------------------------------------------

(define (test-show)
  (let* ((code (asm (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (showvm state)))

;;; (test-show)

;;; test-halt
;;; ----------------------------------------------------------------------

(define (test-halt)
  (let* ((code (asm (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-halt)

;;; test-const
;;; ----------------------------------------------------------------------

(define (test-const)
  (let* ((code (asm (instruction 'CONST 5)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-const)

;;; test-lref
;;; ----------------------------------------------------------------------

(define (test-lref)
  (let* ((code (asm (instruction 'LREF 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '((x . 5)) (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-lref)

;;; test-lset
;;; ----------------------------------------------------------------------

(define (test-lset)
  (let* ((code (asm (instruction 'CONST 0)
                    (instruction 'LSET 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '((x . 5)) (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-lset)

;;; test-gref
;;; ----------------------------------------------------------------------

(define (test-gref)
  (let ((globals (default-globals)))
    (table-set! globals 'x 101)
    (let* ((code (asm (instruction 'GREF 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() globals #f)))
    (vmstart state)
    (showvm state))))

;;; (test-gref)

;;; test-gset
;;; ----------------------------------------------------------------------

(define (test-gset)
  (let ((globals (default-globals)))
    (table-set! globals 'x 101)
    (let* ((code (asm (instruction 'CONST 1)
                      (instruction 'GSET 'x)
                      (instruction 'GREF 'x)
                      (instruction 'HALT)))
           (function (make-fn '() #f code))
           (prog (make-program code))
           (state (make-vmstate prog function 0 0 '() '() globals #f)))
      (vmstart state)
      (showvm state))))

;;; (test-gset)

;;; test-go
;;; ----------------------------------------------------------------------

(define (test-go)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'GO 3)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-go)

;;; test-tgo-true
;;; ----------------------------------------------------------------------

(define (test-tgo-true)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #t)
                    (instruction 'TGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-tgo-true)

;;; test-tgo-false
;;; ----------------------------------------------------------------------

(define (test-tgo-false)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #f)
                    (instruction 'TGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-tgo-false)

;;; test-fn
;;; ----------------------------------------------------------------------

(define (test-fn)
  (let* ((code (asm (instruction 'FN '(a b) 'rest (asm (instruction 'HALT)))
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-fn)

;;; test-cc
;;; ----------------------------------------------------------------------

(define (test-cc)
  (let* ((code (asm (instruction 'CC)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-cc)

;;; test-setcc
;;; ----------------------------------------------------------------------

(define (test-setcc)
  (let* ((code (asm (instruction 'CONST 0)
                    (instruction 'LSET 'x)
                    
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '((k . #f)(x . 0)) (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-setcc)

;;; test-prim
;;; ----------------------------------------------------------------------

(define (test-prim)
  (let* ((code (asm (instruction 'CONST 2)
                    (instruction 'CONST 3)
                    (instruction 'PRIM 'GNMUL)
                    (instruction 'HALT)))
         (function (make-fn '() #f code))
         (prog (make-program code))
         (state (make-vmstate prog function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; (test-prim)


