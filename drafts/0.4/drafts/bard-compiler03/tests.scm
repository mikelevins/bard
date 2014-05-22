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
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (showvm state)))

;;; program: HALT
;;; expected state:
;;;  pc: 0
;;;  stack: []
;;;  env: {}
;;;  globals: {}
;;; (test-show)

;;; test-halt
;;; ----------------------------------------------------------------------

(define (test-halt)
  (let* ((code (asm (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: HALT
;;; expected state:
;;;  pc: 1
;;;  stack: []
;;;  env: {}
;;;  globals: {}
;;; (test-halt)

;;; test-const
;;; ----------------------------------------------------------------------

(define (test-const)
  (let* ((code (asm (instruction 'CONST 5)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 5
;;;          HALT
;;; expected state:
;;;  pc: 2
;;;  stack: [5]
;;;  env: {}
;;;  globals: {}
;;; (test-const)

;;; test-pop
;;; ----------------------------------------------------------------------

(define (test-pop)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST 2)
                    (instruction 'CONST 3)
                    (instruction 'POP 1)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          CONST 2
;;;          CONST 3
;;;          POP 1
;;;          HALT
;;; expected state:
;;;  pc: 5
;;;  stack: [2 1]
;;;  env: {}
;;;  globals: {}
;;; (test-pop)

;;; test-lref
;;; ----------------------------------------------------------------------

(define (test-lref)
  (let* ((code (asm (instruction 'LREF 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '((x . 5)) (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: LREF 'x
;;;          HALT
;;; expected state:
;;;  pc: 2
;;;  stack: [5]
;;;  env: {x: 5}
;;;  globals: {}
;;; (test-lref)

;;; test-lset
;;; ----------------------------------------------------------------------

(define (test-lset)
  (let* ((code (asm (instruction 'CONST 0)
                    (instruction 'LSET 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '((x . 5)) (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 0
;;;          LSET 'x
;;;          HALT
;;; expected state:
;;;  pc: 3
;;;  stack: []
;;;  env: {x: 0}
;;;  globals: {}
;;; (test-lset)

;;; test-gref
;;; ----------------------------------------------------------------------

(define (test-gref)
  (let ((globals (default-globals)))
    (table-set! globals 'x 101)
    (let* ((code (asm (instruction 'GREF 'x)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() globals #f)))
    (vmstart state)
    (showvm state))))

;;; program: GREF 'x
;;;          HALT
;;; expected state:
;;;  pc: 2
;;;  stack: [101]
;;;  env: {}
;;;  globals: {x: 101}
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
           (function (make-fn '() #f (default-environment) code))
           (state (make-vmstate function 0 0 '() '() globals #f)))
      (vmstart state)
      (showvm state))))

;;; program: CONST 1
;;;          GSET 'x
;;;          GREF 'x
;;;          HALT
;;; expected state:
;;;  pc: 4
;;;  stack: [1]
;;;  env: {}
;;;  globals: {x: 1}
;;; (test-gset)

;;; test-go
;;; ----------------------------------------------------------------------

(define (test-go)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'GO 3)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          GO 3
;;;          CONST 2
;;;          HALT
;;; expected state:
;;;  pc: 4
;;;  stack: [1]
;;;  env: {}
;;;  globals: {}
;;; (test-go)

;;; test-tgo-true
;;; ----------------------------------------------------------------------

(define (test-tgo-true)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #t)
                    (instruction 'TGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          CONST #t
;;;          TGO 4
;;;          CONST 2
;;;          HALT
;;; expected state:
;;;  pc: 5
;;;  stack: [1]
;;;  env: {}
;;;  globals: {}
;;; (test-tgo-true)

;;; test-tgo-false
;;; ----------------------------------------------------------------------

(define (test-tgo-false)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #f)
                    (instruction 'TGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          CONST #f
;;;          TGO 4
;;;          CONST 2
;;;          HALT
;;; expected state:
;;;  pc: 5
;;;  stack: [2 1]
;;;  env: {}
;;;  globals: {}
;;; (test-tgo-false)

;;; test-fgo-true
;;; ----------------------------------------------------------------------

(define (test-fgo-true)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #t)
                    (instruction 'FGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          CONST #t
;;;          FGO 4
;;;          CONST 2
;;;          HALT
;;; expected state:
;;;  pc: 5
;;;  stack: [2 1]
;;;  env: {}
;;;  globals: {}
;;; (test-fgo-true)

;;; test-fgo-false
;;; ----------------------------------------------------------------------

(define (test-tgo-false)
  (let* ((code (asm (instruction 'CONST 1)
                    (instruction 'CONST #f)
                    (instruction 'FGO 4)
                    (instruction 'CONST 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 1
;;;          CONST #f
;;;          FGO 4
;;;          CONST 2
;;;          HALT
;;; expected state:
;;;  pc: 5
;;;  stack: [1]
;;;  env: {}
;;;  globals: {}
;;; (test-tgo-false)

;;; test-fn
;;; ----------------------------------------------------------------------

(define (test-fn)
  (let* ((code (asm (instruction 'FN '(a b) 'rest (asm (instruction 'HALT)))
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: FN '(a b) 'rest [(HALT)]
;;;          HALT
;;; expected state:
;;;  pc: 2
;;;  stack: [#<fn>]
;;;  env: {}
;;;  globals: {}
;;; (test-fn)

;;; test-prim
;;; ----------------------------------------------------------------------

(define (test-prim)
  (let* ((code (asm (instruction 'CONST 2)
                    (instruction 'CONST 3)
                    (instruction 'PRIM 'GNMUL 2)
                    (instruction 'HALT)))
         (function (make-fn '() #f (default-environment) code))
         (state (make-vmstate function 0 0 '() '() (default-globals) #f)))
    (vmstart state)
    (showvm state)))

;;; program: CONST 2
;;;          CONST 3
;;;          PRIM 'GNMUL
;;;          HALT
;;; expected state:
;;;  pc: 4
;;;  stack: [6]
;;;  env: {}
;;;  globals: {}
;;; (test-prim)

;;; test-apply
;;; ----------------------------------------------------------------------

(define (test-apply)
  (let ((globals (default-globals))
        (times (make-fn '(a b) #f (default-environment)
                        (asm (instruction 'LREF 'b)
                             (instruction 'LREF 'a)
                             (instruction 'PRIM 'GNMUL 2)
                             (instruction 'RETURN)))))
    (table-set! globals '* times)
    (let* ((code (asm (instruction 'CONST 2)
                      (instruction 'CONST 3)
                      (instruction 'GREF '*)
                      (instruction 'APPLY 2)
                      (instruction 'HALT)))
           (function (make-fn '() #f (default-environment) code))
           (state (make-vmstate function 0 0 '() '() globals #f)))
      (vmstart state)
      (showvm state))))

;;; (test-apply)

;;; test-cc
;;; ----------------------------------------------------------------------

(define (test-cc)
  #f)

;;; (test-cc)

;;; test-setcc
;;; ----------------------------------------------------------------------

(define (test-setcc)
  #f)

;;; (test-setcc)
