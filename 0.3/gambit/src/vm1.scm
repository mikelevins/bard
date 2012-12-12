;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       bard virtual machine
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; prelude
;;; ---------------------------------------------------------------------

(define-macro (^ params . body)
  `(lambda ,params ,@body))

(define-macro (unless test . body)
  `(if (not ,test)
       (begin
         ,@body)))

(define-macro (when test . body)
  `(if ,test
       (begin
         ,@body)))

;;; ---------------------------------------------------------------------
;;; vmstate
;;; ---------------------------------------------------------------------

(define (%makevmstate fn env globals exitfn)
  (let* ((code (%fn-code fn))
         (pc 0)
         (vstack '())
         (cstack '()))
    (vector code pc fn env globals vstack cstack exitfn)))

(define (%make-saved-vmstate vmstate)(vector-copy vmstate))
(define (%copy-vmstate! src dest)(subvector-move! src 0 (vector-length src) dest 0))

;;; ---------------------------------------------------------------------
;;; vmstate accessors
;;; ---------------------------------------------------------------------

(define (%code vmstate)(vector-ref vmstate 0))
(define (%pc vmstate)(vector-ref vmstate 1))
(define (%fn vmstate)(vector-ref vmstate 2))
(define (%env vmstate)(vector-ref vmstate 3))
(define (%globals vmstate)(vector-ref vmstate 4))
(define (%vstack vmstate)(vector-ref vmstate 5))
(define (%cstack vmstate)(vector-ref vmstate 6))
(define (%exit vmstate)(vector-ref vmstate 7))

(define (%setcode! vmstate v)(vector-set! vmstate 0 v))
(define (%setpc! vmstate v)(vector-set! vmstate 1 v))
(define (%setfn! vmstate v)(vector-set! vmstate 2 v))
(define (%setenv! vmstate v)(vector-set! vmstate 3 v))
(define (%setglobals! vmstate v)(vector-set! vmstate 4 v))
(define (%setvstack! vmstate v)(vector-set! vmstate 5 v))
(define (%setcstack! vmstate v)(vector-set! vmstate 6 v))
(define (%setexit! vmstate v)(vector-set! vmstate 7 v))

;;; ---------------------------------------------------------------------
;;; vm ops
;;; ---------------------------------------------------------------------

;;; increment pc
(define (%incvm! state) #f)

;;; value-stack ops
(define (%pushv! state v) #f)
(define (%popv! state) #f)
(define (%topv state) #f)

;;; control-stack ops
(define (%pushc! state c) #f)
(define (%popc! state) #f)
(define (%topc state) #f)

;;; env ops
(define (%pushenv! state frame) #f)
(define (%popenv! state) #f)
(define (%topenv state) #f)

;;; ---------------------------------------------------------------------
;;; the vm
;;; ---------------------------------------------------------------------

(define (%makevm #!key (fn #f)(env (%nullenv))(globals (%bard-globals)))
  (lambda (cmd . args)
    (call/cc 
     (lambda (exit)
       (let* ((state (%makevmstate fn env globals exit))
              (_step! #f))

         (letrec ((NOP    (^ ()     state))
                  (HALT   (^ ()     (exit state)))
                  (CONST  (^ (k)    (%pushv! state k)))
                  (CLASS  (^ (nm)   (%pushv! state (%gdef! (%globals state) nm (%make-class nm) mutable: #t))))
                  (LREF   (^ (i j)  (%pushv! state (%lref (%env state) i j))))
                  (LSET   (^ (i j)  (%lset! (%env state) i j (%topv state))))
                  (LSETR  (^ (i j)  (%pushv! state (%lsetter (%env state) i j))))
                  (GREF   (^ (g)    (%pushv! state (%gref (%env state) g))))
                  (DEF    (^ (g m?) (%pushv! state (%gdef! (%globals state) g (%popv! state) mutable: m?))))
                  (GSET   (^ (g)    (%pushv! state (%gset! (%globals state) g (%topv state)))))
                  (GSETR  (^ (g)    (%pushv! state (%gsetter (%globals state) g))))
                  (POPV   (^ ()     (%popv! state)))
                  (POPC   (^ ()     (%popc! state)))
                  (PRIM   (^ ()     (%pushv! state (%apply-prim state))))
                  (JUMP   (^ (d)    (%setpc! state d)))
                  (FJUMP  (^ (d)    (unless (%popv! state)(%setpc! state d))))
                  (TJUMP  (^ (d)    (when (%popv! state)(%setpc! state d))))
                  (CALL   (^ ()     (%pushc! state (%make-saved-vmstate state))
                                    (%setfn! state (%popv! state))
                                    (%setenv! state (%activate-method-env (%fn-env (%fn state)) (%env state)))
                                    (%setpc! state 0)))
                  (RETURN (^ ()     (let ((vstack (%vstack state)))
                                      (%copy-vmstate! (%popc! state) state)
                                      (%setvstack! state vstack)
                                      (%incvm! state))))
                  (CC     (^ ()     (error "CC not yet implemented")))
                  (SETCC  (^ ()     (error "SETCC not yet implemented")))
                  ;; (spawn fn #!key (on: 'this-process|'new-process|remote-actor))
                  (SPAWN  (^ () (%pushv! state (%spawn-actor (%popv! state)(%popv! state)))))
                  ;; (send msg actor)
                  (SEND   (^ () (%pushv! state (%send-message (%popv! state)(%popv! state)))))
                  ;; (send msg actor)
                  (RECV   (^ () (%pushv! state (%receive-next-message))))
                  (READ   (^ () (%pushv! state (bard:read (%popv! state)))))
                  (WRITE  (^ () (%pushv! state (bard:write (%popv! state)(%popv! state)))))

                  (_link       (lambda (state codevec) ))
                  (_stepfn     (lambda ()(_fetch!)(_inc!)(_exec!)))
                  (_stepshowfn (lambda ()(_fetch!)(_inc!)(_exec!)(_show!)))
                  (_run!       (lambda ()(let loop ()(step!)(loop)))))

           (case cmd
             ((load!)      (%setfn! state (_link state (car args))))
             ((showsteps!) (if (car args)(set! _step! _stepfn)(set! _step! _stepshowfn)))
             ((step)       (_step!))
             ((run)        (_run!))
             (else         (error (str "Unrecognized Bard VM command: " cmd))))))))))
