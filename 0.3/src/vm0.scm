;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard VM
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define (make-vm)
  (let* ((+undefined+ (gensym))
         (fn toplevel)
         (code #f)
         (pc 0)
         (instr #f)
         (env '())
         (globals (make-table test: eq?))
         (stack '()))
    (letrec (;; vm builtins
             (%%push! (lambda (v) (set! stack (cons v stack))))
             (%%pop! (lambda ()
                       (let ((v (car stack)))
                         (set! stack (cdr stack))
                         v)))
             ;; vm ops
             (_HALT (lambda () ))
             (_CONST (lambda (k) (%%push! k)))
             (_LREF (lambda (i j) (%%push! (vector-ref (list-ref env i) j))))
             (_LSET (lambda (i j v)
                      (begin
                        (vector-set! (list-ref env i) j v)
                        (%%push! v))))
             (_GREF (lambda (gv)(%%push! (table-ref globals gv +undefined+))))
             (_GSET (lambda (gv v)
                      (begin
                        (table-ref globals gv +undefined+)
                        (%%push! v))))
             (_FN (lambda (param-list code fenv)(%%push! (%make-method param-list code fenv))))
             (_PRIM (lambda (p args)(%%push! (apply p args))))
             (_JUMP (lambda (dst) (set! pc dst)))
             (_FJUMP (lambda (dst) 
                       (if (%false? (%%pop!))
                           (set! pc dst))))
             (_TJUMP (lambda (dst) 
                       (if (%true? (%%pop!))
                           (set! pc dst))))
             (_SAVE (lambda (dst)(%%push! (%make-return-record dst code env stack))))
             (_RETURN (lambda ()
                        (let ((val (%%pop!))
                              (ret (%%pop!)))
                          (set! pc (%return-record-pc ret))
                          (set! code (%return-record-code ret))
                          (set! env (%return-record-env ret))
                          (set! stack (%return-record-stack ret))
                          (%%push! val))))
             (_CALL (lambda (nargs)
                      (let* ((f (%pop!)))
                        (set! fn f)
                        (set! pc 0)
                        (set! code (%function-code f))
                        (set! env (%merge-env env (%function-env f))))))
             (optable (vector _HALT _CONST _LREF _LSET _GREF _GSET
                              _FN _PRIM _JUMP _FJUMP _TJUMP _SAVE _RETURN _CALL))
             ;; vm control api
             (%%load! (lambda (fn)
                        (let ((f (%load-function fn optable)))
                          (set! fn f)
                          (set! code (%function-code f)))))
             (%%exec! (lambda () 
                        (let ((op (car instr))
                              (args (cdr instr)))
                          (apply op args))))
             (%%state (lambda () (vector fn code pc instr env globals stack)))
             (%%step (lambda () 
                       (set! instr (vector-ref code pc))
                       (set! pc (+ 1 pc))
                       (%%exec!)))
             (%%run (lambda ()(let loop ()(%%step)(loop))))
             (%%print (lambda () (%print-vm fn code pc instr env globals stack))))

      (vector 
       %%state
       %%step
       %%run
       %%load!
       %%print))))

