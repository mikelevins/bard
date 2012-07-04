;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          vm.scm
;;;; Project:       Bard
;;;; Purpose:       the Bard vm -- experimental variation 
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define-macro (-> args . funs)
  (let ((gen-vars (lambda (n)
                    (let loop ((i 0)
                               (result '()))
                      (if (>= i n)
                          result
                          (loop (+ i 1)(cons (gensym) result)))))))
    (if (null? funs)
        `(values ,@args)
        (let ((f (car funs))
              (more-funs (cdr funs))
              (vars (gen-vars (length args))))
          (if (null? more-funs)
              `(receive ,vars (,f ,@args)(values ,@vars))
              `(receive ,vars (,f ,@args)
                        (-> ,vars ,@more-funs)))))))

(define-macro (%asm1 form)
  `(vector ,(car form) ,@(cdr form)))

(define-macro (%asm forms)
  (cons 'vector
        (map (lambda (f)`(%asm1 ,f))
             forms)))

;;; ---------------------------------------------------------------------
;;; multiple return values
;;; ---------------------------------------------------------------------

(define val0 (lambda (v . rest) v))
(define val1 (lambda (v w . rest) w))
(define val2 (lambda (v w x . rest) x))
(define val3 (lambda (v w x y . rest) y))
(define valn (lambda vals (list-ref vals n)))
(define vals (lambda vals vals))

;;; ---------------------------------------------------------------------
;;; ops
;;; ---------------------------------------------------------------------

(define (opHALT halt? pc env code instr stack)
  (-> (#t pc env code instr stack)))

(define (opNIL halt? pc env code instr stack)
  (-> (halt? pc env code instr (cons (delay (-> ('()))) stack))))

(define (opTRUE halt? pc env code instr stack)
  (-> (halt? pc env code instr (cons (delay (-> (#t))) stack))))

(define (opFALSE halt? pc env code instr stack)
  (-> (halt? pc env code instr (cons (delay (-> (#f))) stack))))

(define (opCONST halt? pc env code instr stack)
  (let ((c (instruction:arg 1 instr)))
    (-> (halt? pc env code instr (cons (delay (-> (c))) stack)))))

(define (opJUMP halt? pc env code instr stack)
  (let ((dest (instruction:arg 1 instr)))
    (-> (halt? dest env code instr stack))))

(define (opFJUMP halt? pc env code instr stack)
  (let ((dest (instruction:arg 1 instr)))
    (if (false? (-> (force (car stack)) val0))
        (-> (halt? dest env code (cdr stack)))
        (-> (halt? pc env code instr (cdr stack))))))

(define (opTJUMP halt? pc env code instr stack)
  (let ((dest (instruction:arg 1 instr)))
    (if (false? (-> (force (car stack)) val0))
        (-> (halt? dest env code instr (cdr stack)))
        (-> (halt? pc env code instr (cdr stack))))))

(define (opRETURN halt? pc env code instr stack)
  (let ((vals (car stack))
        (ret (cadr stack)))
    (receive (h p e c i s)(force ret)
             (-> (h p e c i (cons vals s))))))

(define (opSAVE halt? pc env code instr stack)
  (let* ((dest (instruction:arg 1 instr))
         (frame (delay (-> (halt? dest env code instr stack)))))
    (-> (halt? pc env code instr (cons frame stack)))))

;;; ---------------------------------------------------------------------
;;; instructions
;;; ---------------------------------------------------------------------

(define $vm-operations (make-vector 255))
(define $vm-operation-names (make-vector 255))

(define-macro (defop num name . op-args)
  `(begin
     (define ,name ,num)
     (vector-set! $vm-operation-names ,num ',name)
     (vector-set! $vm-operations ,num ,(string->symbol (string-append "op" (symbol->string name))))))

(defop 0 HALT)
(defop 1 NIL)
(defop 2 TRUE)
(defop 3 FALSE)
(defop 4 CONST c)
(defop 5 JUMP dest)
(defop 6 FJUMP dest)
(defop 7 TJUMP dest)
(defop 8 RETURN)
(defop 9 SAVE dest)

(define (instr . args)(list->vector args))
(define (instruction:op inst)
  (vector-ref $vm-operations
              (vector-ref inst 0)))

(define (instruction:arg n inst)
  (vector-ref inst n))

(define (code-ref c n)
  (vector-ref c n))

;;; ---------------------------------------------------------------------
;;; execution
;;; ---------------------------------------------------------------------

(define (fetch halt? pc env code instr stack)
  (-> (halt? (+ 1 pc) env code (code-ref code pc) stack)))

(define (exec halt? pc env code instr stack)
  ((instruction:op instr) halt? pc env code instr stack))

(define (vm:step halt? pc env code instr stack)
  (-> (halt? pc env code instr stack)
      fetch exec))

(define (vm:run halt? pc env code instr stack)
  (if halt?
      (-> (halt? pc env code instr stack))
      (-> (halt? pc env code instr stack)
          vm:step vm:run)))

(define (vm program)
  (receive (halt? pc env code instr stack)
           (vm:run #f 0 '() program #f '())
           (if (null? stack)
               (values)
               (force (car stack)))))

;;; (vm (%asm ((HALT))))
;;; (vm (%asm ((NIL)(HALT))))
;;; (vm (%asm ((TRUE)(HALT))))
;;; (vm (%asm ((FALSE)(HALT))))
;;; (vm (%asm ((CONST 5)(HALT))))
;;; (vm (%asm ((CONST 5)(JUMP 3)(CONST 4)(HALT))))

