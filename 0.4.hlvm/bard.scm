;;; ---------------------------------------------------------------------
;;; bard
;;; a simple compiler and virtual machine for a lisp-lke language
;;; copyright 2015 by mikel evins
;;; ---------------------------------------------------------------------

(define *version* "0.4.0")

;;; ---------------------------------------------------------------------
;;; utilities
;;; ---------------------------------------------------------------------

(define first car)
(define second cadr)
(define (third x)(list-ref x 2))
(define (fourth x)(list-ref x 3))
(define rest cdr)
(define top car)

(define (vector-for-each proc vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (< i len)
          (let ((x (vector-ref vec i)))
            (proc x)
            (loop (+ i 1)))
          (values)))))

(define (find thing ls #!optional (test equal?))
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (test thing (car items))
            (car items)
            (loop (cdr items))))))

(define (position thing ls #!optional (test equal?))
  (let loop ((items ls)
             (i 0))
    (if (null? items)
        #f
        (if (test thing (car items))
            i
            (loop (cdr items)
                  (+ i 1))))))

;;; ---------------------------------------------------------------------
;;; instruction manipulations
;;; ---------------------------------------------------------------------

(define (label? instr)
  (not (list? instr)))

(define (opcode instr)
  (if (label? instr)
      'label
      (first instr)))

(define (args instr)
  (if (list? instr)
      (rest instr)
      '()))

(define (arg1 instr)
  (if (list? instr)
      (second instr)
      '()))

(define (arg1-set! instr val)
  (set-car! (cdr instr) val))

(define (arg2 instr)
  (if (list? instr)
      (third instr)
      '()))

(define (arg3 instr)
  (if (list? instr)
      (fourth instr)
      '()))

(define (is instr op)
  (if (list? op)
      (member (opcode instr) op)
      (eq? (opcode instr) op)))

;;; ---------------------------------------------------------------------
;;; fn
;;; ---------------------------------------------------------------------

(define-structure fn
  code env)

(define (show-fn f)
  (if (not (fn? f))
      (display f)
      (begin (newline)
             (vector-for-each (lambda (instr)
                                (display instr)
                                (newline))
                              (fn-code f)))))

;;; ---------------------------------------------------------------------
;;; assembler
;;; ---------------------------------------------------------------------

(define (assemble f)
  (receive (len labels)
           (asm-first-pass (fn-code f))
           (fn-code-set! f
                         (asm-second-pass (fn-code f)
                                          len labels))
           f))

(define (asm-first-pass code)
  (let ((len 0)
        (labels '()))
    (for-each (lambda (instr)
                (if (label? instr)
                    (set! labels (cons (cons instr len) labels))
                    (set! len (+ 1 len))))
              code)
    (values len labels)))

(define (asm-second-pass code len labels)
  (let ((addr 0)
        (code-vector (make-vector len)))
    (for-each (lambda (instr)
                (if (label? instr)
                    #f
                    (begin (if (is instr '(JUMP TJUMP FJUMP SAVE))
                               (arg1-set! instr (cdr (assoc (arg1 instr) labels))))
                           (vector-set! code-vector addr instr)
                           (set! addr (+ 1 addr)))))
              code)
    code-vector))


;;; ---------------------------------------------------------------------
;;; function returns
;;; ---------------------------------------------------------------------

(define-structure ret-addr fn pc env)

;;; ---------------------------------------------------------------------
;;; environments
;;; ---------------------------------------------------------------------

(define (in-env? symbol env)
  (let ((frame (find symbol env find)))
    (if frame
        (list (position frame env eq?)
              (position symbol frame eq?))
        #f)))

;;; ---------------------------------------------------------------------
;;; macros
;;; ---------------------------------------------------------------------

(define (bard-macro? sym)
  (error "bard-macro? not yet implemented"))

(define (bard-macro-expand x)
  (error "bard-macro-expand not yet implemented"))

;;; ---------------------------------------------------------------------
;;; compiler
;;; ---------------------------------------------------------------------

(define (gen opcode . args)
  (list (cons opcode args)))

(define (gen-var var env)
  (let ((p (in-env? var env)))
    (if p
        (gen 'LREF (first p) (second p) ";" var)
        (gen 'GREF var))))

(define (seq . code)
  (apply append code))

(define (comp-const x val? more?)
  (if val?
      (seq (case x
             ((#t) (gen 'TRUE))
             ((#f) (gen 'FALSE))
             ((()) (gen 'NOTHING))
             ((-1) (gen 'MINUSONE))
             ((0) (gen 'ZERO))
             ((1) (gen 'ONE))
             ((2) (gen 'TWO))
             (else (gen 'CONST x)))
           (if (not more?) (gen 'RETURN)))
      '()))

(define (comp-var x env val? more?)
  (if val?
      (seq (gen-var x env)
           (if more?
               '()
               (gen 'RETURN)))
      '()))

(define (comp x env val? more?)
  (cond
   ((member x '(#t #f ())) (comp-const x val? more?))
   ((symbol? x) (comp-var x env val? more?))
   ((not (list? x)) (comp-const x val? more?))
   ((bard-macro? (first x)) (comp (bard-macro-expand x) env val? more?))
   ((case (first x)
      ((quote)  (begin (arg-count x 1)
                       (comp-const (second x) val? more?)))
      ((begin)  (comp-begin (rest x) env val? more?))
      ((set!)   (begin (arg-count x 2)
                       (if (not (symbol? (second x)))
                           (error "Only symbols can be set!, not " (second x)))
                       (seq (comp (third x) env #t #t)
                            (gen-set (second x) env)
                            (if (not val?) (gen 'POP))
                            (if (not more?) (gen 'RETURN)))))
      ((if)     (begin (arg-count x 2 3)
                       (comp-if (second x) (third x) (fourth x)
                                env val? more?)))
      ((^) (if val?
               (let ((f (comp-lambda (second x) (rest2 x) env)))
                 (seq (gen 'FN f) (unless more? (gen 'RETURN))))))
      (else      (comp-funcall (first x) (rest x) env val? more?))))))
