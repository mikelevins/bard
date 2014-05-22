;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          prims.scm
;;;; Project:       Bard
;;;; Purpose:       primitive procedures, defined in Scheme, but bound to
;;;;                Bard variables in the initial environment
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (extended-bindings))

;;; ---------------------------------------------------------------------
;;; Protocol: Applying
;;; ---------------------------------------------------------------------

(define prim:apply
  (make-primitive
   procedure: (lambda (fn args)(%apply fn args))
   debug-name: 'apply
   required-count: 2
   restarg: #f))

(define prim:complement 
  (make-primitive
   procedure: (lambda (f)
                (make-primitive
                 procedure: (lambda args (not (%apply f args)))
                 debug-name: #f
                 required-count: 0
                 restarg: 'more))
   debug-name: 'complement
   required-count: 1
   restarg: #f))

(define (%compose-functions . fns)
  (make-primitive
   procedure: (lambda (x)
                (let loop ((fs (reverse fns))
                           (val x))
                  (if (null? fs)
                      val
                      (loop (cdr fs)
                            (%funcall (car fs) val)))))
   debug-name: #f
   required-count: 1
   restarg: #f))

(define prim:compose
  (make-primitive
   procedure: %compose-functions
   debug-name: 'compose
   required-count: 0
   restarg: 'functions))

(define prim:constantly 
  (make-primitive
   procedure:
   (lambda (c)
     (make-interpreted-method
      body: `(begin ,c)
      debug-name: #f
      formal-parameters: '(& more)
      environment: (%null-environment)
      restarg: 'more))
   debug-name: 'constantly
   required-count: 1
   restarg: #f))

(define prim:eval
  (make-primitive
   procedure: (lambda (expr #!optional (env (%null-environment)))
                (%eval expr env))
   debug-name: 'eval
   required-count: 1
   restarg: 'more))

(define prim:flip 
  (make-primitive
   procedure: (lambda (f)
                (make-primitive procedure:
                                (lambda (x y) (%apply f (list y x)))
                                debug-name: #f
                                required-count: 2
                                restarg: #f))
   debug-name: 'flip
   required-count: 1
   restarg: #f))

(define prim:identity
  (make-primitive
   procedure: (lambda (x) x)
   debug-name: 'identity
   required-count: 1
   restarg: #f))

(define prim:partial 
  (make-primitive
   procedure: (lambda (f #!rest outer-args)
                (make-primitive
                 procedure: (lambda inner-args (%apply f (append outer-args inner-args)))
                 debug-name: #f
                 required-count: 0
                 restarg: 'more))
   debug-name: 'partial
   required-count: 1
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; protocol: Calculating
;;; ---------------------------------------------------------------------

(define prim:*
  (make-primitive
   procedure: *
   debug-name: '*
   required-count: 0
   restarg: 'more))

(define prim:+
  (make-primitive
   procedure: +
   debug-name: '+
   required-count: 0
   restarg: 'more))

(define prim:-
  (make-primitive
   procedure: -
   debug-name: '-
   required-count: 0
   restarg: 'more))

(define prim:/
  (make-primitive
   procedure: /
   debug-name: '/
   required-count: 0
   restarg: 'more))

(define prim:even?
  (make-primitive
   procedure: even?
   debug-name: 'even?
   required-count: 1
   restarg: #f))

(define prim:max
  (make-primitive
   procedure: max
   debug-name: 'max
   required-count: 0
   restarg: 'more))

(define prim:min
  (make-primitive
   procedure: min
   debug-name: 'min
   required-count: 0
   restarg: 'more))

(define prim:odd?
  (make-primitive
   procedure: odd?
   debug-name: 'odd?
   required-count: 1
   restarg: #f))

(define prim:random
  (make-primitive
   procedure:
   (let ((rs (make-random-source)))
     (random-source-randomize! rs)
     (let ((ri (random-source-make-integers rs)))
       (lambda (n)(ri n))))
   debug-name: 'random
   required-count: 1
   restarg: #f))

(define prim:remainder
  (make-primitive
   procedure: remainder
   debug-name: 'remainder
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Comparing
;;; ---------------------------------------------------------------------

(define prim:=
  (make-primitive
   procedure: =
   debug-name: '=
   required-count: 0
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; protocol: Generating
;;; ---------------------------------------------------------------------

(define (bard:cycle ls)
  (let* ((items (%->list ls))
         (len (length items)))
    (%eval `(generate cache: ,#f
                      ((i 0))
                      (yield (element (list ,@items) i))
                      (resume (remainder (+ i 1) ,len)))
           '())))

(define prim:cycle
  (make-primitive
   procedure: bard:cycle
   debug-name: 'cycle
   required-count: 1
   restarg: #f))

(define (bard:iterate fn arg)
  (%eval `(generate ((out ',arg))
                    (yield out)
                    (resume (,fn out)))
         '()))

(define prim:generated-count
  (make-primitive
   procedure: (lambda (g)(length (generator-instance-results g)))
   debug-name: 'generated-
   required-count: 1
   restarg: #f))

(define prim:generated-values
  (make-primitive
   procedure: (lambda (g)(reverse (generator-instance-results g)))
   debug-name: 'generated-values
   required-count: 1
   restarg: #f))

(define prim:iterate
  (make-primitive
   procedure: bard:iterate
   debug-name: 'iterate
   required-count: 2
   restarg: #f))

(define prim:next
  (make-primitive
   procedure: (lambda (g) (next g))
   debug-name: 'next
   required-count: 1
   restarg: #f))

(define prim:next-n
  (make-primitive
   procedure: (lambda (g n)
                (let loop ((i 0)
                           (result '()))
                  (if (< i n)
                      (loop (+ i 1)(cons (next g) result))
                      (reverse result))))
   debug-name: 'next-n
   required-count: 2
   restarg: #f))

(define (bard:range-from start)
  (%eval `(generate ((i ,start)) (yield i)(resume (+ i 1))) '()))

(define prim:range-from
  (make-primitive
   procedure: bard:range-from
   debug-name: 'range-from
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Listing
;;; ---------------------------------------------------------------------

(define prim:list
  (make-primitive
   procedure: (lambda args args)
   debug-name: 'list
   required-count: 0
   restarg: 'more))

(define (%bard-reduce fn arg #!optional (more #f))
  (let ((init (if more arg (car arg)))
        (args (if more more (cdr arg))))
    (let loop ((items args)
               (result init))
      (if (null? items)
          result
          (loop (cdr items)
                (%funcall fn result (car items)))))))

(define prim:reduce
  (make-primitive
   procedure: %bard-reduce
   debug-name: 'reduce
   required-count: 2
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; map
;;; ---------------------------------------------------------------------

(define (%bard-map fn . lists)
  (let loop ((item-lists (map %->list lists))
             (result '()))
    (if (some? null? item-lists)
        (reverse result)
        (loop (map cdr item-lists)
              (cons (%apply fn (map car item-lists))
                    result)))))

(define prim:map
  (make-primitive
   procedure: (lambda (fn . args)(apply %bard-map fn args))
   debug-name: 'map
   required-count: 1
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; partition
;;; ---------------------------------------------------------------------

(define (%bard-partition . args)
  (if (null? args)
      '()
      (if (null? (cdr args))
          (car args)
          (let* ((len (length args))
                 (fns (take (- len 1) args))
                 (arg (car (drop (- len 1) args))))
            (apply values (map (lambda (f)(map (lambda (arg)(%funcall f arg)) arg))
                               fns))))))

(define prim:partition
  (make-primitive
   procedure: %bard-partition
   debug-name: 'partition
   required-count: 0
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; range
;;; ---------------------------------------------------------------------

(define (bard:range start end)
  (if (= start end)
      (list start)
      (let ((op (if (< start end) + -))
            (test (if (< start end) >= <=)))
        (let loop ((i start)
                   (result '()))
          (if (test i end)
              (reverse result)
              (loop (op i 1)
                    (cons i result)))))))

(define prim:range
  (make-primitive
   procedure: bard:range
   debug-name: 'range
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Mapping
;;; ---------------------------------------------------------------------

(define prim:get
  (make-primitive
   procedure: (lambda (tbl key)(%apply tbl key))
   debug-name: 'get
   required-count: 2
   restarg: #f))

(define (%bard-put tbl key val)
  (cond
   ((pair? tbl)(%bard-list-put-key tbl key val))
   ((string? tbl)(list->string (%bard-list-put-key (string->list tbl) key val)))
   ((alist-table-instance? tbl)(alist-table-put tbl key val))
   ((record-instance? tbl) (record-put tbl key val))
   ((tuple-instance? tbl)(tuple-put tbl key val))
   (else (%make-alist-table `((,key . ,val)(value: . ,tbl))))))

(define prim:put
  (make-primitive
   procedure: %bard-put
   debug-name: 'put
   required-count: 3
   restarg: #f))

(define prim:table
  (make-primitive
   procedure: (lambda args (%make-alist-table (plist->alist args)))
   debug-name: 'table
   required-count: 0
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; protocol: Ordering
;;; ---------------------------------------------------------------------

(define prim:<
  (make-primitive
   procedure: <
   debug-name: '<
   required-count: 0
   restarg: 'more))

(define prim:<=
  (make-primitive
   procedure: <=
   debug-name: '<=
   required-count: 0
   restarg: 'more))

(define prim:>
  (make-primitive
   procedure: >
   debug-name: '>
   required-count: 0
   restarg: 'more))

(define prim:>=
  (make-primitive
   procedure: >=
   debug-name: '>=
   required-count: 0
   restarg: 'more))

;;; ---------------------------------------------------------------------
;;; protocol: Pairing
;;; ---------------------------------------------------------------------

(define prim:pair
  (make-primitive
   procedure: cons
   debug-name: 'cons
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Reading
;;; ---------------------------------------------------------------------

(define prim:current-input
  (make-primitive
   procedure: current-input-port
   debug-name: 'current-input
   required-count: 0
   restarg: #f))

(define prim:load
  (make-primitive
   procedure: (lambda (path)(%bard-load path))
   debug-name: 'load
   required-count: 1
   restarg: #f))

(define prim:read
  (make-primitive
   procedure: (lambda (#!optional (in (current-input-port)))(bard:read in))
   debug-name: 'read
   required-count: 0
   restarg: 'more))

(define (%bard-read-file path)
  (call-with-input-file path
    (lambda (in)
      (let* ((finfo (file-info path))
             (fsize (file-info-size finfo))
             (buf (make-string fsize)))
        (read-substring buf 0 fsize in)
        buf))))

(define prim:read-file
  (make-primitive
   procedure: %bard-read-file
   debug-name: 'read-file
   required-count: 1
   restarg: #f))

(define prim:read-line
  (make-primitive
   procedure: read-line
   debug-name: 'read-line
   required-count: 1
   restarg: #f))

(define (%bard-read-lines in)
  (cond
   ((input-port? in)(read-all in read-line))
   ((string? in)(call-with-input-string in (lambda (stream)(read-all stream read-line))))
   (else (error (string-append "Invalid argument to read-lines: "
                               (object->string in))))))

(define prim:read-lines
  (make-primitive
   procedure: %bard-read-lines
   debug-name: 'read-lines
   required-count: 1
   restarg: #f))

(define prim:read-text
  (make-primitive
   procedure: (lambda (text)(bard:read-from-string text))
   debug-name: 'read-text
   required-count: 1
   restarg: #f))


;;; ---------------------------------------------------------------------
;;; Protocol: System
;;; ---------------------------------------------------------------------

(define prim:error
  (make-primitive
   procedure: (lambda (msg)
                (let ((msg (if (string? msg)
                               msg
                               (object->string msg))))
                  (display (string-append "ERROR: " msg))))
   debug-name: 'error
   required-count: 1
   restarg: #f))

(define prim:exit
  (make-primitive
   procedure: exit
   debug-name: 'exit
   required-count: 0
   restarg: #f))

(define prim:gc
  (make-primitive
   procedure: ##gc
   debug-name: 'gc
   required-count: 0
   restarg: #f))

(define prim:gensym
  (make-primitive
   procedure: (lambda ()(gensym))
   debug-name: 'gensym
   required-count: 0
   restarg: #f))

(define prim:quit
  (make-primitive
   procedure: exit
   debug-name: 'quit
   required-count: 0
   restarg: #f))

(define prim:room
  (make-primitive
   procedure: (lambda ()
                (begin
                  (gc-report-set! #t)
                  (##gc)
                  (gc-report-set! #f)))
   debug-name: 'room
   required-count: 0
   restarg: #f))

(define prim:uuid
  (make-primitive
   procedure: (lambda () (make-uuid))
   debug-name: 'uuid
   required-count: 0
   restarg: #f))

(define prim:version
  (make-primitive
   procedure: (lambda () $bard-version-string)
   debug-name: 'version
   required-count: 0
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Typing
;;; ---------------------------------------------------------------------

(define (%applicable? thing)
  (or (procedure? thing)
      (primitive-instance? thing)
      (function-instance? thing)
      (interpreted-method-instance? thing)))

(define prim:applicable?
  (make-primitive
   procedure: %applicable?
   debug-name: 'applicable?
   required-count: 1
   restarg: #f))

(define prim:boolean?
  (make-primitive 
   procedure: boolean?
   debug-name: 'boolean?
   required-count: 1
   restarg: #f))

(define prim:char?
  (make-primitive
   procedure: char?
   debug-name: 'char?
   required-count: 1
   restarg: #f))

(define (%false? x)
  (or (eqv? x #f)
      (null? x)))

(define prim:false?
  (make-primitive 
   procedure: %false?
   debug-name: 'false?
   required-count: 1
   restarg: #f))

(define prim:float?
  (make-primitive
   procedure: flonum?
   debug-name: 'float?
   required-count: 1
   restarg: #f))

(define prim:foreign-value?
  (make-primitive
   procedure: ##foreign?
   debug-name: 'foreign-value?
   required-count: 1
   restarg: #f))

(define prim:function?
  (make-primitive
   procedure: function?
   debug-name: 'function?
   required-count: 1
   restarg: #f))

(define prim:input-stream?
  (make-primitive
   procedure: input-port?
   debug-name: 'prim:input-stream?
   required-count: 1
   restarg: #f))

(define prim:integer?
  (make-primitive
   procedure: integer?
   debug-name: 'integer?
   required-count: 1
   restarg: #f))

(define prim:interpreted-method?
  (make-primitive
   procedure: interpreted-method?
   debug-name: 'interpreted-method?
   required-count: 1
   restarg: #f))

(define prim:iostream?
  (make-primitive
   procedure: (lambda (x)(or (input-port? x)(output-port? x)))
   debug-name: 'iostream?
   required-count: 1
   restarg: #f))

(define prim:list?
  (make-primitive
   procedure: list?
   debug-name: 'list?
   required-count: 1
   restarg: #f))

(define (%bard-list-protocols)
  (let ((protos '()))
    (table-for-each (lambda (k v)(set! protos (cons k protos)))
                    +protocols+)
    (sort protos (lambda (p1 p2)(string<? (symbol->string p1)(symbol->string p2))))))

(define prim:list-protocols
  (make-primitive
   procedure: %bard-list-protocols
   debug-name: 'list-protocols
   required-count: 0
   restarg: #f))

(define prim:output-stream?
  (make-primitive
   procedure: output-port?
   debug-name: 'output-stream?
   required-count: 1
   restarg: #f))


(define (%bard-protocols)
  (let ((protos '()))
    (table-for-each (lambda (k v)(set! protos (cons v protos)))
                    +protocols+)
    protos))

(define prim:protocols
  (make-primitive
   procedure: %bard-protocols
   debug-name: 'protocols
   required-count: 0
   restarg: #f))

(define prim:singleton
  (make-primitive
   procedure: %singleton
   debug-name: 'singleton
   required-count: 1
   restarg: #f))

(define (%true? x)
  (and (not (%false? x))
       (not (eqv? x #!unbound))))

(define prim:true?
  (make-primitive 
   procedure: %true?
   debug-name: 'true?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; protocol: Writing
;;; ---------------------------------------------------------------------

(define prim:current-output
  (make-primitive
   procedure: current-output-port
   debug-name: 'current-output
   required-count: 0
   restarg: #f))

(define prim:display
  (make-primitive
   procedure: display
   debug-name: 'display
   required-count: 1
   restarg: 'more))

(define prim:newline
  (make-primitive
   procedure: (lambda ()(newline))
   debug-name: 'newline
   required-count: 0
   restarg: 'more))

(define prim:print
  (make-primitive
   procedure: (lambda (thing #!optional (out (current-output-port)))
                (%print thing out))
   debug-name: 'print
   required-count: 1
   restarg: 'more))

(define prim:show
  (make-primitive
   procedure: (lambda (thing)(%show thing))
   debug-name: 'show
   required-count: 1
   restarg: #f))

(define prim:vector
  (make-primitive
   procedure: vector
   debug-name: 'vector
   required-count: 0
   restarg: 'more))

(define (%bard-write data out)
  (let ((data (if (string? data)
                  data
                  (%as-string data))))
    (cond
     ((output-port? out)(write-substring data 0 (string-length data) out))
     ((string? out)(call-with-output-file out 
                     (lambda (out)
                       (write-substring data 0 (string-length data) out))))
     (else (error (string-append "Invalid output argument to write: "
                                 (object->string out)))))))

(define prim:write
  (make-primitive
   procedure: %bard-write
   debug-name: 'write
   required-count: 2
   restarg: #f))
