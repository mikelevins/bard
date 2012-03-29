;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          applicable.scm
;;;; Project:       Bard
;;;; Purpose:       applicable types (including lists and text)
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "function-macros.scm")
(##include "type-macros.scm")

;;; ---------------------------------------------------------------------
;;; cons
;;; ---------------------------------------------------------------------

(define bard:cons? pair?)

;;; ---------------------------------------------------------------------
;;; text
;;; ---------------------------------------------------------------------

(define bard:text? string?)

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------

(define-type bard:frame
  id: 08C172EF-8046-4ADC-BC26-86E4244C9F5A
  constructor: bard:%make-frame
  (slots bard:%frame-slots bard:%set-frame-slots!))

(define (bard:make-frame key-val-plist)
  (let loop ((kvs key-val-plist)
             (slots '()))
    (if (null? kvs)
        (bard:%make-frame slots)
        (if (null? (cdr kvs))
            (error "malformed inputs to make-frame" key-val-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define (bard:frame . kvs)
  (bard:make-frame kvs))

(bard:define-structure-type <frame> bard:frame?)
(bard:define-method bard:type ((thing <frame>))(%object->bard-type thing))

(define (%frame-add-slot fr key val)
  (bard:%make-frame (cons (cons key val) 
                          (bard:%frame-slots fr))))

(define (%frame-add-slots fr . kv-plist)
  (let loop ((kvs kv-plist)
             (slots (bard:%frame-slots fr)))
    (if (null? kvs)
        (bard:%make-frame slots)
        (if (null? (cdr kvs))
            (error "odd number of arguments" kv-plist)
            (let ((k (car kvs))
                  (v (cadr kvs))
                  (more (cddr kvs)))
              (loop more (cons (cons k v) slots)))))))

(define $frame-for-nonframe-value-table (make-table test: equal?))

(define (%ensure-frame-for-nonframe-value! tp . kv-plist)
  (let ((fr (table-ref $frame-for-nonframe-value-table tp #f)))
    (or fr
        (let* ((kvs `(type: ,tp ,@kv-plist))
               (fr (apply bard:frame kvs)))
          (table-set! $frame-for-nonframe-value-table tp fr)
          fr))))

(bard:define-function %frame-for-nonframe-value (val))

(bard:define-method %frame-for-nonframe-value ((val Anything)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <null>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <null>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <null>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <character>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <character>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <boolean>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <boolean>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <symbol>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <symbol>)
                                      value: val
                                      name: (symbol->string val)))

(bard:define-method %frame-for-nonframe-value ((val <keyword>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <keyword>)
                                      value: val
                                      name: (keyword->string val)))

(bard:define-method %frame-for-nonframe-value ((val <flonum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <flonum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <ratio>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <ratio>)
                                      value: val
                                      numerator: (numerator val)
                                      denominator: (denominator val)))

(bard:define-method %frame-for-nonframe-value ((val <fixnum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <fixnum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <bignum>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <bignum>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <closure>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <closure>)
                                      value: val
                                      closure-type: (cond
                                                     ((%method? val) 'method)
                                                     ((%function? val) 'function)
                                                     (else 'closure))))


(bard:define-method %frame-for-nonframe-value ((val <cons>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <cons>)
                                      value: val))

(bard:define-method %frame-for-nonframe-value ((val <text>)) 
                    (%frame-add-slots (%ensure-frame-for-nonframe-value! <text>)
                                      value: val))

;;; ---------------------------------------------------------------------
;;; frame API
;;; ---------------------------------------------------------------------

(bard:define-function bard:keys (frame))
(bard:define-method bard:keys ((frame Anything))(bard:keys (%frame-for-nonframe-value frame)))
(bard:define-method bard:keys ((frame <frame>))(map car (bard:%frame-slots frame)))

(define (%frame-lookup-entry frame key)
  (assoc key (bard:%frame-slots frame)))

(define (%frame-get-key frame key)
  (let ((entry (%frame-lookup-entry frame key)))
    (if entry (cdr entry) bard:nothing)))

(bard:define-function bard:get (frame key))

(bard:define-method bard:get ((frame Anything)(key Anything)) (bard:get (%frame-for-nonframe-value frame) key))
(bard:define-method bard:get ((frame <frame>)(key <undefined>))(error "can't use undefined as a frame key"))
(bard:define-method bard:get ((frame <frame>)(key <null>))(error "can't use nothing as a frame key"))
(bard:define-method bard:get ((frame <frame>)(key Anything))(%frame-get-key frame key))

(define (%frame-set-key! frame key val)
  (let ((entry (%frame-lookup-entry frame key)))
    (if entry
        (begin
          (set-cdr! entry val)
          frame)
        (begin
          (bard:%set-frame-slots! frame (cons (cons key val) (bard:%frame-slots frame)))
          frame))))

(bard:define-function bard:put! (frame key val))
(bard:define-method bard:put! ((frame Anything)(key Anything)(val Anything)) (error "frames of that type are read-only" val))
(bard:define-method bard:put! ((frame <frame>)(key Anything)(val Anything)) (%frame-set-key! frame key val))

;;; ForeignFrame <- <ObjCFrame>

;;; ---------------------------------------------------------------------
;;; function
;;; ---------------------------------------------------------------------

(define bard:function? %function?)

;;; ---------------------------------------------------------------------
;;; method
;;; ---------------------------------------------------------------------

(define bard:method? %method?)

;;; ---------------------------------------------------------------------
;;; List protocol
;;; ---------------------------------------------------------------------

;;;(define bard:list? (%make-function name: 'list? signature: '(thingg)))

(bard:define-function bard:list? (thing))
(bard:define-method bard:list? ((thing Anything)) #f)
(bard:define-method bard:list? ((thing <null>)) #t)
(bard:define-method bard:list? ((thing <cons>)) #t)
(bard:define-method bard:list? ((thing <text>)) #t)

(define bard:list list)

(bard:define-function bard:empty? (ls))
(bard:define-method bard:empty? ((thing <null>)) #t)
(bard:define-method bard:empty? ((thing <cons>)) #f)
(bard:define-method bard:empty? ((thing <text>))
                    (<= (string-length thing) 0))

(bard:define-function bard:length (ls))
(bard:define-method bard:length ((thing <null>)) 0)
(bard:define-method bard:length ((thing <cons>)) (length thing))
(bard:define-method bard:length ((thing <text>)) (string-length thing))

(bard:define-function bard:first (ls))
(bard:define-method bard:first ((thing <cons>)) (car thing))
(bard:define-method bard:first ((thing <text>)) (string-ref thing 0))

(bard:define-function bard:rest (ls))
(bard:define-method bard:rest ((thing <cons>)) (cdr thing))
(bard:define-method bard:rest ((thing <text>)) (substring thing 1 (string-length thing)))

(bard:define-function bard:last (ls))
(bard:define-method bard:last ((thing <cons>)) (list-ref thing (- (length thing) 1)))
(bard:define-method bard:last ((thing <text>)) (string-ref thing (- (string-length thing) 1)))

(bard:define-function bard:nth (ls n))
(bard:define-method bard:nth ((thing <cons>)(n <fixnum>)) (list-ref thing n))
(bard:define-method bard:nth ((thing <text>)(n <fixnum>)) (string-ref thing n))

(bard:define-function bard:second (ls))
(bard:define-method bard:second ((thing <cons>)) (list-ref thing 1))
(bard:define-method bard:second ((thing <text>)) (string-ref thing 1))

(bard:define-function bard:third (ls))
(bard:define-method bard:third ((thing <cons>)) (list-ref thing 2))
(bard:define-method bard:third ((thing <text>)) (string-ref thing 2))

(bard:define-function bard:tails (ls))
(bard:define-method bard:tails ((thing <null>)) '(()))
(bard:define-method bard:tails ((thing <cons>)) (cons thing (bard:tails (cdr thing))))
(bard:define-method bard:tails ((thing <text>)) 
                    (if (<= (string-length thing) 0)
                        '("")
                        (cons thing (bard:tails (bard:rest thing)))))

(bard:define-function bard:take (n ls))
(bard:define-method bard:take ((n <fixnum>)(thing <null>))(if (zero? n) '() (error "can't take that many elements" n)))
(bard:define-method bard:take ((count <fixnum>)(thing <cons>)) 
                    (let loop ((n count)
                               (ls thing))
                      (if (<= n 0)
                          '()
                          (if (null? ls)
                              (error "can't take that many elements" count)
                              (cons (car ls)
                                    (loop (- n 1) (cdr ls)))))))

(bard:define-method bard:take ((n <fixnum>)(thing <text>))
                    (substring thing 0 n))

(bard:define-function bard:drop (n ls))
(bard:define-method bard:drop ((n <fixnum>)(thing <null>)) (if (zero? n) '() (error "can't drop that many elements" n)))

(bard:define-method bard:drop ((count <fixnum>)(thing <cons>)) 
                    (let loop ((n count)
                               (ls thing))
                      (if (<= n 0)
                          ls
                          (loop (- n 1)(cdr ls)))))

(bard:define-method bard:drop ((n <fixnum>)(thing <text>)) 
                    (substring thing n (string-length thing)))

(bard:define-function bard:filter (pred ls))
(bard:define-method bard:filter ((pred <closure>)(thing <null>)) '())
(bard:define-method bard:filter ((pred <closure>)(thing <cons>)) 
                    (let loop ((items thing))
                      (if (null? items)
                          '()
                          (if (pred (car items))
                              (cons (car items)
                                    (loop (cdr items)))
                              (loop (cdr items))))))

(bard:define-method bard:filter ((pred <closure>)(thing <text>))
                    (list->string (bard:filter pred (string->list thing))))

(bard:define-function bard:any? (fn ls))
(bard:define-method bard:any? ((pred <closure>)(thing <null>)) bard:nothing)
(bard:define-method bard:any? ((pred <closure>)(items <cons>))
                    (if (pred (car items))
                          (car items)
                          (any? pred (cdr items))))

(bard:define-method bard:any? ((pred <closure>)(thing <text>)) 
                    (let ((len (string-length thing)))
                      (let loop ((i 0))
                        (if (>= i len)
                            bard:nothing
                            (let ((ch (string-ref thing i)))
                              (if (pred ch)
                                  ch
                                  (loop (+ i 1))))))))


(bard:define-function bard:every? (fn ls))
(bard:define-method bard:every? ((pred <closure>)(thing <null>)) (bard:true))

(bard:define-method bard:every? ((pred <closure>)(thing <cons>)) 
                    (if (pred (bard:first thing))
                        (bard:every? pred (bard:rest thing))
                        #f))

(bard:define-method bard:every? ((pred <closure>)(thing <text>)) 
                    (bard:every? pred (string->list thing)))


(bard:define-function bard:add-first (x ls))

(bard:define-method bard:add-first ((x Anything)(ls <null>)) (cons x ls))
(bard:define-method bard:add-first ((x Anything)(ls <cons>)) (cons x ls))
(bard:define-method bard:add-first ((ch <character>)(ls <text>))(string-append (string ch) ls))

(bard:define-function bard:add-last (ls x))

(bard:define-method bard:add-last ((ls <null>)(x Anything)) (append ls (list x)))
(bard:define-method bard:add-last ((ls <cons>)(x Anything)) (append ls (list x)))
(bard:define-method bard:add-last ((ls <text>)(ch <character>))(string-append ls (string ch)))

(bard:define-function bard:append (ls1 ls2))

(bard:define-method bard:append ((ls1 <null>)(ls2 <null>)) '())
(bard:define-method bard:append ((ls1 <cons>)(ls2 <null>)) ls1)
(bard:define-method bard:append ((ls1 <null>)(ls2 <cons>)) ls2)
(bard:define-method bard:append ((ls1 <cons>)(ls2 <cons>)) (append ls1 ls2))
(bard:define-method bard:append ((ls1 <text>)(ls2 <text>))(string-append ls1 ls2))

(bard:define-function bard:reverse (ls))
(bard:define-method bard:reverse ((thing <null>)) thing)
(bard:define-method bard:reverse ((thing <cons>)) (reverse thing))
(bard:define-method bard:reverse ((thing <text>)) (list->string (reverse (string->list thing))))

(define (bard:iota count start step)
  (let loop ((i 0)
             (n start)
             (result '()))
    (if (>= i count)
        (reverse result)
        (loop (+ i 1)
              (+ n step)
              (cons n result)))))

(bard:define-function bard:map (fn ls))
(bard:define-method bard:map ((fn <closure>)(thing <null>)) thing)
(bard:define-method bard:map ((fn <closure>)(thing <cons>)) (map fn thing))
(bard:define-method bard:map ((fn <closure>)(thing <text>)) (map fn (string->list thing)))

(bard:define-function bard:reduce (op init ls))
(bard:define-method bard:reduce ((op <closure>)(init Anything)(ls <null>)) init)
(bard:define-method bard:reduce ((op <closure>)(init Anything)(ls <cons>)) #f)
(bard:define-method bard:reduce ((op <closure>)(init Anything)(ls <text>)) #f)

(bard:define-function bard:member? (k ls test))
(bard:define-method bard:member? ((k Anything)(ls <null>)(test <closure>)) (bard:false))

(bard:define-method bard:member? ((k Anything)(ls <cons>)(test <closure>)) 
                    (if (test k (car ls))
                        ls
                        (bard:member? k (cdr ls) test)))

(bard:define-method bard:member? ((k <character>)(ls <text>)(test <closure>)) 
                    (let ((len (string-length ls)))
                      (if (<= len 0)
                          #f
                          (let loop ((i 0))
                            (if (>= i len)
                                #f
                                (let ((ch (string-ref ls i)))
                                  (if (test k ch)
                                      (substring ls i len)
                                      (loop (+ i 1)))))))))

(bard:define-function bard:assoc (k ls test))
(bard:define-method bard:assoc ((k Anything)(ls <null>)(test <closure>)) (bard:false))
(bard:define-method bard:assoc ((k Anything)(ls <cons>)(test <closure>)) 
                    (if (test k (caar ls))
                        (car ls)
                        (bard:assoc k (cdr ls) test)))

;;; ForeignList <- <NSArray>
;;; ForeignText <- <NSString>

;;; ---------------------------------------------------------------------
;;; Applicable protocol
;;; ---------------------------------------------------------------------

(bard:define-function bard:applicable? (thing))
(bard:define-method bard:applicable? ((thing Anything)) #f)
(bard:define-method bard:applicable? ((thing <null>)) #t)
(bard:define-method bard:applicable? ((thing <closure>)) #t)
(bard:define-method bard:applicable? ((thing <frame>)) #t)

(bard:define-function bard:apply (app arg))
