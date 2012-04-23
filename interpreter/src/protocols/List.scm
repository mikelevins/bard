;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          List.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Method protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; private utilitiess
;;; ---------------------------------------------------------------------

(define (%maybe-as-string ls)
  (if (every? char? ls)
      (list->string ls)
      ls))

(define (%maybe-as-frame ls)
  (if (every? %frame-slot? ls)
      (%list->frame ls)
      ls))

(define (%to-type tp ls)
  (cond
   ((equal? tp <string>)(%maybe-as-string ls))
   ((equal? tp <frame>)(%maybe-as-frame ls))
   (else ls)))

(define (%as-list ls)
  (let ((tp (%object->bard-type ls)))
    (cond
     ((equal? tp <string>)(string->list ls))
     ((equal? tp <frame>)(%frame->list ls))
     (else ls))))

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

;;; add-first
;;; ---------------------------------------------------------------------

(define bard:add-first (%make-function name: 'add-first))

(%function-add-method! bard:add-first `(,Anything ,<null>) 
                       (lambda (x ls)(cons x ls)))

(%function-add-method! bard:add-first `(,Anything ,<cons>) 
                       (lambda (x ls)(cons x ls)))

(%function-add-method! bard:add-first `(,<character> ,<string>) 
                       (lambda (ch str)(string-append (string ch) str)))

(%function-add-method! bard:add-first `(,<cons> ,<frame>) 
                       (lambda (kv fr)
                         (if (and (not (null? (cdr kv)))
                                  (null? (cddr kv)))
                             (let* ((matching-key? (lambda (i x)(equal? i (car x))))
                                    (k (car kv))
                                    (v (cadr kv))
                                    (slots (cons (cons k v)
                                                 (remove k (%frame-slots fr) matching-key?))))
                               (%private-make-frame slots))
                             (error "when adding an element to a frame, the new element must be a list of two elements, a key and a value"))))



;;; add-last
;;; ---------------------------------------------------------------------

(define bard:add-last (%make-function name: 'add-last))

(%function-add-method! bard:add-last `(,<null> ,Anything) 
                       (lambda (ls x)(list x)))

(%function-add-method! bard:add-last `(,<cons> ,Anything) 
                       (lambda (ls x)(reverse (cons x (reverse ls)))))

(%function-add-method! bard:add-last `(,<string> ,<character>) 
                       (lambda (ls x)(string-append ls (string x))))


(%function-add-method! bard:add-last `(,<frame> ,<cons>) 
                       (lambda (fr kv)
                         (if (and (not (null? (cdr kv)))
                                  (null? (cddr kv)))
                             (let* ((matching-key? (lambda (i x)(equal? i (car x))))
                                    (k (car kv))
                                    (v (cadr kv))
                                    (slots (remove k (%frame-slots fr) matching-key?))
                                    (new-slots (reverse (cons (cons k v) 
                                                              (reverse slots)))))
                               (%private-make-frame new-slots))
                             (error "when adding an element to a frame, the new element must be a list of two elements, a key and a value"))))

;;; any
;;; ---------------------------------------------------------------------

(define bard:any (%make-function name: 'any))

(%function-add-method! bard:any `(,<null>) (lambda (ls)(bard:nothing)))
(%function-add-method! bard:any `(,<cons>) (lambda (ls)(list-ref ls (random-integer (length ls)))))
(%function-add-method! bard:any `(,<string>) 
                       (lambda (ls)
                         (if (> (string-length ls) 0)
                             (string-ref ls (random-integer (string-length ls)))
                             (bard:nothing))))
(%function-add-method! bard:any `(,<frame>) 
                       (lambda (fr)
                         (let* ((ls (%frame-slots fr))
                                (slot (list-ref ls (random-integer (length ls)))))
                           (list (car slot)(cdr slot)))))

;;; append
;;; ---------------------------------------------------------------------

(define bard:append (%make-function name: 'append))

(%function-add-method! bard:append `(,<null> ,<null>) (lambda (ls1 ls2)(bard:nothing)))
(%function-add-method! bard:append `(,<cons> ,<cons>) (lambda (ls1 ls2) (append ls1 ls2)))
(%function-add-method! bard:append `(,<string> ,<string>) (lambda (ls1 ls2) (string-append ls1 ls2)))
(%function-add-method! bard:append `(,<frame> ,<frame>) (lambda (ls1 ls2) (%frame-merge ls1 ls2)))
(%function-add-method! bard:append `(,<null> ,<cons>) (lambda (ls1 ls2) ls2))
(%function-add-method! bard:append `(,<cons> ,<null>) (lambda (ls1 ls2) ls1))
(%function-add-method! bard:append `(,<null> ,<string>) (lambda (ls1 ls2) ls2))
(%function-add-method! bard:append `(,<string> ,<null>) (lambda (ls1 ls2) ls1))
(%function-add-method! bard:append `(,<null> ,<frame>) (lambda (ls1 ls2) ls2))
(%function-add-method! bard:append `(,<frame> ,<null>) (lambda (ls1 ls2) ls1))
(%function-add-method! bard:append `(,<cons> ,<string>) (lambda (ls1 ls2) (append ls1 (string->list ls2))))

(%function-add-method! bard:append `(,<string> ,<cons>) 
                       (lambda (ls1 ls2)
                         (if (every? char? ls2)
                             (string-append ls1 (list->string ls2))
                             (append (string->list) ls2))))

(%function-add-method! bard:append `(,<cons> ,<frame>) (lambda (ls1 ls2) (append ls1 (%frame->list ls2))))
(%function-add-method! bard:append `(,<frame> ,<cons>) (lambda (ls1 ls2) (%frame-merge ls1 (%list->frame ls2))))

;;; contains?
;;; ---------------------------------------------------------------------

(define bard:contains? (%make-function name: 'contains?))

;;; <null>

(%function-add-method! bard:contains? `(,<null> ,Anything ,<primitive-procedure>) (lambda (ls thing test) #f))
(%function-add-method! bard:contains? `(,<null> ,Anything ,<function>) (lambda (ls thing test) #f))
(%function-add-method! bard:contains? `(,<null> ,Anything ,<method>) (lambda (ls thing test) #f))

;;; <cons>

(%function-add-method! bard:contains? `(,<cons> ,Anything ,<primitive-procedure>)
                       (lambda (ls thing test) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               #f
                               (if (test thing (car ls))
                                   #t
                                   (loop (cdr ls)))))))

(%function-add-method! bard:contains? `(,<cons> ,Anything ,<function>) 
                       (lambda (ls thing test) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               #f
                               (if (%apply test (list thing (car ls)))
                                   #t
                                   (loop (cdr ls)))))))

(%function-add-method! bard:contains? `(,<cons> ,Anything ,<method>) 
                       (lambda (ls thing test) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               #f
                               (if (%apply test (list thing (car ls)))
                                   #t
                                   (loop (cdr ls)))))))

;;; <string>

(%function-add-method! bard:contains? `(,<string> ,Anything ,<primitive-procedure>)
                       (lambda (ls thing test) 
                         (let ((len (string-length ls)))
                           (let loop ((i 0))
                             (if (>= i len)
                                 #f
                                 (if (test thing (string-ref ls i))
                                     #t
                                     (loop (+ i 1))))))))

(%function-add-method! bard:contains? `(,<string> ,Anything ,<function>)
                       (lambda (ls thing test) 
                         (let ((len (string-length ls)))
                           (let loop ((i 0))
                             (if (>= i len)
                                 #f
                                 (if (%apply test (list thing (string-ref ls i)))
                                     #t
                                     (loop (+ i 1))))))))

(%function-add-method! bard:contains? `(,<string> ,Anything ,<method>)
                       (lambda (ls thing test) 
                         (let ((len (string-length ls)))
                           (let loop ((i 0))
                             (if (>= i len)
                                 #f
                                 (if (%apply test (list thing (string-ref ls i)))
                                     #t
                                     (loop (+ i 1))))))))

;;; <frame>

(%function-add-method! bard:contains? `(,<frame> ,<cons> ,<primitive-procedure>)
                       (lambda (ls thing test) 
                         (if (and (not (null? (cdr thing))
                                       (null? (cddr thing))))
                             (let ((k (car thing))
                                   (v (cadr thing)))
                               (test (list k v)
                                     (list k (%frame-get ls k default: (bard:nothing))))))))

(%function-add-method! bard:contains? `(,<frame> ,<cons> ,<function>)
                       (lambda (ls thing test) 
                         (if (and (not (null? (cdr thing)))
                                  (null? (cddr thing)))
                             (let ((k (car thing))
                                   (v (cadr thing)))
                               (%apply test
                                       (list (list k v)
                                             (list k (%frame-get ls k))))))))

(%function-add-method! bard:contains? `(,<frame> ,<cons> ,<method>)
                       (lambda (ls thing test) 
                         (if (and (not (null? (cdr thing)))
                                  (null? (cddr thing)))
                             (let ((k (car thing))
                                   (v (cadr thing)))
                               (%apply test
                                       (list (list k v)
                                             (list k (%frame-get ls k))))))))

;;; difference
;;; ---------------------------------------------------------------------

(define bard:difference (%make-function name: 'difference))

;;; <null>

(%function-add-method! bard:difference `(,<null> ,<null> ,<primitive-procedure>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<null> ,<function>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<null> ,<method>) (lambda (ls thing test) (bard:nothing)))

(%function-add-method! bard:difference `(,<null> ,<cons> ,<primitive-procedure>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<cons> ,<function>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<cons> ,<method>) (lambda (ls thing test) (bard:nothing)))

(%function-add-method! bard:difference `(,<null> ,<string> ,<primitive-procedure>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<string> ,<function>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<string> ,<method>) (lambda (ls thing test) (bard:nothing)))

(%function-add-method! bard:difference `(,<null> ,<frame> ,<primitive-procedure>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<frame> ,<function>) (lambda (ls thing test) (bard:nothing)))
(%function-add-method! bard:difference `(,<null> ,<frame> ,<method>) (lambda (ls thing test) (bard:nothing)))

;;; <cons>

(%function-add-method! bard:difference `(,<cons> ,<null> ,<primitive-procedure>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<cons> ,<null> ,<function>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<cons> ,<null> ,<method>) (lambda (ls thing test) ls))

(%function-add-method! bard:difference `(,<cons> ,<cons> ,<primitive-procedure>)
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(test x y)) ls2)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<cons> ,<cons> ,<function>)
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(%apply test (list x y))) ls2)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<cons> ,<cons> ,<method>)
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(%apply test (list x y))) ls2)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<cons> ,<string> ,<primitive-procedure>)
                       (lambda (ls1 str test)
                         (let ((ls2 (string->list str)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(test x y)) ls2)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<cons> ,<string> ,<function>) 
                       (lambda (ls1 str test)
                         (let ((ls2 (string->list str)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(%apply test (list x y))) ls2)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<cons> ,<string> ,<method>)
                       (lambda (ls1 str test)
                         (let ((ls2 (string->list str)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(%apply test (list x y))) ls2)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<cons> ,<frame> ,<primitive-procedure>)
                       (lambda (ls fr test)
                         (let ((slots ( %frame-slots fr)))
                           (let loop ((items ls)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (slot)(test x (list (car slot)(cdr slot)))) slots)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<cons> ,<frame> ,<function>) 
                       (lambda (ls fr test)
                         (let ((slots ( %frame-slots fr)))
                           (let loop ((items ls)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (slot)(%apply test (list x (list (car slot)(cdr slot))))) slots)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<cons> ,<frame> ,<method>) 
                       (lambda (ls fr test)
                         (let ((slots ( %frame-slots fr)))
                           (let loop ((items ls)
                                      (result '()))
                             (if (null? items)
                                 (reverse result)
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (slot)(%apply test (list x (list (car slot)(cdr slot))))) slots)
                                       (loop more result)
                                       (loop more (cons x result)))))))))
;;; <string>

(%function-add-method! bard:difference `(,<string> ,<null> ,<primitive-procedure>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<string> ,<null> ,<function>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<string> ,<null> ,<method>) (lambda (ls thing test) ls))

(%function-add-method! bard:difference `(,<string> ,<cons> ,<primitive-procedure>)
                       (lambda (str ls test)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(test x y)) ls)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<string> ,<cons> ,<function>)
                       (lambda (str ls test)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(%apply test (list x y))) ls)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<string> ,<cons> ,<method>)
                       (lambda (str ls test)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (let ((x (car items))
                                     (more (cdr items)))
                                 (if (any? (lambda (y)(%apply test (list x y))) ls)
                                     (loop more result)
                                     (loop more (cons x result))))))))

(%function-add-method! bard:difference `(,<string> ,<frame> ,<primitive-procedure>)
                       (lambda (str fr test)
                         (let ((ls (%frame->list fr)))
                           (let loop ((items (string->list str))
                                      (result '()))
                             (if (null? items)
                                 (list->string (reverse result))
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(test x y)) ls)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<string> ,<frame> ,<function>)
                       (lambda (str fr test)
                         (let ((ls (%frame->list fr)))
                           (let loop ((items (string->list str))
                                      (result '()))
                             (if (null? items)
                                 (list->string (reverse result))
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(%apply test (list x y))) ls)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

(%function-add-method! bard:difference `(,<string> ,<frame> ,<method>)
                       (lambda (str fr test)
                         (let ((ls (%frame->list fr)))
                           (let loop ((items (string->list str))
                                      (result '()))
                             (if (null? items)
                                 (list->string (reverse result))
                                 (let ((x (car items))
                                       (more (cdr items)))
                                   (if (any? (lambda (y)(%apply test (list x y))) ls)
                                       (loop more result)
                                       (loop more (cons x result)))))))))

;;; <frame>

(%function-add-method! bard:difference `(,<frame> ,<null> ,<primitive-procedure>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<frame> ,<null> ,<function>) (lambda (ls thing test) ls))
(%function-add-method! bard:difference `(,<frame> ,<null> ,<method>) (lambda (ls thing test) ls))

(%function-add-method! bard:difference `(,<frame> ,<cons> ,<primitive-procedure>) 
                       (lambda (fr ls test)
                         (let* ((old-slots (%frame->list fr))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(test slot x))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<cons> ,<function>) 
                       (lambda (fr ls test)
                         (let* ((old-slots (%frame->list fr))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<cons> ,<method>) 
                       (lambda (fr ls test)
                         (let* ((old-slots (%frame->list fr))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))


(%function-add-method! bard:difference `(,<frame> ,<string> ,<primitive-procedure>) 
                       (lambda (fr str test)
                         (let* ((old-slots (%frame->list fr))
                                (ls (string->list str))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(test slot x))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<string> ,<function>) 
                       (lambda (fr str test)
                         (let* ((old-slots (%frame->list fr))
                                (ls (string->list str))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<string> ,<method>) 
                       (lambda (fr str test)
                         (let* ((old-slots (%frame->list fr))
                                (ls (string->list str))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                ls)))
                                                   old-slots)))
                           (%list->frame new-slots))))


(%function-add-method! bard:difference `(,<frame> ,<frame> ,<primitive-procedure>) 
                       (lambda (fr1 fr2 test)
                         (let* ((slots1 (%frame->list fr1))
                                (slots2 (%frame->list fr2))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(test slot x))
                                                                slots2)))
                                                   slots1)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<frame> ,<function>) 
                       (lambda (fr1 fr2 test)
                         (let* ((slots1 (%frame->list fr1))
                                (slots2 (%frame->list fr2))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                slots2)))
                                                   slots1)))
                           (%list->frame new-slots))))

(%function-add-method! bard:difference `(,<frame> ,<frame> ,<method>) 
                       (lambda (fr1 fr2 test)
                         (let* ((slots1 (%frame->list fr1))
                                (slots2 (%frame->list fr2))
                                (new-slots (filter (lambda (slot)
                                                     (not (any? (lambda (x)(%apply test (list slot x)))
                                                                slots2)))
                                                   slots1)))
                           (%list->frame new-slots))))

;;; drop
;;; ---------------------------------------------------------------------

(define bard:drop (%make-function name: 'drop))

(%function-add-method! bard:drop `(,<fixnum> ,<null>) (lambda (n ls)(if (zero? n) ls (error "count out of range" n))))

(%function-add-method! bard:drop `(,<fixnum> ,<cons>)
                       (lambda (n ls)
                         (let loop ((i n)
                                    (items ls))
                           (if (<= i 0)
                               items
                               (if (null? items)
                                   (error "count out of range" n)
                                   (loop (- i 1)(cdr items)))))))

(%function-add-method! bard:drop `(,<fixnum> ,<string>)
                       (lambda (n str)
                         (let loop ((i n)
                                    (items (string->list str)))
                           (if (<= i 0)
                               (list->string items)
                               (if (null? items)
                                   (error "count out of range" n)
                                   (loop (- i 1)(cdr items)))))))

(%function-add-method! bard:drop `(,<fixnum> ,<frame>)
                       (lambda (n fr)
                         (let loop ((i n)
                                    (items (%frame->list fr)))
                           (if (<= i 0)
                               (%list->frame items)
                               (if (null? items)
                                   (error "count out of range" n)
                                   (loop (- i 1)(cdr items)))))))


;;; drop-before
;;; ---------------------------------------------------------------------

(define bard:drop-before (%make-function name: 'drop-before))

(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<null>) 
                       (lambda (test ls) ls))

(%function-add-method! bard:drop-before `(,<function> ,<null>) 
                       (lambda (test ls) ls))

(%function-add-method! bard:drop-before `(,<method> ,<null>) 
                       (lambda (test ls) ls))



(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               items
                               (if (test (car items))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<function> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               items
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<method> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               items
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))



(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (list->string items)
                               (if (test (car items))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<function> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (list->string items)
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<method> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (list->string items)
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))




(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               (%list->frame items)
                               (if (test (car items))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<function> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               (%list->frame items)
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<method> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               (%list->frame items)
                               (if (%apply test (list (car items)))
                                   items
                                   (loop (cdr items)))))))

;;; element
;;; ---------------------------------------------------------------------

(define bard:element (%make-function name: 'element))

(%function-add-method! bard:element `(,<null> ,<fixnum>) (lambda (ls n)(error "index out of range" n)))

(%function-add-method! bard:element `(,<cons> ,<fixnum>)
                       (lambda (ls n)(list-ref ls n)))

(%function-add-method! bard:element `(,<string> ,<fixnum>)
                       (lambda (str n)(string-ref str n)))

(%function-add-method! bard:element `(,<frame> ,<fixnum>)
                       (lambda (fr n)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (error "index out of range" n)
                               (list-ref ls n)))))

;;; empty?
;;; ---------------------------------------------------------------------

(define bard:empty? (%make-function name: 'empty?))

(%function-add-method! bard:empty? `(,<null>) (lambda (ls)(bard:true)))
(%function-add-method! bard:empty? `(,<cons>)(lambda (ls)(null? ls)))
(%function-add-method! bard:empty? `(,<string>)(lambda (str)(<= (string-length str) 0)))
(%function-add-method! bard:empty? `(,<frame>)(lambda (fr)(null? (bard:keys fr))))

;;; every?
;;; ---------------------------------------------------------------------

(define bard:every? (%make-function name: 'every?))

(%function-add-method! bard:every? `(,<primitive-procedure> ,<null>) (lambda (test ls) #t))
(%function-add-method! bard:every? `(,<function> ,<null>) (lambda (test ls) #t))
(%function-add-method! bard:every? `(,<method> ,<null>) (lambda (test ls) #t))



(%function-add-method! bard:every? `(,<primitive-procedure> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               #t
                               (if (test (car items))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<function> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<method> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))



(%function-add-method! bard:every? `(,<primitive-procedure> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls)))
                           (if (null? items)
                               #t
                               (if (test (car items))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<function> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls)))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<method> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls)))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))



(%function-add-method! bard:every? `(,<primitive-procedure> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               #t
                               (if (test (car items))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<function> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))

(%function-add-method! bard:every? `(,<method> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls)))
                           (if (null? items)
                               #t
                               (if (%apply test (list (car items)))
                                   (loop (cdr items))
                                   #f)))))



;;; filter
;;; ---------------------------------------------------------------------

(define bard:filter (%make-function name: 'filter))

(%function-add-method! bard:filter `(,<primitive-procedure> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:filter `(,<function> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:filter `(,<method> ,<null>) (lambda (test ls) (bard:nothing)))


(%function-add-method! bard:filter `(,<primitive-procedure> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (test (car items))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<function> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<method> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))



(%function-add-method! bard:filter `(,<primitive-procedure> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (if (test (car items))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<function> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<method> ,<string>)
                       (lambda (test ls)
                         (let loop ((items (string->list ls))
                                    (result '()))
                           (if (null? items)
                               (list->string (reverse result))
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))



(%function-add-method! bard:filter `(,<primitive-procedure> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls))
                                    (result '()))
                           (if (null? items)
                               (%list->frame (reverse result))
                               (if (test (car items))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<function> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls))
                                    (result '()))
                           (if (null? items)
                               (%list->frame (reverse result))
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<method> ,<frame>)
                       (lambda (test ls)
                         (let loop ((items (%frame->list ls))
                                    (result '()))
                           (if (null? items)
                               (%list->frame (reverse result))
                               (if (%apply test (list (car items)))
                                   (loop (cdr items)(cons (car items) result))
                                   (loop (cdr items) result))))))


;;; find
;;; ---------------------------------------------------------------------

(define bard:find (%make-function name: 'find))

(%function-add-method! bard:find `(,<primitive-procedure> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:find `(,<function> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:find `(,<method> ,<null>) (lambda (test ls) (bard:nothing)))


(%function-add-method! bard:find `(,<primitive-procedure> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   (car items)
                                   (loop (cdr items)))))))

(%function-add-method! bard:find `(,<function> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))

(%function-add-method! bard:find `(,<method> ,<cons>)
                       (lambda (test ls)
                         (let loop ((items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))



(%function-add-method! bard:find `(,<primitive-procedure> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   (car items)
                                   (loop (cdr items)))))))

(%function-add-method! bard:find `(,<function> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))

(%function-add-method! bard:find `(,<method> ,<string>)
                       (lambda (test str)
                         (let loop ((items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))



(%function-add-method! bard:find `(,<primitive-procedure> ,<frame>)
                       (lambda (test str)
                         (let loop ((items (%frame->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   (car items)
                                   (loop (cdr items))))))                       )

(%function-add-method! bard:find `(,<function> ,<frame>)
                       (lambda (test str)
                         (let loop ((items (%frame->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))

(%function-add-method! bard:find `(,<method> ,<frame>)
                       (lambda (test str)
                         (let loop ((items (%frame->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   (car items)
                                   (loop (cdr items)))))))



;;; interleave
;;; ---------------------------------------------------------------------

(define bard:interleave (%make-function name: 'interleave))

;;; <null>

(%function-add-method! bard:interleave `(,<null> ,<null>) (lambda (ls1 ls2) (bard:nothing)))
(%function-add-method! bard:interleave `(,<null> ,<cons>) (lambda (ls1 ls2) (bard:nothing)))
(%function-add-method! bard:interleave `(,<null> ,<string>) (lambda (ls1 ls2) (bard:nothing)))
(%function-add-method! bard:interleave `(,<null> ,<frame>) (lambda (ls1 ls2) (bard:nothing)))

;;; <cons>

(%function-add-method! bard:interleave `(,<cons> ,<null>) (lambda (ls1 ls2) (bard:nothing)))

(%function-add-method! bard:interleave `(,<cons> ,<cons>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 ls1)
                                    (ls2 ls2)
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (reverse result)
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<cons> ,<string>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 ls1)
                                    (ls2 (string->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (reverse result)
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<cons> ,<frame>)
                       (lambda (ls1 ls2)
                         (let loop ((ls1 ls1)
                                    (ls2 (%frame->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (reverse result)
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

;;; <string>

(%function-add-method! bard:interleave `(,<string> ,<null>) (lambda (ls1 ls2) (bard:nothing)))

(%function-add-method! bard:interleave `(,<string> ,<cons>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (string->list ls1))
                                    (ls2 ls2)
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (if (every? char? result)
                                   (list->string (reverse result))
                                   (reverse result))
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<string> ,<string>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (string->list ls1))
                                    (ls2 (string->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (list->string (reverse result))
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<string> ,<frame>)
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (string->list ls1))
                                    (ls2 (%frame->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (reverse result)
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

;;; <frame>

(%function-add-method! bard:interleave `(,<frame> ,<null>) (lambda (ls1 ls2) (%list->frame '())))

(%function-add-method! bard:interleave `(,<frame> ,<cons>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (%frame->list ls1))
                                    (ls2 ls2)
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (if (every? %frame-slot? result)
                                   (%list->frame (reverse result))
                                   (reverse result))
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<frame> ,<string>) 
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (%frame->list ls1))
                                    (ls2 (string->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (reverse result)
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))

(%function-add-method! bard:interleave `(,<frame> ,<frame>)
                       (lambda (ls1 ls2)
                         (let loop ((ls1 (%frame->list ls1))
                                    (ls2 (%frame->list ls2))
                                    (result '()))
                           (if (or (null? ls1)
                                   (null? ls2))
                               (%list->frame (reverse result))
                               (loop (cdr ls1)
                                     (cdr ls2)
                                     (cons (car ls2)
                                           (cons (car ls1)
                                                 result)))))))


;;; interpose
;;; ---------------------------------------------------------------------

(define bard:interpose (%make-function name: 'interpose))

;;; <null>

(%function-add-method! bard:interpose `(,Anything ,<null>) (lambda (fn ls) (bard:nothing)))

(%function-add-method! bard:interpose `(,Anything ,<cons>)
                       (lambda (thing ls)
                         (let loop ((items ls)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (null? result)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) (cons (car items) (cons thing result))))))))

(%function-add-method! bard:interpose `(,Anything ,<string>)
                       (lambda (thing ls)
                         (let loop ((items (string->list ls))
                                    (result '()))
                           (if (null? items)
                               (if (every? char? result)
                                   (list->string (reverse result))
                                   (reverse result))
                               (if (null? result)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) (cons (car items) (cons thing result))))))))

(%function-add-method! bard:interpose `(,Anything ,<frame>)
                       (lambda (thing ls)
                         (let loop ((items (%frame->list ls))
                                    (result '()))
                           (if (null? items)
                               (if (every? %frame-slot? result)
                                   (%list->frame (reverse result))
                                   (reverse result))
                               (if (null? result)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) (cons (car items) (cons thing result))))))))

;;; intersection
;;; ---------------------------------------------------------------------

(define bard:intersection (%make-function name: 'intersection))

;;; <null>

(%function-add-method! bard:intersection `(,<null> ,<null> ,<primitive-procedure>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<null> ,<function>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<null> ,<method>) (lambda (ls1 ls2 test) '()))

(%function-add-method! bard:intersection `(,<null> ,<cons> ,<primitive-procedure>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<cons> ,<function>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<cons> ,<method>) (lambda (ls1 ls2 test) '()))

(%function-add-method! bard:intersection `(,<null> ,<string> ,<primitive-procedure>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<string> ,<function>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<string> ,<method>) (lambda (ls1 ls2 test) '()))

(%function-add-method! bard:intersection `(,<null> ,<frame> ,<primitive-procedure>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<frame> ,<function>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<null> ,<frame> ,<method>) (lambda (ls1 ls2 test) '()))

;;; <cons>

(%function-add-method! bard:intersection `(,<cons> ,<null> ,<primitive-procedure>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<cons> ,<null> ,<function>) (lambda (ls1 ls2 test) '()))
(%function-add-method! bard:intersection `(,<cons> ,<null> ,<method>) (lambda (ls1 ls2 test) '()))


(%function-add-method! bard:intersection `(,<cons> ,<cons> ,<primitive-procedure>) 
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (any? (lambda (x)(test (car items) x)) ls2)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:intersection `(,<cons> ,<cons> ,<function>) 
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) result))))))

(%function-add-method! bard:intersection `(,<cons> ,<cons> ,<method>) 
                       (lambda (ls1 ls2 test)
                         (let loop ((items ls1)
                                    (result '()))
                           (if (null? items)
                               (reverse result)
                               (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                   (loop (cdr items) (cons (car items) result))
                                   (loop (cdr items) result))))))


(%function-add-method! bard:intersection `(,<cons> ,<string> ,<primitive-procedure>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (string->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? char? result)
                                     (list->string (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(test (car items) x)) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))

(%function-add-method! bard:intersection `(,<cons> ,<string> ,<function>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (string->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? char? result)
                                     (list->string (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))

(%function-add-method! bard:intersection `(,<cons> ,<string> ,<method>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (string->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? char? result)
                                     (list->string (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))


(%function-add-method! bard:intersection `(,<cons> ,<frame> ,<primitive-procedure>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (%frame->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? %frame-slot? result)
                                     (%list->frame (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(test (car items) x)) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))

(%function-add-method! bard:intersection `(,<cons> ,<frame> ,<function>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (%frame->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? %frame-slot? result)
                                     (%list->frame (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))

(%function-add-method! bard:intersection `(,<cons> ,<frame> ,<method>) 
                       (lambda (ls1 ls2 test)
                         (let ((ls2 (%frame->list ls2)))
                           (let loop ((items ls1)
                                      (result '()))
                             (if (null? items)
                                 (if (every? %frame-slot? result)
                                     (%list->frame (reverse result))
                                     (reverse result))
                                 (if (any? (lambda (x)(%apply test (list (car items) x))) ls2)
                                     (loop (cdr items) (cons (car items) result))
                                     (loop (cdr items) result)))))))

;;; last
;;; ---------------------------------------------------------------------

(define bard:last (%make-function name: 'last))

(%function-add-method! bard:last `(,<null>)(lambda (ls)(bard:nothing)))
(%function-add-method! bard:last `(,<cons>) (lambda (ls)(list-ref ls (- (length ls) 1))))
(%function-add-method! bard:last `(,<string>)(lambda (str)(string-ref str (- (string-length str) 1))))

(%function-add-method! bard:last `(,<frame>) 
                       (lambda (fr)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (bard:nothing)
                               (list-ref ls (- (length ls) 1))))))

;;; length
;;; ---------------------------------------------------------------------

(define bard:length (%make-function name: 'length))

(%function-add-method! bard:length `(,<null>)(lambda (ls) 0))
(%function-add-method! bard:length `(,<cons>) (lambda (ls)(length ls)))
(%function-add-method! bard:length `(,<string>)(lambda (str)(string-length str)))
(%function-add-method! bard:length `(,<frame>)(lambda (fr)(length (bard:keys fr))))

;;; list?
;;; ---------------------------------------------------------------------

(define bard:list? (%make-function name: 'list?))

(%function-add-method! bard:list? `(,Anything)(lambda (ls) (bard:false)))
(%function-add-method! bard:list? `(,<null>)(lambda (ls) (bard:true)))
(%function-add-method! bard:list? `(,<cons>) (lambda (ls) (bard:true)))
(%function-add-method! bard:list? `(,<string>)(lambda (str) (bard:true)))
(%function-add-method! bard:list? `(,<frame>)(lambda (fr) (bard:true)))


;;; map
;;; ---------------------------------------------------------------------

(define bard:map (%make-function name: 'map))

;;; <null>

(%function-add-method! bard:map `(,<primitive-procedure> ,<null>) (lambda (fn ls) '()))
(%function-add-method! bard:map `(,<function> ,<null>) (lambda (fn ls) '()))
(%function-add-method! bard:map `(,<method> ,<null>) (lambda (fn ls) '()))

;;; <cons>

(%function-add-method! bard:map `(,<primitive-procedure> ,<cons>)
                       (lambda (fn ls) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               '()
                               (cons (fn (car ls))
                                     (loop (cdr ls)))))))

(%function-add-method! bard:map `(,<function> ,<cons>)
                       (lambda (fn ls) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               '()
                               (cons (%apply fn (list (car ls)))
                                     (loop (cdr ls)))))))

(%function-add-method! bard:map `(,<method> ,<cons>)
                       (lambda (fn ls) 
                         (let loop ((ls ls))
                           (if (null? ls)
                               '()
                               (cons (%apply fn (list (car ls)))
                                     (loop (cdr ls)))))))

;;; <string>

(%function-add-method! bard:map `(,<primitive-procedure> ,<string>)
                       (lambda (fn str)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (if (every? char? result)
                                   (list->string (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (fn (car items))
                                           result))))))

(%function-add-method! bard:map `(,<function> ,<string>)
                       (lambda (fn str)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (if (every? char? result)
                                   (list->string (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (%apply fn (list (car items)))
                                           result))))))

(%function-add-method! bard:map `(,<method> ,<string>)
                       (lambda (fn str)
                         (let loop ((items (string->list str))
                                    (result '()))
                           (if (null? items)
                               (if (every? char? result)
                                   (list->string (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (%apply fn (list (car items)))
                                           result))))))

;;; <frame>

(%function-add-method! bard:map `(,<primitive-procedure> ,<frame>)
                       (lambda (fn fr)
                         (let loop ((items (%frame->list fr))
                                    (result '()))
                           (if (null? items)
                               (if (every? %frame-slot? result)
                                   (%list->frame (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (fn (car items))
                                           result))))))

(%function-add-method! bard:map `(,<function> ,<frame>)
                       (lambda (fn fr)
                         (let loop ((items (%frame->list fr))
                                    (result '()))
                           (if (null? items)
                               (if (every? %frame-slot? result)
                                   (%list->frame (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (%apply fn (list (car items)))
                                           result))))))

(%function-add-method! bard:map `(,<method> ,<frame>)
                       (lambda (fn fr)
                         (let loop ((items (%frame->list fr))
                                    (result '()))
                           (if (null? items)
                               (if (every? %frame-slot? result)
                                   (%list->frame (reverse result))
                                   (reverse result))
                               (loop (cdr items)
                                     (cons (%apply fn (list (car items)))
                                           result))))))

;;; position
;;; ---------------------------------------------------------------------

(define bard:position (%make-function name: 'position))

(%function-add-method! bard:position `(,<primitive-procedure> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:position `(,<function> ,<null>) (lambda (test ls) (bard:nothing)))
(%function-add-method! bard:position `(,<method> ,<null>) (lambda (test ls) (bard:nothing)))

(%function-add-method! bard:position `(,<primitive-procedure> ,<cons>) 
                       (lambda (test ls) 
                         (let loop ((i 0)
                                    (items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<function> ,<cons>) 
                       (lambda (test ls) 
                         (let loop ((i 0)
                                    (items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<method> ,<cons>) 
                       (lambda (test ls) 
                         (let loop ((i 0)
                                    (items ls))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))



(%function-add-method! bard:position `(,<primitive-procedure> ,<string>) 
                       (lambda (test str) 
                         (let loop ((i 0)
                                    (items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<function> ,<string>) 
                       (lambda (test str) 
                         (let loop ((i 0)
                                    (items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<method> ,<string>) 
                       (lambda (test str) 
                         (let loop ((i 0)
                                    (items (string->list str)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))



(%function-add-method! bard:position `(,<primitive-procedure> ,<frame>) 
                       (lambda (test fr) 
                         (let loop ((i 0)
                                    (items (%frame->list fr)))
                           (if (null? items)
                               (bard:nothing)
                               (if (test (car items))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<function> ,<frame>) 
                       (lambda (test fr) 
                         (let loop ((i 0)
                                    (items (%frame->list fr)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

(%function-add-method! bard:position `(,<method> ,<frame>) 
                       (lambda (test fr) 
                         (let loop ((i 0)
                                    (items (%frame->list fr)))
                           (if (null? items)
                               (bard:nothing)
                               (if (%apply test (list (car items)))
                                   i
                                   (loop (+ i 1)(cdr items)))))))

;;; range
;;; ---------------------------------------------------------------------

(define bard:range (%make-function name: 'range))

(define (%bard-range start end step)
  (if (and (> end start)
           (<= step 0))
      (error "bad range"))
  (if (and (< end start)
           (>= step 0))
      (error "bad range"))
  (let ((test (if (< step 0) <= >=)))
    (let loop ((i start)
               (result '()))
      (if (test i end)
          (reverse result)
          (loop (+ i step)(cons i result))))))

(%function-add-method! bard:range `(,<fixnum> ,<fixnum> ,<fixnum>) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<fixnum> ,<fixnum>) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<bignum> ,<fixnum>) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<fixnum> ,<bignum>) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<fixnum> ,<bignum>) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<bignum> ,<fixnum>) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<bignum> ,<bignum>) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<bignum> ,<bignum>) %bard-range)

;;; reduce
;;; ---------------------------------------------------------------------

(define bard:reduce (%make-function name: 'reduce))

(define (%bard-reduce fn ls init)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result init))
      (if (null? items)
          (%to-type tp result)
          (loop (cdr items) 
                (%apply fn (list result (car items))))))))

(%function-add-method! bard:reduce `(,<primitive-procedure> ,<null> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,<null> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,<null> ,Anything) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,<cons> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,<cons> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,<cons> ,Anything) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,<string> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,<string> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,<string> ,Anything) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,<frame> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,<frame> ,Anything) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,<frame> ,Anything) %bard-reduce)

;;; repeat
;;; ---------------------------------------------------------------------

(define bard:repeat (%make-function name: 'repeat))

(define (%bard-repeat n thing)
  (let loop ((i n)
             (result '()))
    (if (<= i 0)
        result
        (loop (- i 1)
              (cons thing result)))))

(%function-add-method! bard:repeat `(,<fixnum> ,Anything) %bard-repeat)
(%function-add-method! bard:repeat `(,<bignum> ,Anything) %bard-repeat)

;;; reverse
;;; ---------------------------------------------------------------------

(define bard:reverse (%make-function name: 'reverse))

(define (%bard-reverse ls)
  (%to-type (%object->bard-type ls)
            (reverse (%as-list ls))))

(%function-add-method! bard:reverse `(,<null>) %bard-reverse)
(%function-add-method! bard:reverse `(,<cons>) %bard-reverse)
(%function-add-method! bard:reverse `(,<string>) %bard-reverse)
(%function-add-method! bard:reverse `(,<frame>) %bard-reverse)

