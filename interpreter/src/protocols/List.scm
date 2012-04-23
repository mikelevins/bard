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

(%function-add-method! bard:add-first `(,Anything ,<null>) (lambda (x ls)(cons x ls)))
(%function-add-method! bard:add-first `(,Anything ,<cons>) (lambda (x ls)(cons x ls)))
(%function-add-method! bard:add-first `(,<character> ,<string>) (lambda (ch str)(string-append (string ch) str)))

(%function-add-method! bard:add-first `(,<cons> ,<frame>) 
                       (lambda (kv fr)
                         (if (%frame-slot? kv)
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

(%function-add-method! bard:add-last `(,<null> ,Anything) (lambda (ls x)(list x)))
(%function-add-method! bard:add-last `(,<cons> ,Anything) (lambda (ls x)(reverse (cons x (reverse ls)))))
(%function-add-method! bard:add-last `(,<string> ,<character>) (lambda (ls x)(string-append ls (string x))))

(%function-add-method! bard:add-last `(,<frame> ,<cons>) 
                       (lambda (fr kv)
                         (if (%frame-slot? kv)
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

(define (%bard-any ls)
  (let ((items (%as-list ls)))
    (if (null? items)
        (bard:nothing)
        (list-ref items (random-integer (length items))))))

(%function-add-method! bard:any `(,<null>) (lambda (ls)(bard:nothing)))
(%function-add-method! bard:any `(,<cons>) %bard-any)
(%function-add-method! bard:any `(,<string>) %bard-any)
(%function-add-method! bard:any `(,<frame>) %bard-any)

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

(define (%bard-contains? ls thing . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (items (%as-list ls)))
    (if (any? (lambda (i) (%apply test (list i thing))) items)
        (bard:true)
        (bard:false))))

(%function-add-method! bard:contains? `(,<null> ,Anything & args) (lambda (ls thing . args)(bard:false)))
(%function-add-method! bard:contains? `(,<cons> ,Anything & args) %bard-contains?)
(%function-add-method! bard:contains? `(,<string> ,Anything & args) %bard-contains?)
(%function-add-method! bard:contains? `(,<frame> ,<cons> & args) %bard-contains?)


;;; difference
;;; ---------------------------------------------------------------------

(define bard:difference (%make-function name: 'difference))

(define (%bard-difference ls1 ls2 . args)
  (let ((tp (%object->bard-type ls1))
        (test (if (null? args)
                  bard:=
                  (car args)))
        (items1 (%as-list ls1))
        (items2 (%as-list ls2)))
    (let loop ((items1 items1)
               (result '()))
      (if (null? items1)
          (%to-type tp (reverse result))
          (if (any? (lambda (i)(%apply test (list (car items1) i))) items2)
              (loop (cdr items1) result)
              (loop (cdr items1) (cons (car items1) result)))))))

(%function-add-method! bard:difference `(,<null> ,<null> & args) %bard-difference)
(%function-add-method! bard:difference `(,<null> ,<cons> & args) %bard-difference)
(%function-add-method! bard:difference `(,<null> ,<string> & args) %bard-difference)
(%function-add-method! bard:difference `(,<null> ,<frame> & args) %bard-difference)

(%function-add-method! bard:difference `(,<cons> ,<null> & args) %bard-difference)
(%function-add-method! bard:difference `(,<cons> ,<cons> & args) %bard-difference)
(%function-add-method! bard:difference `(,<cons> ,<string> & args) %bard-difference)
(%function-add-method! bard:difference `(,<cons> ,<frame> & args) %bard-difference)

(%function-add-method! bard:difference `(,<string> ,<null> & args) %bard-difference)
(%function-add-method! bard:difference `(,<string> ,<cons> & args) %bard-difference)
(%function-add-method! bard:difference `(,<string> ,<string> & args) %bard-difference)
(%function-add-method! bard:difference `(,<string> ,<frame> & args) %bard-difference)

(%function-add-method! bard:difference `(,<frame> ,<null> & args) %bard-difference)
(%function-add-method! bard:difference `(,<frame> ,<cons> & args) %bard-difference)
(%function-add-method! bard:difference `(,<frame> ,<string> & args) %bard-difference)
(%function-add-method! bard:difference `(,<frame> ,<frame> & args) %bard-difference)

;;; drop
;;; ---------------------------------------------------------------------

(define bard:drop (%make-function name: 'drop))

(define (%bard-drop n ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (i n))
      (if (<= i 0)
          (%to-type tp items)
          (if (null? items)
              (error "count out of range" n)
              (loop (cdr items) (- i 1)))))))

(%function-add-method! bard:drop `(,<fixnum> ,<null>) (lambda (n ls)(if (zero? n) ls (error "count out of range" n))))

(%function-add-method! bard:drop `(,<fixnum> ,<cons>) %bard-drop)
(%function-add-method! bard:drop `(,<fixnum> ,<string>) %bard-drop)
(%function-add-method! bard:drop `(,<fixnum> ,<frame>) %bard-drop)


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



;;; first
;;; ---------------------------------------------------------------------

(define bard:first (%make-function name: 'first))

(%function-add-method! bard:first `(,<null>)(lambda (ls)(bard:nothing)))
(%function-add-method! bard:first `(,<cons>) (lambda (ls)(car ls)))
(%function-add-method! bard:first `(,<string>)
                       (lambda (str)
                         (if (> (string-length str) 0)
                             (string-ref str 0)
                             (bard:nothing))))

(%function-add-method! bard:first `(,<frame>) 
                       (lambda (fr)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (bard:nothing)
                               (car ls)))))

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

(define (%bard-range start end . args)
  (let ((step (if (null? args)
                  1
                  (car args))))
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
            (loop (+ i step)(cons i result)))))))

(%function-add-method! bard:range `(,<fixnum> ,<fixnum> & args) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<fixnum> & args) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<bignum> & args) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<fixnum> & args) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<fixnum> & args) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<bignum> & args) %bard-range)
(%function-add-method! bard:range `(,<fixnum> ,<bignum> & args) %bard-range)
(%function-add-method! bard:range `(,<bignum> ,<bignum> & args) %bard-range)

;;; reduce
;;; ---------------------------------------------------------------------

(define bard:reduce (%make-function name: 'reduce))

(define (%bard-reduce fn init ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result init))
      (if (null? items)
          (%to-type tp result)
          (loop (cdr items) 
                (%apply fn (list result (car items))))))))

(%function-add-method! bard:reduce `(,<primitive-procedure> ,Anything ,<null>) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,Anything ,<null>) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,Anything ,<null>) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,Anything ,<cons>) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,Anything ,<cons>) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,Anything ,<cons>) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,Anything ,<string>) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,Anything ,<string>) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,Anything ,<string>) %bard-reduce)

(%function-add-method! bard:reduce `(,<primitive-procedure> ,Anything ,<frame>) %bard-reduce)
(%function-add-method! bard:reduce `(,<function> ,Anything ,<frame>) %bard-reduce)
(%function-add-method! bard:reduce `(,<method> ,Anything ,<frame>) %bard-reduce)

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

;;; second
;;; ---------------------------------------------------------------------

(define bard:second (%make-function name: 'second))

(%function-add-method! bard:second `(,<null>)(lambda (ls)(bard:nothing)))
(%function-add-method! bard:second `(,<cons>) (lambda (ls)(cadr ls)))
(%function-add-method! bard:second `(,<string>)
                       (lambda (str)
                         (if (> (string-length str) 1)
                             (string-ref str 1)
                             (error "index out of range" 1))))

(%function-add-method! bard:second `(,<frame>) 
                       (lambda (fr)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (bard:nothing)
                               (if (null? (cdr ls))
                                   (error "index out of range" 1)
                                   (cadr ls))))))

;;; select
;;; ---------------------------------------------------------------------

(define bard:select (%make-function name: 'select))

(define (%bard-select indexes ls)
  (let* ((tp (%object->bard-type ls))
         (items (%as-list ls)))
    (%to-type tp (map (lambda (i)(list-ref items i)) indexes))))

(%function-add-method! bard:select `(,<null> ,Anything) (lambda (indexes ls)(bard:nothing)))
(%function-add-method! bard:select `(,<cons> ,<cons>) %bard-select)
(%function-add-method! bard:select `(,<cons> ,<string>) %bard-select)
(%function-add-method! bard:select `(,<cons> ,<frame>) %bard-select)


;;; shuffle
;;; ---------------------------------------------------------------------

(define bard:shuffle (%make-function name: 'shuffle))

(define (%bard-shuffle ls)
  (let* ((tp (%object->bard-type ls))
         (items (%as-list ls))
         (whatever (lambda (x y)(if (even? (random-integer 1000)) #t #f)))
         (result (sort items whatever)))
    (%to-type tp result)))

(%function-add-method! bard:shuffle `(,<null>) %bard-shuffle)
(%function-add-method! bard:shuffle `(,<cons>) %bard-shuffle)
(%function-add-method! bard:shuffle `(,<string>) %bard-shuffle)
(%function-add-method! bard:shuffle `(,<frame>) %bard-shuffle)

;;; slice
;;; ---------------------------------------------------------------------

(define bard:slice (%make-function name: 'slice))

(define (%bard-slice ls start . args)
  (let* ((end (if (null? args)
                  (%apply bard:length (list ls))
                  (car args)))
         (tp (%object->bard-type ls))
         (items (%as-list ls))
         (result (drop start (take end items))))
    (%to-type tp result)))

(%function-add-method! bard:slice `(,<null> ,<fixnum> & args) %bard-slice)
(%function-add-method! bard:slice `(,<cons> ,<fixnum> & args) %bard-slice)
(%function-add-method! bard:slice `(,<string> ,<fixnum> & args) %bard-slice)
(%function-add-method! bard:slice `(,<frame> ,<fixnum> & args) %bard-slice)

;;; some?
;;; ---------------------------------------------------------------------

(define bard:some? (%make-function name: 'some?))

(define (%bard-some? test ls)
  (let loop ((items (%as-list ls)))
    (if (null? items)
        (bard:nothing)
        (if (%apply test (list (car items)))
            (car items)
            (loop (cdr items))))))


(%function-add-method! bard:some? `(,<primitive-procedure> ,<null>) %bard-some?)
(%function-add-method! bard:some? `(,<function> ,<null>) %bard-some?)
(%function-add-method! bard:some? `(,<method> ,<null>) %bard-some?)

(%function-add-method! bard:some? `(,<primitive-procedure> ,<cons>) %bard-some?)
(%function-add-method! bard:some? `(,<function> ,<cons>) %bard-some?)
(%function-add-method! bard:some? `(,<method> ,<cons>) %bard-some?)

(%function-add-method! bard:some? `(,<primitive-procedure> ,<string>) %bard-some?)
(%function-add-method! bard:some? `(,<function> ,<string>) %bard-some?)
(%function-add-method! bard:some? `(,<method> ,<string>) %bard-some?)

(%function-add-method! bard:some? `(,<primitive-procedure> ,<frame>) %bard-some?)
(%function-add-method! bard:some? `(,<function> ,<frame>) %bard-some?)
(%function-add-method! bard:some? `(,<method> ,<frame>) %bard-some?)


;;; sort
;;; ---------------------------------------------------------------------

(define bard:sort (%make-function name: 'sort))

(define (%bard-sort test ls)
  (let* ((tp (%object->bard-type ls))
         (items (%as-list ls))
         (test (lambda (x y)(%apply test (list x y))))
         (result (sort items test)))
    (%to-type tp result)))

(%function-add-method! bard:sort `(,<primitive-procedure> ,<null>) %bard-sort)
(%function-add-method! bard:sort `(,<function> ,<null>) %bard-sort)
(%function-add-method! bard:sort `(,<method> ,<null>) %bard-sort)

(%function-add-method! bard:sort `(,<primitive-procedure> ,<cons>) %bard-sort)
(%function-add-method! bard:sort `(,<function> ,<cons>) %bard-sort)
(%function-add-method! bard:sort `(,<method> ,<cons>) %bard-sort)

(%function-add-method! bard:sort `(,<primitive-procedure> ,<string>) %bard-sort)
(%function-add-method! bard:sort `(,<function> ,<string>) %bard-sort)
(%function-add-method! bard:sort `(,<method> ,<string>) %bard-sort)

(%function-add-method! bard:sort `(,<primitive-procedure> ,<frame>) %bard-sort)
(%function-add-method! bard:sort `(,<function> ,<frame>) %bard-sort)
(%function-add-method! bard:sort `(,<method> ,<frame>) %bard-sort)

;;; tail
;;; ---------------------------------------------------------------------

(define bard:tail (%make-function name: 'tail))

(%function-add-method! bard:tail `(,<null>) (lambda (ls)(bard:nothing)))
(%function-add-method! bard:tail `(,<cons>) (lambda (ls)(cdr ls)))

(%function-add-method! bard:tail `(,<string>) 
                       (lambda (ls)
                         (if (> (string-length ls) 0)
                             (substring ls 1 (string-length ls))
                             "")))

(%function-add-method! bard:tail `(,<frame>) 
                       (lambda (fr)
                         (let ((items (%as-list fr)))
                           (if (null? items)
                               (%to-type <frame> '())
                               (%to-type <frame> (cdr items))))))


;;; tails
;;; ---------------------------------------------------------------------

(define bard:tails (%make-function name: 'tails))

(define (%bard-tails ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result '()))
      (if (null? items)
          (reverse result)
          (loop (cdr items)
                (cons (%to-type tp items) result))))))


(%function-add-method! bard:tails `(,<null>) %bard-tails)
(%function-add-method! bard:tails `(,<cons>) %bard-tails)
(%function-add-method! bard:tails `(,<string>) %bard-tails)
(%function-add-method! bard:tails `(,<frame>) %bard-tails)


;;; take
;;; ---------------------------------------------------------------------

(define bard:take (%make-function name: 'take))

(define (%bard-take n ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (i n)
               (result '()))
      (if (<= i 0)
          (%to-type tp (reverse result))
          (if (null? items)
              (error "count out of range" n)
              (loop (cdr items)
                    (- i 1)
                    (cons (car items) result)))))))


(%function-add-method! bard:take `(,<fixnum> ,<null>) %bard-take)
(%function-add-method! bard:take `(,<bignum> ,<null>) %bard-take)

(%function-add-method! bard:take `(,<fixnum> ,<cons>) %bard-take)
(%function-add-method! bard:take `(,<bignum> ,<cons>) %bard-take)

(%function-add-method! bard:take `(,<fixnum> ,<string>) %bard-take)
(%function-add-method! bard:take `(,<bignum> ,<string>) %bard-take)

(%function-add-method! bard:take `(,<fixnum> ,<frame>) %bard-take)
(%function-add-method! bard:take `(,<bignum> ,<frame>) %bard-take)

;;; take-before
;;; ---------------------------------------------------------------------

(define bard:take-before (%make-function name: 'take-before))

(define (%bard-take-before test ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result '()))
      (if (null? items)
          (%to-type tp (reverse result))
          (if (%apply test (list (car items)))
              (%to-type tp (reverse result))
              (loop (cdr items) (cons (car items) result)))))))


(%function-add-method! bard:take-before `(,<primitive-procedure> ,<null>) %bard-take-before)
(%function-add-method! bard:take-before `(,<function> ,<null>) %bard-take-before)
(%function-add-method! bard:take-before `(,<method> ,<null>) %bard-take-before)

(%function-add-method! bard:take-before `(,<primitive-procedure> ,<cons>) %bard-take-before)
(%function-add-method! bard:take-before `(,<function> ,<cons>) %bard-take-before)
(%function-add-method! bard:take-before `(,<method> ,<cons>) %bard-take-before)

(%function-add-method! bard:take-before `(,<primitive-procedure> ,<string>) %bard-take-before)
(%function-add-method! bard:take-before `(,<function> ,<string>) %bard-take-before)
(%function-add-method! bard:take-before `(,<method> ,<string>) %bard-take-before)

(%function-add-method! bard:take-before `(,<primitive-procedure> ,<frame>) %bard-take-before)
(%function-add-method! bard:take-before `(,<function> ,<frame>) %bard-take-before)
(%function-add-method! bard:take-before `(,<method> ,<frame>) %bard-take-before)


;;; unique
;;; ---------------------------------------------------------------------

(define bard:unique (%make-function name: 'unique))

(define (%bard-unique ls . args)
  (let ((test (if (null? args)
                  bard:=
                  (car args)))
        (tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result '()))
      (if (null? items)
          (%to-type tp (reverse result))
          (if (any? (lambda (x)(%apply test (list (car items) x))) result)
              (loop (cdr items) result)
              (loop (cdr items)(cons (car items) result)))))))

(%function-add-method! bard:unique `(,<null> & args) %bard-unique)
(%function-add-method! bard:unique `(,<cons> & args) %bard-unique)
(%function-add-method! bard:unique `(,<string> & args) %bard-unique)
(%function-add-method! bard:unique `(,<frame> & args) %bard-unique)

;;; unzip
;;; ---------------------------------------------------------------------

(define bard:unzip (%make-function name: 'unzip))

(%function-add-method! bard:unzip `(,<null>) (lambda (ls)(bard:nothing)))
(%function-add-method! bard:unzip `(,<cons>) 
                       (lambda (ls)
                         (let loop ((items ls)
                                    (lefts '())
                                    (rights '()))
                           (if (null? items)
                               (list (reverse lefts)
                                     (reverse rights))
                               (let ((item (car items)))
                                 (loop (cdr items)
                                       (cons (%apply bard:first (list item)) lefts)
                                       (cons (%apply bard:second (list item)) rights)))))))

(%function-add-method! bard:unzip `(,<frame>) 
                       (lambda (fr)
                         (let ((keys (bard:keys fr)))
                           (list keys
                                 (map (lambda (k)(%frame-get fr k)) keys)))))

;;; zip
;;; ---------------------------------------------------------------------

(define bard:zip (%make-function name: 'zip))

(%function-add-method! bard:zip `(,<null> ,<null>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<null> ,<cons>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<null> ,<string>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<null> ,<frame>) (lambda (x y)(bard:nothing)))

(%function-add-method! bard:zip `(,<cons> ,<null>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<cons> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) x y)))
(%function-add-method! bard:zip `(,<cons> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) x (string->list y))))
(%function-add-method! bard:zip `(,<cons> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) x (%frame->list y))))

(%function-add-method! bard:zip `(,<string> ,<null>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<string> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) (string->list y))))
(%function-add-method! bard:zip `(,<string> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) y)))
(%function-add-method! bard:zip `(,<string> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) (%frame->list y))))

(%function-add-method! bard:zip `(,<frame> ,<null>) (lambda (x y)(bard:nothing)))
(%function-add-method! bard:zip `(,<frame> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) y)))
(%function-add-method! bard:zip `(,<frame> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) (string->list y))))
(%function-add-method! bard:zip `(,<frame> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) (%frame->list y))))

