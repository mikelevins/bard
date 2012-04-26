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

(##include "../values/type-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol List)

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


;;; list?
;;; ---------------------------------------------------------------------

(define bard:list? (%make-function name: 'list?))

(%function-add-method! bard:list? `(,Anything)(lambda (ls) (%false)))
(%function-add-method! bard:list? `(,<null>)(lambda (ls) (%true)))
(%function-add-method! bard:list? `(,<cons>) (lambda (ls) (%true)))
(%function-add-method! bard:list? `(,<string>)(lambda (str) (%true)))
(%function-add-method! bard:list? `(,<frame>)(lambda (fr) (%true)))


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
        (%nothing)
        (list-ref items (random-integer (length items))))))

(%function-add-method! bard:any `(,<null>) (lambda (ls)(%nothing)))
(%function-add-method! bard:any `(,<cons>) %bard-any)
(%function-add-method! bard:any `(,<string>) %bard-any)
(%function-add-method! bard:any `(,<frame>) %bard-any)

;;; append
;;; ---------------------------------------------------------------------

(define bard:append (%make-function name: 'append))

(%function-add-method! bard:append `(,<null> ,<null>) (lambda (ls1 ls2)(%nothing)))
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
        (%true)
        (%false))))

(%function-add-method! bard:contains? `(,<null> ,Anything & args) (lambda (ls thing . args)(%false)))
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

(define (%bard-drop-before fn ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls)))
      (if (null? items)
          (%nothing)
          (if (%apply fn (list (car items)))
              (%to-type tp items)
              (loop (cdr items)))))))

(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<null>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<function> ,<null>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<method> ,<null>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<cons>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<function> ,<cons>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<method> ,<cons>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<string>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<function> ,<string>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<method> ,<string>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<primitive-procedure> ,<frame>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<function> ,<frame>) %bard-drop-before)
(%function-add-method! bard:drop-before `(,<method> ,<frame>) %bard-drop-before)

;;; element
;;; ---------------------------------------------------------------------

(define bard:element (%make-function name: 'element))

(%function-add-method! bard:element `(,<null> ,<fixnum>) (lambda (ls n)(error "index out of range" n)))
(%function-add-method! bard:element `(,<cons> ,<fixnum>)(lambda (ls n)(list-ref ls n)))
(%function-add-method! bard:element `(,<string> ,<fixnum>)(lambda (str n)(string-ref str n)))

(%function-add-method! bard:element `(,<frame> ,<fixnum>)
                       (lambda (fr n)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (error "index out of range" n)
                               (list-ref ls n)))))

;;; empty?
;;; ---------------------------------------------------------------------

(define bard:empty? (%make-function name: 'empty?))

(%function-add-method! bard:empty? `(,<null>) (lambda (ls)(%true)))
(%function-add-method! bard:empty? `(,<cons>)(lambda (ls)(null? ls)))
(%function-add-method! bard:empty? `(,<string>)(lambda (str)(<= (string-length str) 0)))
(%function-add-method! bard:empty? `(,<frame>)(lambda (fr)(null? (%keys fr))))

;;; every?
;;; ---------------------------------------------------------------------

(define bard:every? (%make-function name: 'every?))

(define (%bard-every? fn ls)(every? (lambda (i)(%apply fn (list i))) (%as-list ls)))

(%function-add-method! bard:every? `(,<primitive-procedure> ,<null>) %bard-every?)
(%function-add-method! bard:every? `(,<function> ,<null>) %bard-every?)
(%function-add-method! bard:every? `(,<method> ,<null>) %bard-every?)

(%function-add-method! bard:every? `(,<primitive-procedure> ,<cons>) %bard-every?)
(%function-add-method! bard:every? `(,<function> ,<cons>) %bard-every?)
(%function-add-method! bard:every? `(,<method> ,<cons>) %bard-every?)

(%function-add-method! bard:every? `(,<primitive-procedure> ,<string>) %bard-every?)
(%function-add-method! bard:every? `(,<function> ,<string>) %bard-every?)
(%function-add-method! bard:every? `(,<method> ,<string>) %bard-every?)

(%function-add-method! bard:every? `(,<primitive-procedure> ,<frame>) %bard-every?)
(%function-add-method! bard:every? `(,<function> ,<frame>) %bard-every?)
(%function-add-method! bard:every? `(,<method> ,<frame>) %bard-every?)


;;; filter
;;; ---------------------------------------------------------------------

(define bard:filter (%make-function name: 'filter))

(define (%bard-filter test ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result '()))
      (if (null? items)
          (%to-type tp (reverse result))
          (if (%apply test (list (car items)))
              (loop (cdr items)(cons (car items) result))
              (loop (cdr items) result))))))

(%function-add-method! bard:filter `(,<primitive-procedure> ,<null>) %bard-filter)
(%function-add-method! bard:filter `(,<function> ,<null>) %bard-filter)
(%function-add-method! bard:filter `(,<method> ,<null>) %bard-filter)
(%function-add-method! bard:filter `(,<primitive-procedure> ,<cons>) %bard-filter)
(%function-add-method! bard:filter `(,<function> ,<cons>) %bard-filter)
(%function-add-method! bard:filter `(,<method> ,<cons>) %bard-filter)
(%function-add-method! bard:filter `(,<primitive-procedure> ,<string>) %bard-filter)
(%function-add-method! bard:filter `(,<function> ,<string>) %bard-filter)
(%function-add-method! bard:filter `(,<method> ,<string>) %bard-filter)
(%function-add-method! bard:filter `(,<primitive-procedure> ,<frame>) %bard-filter)
(%function-add-method! bard:filter `(,<function> ,<frame>) %bard-filter)
(%function-add-method! bard:filter `(,<method> ,<frame>) %bard-filter)


;;; find
;;; ---------------------------------------------------------------------

(define bard:find (%make-function name: 'find))

(define (%bard-find test ls)
  (let loop ((items (%as-list ls)))
    (if (null? items)
        (%nothing)
        (if (%apply test (list (car items)))
            (car items)
            (loop (cdr items))))))

(%function-add-method! bard:find `(,<primitive-procedure> ,<null>) %bard-find)
(%function-add-method! bard:find `(,<function> ,<null>) %bard-find)
(%function-add-method! bard:find `(,<method> ,<null>) %bard-find)
(%function-add-method! bard:find `(,<primitive-procedure> ,<cons>) %bard-find)
(%function-add-method! bard:find `(,<function> ,<cons>) %bard-find)
(%function-add-method! bard:find `(,<method> ,<cons>) %bard-find)
(%function-add-method! bard:find `(,<primitive-procedure> ,<string>) %bard-find)
(%function-add-method! bard:find `(,<function> ,<string>) %bard-find)
(%function-add-method! bard:find `(,<method> ,<string>) %bard-find)
(%function-add-method! bard:find `(,<primitive-procedure> ,<frame>) %bard-find)
(%function-add-method! bard:find `(,<function> ,<frame>) %bard-find)
(%function-add-method! bard:find `(,<method> ,<frame>) %bard-find)

;;; first
;;; ---------------------------------------------------------------------

(define bard:first (%make-function name: 'first))

(%function-add-method! bard:first `(,<null>)(lambda (ls)(%nothing)))
(%function-add-method! bard:first `(,<cons>) (lambda (ls)(car ls)))
(%function-add-method! bard:first `(,<string>)
                       (lambda (str)
                         (if (> (string-length str) 0)
                             (string-ref str 0)
                             (%nothing))))

(%function-add-method! bard:first `(,<frame>) 
                       (lambda (fr)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (%nothing)
                               (car ls)))))

;;; interleave
;;; ---------------------------------------------------------------------

(define bard:interleave (%make-function name: 'interleave))

(define (%bard-interleave ls1 ls2)
  (let ((tp (%object->bard-type ls1)))
    (let loop ((items1 (%as-list ls1))
               (items2 (%as-list ls2))
               (result '()))
      (if (or (null? items1)
              (null? items2))
          (%to-type tp (reverse result))
          (loop (cdr items1)
                (cdr items2)
                (cons (car items2)
                      (cons (car items1)
                            result)))))))

(%function-add-method! bard:interleave `(,<null> ,<null>) %bard-interleave)
(%function-add-method! bard:interleave `(,<null> ,<cons>) %bard-interleave)
(%function-add-method! bard:interleave `(,<null> ,<string>) %bard-interleave)
(%function-add-method! bard:interleave `(,<null> ,<frame>) %bard-interleave)
(%function-add-method! bard:interleave `(,<cons> ,<null>) %bard-interleave)
(%function-add-method! bard:interleave `(,<cons> ,<cons>) %bard-interleave)
(%function-add-method! bard:interleave `(,<cons> ,<string>) %bard-interleave)
(%function-add-method! bard:interleave `(,<cons> ,<frame>) %bard-interleave)
(%function-add-method! bard:interleave `(,<string> ,<null>) %bard-interleave)
(%function-add-method! bard:interleave `(,<string> ,<cons>) %bard-interleave)
(%function-add-method! bard:interleave `(,<string> ,<string>) %bard-interleave)
(%function-add-method! bard:interleave `(,<string> ,<frame>) %bard-interleave)
(%function-add-method! bard:interleave `(,<frame> ,<null>) %bard-interleave)
(%function-add-method! bard:interleave `(,<frame> ,<cons>) %bard-interleave)
(%function-add-method! bard:interleave `(,<frame> ,<string>) %bard-interleave)
(%function-add-method! bard:interleave `(,<frame> ,<frame>) %bard-interleave)


;;; interpose
;;; ---------------------------------------------------------------------

(define bard:interpose (%make-function name: 'interpose))

(define (%bard-interpose thing ls)
  (let ((tp (%object->bard-type ls)))
    (let loop ((items (%as-list ls))
               (result '()))
      (if (null? items)
          (%to-type tp (reverse result))
          (if (null? result)
              (loop (cdr items)(cons (car items) result))
              (loop (cdr items)(cons (car items) (cons thing result))))))))

(%function-add-method! bard:interpose `(,Anything ,<null>) %bard-interpose)
(%function-add-method! bard:interpose `(,Anything ,<cons>) %bard-interpose)
(%function-add-method! bard:interpose `(,Anything ,<string>) %bard-interpose)
(%function-add-method! bard:interpose `(,Anything ,<frame>) %bard-interpose)

;;; intersection
;;; ---------------------------------------------------------------------

(define bard:intersection (%make-function name: 'intersection))

(define (%bard-intersection ls1 ls2 . args)
  (let* ((tp (%object->bard-type ls1))
         (test (if (null? args)
                   bard:=
                   (car args)))
         (member? (lambda (x s)(any? (lambda (i)(%apply test (list i x))) s)))
         (items2 (%as-list ls2)))
    (let loop ((items1 (%as-list ls1))
               (result '()))
      (if (null? items1)
          (%to-type tp (reverse result))
          (let* ((item (car items1))
                 (new-result (if (member? item items2)
                                 (if (member? item result)
                                     result
                                     (cons item result))
                                 result)))
            (loop (cdr items1) new-result))))))

(%function-add-method! bard:intersection `(,<null> ,<null> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<null> ,<cons> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<null> ,<string> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<null> ,<frame> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<cons> ,<null> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<cons> ,<cons> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<cons> ,<string> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<cons> ,<frame> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<string> ,<null> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<string> ,<cons> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<string> ,<string> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<string> ,<frame> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<frame> ,<null> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<frame> ,<cons> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<frame> ,<string> & args) %bard-intersection)
(%function-add-method! bard:intersection `(,<frame> ,<frame> & args) %bard-intersection)

;;; last
;;; ---------------------------------------------------------------------

(define bard:last (%make-function name: 'last))

(%function-add-method! bard:last `(,<null>)(lambda (ls)(%nothing)))
(%function-add-method! bard:last `(,<cons>) (lambda (ls)(list-ref ls (- (length ls) 1))))
(%function-add-method! bard:last `(,<string>)(lambda (str)(string-ref str (- (string-length str) 1))))

(%function-add-method! bard:last `(,<frame>) 
                       (lambda (fr)
                         (let ((ls (%frame->list fr)))
                           (if (null? ls)
                               (%nothing)
                               (list-ref ls (- (length ls) 1))))))

;;; length
;;; ---------------------------------------------------------------------

(define bard:length (%make-function name: 'length))

(%function-add-method! bard:length `(,<null>)(lambda (ls) 0))
(%function-add-method! bard:length `(,<cons>) (lambda (ls)(length ls)))
(%function-add-method! bard:length `(,<string>)(lambda (str)(string-length str)))
(%function-add-method! bard:length `(,<frame>)(lambda (fr)(length (%keys fr))))

;;; map
;;; ---------------------------------------------------------------------

(define bard:map (%make-function name: 'map))

(define (%bard-map fn . args)
  (if (null? args)
      (%nothing)
      (let ((tp (%object->bard-type (car args))))
        (let loop ((lists (map %as-list args))
                   (result '()))
          (if (any? null? lists)
              (%to-type tp (reverse result))
              (loop (map cdr lists)
                    (cons (%apply fn (map car lists)) result)))))))

(%function-add-method! bard:map `(,<primitive-procedure> & args) %bard-map)
(%function-add-method! bard:map `(,<function> & args) %bard-map)
(%function-add-method! bard:map `(,<method> & args) %bard-map)

;;; position
;;; ---------------------------------------------------------------------

(define bard:position (%make-function name: 'position))

(define (%bard-position test ls)
  (let* ((tp (%object->bard-type ls))
         (items (%as-list ls))
         (len (length items)))
    (let loop ((items items)
               (i 0))
      (if (null? items)
          (nothing)
          (if (%apply test (list (car items)))
              i
              (loop (cdr items) (+ i 1)))))))

(%function-add-method! bard:position `(,<primitive-procedure> ,<null>) %bard-position)
(%function-add-method! bard:position `(,<function> ,<null>) %bard-position)
(%function-add-method! bard:position `(,<method> ,<null>) %bard-position)
(%function-add-method! bard:position `(,<primitive-procedure> ,<cons>) %bard-position)
(%function-add-method! bard:position `(,<function> ,<cons>) %bard-position)
(%function-add-method! bard:position `(,<method> ,<cons>) %bard-position)
(%function-add-method! bard:position `(,<primitive-procedure> ,<string>) %bard-position)
(%function-add-method! bard:position `(,<function> ,<string>) %bard-position)
(%function-add-method! bard:position `(,<method> ,<string>) %bard-position)
(%function-add-method! bard:position `(,<primitive-procedure> ,<frame>) %bard-position)
(%function-add-method! bard:position `(,<function> ,<frame>) %bard-position)
(%function-add-method! bard:position `(,<method> ,<frame>) %bard-position)

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

(%function-add-method! bard:second `(,<null>)(lambda (ls)(%nothing)))
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
                               (%nothing)
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

(%function-add-method! bard:select `(,<null> ,Anything) (lambda (indexes ls)(%nothing)))
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
        (%nothing)
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

(%function-add-method! bard:tail `(,<null>) (lambda (ls)(%nothing)))
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

(%function-add-method! bard:unzip `(,<null>) (lambda (ls)(%nothing)))
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
                         (let ((keys (%keys fr)))
                           (list keys
                                 (map (lambda (k)(%frame-get fr k)) keys)))))

;;; zip
;;; ---------------------------------------------------------------------

(define bard:zip (%make-function name: 'zip))

(%function-add-method! bard:zip `(,<null> ,<null>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<null> ,<cons>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<null> ,<string>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<null> ,<frame>) (lambda (x y)(%nothing)))

(%function-add-method! bard:zip `(,<cons> ,<null>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<cons> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) x y)))
(%function-add-method! bard:zip `(,<cons> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) x (string->list y))))
(%function-add-method! bard:zip `(,<cons> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) x (%frame->list y))))

(%function-add-method! bard:zip `(,<string> ,<null>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<string> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) (string->list y))))
(%function-add-method! bard:zip `(,<string> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) y)))
(%function-add-method! bard:zip `(,<string> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) (string->list x) (%frame->list y))))

(%function-add-method! bard:zip `(,<frame> ,<null>) (lambda (x y)(%nothing)))
(%function-add-method! bard:zip `(,<frame> ,<cons>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) y)))
(%function-add-method! bard:zip `(,<frame> ,<string>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) (string->list y))))
(%function-add-method! bard:zip `(,<frame> ,<frame>) (lambda (x y)(map (lambda (a b)(list a b)) (%frame->list x) (%frame->list y))))

