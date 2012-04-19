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

(define bard:list? (%make-function name: 'list?))

(%function-add-method! bard:list? `(,Anything) (lambda (x) #f))
(%function-add-method! bard:list? `(,<null>) (lambda (x) #t))
(%function-add-method! bard:list? `(,<cons>) (lambda (x) #t))
(%function-add-method! bard:list? `(,<string>) (lambda (x) #t))
(%function-add-method! bard:list? `(,<frame>) (lambda (x) #t))

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
                                     (list k (%frame-get ls k default: '())))))))

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
