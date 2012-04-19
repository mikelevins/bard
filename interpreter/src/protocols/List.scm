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

;;; choose-any
;;; ---------------------------------------------------------------------

(define bard:choose-any (%make-function name: 'choose-any))

(%function-add-method! bard:choose-any `(,<null>) (lambda (ls)(bard:nothing)))
(%function-add-method! bard:choose-any `(,<cons>) (lambda (ls)(list-ref ls (random-integer (length ls)))))
(%function-add-method! bard:choose-any `(,<string>) 
                       (lambda (ls)
                         (if (> (string-length ls) 0)
                             (string-ref ls (random-integer (string-length ls)))
                             (bard:nothing))))
(%function-add-method! bard:choose-any `(,<frame>) 
                       (lambda (fr)
                         (let* ((ls (%frame-slots fr))
                                (slot (list-ref ls (random-integer (length ls)))))
                           (list (car slot)(cdr slot)))))

;;; concatenate
;;; ---------------------------------------------------------------------

(define bard:concatenate (%make-function name: 'concatenate))

(%function-add-method! bard:concatenate `(,<null> ,<null>) (lambda (ls1 ls2)(bard:nothing)))
(%function-add-method! bard:concatenate `(,<cons> ,<cons>) (lambda (ls1 ls2) (append ls1 ls2)))
(%function-add-method! bard:concatenate `(,<string> ,<string>) (lambda (ls1 ls2) (string-append ls1 ls2)))
(%function-add-method! bard:concatenate `(,<frame> ,<frame>) (lambda (ls1 ls2) (%frame-merge ls1 ls2)))
(%function-add-method! bard:concatenate `(,<null> ,<cons>) (lambda (ls1 ls2) ls2))
(%function-add-method! bard:concatenate `(,<cons> ,<null>) (lambda (ls1 ls2) ls1))
