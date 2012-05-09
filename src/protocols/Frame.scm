;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Frame.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Frame protocol
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../values/type-macros.scm")
(##include "../values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Frame)

;;; frame?
;;; ---------------------------------------------------------------------

(define bard:frame? (%make-function name: 'frame?))

(%function-add-method! bard:frame? `(,Anything) (%method (thing) false))
(%function-add-method! bard:frame? `(,<null>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<cons>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<string>) (%method (thing) true))
(%function-add-method! bard:frame? `(,<frame>) (%method (thing) true))

;;; contains-key?
;;; ---------------------------------------------------------------------

(define bard:contains-key? (%make-function name: 'contains-key?))

(%function-add-method! bard:contains-key? `(,<null> ,Anything & args) (%method (thing) false))

(%function-add-method! bard:contains-key? `(,<cons> ,Anything & args)
                       (%primitive-method (ls thing & args)
                                          (if (null? args)
                                              (and (integer? thing)
                                                   (< -1 thing (length ls)))
                                              (let ((test (car args)))
                                                (let loop ((items ls)
                                                           (i 0))
                                                  (if (null? items)
                                                      (%false)
                                                      (if (%funcall test thing i)
                                                          (%true)
                                                          (loop (cdr items)(+ i 1)))))))))

(%function-add-method! bard:contains-key? `(,<string> ,Anything & args)
                       (%primitive-method (str thing & args)
                                          (let ((len (string-length str)))
                                            (if (null? args)
                                                (and (integer? thing)
                                                     (< -1 thing len))
                                                (let ((test (car args)))
                                                  (let loop ((i 0))
                                                    (if (>= i len)
                                                        (%false)
                                                        (if (%funcall test thing i)
                                                            (%true)
                                                            (loop (+ i 1))))))))))

(%function-add-method! bard:contains-key? `(,<frame> ,Anything & args) 
                       (%primitive-method (fr thing & args)
                                          (let ((test (if (null? args) bard:= (car args))))
                                            (let loop ((slots (%frame-slots fr)))
                                              (if (null? slots)
                                                  (%false)
                                                  (let ((slot (car slots)))
                                                    (if (%funcall test (car slot))
                                                        (%true)
                                                        (loop (cdr slots)))))))))

;;; contains-value?
;;; ---------------------------------------------------------------------

(define bard:contains-value? (%make-function name: 'contains-value?))

(%function-add-method! bard:contains-value? `(,<null> ,Anything & args) (%method (thing) false))

(%function-add-method! bard:contains-value? `(,<cons> ,Anything & args)
                       (%primitive-method (ls thing & args)
                                          (let ((test (if (null? args) bard:= (car args))))
                                            (let loop ((items ls))
                                              (if (null? items)
                                                  (%false)
                                                  (if (%funcall test thing (car items))
                                                      (%true)
                                                      (loop (cdr items))))))))

(%function-add-method! bard:contains-value? `(,<string> ,Anything & args)
                       (%primitive-method (str thing & args)
                                          (let ((len (string-length str))
                                                (test (if (null? args) bard:= (car args))))
                                            (let loop ((i 0))
                                              (if (>= i len)
                                                  (%false)
                                                  (if (%funcall test thing (string-ref str i))
                                                      (%true)
                                                      (loop (+ i 1))))))))

(%function-add-method! bard:contains-value? `(,<frame> ,Anything & args)
                       (%primitive-method (fr thing & args)
                                          (let ((test (if (null? args) bard:= (car args))))
                                            (let loop ((slots (%frame-slots fr)))
                                              (if (null? slots)
                                                  (%false)
                                                  (let ((slot (car slots)))
                                                    (if (%funcall test (cdr slot))
                                                        (%true)
                                                        (loop (cdr slots)))))))))

;;; get
;;; ---------------------------------------------------------------------

(define bard:get (%make-function name: 'get))

(define %bard-get-from-list 
  (%primitive-method (ls i & args)
                     (let ((default (if (null? args)
                                        (%nothing)
                                        (cadr args))))
                       (if (< -1 i (length ls))
                           (list-ref ls i)
                           default))))

(define %bard-get-from-string 
  (%primitive-method (str i & args)
                     (let ((default (if (null? args)(%nothing)(cadr args))))
                       (if (< -1 i (string-length ls))
                           (string-ref ls i)
                           default))))

(%function-add-method! bard:get `(,<null> ,Anything & args) (%primitive-method (fr thing & args)(%nothing)))

(%function-add-method! bard:get `(,<cons> ,<fixnum> & args) %bard-get-from-list)
(%function-add-method! bard:get `(,<cons> ,<bignum> & args) %bard-get-from-list)

(%function-add-method! bard:get `(,<string> ,<fixnum> & args) %bard-get-from-string)
(%function-add-method! bard:get `(,<string> ,<bignum> & args) %bard-get-from-string)

(%function-add-method! bard:get `(,<frame> ,Anything & args)
                       (%primitive-method (fr thing & args)
                                          (%frame-get fr thing (if (null? args)(%nothing)(cadr args)))))

;;; keys
;;; ---------------------------------------------------------------------

(define bard:keys (%make-function name: 'keys))

(%function-add-method! bard:keys `(,<null>) (%primitive-method (fr)(%nothing)))
(%function-add-method! bard:keys `(,<cons>) (%primitive-method (ls)(range 0 (length ls))))
(%function-add-method! bard:keys `(,<string>) (%primitive-method (str)(range 0 (string-length str))))
(%function-add-method! bard:keys `(,<frame>) (%primitive-method (fr)(%keys fr)))

;;; merge
;;; ---------------------------------------------------------------------
;;; (merge fr1 fr2) => fr3, with slots from both frames
;;; where keys appear in both frames, the values from fr2 are used
;;; when sequences are treated as frames, their keys are the indexes
;;; of their elements

(define bard:merge (%make-function name: 'merge))

(define %bard-merge-cons-cons
  (%primitive-method (ls1 ls2)
                     (let ((len1 (length ls1))
                           (len2 (length ls2)))
                       (if (<= len1 len2)
                           ls2
                           (append ls2 (drop len2 ls1))))))

(define %bard-merge-cons-string
  (%primitive-method (ls str)
                     (let ((len1 (length ls))
                           (len2 (string-length str)))
                       (if (<= len1 len2)
                           str
                           (let ((tl (drop len2 ls)))
                             (if (every? char? tl)
                                 (string-append str (list->string tl))
                                 (append (string->list str) tl)))))))

(define %bard-merge-cons-frame
  (%primitive-method (ls fr)
                     (%frame-merge (%private-make-frame (map cons (range 0 (length ls)) ls))
                                   fr)))

(define %bard-merge-string-cons
  (%primitive-method (str ls)
                     (let ((len1 (string-length str))
                           (len2 (length ls)))
                       (if (<= len1 len2)
                           ls
                           (let ((tl (substring str len2 len1)))
                             (if (every? char? ls)
                                 (string-append (list->string ls) tl)
                                 (append ls (string->list tl))))))))

(define %bard-merge-string-string
  (%primitive-method (str1 str2)
                     (let ((len1 (string-length str1))
                           (len2 (string-length str2)))
                       (if (<= len1 len2)
                           str2
                           (string-append str2 (substring str1 len2 len1))))))

(define (%maybe-frame-to-string fr)
  (let ((slots (%frame-slots fr)))
    (if (and (every? integer? (map car slots))
             (every? char? (map cdr slots)))
        (let* ((sorted-slots (sort slots (lambda (s t)(< (car s)(car t)))))
               (indexes (map car sorted-slots))
               (index-count (length indexes))
               (indexes1 (take (- index-count 1) indexes))
               (indexes2 (drop 1 indexes)))
          (if (every? (lambda (x y)(= y (+ x 1))) indexes1 indexes2)
              (list->string (map cdr sorted-slots))
              fr))
        fr)))

(define %bard-merge-string-frame
  (%primitive-method (str fr)
                     (%maybe-frame-to-string
                      (%frame-merge (%private-make-frame (map cons (range 0 (string-length str)) (string->list str)))
                                    fr))))

(define %bard-merge-frame-cons
  (%primitive-method (fr ls)
                     (%frame-merge fr
                                   (%private-make-frame (map cons (range 0 (length ls)) ls)))))

(define %bard-merge-frame-string
  (%primitive-method (fr str)
                     (%frame-merge fr
                                   (%private-make-frame (map cons (range 0 (string-length str)) (string->list str))))))

(%function-add-method! bard:merge `(,<null> ,<null>) (%primitive-method (fr1 fr2) (%nothing)))
(%function-add-method! bard:merge `(,<null> ,<cons>) (%primitive-method (fr1 fr2) fr2))
(%function-add-method! bard:merge `(,<null> ,<string>) (%primitive-method (fr1 fr2) fr2))
(%function-add-method! bard:merge `(,<null> ,<frame>) (%primitive-method (fr1 fr2) fr2))
(%function-add-method! bard:merge `(,<cons> ,<null>) (%primitive-method (fr1 fr2) fr1))
(%function-add-method! bard:merge `(,<cons> ,<cons>) %bard-merge-cons-cons)
(%function-add-method! bard:merge `(,<cons> ,<string> ) %bard-merge-cons-string)
(%function-add-method! bard:merge `(,<cons> ,<frame> ) %bard-merge-cons-frame)
(%function-add-method! bard:merge `(,<string> ,<null> ) (%primitive-method (fr1 fr2) fr1))
(%function-add-method! bard:merge `(,<string> ,<cons> ) %bard-merge-string-cons)
(%function-add-method! bard:merge `(,<string> ,<string> ) %bard-merge-string-string)
(%function-add-method! bard:merge `(,<string> ,<frame> ) %bard-merge-string-frame)
(%function-add-method! bard:merge `(,<frame> ,<null> ) (%primitive-method (fr1 fr2) fr1))
(%function-add-method! bard:merge `(,<frame> ,<cons> ) %bard-merge-frame-cons)
(%function-add-method! bard:merge `(,<frame> ,<string> ) %bard-merge-frame-string)
(%function-add-method! bard:merge `(,<frame> ,<frame> ) (%primitive-method (fr1 fr2) (%frame-merge fr1 fr2)))


;;; put
;;; ---------------------------------------------------------------------

(define bard:put (%make-function name: 'put))

(define %bard-put 
  (%primitive-method (fr k v)
                     (%frame-put fr k v)))



(define %bard-put-in-null
  (%primitive-method (ls k v)
                     (if (zero? k)
                         (list v)
                         (error "index out of range"))))

(define %bard-put-in-list 
  (%primitive-method (ls k v)
                     (let* ((tp (%object->bard-type ls))
                            (items (%as-list ls))
                            (new-items (if (= k (length items))
                                           (reverse (cons v (reverse items)))
                                           (if (< -1 k (length items))
                                               (append (take k items)
                                                       (cons v
                                                             (drop (+ k 1) items)))
                                               (error "Index out of range")))))
                       (%to-type tp new-items))))

(%function-add-method! bard:put `(,<null> ,Anything ,Anything) %bard-put-in-null)
(%function-add-method! bard:put `(,<null> ,<fixnum> ,Anything) %bard-put-in-null)
(%function-add-method! bard:put `(,<cons> ,<fixnum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<cons> ,<bignum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<string> ,<fixnum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<string> ,<bignum> ,Anything) %bard-put-in-list)
(%function-add-method! bard:put `(,<frame> ,Anything ,Anything) %bard-put)

;;; vals
;;; ---------------------------------------------------------------------

(define bard:vals (%make-function name: 'vals))

(%function-add-method! bard:vals `(,<null>) (%method (x) x))
(%function-add-method! bard:vals `(,<cons>) (%method (x) x))
(%function-add-method! bard:vals `(,<string>) (%primitive-method (x) (%as-list x)))
(%function-add-method! bard:vals `(,<frame>) (%primitive-method (fr) (%vals fr)))
