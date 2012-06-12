;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ----------------------------------------------------------------------
;;; undefined
;;; ----------------------------------------------------------------------

(define (%undefined) #!unbound)
(define (%undefined? x) (or (eqv? x #!unbound)(eqv? x #!void)))
(define (%defined? x)(not (%undefined? x)))

;;; ---------------------------------------------------------------------
;;; primitive-procedure
;;; ---------------------------------------------------------------------

(define %primitive-procedure? procedure?)

;;; ----------------------------------------------------------------------
;;; nothing
;;; ----------------------------------------------------------------------

(define (%nothing) '())
(define %nothing? null?)
(define (%something? x)(not (%nothing? x)))

;;; ----------------------------------------------------------------------
;;; characters
;;; ----------------------------------------------------------------------

(define %character? char?)

;;; ----------------------------------------------------------------------
;;; boolean
;;; ----------------------------------------------------------------------

(define (%false) #f)

(define (%false? x) 
  (or (eqv? x (%false))
      (%nothing? x)))

(define (%true) #t)
(define (%true? x) (not (%false? x)))

(define (%boolean? x)
  (or (eqv? x (%false))
      (eqv? x (%true))))

;;; ----------------------------------------------------------------------
;;; numbers
;;; ----------------------------------------------------------------------

(define %fixnum? ##fixnum?)
(define %fixnum? ##bignum?)

(define (%integer? x)
  (or (##fixnum? x)
      (##bignum? x)))

(define (%float? x)
  (##flonum? x))

(define (%ratio? x)
  (##ratnum? x))

(define (%number? x) 
  (or (%integer? x)
      (%float? x)
      (%ratio? x)))

;;; ----------------------------------------------------------------------
;;; names
;;; ----------------------------------------------------------------------

(define %symbol? symbol?)
(define %keyword? keyword?)
(define (%name? x) 
  (or (%symbol? x)
      (%keyword? x)))

;;; ----------------------------------------------------------------------
;;; text
;;; ----------------------------------------------------------------------

(define %string? string?)
(define %text? string?)

;;; ---------------------------------------------------------------------
;;; list
;;; ---------------------------------------------------------------------

(define %nil '())
(define %null? null?)
(define %list? list?)
(define %cons cons)
(define %car car)
(define %cdr cdr)
(define %cadr cadr)
(define %cddr cddr)
(define %first car)
(define (%last ls) (%list-ref ls (- (%length ls) 1)))
(define %list list)
(define %length length)
(define %append append)
(define %reverse reverse)

(define (%some? test ls)
  (let loop ((items ls))
    (if (%null? items)
        (%nothing)
        (if (test (%car items))
            (%car items)
            (loop (%cdr items))))))

(define (%every? test ls #!optional (ls2 #f))
  (if ls2
      (let loop ((items1 ls)
                 (items2 ls2))
        (if (or (%null? items1)
                (%null? items2))
            (%true)
            (if (test (%car items1)(%car items2))
                (loop (%cdr items1)(%cdr items2))
                (%false))))
      (let loop ((items ls))
        (if (%null? items)
            (%true)
            (if (test (%car items))
                (loop (%cdr items))
                (%false))))))

(define (%position test ls)
  (let ((len (%length ls)))
    (let loop ((items ls)
               (i 0))
      (if (>= i len)
          (%false)
          (if (test (%car items))
              i
              (loop (%cdr items)(+ 1 i)))))))

(define (%drop n ls)(list-tail ls n))

(define (%take n ls)
  (let ((len (%length ls)))
    (let loop ((items ls)
               (i 0)
               (result %nil))
      (if (>= i n)
          result
          (if (%null? items)
              (error (string-append "Can't take " (%as-string n) " items from " (%as-string ls)))
              (loop (%cdr items)(+ i 1)(%append result (%list (%car items)))))))))


(define (%remove x ls #!optional (test equal?))
  (let loop ((items ls)
             (result %nil))
    (if (%null? items)
        result
        (let ((item (%car items)))
          (if (test x item)
              (loop (%cdr items) result)
              (loop (%cdr items)(%append result (%list item))))))))

(define %list-ref list-ref)
(define (%list-put ls n val)(%append (%take n ls) (%cons val (%drop (+ 1 n) ls))))
(define %map map)
(define %for-each for-each)

(define (%bard-list->cons x) x)
(define (%cons->bard-list x) x)

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------

(define $empty-slots (make-table test: equal?))

(define-type %frame
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  constructor: %private-make-frame
  (slots %frame-slots)
  (keys %frame-keys))

(define $empty-frame (%private-make-frame $empty-slots '()))

(define <frame> (%define-standard-type '<frame> (##structure-type (%private-make-frame $empty-slots (%list)))))

(define (%frame-slot? x)
  (and (%list? x)
       (not (%null? x))
       (not (%null? (%cdr x)))
       (%null? (%cddr x))))

(define (%make-frame kv-plist)
  (let* ((alist (plist->alist kv-plist))
         (keys (map car alist))
         (slots (list->table alist test: equal?)))
    (%private-make-frame slots keys)))

(define (%maybe-slot-list->frame slist)
  (with-exception-catcher
   (lambda (err) slist)
   (lambda ()
     (let* ((alist (map (lambda (slot)(cons (car slot)(cadr slot))) 
                        slist))
            (keys (map car alist))
            (slots (list->table alist test: equal?)))
       (%private-make-frame slots keys)))))

(define (%frame . kv-plist)(%make-frame (%cons->bard-list kv-plist)))

(define (%frame-get fr key #!optional (default (%nothing)))
  (table-ref (%frame-slots fr) key default))

(define (%frame-put fr key value)
  (let* ((new-slots (table-copy (%frame-slots fr)))
         (new-keys (%append (%remove key (%frame-keys fr))(%list key))))
    (table-set! new-slots key value)
    (%private-make-frame new-slots new-keys)))

;;; ---------------------------------------------------------------------
;;; generator
;;; ---------------------------------------------------------------------
;;; a generator is a restartable computation that yields one computed
;;; result per invocation
;;;
;;; (generate ((x 1))
;;;           (yield (* x x))
;;;           (then (+ x 1)))
;;;
;;; a fib generator
;;; > (define g
;;;     (generate ((x 0)
;;;                (y 1)
;;;                (z 1))
;;;               (yield x)
;;;               (next (+ x y) z (+ y z))))
;;;
;;; > (g)
;;; 0
;;; > (g)
;;; 1
;;; > (g)
;;; 1
;;; > (g)
;;; 2
;;; > (g)
;;; 3




;;; ---------------------------------------------------------------------
;;; series
;;; ---------------------------------------------------------------------
;;; a series is an object that encapsulates a generator and caches the
;;; yielded values so that they can be treated as a list





