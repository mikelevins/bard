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
;;; Character
;;; ----------------------------------------------------------------------

(define %character? char?)

;;; ----------------------------------------------------------------------
;;; False, True, and Boolean
;;; ----------------------------------------------------------------------

(define (%false) #f)

(define (%false? x) 
  (or (eqv? x (%false))
      (%nothing? x)))

(define (%true) #t)
(define (%true? x) 
  (and (not (%false? x))
       (not (%undefined? x))))

(define (%boolean? x)
  (or (eqv? x (%false))
      (eqv? x (%true))))

;;; ----------------------------------------------------------------------
;;; Keyword
;;; ----------------------------------------------------------------------

(define %keyword? keyword?)

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

(define %nil '())
(define %null? null?)
(define %list? list?)
(define %list list)
(define %cons cons)
(define %car car)
(define %cdr cdr)
(define %cadr cadr)
(define %cddr cddr)
(define %first car)
(define (%last ls) (%list-ref ls (- (%length ls) 1)))
(define %length length)
(define %append append)
(define %reverse reverse)
(define (%drop n ls)(list-tail ls n))

(define (%take n ls)
  (let loop ((n n) (ls ls))
    (if (<= n 0)
        '()
        (cons (car ls)
              (loop (- n 1)(cdr ls))))))

(define (%remove pred ls)
  (let loop ((items ls))
    (if (%null? items)
        %nil
        (if (pred (%car items))
            (loop (%cdr items))
            (%cons (%car items) (loop (%cdr items)))))))

(define %list-ref list-ref)
(define %map map)
(define %for-each for-each)

(define (%bard-list->cons x) x)
(define (%cons->bard-list x) x)

;;; ----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(define %primitive-procedure? procedure?)

;;; ----------------------------------------------------------------------
;;; Null
;;; ----------------------------------------------------------------------

(define (%nothing) '())
(define %nothing? %null?)
(define (%something? x)(not (%nothing? x)))

;;; ----------------------------------------------------------------------
;;; Number
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

(define %number? number?)

;;; ----------------------------------------------------------------------
;;; Series
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Symbol
;;; ----------------------------------------------------------------------

(define %symbol? symbol?)

(define (%name? x) 
  (or (%symbol? x)
      (%keyword? x)))

;;; ---------------------------------------------------------------------
;;; Table
;;; ---------------------------------------------------------------------

(define $empty-slots '())

(define-type %table
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  extender: %define-table-type
  constructor: %private-make-table
  (slots %table-slots))

(define $empty-table (%private-make-table $empty-slots))

(define (%table-slot? x)
  (and (%list? x)
       (not (%null? x))
       (not (%null? (%cdr x)))
       (%null? (%cddr x))))

(define (%plist->slots plist)
  (let loop ((kvs plist))
    (if (null? kvs)
        '()
        (if (null? (cdr kvs))
            (error (str "Malformed plist: " plist))
            (cons (list (car kvs)
                        (cadr kvs))
                  (loop (cddr kvs)))))))

(define (%make-table kv-plist)
  (let* ((slots (%plist->slots kv-plist)))
    (%private-make-table slots)))

(define (%maybe-slot-list->table slist)
  (let loop ((slist slist)
             (slots '())
             (keys '()))
    (if (null? slist)
        (%private-make-table (reverse slots))
        (let ((slot (car slist)))
          (if (%table-slot? slot)
              (let ((key (car slot)))
                (if (member key keys)
                    slist
                    (loop (cdr slist)
                          (cons slot slots)
                          (cons key keys))))
              slist)))))

(define (%table . kv-plist)(%make-table kv-plist))

(define (%table-get fr key #!optional (default (%nothing)))
  (let ((slot (assoc key (%table-slots fr))))
    (if slot (cadr slot) default)))

(define (%table-put fr key value)
  (let* ((new-slots (append
                     (remove-if (lambda (slot)(equal? key (car slot)))
                                (%table-slots fr))
                     (list (list key value)))))
    (%private-make-table new-slots)))

(define (%table-keys fr)(map car (%table-slots fr)))
(define (%table-vals fr)(map cadr (%table-slots fr)))

;;; ----------------------------------------------------------------------
;;; Text
;;; ----------------------------------------------------------------------

(define %string? string?)
(define %text? string?)

;;; ----------------------------------------------------------------------
;;; Undefined
;;; ----------------------------------------------------------------------

(define (%undefined) #!unbound)
(define (%undefined? x) (or (eqv? x #!unbound)(eqv? x #!void)))
(define (%defined? x)(not (%undefined? x)))



