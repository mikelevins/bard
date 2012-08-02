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
(define (%true? x) 
  (and (not (%false? x))
       (not (%undefined? x))))

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
(define %map map)
(define %for-each for-each)

(define (%bard-list->cons x) x)
(define (%cons->bard-list x) x)

;;; ---------------------------------------------------------------------
;;; frame
;;; ---------------------------------------------------------------------

(define $empty-slots '())

(define-type %frame
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  extender: %define-frame-type
  constructor: %private-make-frame
  (slots %frame-slots))

(define $empty-frame (%private-make-frame $empty-slots))

(define <frame> (%define-standard-type '<frame> (##structure-type (%private-make-frame $empty-slots))))

(define (%frame-slot? x)
  (and (%list? x)
       (not (%null? x))
       (not (%null? (%cdr x)))
       (%null? (%cddr x))))

(define (%plist->slots plist)
  (let loop ((kvs plist))
    (if (null? kvs)
        '()
        (if (null? (cdr kvs))
            (error (string-append "Malformed plist: " (object->string plist)))
            (cons (list (car kvs)
                        (cadr kvs))
                  (loop (cddr kvs)))))))

(define (%make-frame kv-plist)
  (let* ((slots (%plist->slots kv-plist)))
    (%private-make-frame slots)))

(define (%maybe-slot-list->frame slist)
  (let loop ((slist slist)
             (slots '())
             (keys '()))
    (if (null? slist)
        (%private-make-frame (reverse slots))
        (let ((slot (car slist)))
          (if (%frame-slot? slot)
              (let ((key (car slot)))
                (if (member key keys)
                    slist
                    (loop (cdr slist)
                          (cons slot slots)
                          (cons key keys))))
              slist)))))

(define (%frame . kv-plist)(%make-frame kv-plist))

(define (%frame-get fr key #!optional (default (%nothing)))
  (let ((slot (assoc key (%frame-slots fr))))
    (if slot (cadr slot) default)))

(define (%frame-put fr key value)
  (let* ((new-slots (append
                     (remove-if (lambda (slot)(equal? key (car slot)))
                                (%frame-slots fr))
                     (list (list key value)))))
    (%private-make-frame new-slots)))

(define (%list-put ls key value)
  (if (and (integer? key)
           (< -1 key (length ls)))
      (append (take key ls) (list value) (drop (+ 1 key) ls))
      (let* ((pairs (map list (iota (length ls)) ls))
             (fr (%private-make-frame pairs)))
        (%frame-put fr key value))))

(define (%string-put str k v)
  (if (and (integer? k)
           (< -1 k (string-length str))
           (char? v))
      (string-append (substring str 0 k) (string v) (substring str (+ k 1)(string-length str)))
      (let* ((chars (string->list str))
             (pairs (map list (iota (string-length str)) chars))
             (fr (%private-make-frame pairs)))
        (%frame-put fr k v))))

(define (%frame-keys fr)(map car (%frame-slots fr)))
(define (%frame-vals fr)(map cadr (%frame-slots fr)))

;;; ---------------------------------------------------------------------
;;; schemas (user-defined types)
;;; ---------------------------------------------------------------------
;;;
;;; (define-schema <foo> (<bar> <baz>)
;;;   grault
;;;   (quux :default 0 :mutable true)
;;;   (wibble :initializer (method (instance supplied-value) ...)))
;;; (define x (make <foo> grault: 1))
;;; (grault x) => 1
;;; (quux x) => 0
;;; (set! (quux x) 101)
;;; (quux x) => 101

(%define-frame-type %schema
 constructor: %private-make-schema
 name
 includes
 slot-names
 tag)

(define <schema> 
  (%define-standard-type '<schema> (##structure-type (%private-make-schema $empty-slots #f '() '() -1))))

(define (%all-slots schema) (%frame-slots schema))

(define (%spec->slot spec)
  (let* ((sname (car spec))
         (attrs (cdr spec))
         (default (getf default: attrs (%nothing))))
    (cons sname default)))

(define (%ensure-no-conflicting-slots slots)
  (let loop ((ss slots)
             (keys '()))
    (if (null? ss)
        slots
        (let ((slot (car ss)))
          (if (member (car slot) keys)
              (error (string-append "Duplicate key in schema slot-descriptions: " (car slot)))
              (loop (cdr ss) 
                    (cons (car slot)
                          keys)))))))

(define (%assemble-schema-slots includes slot-specs)
  (let* ((included-slots (apply append (map %all-slots includes)))
         (new-slots (map %spec->slot slot-specs)))
    (%ensure-no-conflicting-slots (append included-slots new-slots))))

(define (%make-schema name includes slot-specs)
  (let* ((slots (%assemble-schema-slots includes slot-specs))
         (slot-names (map car slots))
         (tag (%next-available-type-tag))
         (sc (%private-make-schema slots name includes slot-names tag)))
    (%assert-type! tag sc)
    sc))


(%define-frame-type %schema-instance
 constructor: %private-make-schema-instance
 schema)

(define (%validate-initargs schema initargs)
  (let ((specs (%frame-slots schema)))
    (let loop ((inits initargs))
      (if (null? inits)
          #t
          (if (null? (cdr inits))
              (error (string-append "Malformed schema init list:"
                                    (object->string initargs)))
              (if (assoc (car inits) specs)
                  (loop (cddr inits))
                  (error (string-append "Unrecognized slot name in initializer:"
                                        (object->string (car inits))))))))))

(define (%make schema . initargs)
  (%validate-initargs schema initargs)
  (let loop ((specs (%frame-slots schema))
             (slots '()))
    (if (null? specs)
        (%private-make-schema-instance (reverse slots) schema)
        (let* ((spec (car specs))
               (sname (car spec))
               (sdefault (cdr spec))
               (init (member sname initargs))
               (sval (if init
                        (cadr init)
                        sdefault)))
          (loop (cdr specs)
                (cons (list sname sval)
                      slots))))))

(define (%schema x)
  (if (%schema-instance? x)
      (%schema-instance-schema x)
      (%nothing)))
