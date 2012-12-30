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
;;; table
;;; ---------------------------------------------------------------------

(define $empty-slots '())

(define-type %table
  id: 87DD4EB3-09F7-41A4-BEED-0B74FF5C92CE
  extender: %define-table-type
  constructor: %private-make-table
  (slots %table-slots))

(define $empty-table (%private-make-table $empty-slots))

(define <table> (%define-standard-type '<table> (##structure-type (%private-make-table $empty-slots))))

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
            (error (string-append "Malformed plist: " (object->string plist)))
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

(define (%list-get ls key #!optional (default (%nothing)))
  (if (and (integer? key)
           (< -1 key (length ls)))
      (list-ref ls key)
      default))

(define (%list-put ls key value)
  (if (and (integer? key)
           (< -1 key (length ls)))
      (append (take key ls) (list value) (drop (+ 1 key) ls))
      (let* ((pairs (map list (iota (length ls)) ls))
             (fr (%private-make-table pairs)))
        (%table-put fr key value))))

(define (%string-get str key #!optional (default (%nothing)))
  (if (and (integer? key)
           (< -1 key (string-length str)))
      (string-ref str key)
      default))

(define (%string-put str k v)
  (if (and (integer? k)
           (< -1 k (string-length str))
           (char? v))
      (string-append (substring str 0 k) (string v) (substring str (+ k 1)(string-length str)))
      (let* ((chars (string->list str))
             (pairs (map list (iota (string-length str)) chars))
             (fr (%private-make-table pairs)))
        (%table-put fr k v))))

(define (%table-keys fr)(map car (%table-slots fr)))
(define (%table-vals fr)(map cadr (%table-slots fr)))

(define (%get obj key)
  (cond
   ((string? obj) (%string-get obj key))
   ((pair? obj) (%list-get obj key))
   ((%table? obj) (%table-get obj key))))

(define (%parse-slot-path path)
  (cond 
   ((string? path)
    (string-split-on #\. path))
   ((list? path)
    (if (every? string? path)
        path
        (error (string-append "Invalid slot path: "
                              (object->string path)))))
   (else (error (string-append "Invalid slot path: "
                               (object->string path))))))

(define (%get-any-of obj keys)
  (let loop ((keys keys))
    (if (null? keys)
        (%nothing)
        (let ((val (%get obj (car keys))))
          (if (%something? val)
              val
              (loop (cdr keys)))))))

(define (%as-keyword-key key)
  (cond
   ((keyword? key) key)
   ((symbol? key) (string->keyword (symbol->string key)))
   ((string? key)(string->keyword key))
   (else (error (string-append "Invalid key: "
                               (object->string key))))))

(define (%as-symbol-key key)
  (cond
   ((keyword? key)(string->symbol (keyword->string key)))
   ((symbol? key) key)
   ((string? key)(string->symbol key))
   (else (error (string-append "Invalid key: "
                               (object->string key))))))

(define (%as-string-key key)
  (cond
   ((keyword? key)(keyword->string key))
   ((symbol? key)(symbol->string key))
   ((string? key) key)
   (else (error (string-append "Invalid key: "
                               (object->string key))))))

(define (%get-path obj path)
  (if (null? path)
      (%nothing)
      (let* ((path (%parse-slot-path path))
             (key (car path))
             (keys (list (%as-keyword-key key)
                         (%as-symbol-key key)
                         (%as-string-key key)))
             (new-obj (%get-any-of obj keys)))
        (if new-obj
            (if (null? (cdr path))
                new-obj
                (%get-path new-obj (cdr path)))
            (%nothing)))))

(define (%get-keyword-symbol-or-string-key obj key)
  (if (or (null? key)(null? obj))
      (%nothing)
      (let ((keys (list (%as-keyword-key key)
                        (%as-symbol-key key)
                        (%as-string-key key))))
        (%get-any-of obj keys))))

(define (%put obj key val)
  (cond
   ((string? obj) (%string-put obj key val))
   ((pair? obj) (%list-put obj key val))
   ((%table? obj) (%table-put obj key val))))

