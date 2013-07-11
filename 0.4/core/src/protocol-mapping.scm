;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-mapping.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values into lists of key/value pairs
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; type discrimination
;;; ---------------------------------------------------------------------

(define (%Table? x)(not (eq? x #!unbound)))

;;; ---------------------------------------------------------------------
;;; the Table abstraction: all values are (virtually) tables
;;; ---------------------------------------------------------------------
;;; All values are tables.
;;;
;;; every Bard value supports the Mapping protocol, and so can be
;;; treated as a Table. Several types are represented as concrete
;;; associations between explicit keys and values, and supporting the
;;; Mapping protocol for these values is simple and straightforward.
;;; Besides schemas like <alist-table> that are specifically designed 
;;; as representations of tables, records can also be straightforwardly
;;; treated as tables.
;;;
;;; The schemas that are represented as ordered sequences are only
;;; slightly more complicated: their implementations of Mapping
;;; functions treat indexes into the sequence as if they were keys in
;;; a table. This treatment also extends to tuples. It also extends
;;; partially to types like generators and input streams; they support
;;; parts of the Listing protocol, and the same technique of treating
;;; indexes as keys works for those types for which indexes can be
;;; computed.
;;;
;;; Mapping support in Bard's remaining types is less straightfoward.
;;; For types that are not obviously tables or lists, Bard adopts a
;;; fiction similar to Smalltalk's fiction that everything is an
;;; object, and supports it in a similar way.
;;;
;;; Take the integer 5, for example. Bard supports applying the
;;; Mapping functions to the number, as if it were represented by a
;;; finite map of some kind. In fact, of course, it's really an
;;; immediate integer with no internal structure, but Bard presents
;;; the programmer with the fiction that it's a Table. You can call
;;; get-key to fetch "components" of 5; you can call keys to list its
;;; keys; and so on.
;;;
;;; Because 5 has no internal structure, (get-key 5 key) returns
;;; nothing for any key.
;;;
;;; Importantly, though, you can also use put-key to add key/value
;;; associations to 5. The result of doing that isn't the number 5;
;;; it's a newly-created instance of a concrete table type, like
;;; <alist-table>. Besides the key/value pair you explicitly add to
;;; this new table, it also contains a reference to the original value
;;; used to create it, stored on the key value:.

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

;;; ---------------------------------------------------------------------
;;; get-key
;;; ---------------------------------------------------------------------

(define-protocol-function Mapping get-key
  signatures: (list (signature (Table Anything) #f (Anything))))

(define-primitive-method get-key (Anything Anything)
  (constantly '()))

(define-primitive-method get-key (<pair> <fixnum>)
  (lambda (ls k)(list-ref ls k)))

(define-primitive-method get-key (<string> <fixnum>)
  (lambda (str k)(string-ref str k)))

(define-primitive-method get-key (<alist-table> Anything)
  (lambda (tbl k)(alist-table-get tbl k)))

(define-primitive-method get-key (<generator> <fixnum>)
  (lambda (gen k)(%bard-generator-element gen k)))

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(define-protocol-function Mapping keys
  signatures: (list (signature (Table) #f (List))))

(define-primitive-method keys (Anything)
  (constantly '()))

(define (%bard-pair-keys pair)
  (let loop ((i 0)
             (tls pair)
             (keys '()))
    (if (null? tls)
        keys
        (if (null? (cdr tls))
            (reverse (cons i keys))
            (if (pair? (cdr tls))
                (loop (+ i 1) (cdr tls) (cons i keys))
                (reverse (cons i keys)))))))

(define-primitive-method keys (<pair>)
  %bard-pair-keys)

(define-primitive-method keys (<string>)
  (lambda (str)(iota (string-length str))))

(define-primitive-method keys (<alist-table>)
  (lambda (tbl)(map car (alist-table-slots tbl))))

(define-primitive-method keys (<generator>)
  (lambda (gen) (%eval '(generate ((i 0))
                                  (yield i)
                                  (resume (+ i 1)))
                       '())))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(define (%bard-merge-lists l1 l2)
  (map cdr
       (merge-alists 
        (zip (iota (length l1)) l1)
        (zip (iota (length l2)) l2))))

(define-protocol-function Mapping merge
  signatures: (list (signature (Table Table) #f (Table))))

(define-primitive-method merge (<pair> <pair>)
  %bard-merge-lists)

(define-primitive-method merge (<pair> <string>)
  (lambda (p s)(%bard-merge-lists p (string->list s))))

(define-primitive-method merge (<pair> <alist-table>)
  (lambda (p a)(merge-alists (zip (iota (length p)) p)(alist-table-instance-slots a))))

(define-primitive-method merge (<string> <string>)
  (lambda (s1 s2)(let ((result (%bard-merge-lists (string->list s1)(string->list s2))))
                   (if (every? char? result)
                       (list->string result)
                       result))))

(define-primitive-method merge (<string> <pair>)
  (lambda (s p)
    (let ((result (%bard-merge-lists p (string->list s))))
      (if (every? char? result)
          (list->string result)
          result))))

(define-primitive-method merge (<string> <alist-table>)
  (lambda (s a)
    (let ((p (string->list s)))
      (merge-alists (zip (iota (length p)) p)(alist-table-instance-slots a)))))

(define-primitive-method merge (<alist-table> <alist-table>)
  %merge-alist-tables)

(define-primitive-method merge (<alist-table> <pair>)
  (lambda (a p)(merge-alists (alist-table-instance-slots a)(zip (iota (length p)) p))))

(define-primitive-method merge (<alist-table> <string>)
  (lambda (a s)
    (let ((p (string->list s)))
      (merge-alists (alist-table-instance-slots a)(zip (iota (length p)) p)))))

;;; ---------------------------------------------------------------------
;;; put-key
;;; ---------------------------------------------------------------------

(define-protocol-function Mapping put-key
  signatures: (list (signature (Anything Anything Anything) #f (Table))))

(define-primitive-method put-key (Anything Anything Anything)
  (lambda (tbl key val)
    (%make-alist-table (list (cons value: tbl)
                             (cons key val)))))

(define (%bard-list-put-key ls key val)
  (if (<= 0 key (- (length ls) 1))
      (append (take key ls)
              (cons val (drop (+ 1 key) ls)))
      (error "Index out of range")))

(define-primitive-method put-key (<pair> <fixnum> Anything)
  (lambda (ls k v)
    (if (list? ls)
        (%bard-list-put-key ls k v)
        (cond ((eq? k 'left)(cons v (cdr ls)))
              ((eq? k 'right)(cons (car ls) v))
              (else (error (str "invalid pair key: " (%as-string k))))))))

(define-primitive-method put-key (<string> <fixnum> <character>)
  (lambda (str key val)
    (list->string (%bard-list-put-key (string->list str) key val))))

(define-primitive-method put-key (<alist-table> Anything Anything)
  alist-table-put)

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(define-protocol-function Mapping vals
  signatures: (list (signature (Table) #f (List))))

(define-primitive-method vals (<pair>)
  identity)

(define-primitive-method vals (<string>)
  identity)

(define-primitive-method vals (<alist-table>)
  alist-table-vals)

(define-primitive-method vals (<generator>)
  identity)


