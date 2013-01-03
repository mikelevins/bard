;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-mapping.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values in associative arrays
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

;;; ---------------------------------------------------------------------
;;; contains-key?
;;; ---------------------------------------------------------------------

(define (%bard-contains-key? fr k)
  (cond
   ((%table? fr)(let loop ((slots (%table-slots fr)))
                  (if (null? slots)
                      #f
                      (let ((slot (car slots))
                            (more (cdr slots)))
                        (if (equal? k (car slot))
                            #t
                            (loop (cdr slots)))))))
   ((list? fr)(< -1 k (length fr)))
   ((string? fr)(< -1 k (string-length fr)))
   (else #f)))

(define bard:contains-key?
  (make-primitive
   procedure: %bard-contains-key?
   debug-name: 'contains-key?
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; contains-value?
;;; ---------------------------------------------------------------------

(define (%bard-contains-value? fr v)
  (cond
   ((%table? fr)(let loop ((slots (%table-slots fr)))
                  (if (null? slots)
                      #f
                      (let ((slot (car slots))
                            (more (cdr slots)))
                        (if (equal? v (cadr slot))
                            #t
                            (loop (cdr slots)))))))
   ((list? fr)(and (member v fr) #t))
   ((string? fr)(and (char? v)(string-char-position v fr) #t))
   (else #f)))

(define bard:contains-value?
  (make-primitive
   procedure: %bard-contains-value?
   debug-name: 'contains-value?
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; get
;;; ---------------------------------------------------------------------

(define (%bard-get fr k)
  (cond
   ((%table? fr)(%table-get fr k))
   ((list? fr)(if (integer? k)
                   (list-ref fr k)
                   (getf k fr '())))
   ((string? fr)(string-ref fr k))
   (else '())))

(define bard:get
  (make-primitive
   procedure: %bard-get
   debug-name: 'get
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; keys
;;; ---------------------------------------------------------------------

(define (%bard-keys fr)
  (cond
   ((%table? fr)(%table-keys fr))
   ((list? fr)(iota (length fr)))
   ((string? fr)(iota (string-length fr)))
   (else '())))

(define bard:keys
  (make-primitive
   procedure: %bard-keys
   debug-name: 'keys
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; merge
;;; ---------------------------------------------------------------------

(define (%table->list t)(%table-slots t))

(define (list->slots l)
  (map (lambda (k v)(list k v)) 
       (iota (length l))
       l))

(define (%string->slots s)
  (map (lambda (k v)(list k v)) 
       (iota (string-length s))
       (string->list s)))

(define (%merge-slots s1 s2)
  (let loop ((slots1 s1)
             (result (reverse s2)))
    (if (null? slots1)
        (reverse result)
        (let ((slot (car slots1))
              (more (cdr slots1)))
          (if (some? (lambda (r)
                       (equal? (car slot)
                               (car r)))
                     result)
              (loop (cdr slots1) result)
              (loop (cdr slots1)
                    (cons (car slots1)
                          result)))))))

(define (%merge t1 t2)
  (cond
   ((%table? t1)(cond
                 ((%table? t2)(%private-make-table (%merge-slots (%table-slots t1)(%table-slots t2))))
                 ((list? t2)(%private-make-table (%merge-slots (%table-slots t1)
                                                                (list->slots t2))))
                 ((string? t2)(%private-make-table (%merge-slots (%table-slots t1)
                                                                 (%string->slots t2))))
                 (else (error (str "Unable to merge values " t1 " and " t2)))))
   ((list? t1)(cond
                ((%table? t2)(append t1 (%table->list t2)))
                ((list? t2)(append t1 t2))
                ((string? t2)(append t1 (string->list t2)))
                (else (error (str "Unable to merge values " t1 " and " t2)))))
   ((string? t1)(cond
                 ((%table? t2)(append (string->list t1)(%table->list t2)))
                 ((list? t2)(if (every? char? t2)
                                 (string-append t1 (list->string t2))
                                 (append (string->list t1) t2)))
                 ((string? t2)(string-append t1 t2))
                 (else (error (str "Unable to merge values " t1 " and " t2)))))
   (else (error (str "Unable to merge values " t1 " and " t2)))))

(define bard:merge
  (make-primitive
   procedure: %merge
   debug-name: 'merge
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; put
;;; ---------------------------------------------------------------------

(define (%bard-put fr k v)
  (cond
   ((%table? fr)(%table-put fr k v))
   ((list? fr)(%list-put fr k v))
   ((string? fr)(%string-put fr k v))
   (else (%table value: fr k v))))

(define bard:put
  (make-primitive
   procedure: %bard-put
   debug-name: 'put
   required-count: 3
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; vals
;;; ---------------------------------------------------------------------

(define (%bard-vals fr)
  (cond
   ((%table? fr)(%table-vals fr))
   ((list? fr) fr)
   ((string? fr) fr)
   (else '())))

(define bard:vals
  (make-primitive
   procedure: %bard-vals
   debug-name: 'vals
   required-count: 1
   restarg: #f))

