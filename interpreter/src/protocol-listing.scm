;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocol-listing.scm
;;;; Project:       Bard
;;;; Purpose:       arranging values into lists
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "type-signature-macros.scm")

;;; ---------------------------------------------------------------------
;;; general utilities
;;; ---------------------------------------------------------------------

(define (%List? x)
  (or (null? x)
      (list? x)
      (string? x)
      (tuple-instance? x)
      (alist-table-instance? x)
      (generator-instance? x)
      (and (singleton-instance? x)
           (%List? (singleton-instance-value x)))))

;;; ---------------------------------------------------------------------
;;; add-first
;;; ---------------------------------------------------------------------

(define bard:add-first (make-function debug-name: 'add-first
                                      signatures: (list (signature (Anything List) #f (List)))))

(%add-primitive-method! bard:add-first
                        (list Anything <null>)
                        cons
                        debug-name: 'add-first)

(%add-primitive-method! bard:add-first
                        (list Anything <pair>)
                        cons
                        debug-name: 'add-first)

(%add-primitive-method! bard:add-first
                        (list <character> <string>)
                        (lambda (ch str)(string-append (string ch) str))
                        debug-name: 'add-first)

(%add-primitive-method! bard:add-first
                        (list <pair> <alist-table>)
                        (lambda (entry table)(%make-alist-table (cons entry (alist-table-slots table))))
                        debug-name: 'add-first)

;;; ---------------------------------------------------------------------
;;; add-last
;;; ---------------------------------------------------------------------

(define bard:add-last (make-function debug-name: 'add-last
                                     signatures: (list (signature (Anything List) #f (List)))))

(%add-primitive-method! bard:add-last
                        (list <null> Anything)
                        (lambda (ls thing)(list thing))
                        debug-name: 'add-last)

(%add-primitive-method! bard:add-last
                        (list <pair> Anything)
                        (lambda (ls thing)
                          (if (list? ls)
                              (append ls (list thing))
                              (error (str "Improper list: " ls))))
                        debug-name: 'add-last)

(%add-primitive-method! bard:add-last
                        (list <string> <character>)
                        (lambda (str ch)(string-append str (string ch)))
                        debug-name: 'add-last)

(%add-primitive-method! bard:add-last
                        (list <alist-table> <pair>)
                        (lambda (table entry)(%make-alist-table (append (alist-table-slots table) (list entry))))
                        debug-name: 'add-last)


;;; ---------------------------------------------------------------------
;;; any
;;; ---------------------------------------------------------------------

(define bard:any (make-function debug-name: 'any
                                signatures: (list (signature (List) #f (Anything)))))

(%add-primitive-method! bard:any
                        (list <null>)
                        (constantly '())
                        debug-name: 'any)

(%add-primitive-method! bard:any
                        (list <pair>)
                        (lambda (ls)
                          (if (list? ls)
                              (list-ref ls (random-integer (length ls)))
                              (error (str "Improper list: " ls))))
                        debug-name: 'any)

(%add-primitive-method! bard:any
                        (list <string>)
                        (lambda (ls)(string-ref ls (random-integer (string-length ls))))
                        debug-name: 'any)

(%add-primitive-method! bard:any
                        (list <alist-table>)
                        (lambda (table)(alist-table-get table (car (any (alist-table-keys table)))))
                        debug-name: 'any)

;;; ---------------------------------------------------------------------
;;; append
;;; ---------------------------------------------------------------------

(define bard:append (make-function debug-name: 'append
                                   signatures: (list (signature (List List) #f (List)))))

(%add-primitive-method! bard:append
                        (list <null>  <null>)
                        (constantly '())
                        debug-name: 'append)

(%add-primitive-method! bard:append
                        (list <null>  <pair>)
                        (lambda (ls1 ls2) ls2)
                        debug-name: 'append)

(%add-primitive-method! bard:append
                        (list <pair>  <null>)
                        (lambda (ls1 ls2) ls1)
                        debug-name: 'append)

(%add-primitive-method! bard:append
                        (list <pair>  <pair>)
                        append
                        debug-name: 'append)

(%add-primitive-method! bard:append
                        (list <string>  <string>)
                        string-append
                        debug-name: 'append)

(%add-primitive-method! bard:append
                        (list <alist-table>  <alist-table>)
                        (lambda (t1 t2)(%make-alist-table (append (alist-table-slots t1)(alist-table-slots t2))))
                        debug-name: 'append)

;;; ---------------------------------------------------------------------
;;; by
;;; ---------------------------------------------------------------------

(define bard:by (make-function debug-name: 'by
                               signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:by
                        (list <fixnum> <null>)
                        (constantly '())
                        debug-name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <pair>)
                        by
                        debug-name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <string>)
                        (lambda (n s)(map (lambda (i)(list->string i))
                                          (by n (string->list s))))
                        debug-name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <alist-table>)
                        (lambda (n s)(by n (alist-table-slots s)))
                        debug-name: 'by)

;;; ---------------------------------------------------------------------
;;; drop
;;; ---------------------------------------------------------------------

(define bard:drop (make-function debug-name: 'drop
                                 signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:drop
                        (list <fixnum>  <null>)
                        (lambda (n ls)(error (str "Can't drop more elements from the empty list")))
                        debug-name: 'drop)

(%add-primitive-method! bard:drop
                        (list (%singleton 0)  <null>)
                        (constantly '())
                        debug-name: 'drop)

(%add-primitive-method! bard:drop
                        (list <fixnum> <pair>)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0))
                            (if (>= i n)
                                items
                                (if (null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1))))))
                        debug-name: 'drop)

(%add-primitive-method! bard:drop
                        (list <fixnum>  <string>)
                        (lambda (n str)(substring str n (string-length str)))
                        debug-name: 'drop)

(%add-primitive-method! bard:drop
                        (list <fixnum> <alist-table>)
                        (lambda (n s)(%make-alist-table (drop n (alist-table-slots s))))
                        debug-name: 'drop)

;;; ---------------------------------------------------------------------
;;; element
;;; ---------------------------------------------------------------------

(define bard:element (make-function debug-name: 'element
                                    signatures: (list (signature (List Integer) #f (Anything)))))

(%add-primitive-method! bard:element
                        (list <null> <fixnum>)
                        (lambda (ls n)(error (str "element index out of range")))
                        debug-name: 'element)

(define (%bard-list-element ls n)
  (if (= n 0)
      (car ls)
      (if (pair? (cdr ls))
          (%bard-list-element (cdr ls) (- n 1))
          (else: (if (= n 1)
                     (cdr ls)
                     (error (str "element index out of range")))))))

(%add-primitive-method! bard:element
                        (list <pair> <fixnum>)
                        %bard-list-element
                        debug-name: 'element)

(%add-primitive-method! bard:element
                        (list <string> <fixnum>)
                        string-ref
                        debug-name: 'element)

(%add-primitive-method! bard:element
                        (list <generator> <fixnum>)
                        (lambda (gen n)
                          (let loop ((len (length (generator-results gen))))
                            (if (< n len)
                                (list-ref (generator-results gen)
                                          (- len (+ n 1)))
                                (begin
                                  (next gen)
                                  (loop (+ len 1))))))
                        debug-name: 'element)

(%add-primitive-method! bard:element
                        (list <alist-table> <fixnum>)
                        (lambda (table n)
                          (let* ((keys (alist-table-keys table))
                                 (key (list-ref keys n)))
                            (alist-table-get table key)))
                        debug-name: 'element)

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(define (%empty? x)
  (or (eq? x #!unbound)
      (null? x)
      (equal? x "")
      (and (alist-table-instance? x)
           (zero? (length (alist-table-slots x))))))

(define bard:empty? (make-function debug-name: 'empty?
                                   signatures: (list (signature (List) #f (Boolean)))))

(%add-primitive-method! bard:empty?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'empty?)

(%add-primitive-method! bard:empty?
                        (list <pair>)
                        (constantly #f)
                        debug-name: 'empty?)

(%add-primitive-method! bard:empty?
                        (list <string>)
                        (lambda (str)(<= (string-length str) 0))
                        debug-name: 'empty?)

(%add-primitive-method! bard:empty?
                        (list <alist-table>)
                        (lambda (table)(null? (alist-table-slots table)))
                        debug-name: 'empty?)

;;; ---------------------------------------------------------------------
;;; filter
;;; ---------------------------------------------------------------------

(define bard:filter (make-function debug-name: 'filter
                                   signatures: (list (signature (Applicable List) #f (List)))))

(define (%bard-filter-list test ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (if (or (null? (cdr items))
                (pair? (cdr items)))
            (if (%funcall test (car items))
                (loop (cdr items)
                      (cons (car items) result))
                (loop (cdr items)
                      result))
            (error "No applicable method on filter for values of type <pair>")))))

;;; <null>

(%add-primitive-method! bard:filter
                        (list Anything <null>)
                        (constantly '())
                        debug-name: 'filter)

;;; <pair>

(%add-primitive-method! bard:filter
                        (list <primitive> <pair>)
                        %bard-filter-list
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <interpreted-method> <pair>)
                        %bard-filter-list
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <function> <pair>)
                        %bard-filter-list
                        debug-name: 'filter)

;;; <string>

(define (%bard-filter-string fn str)
  (let* ((inchars (string->list str))
         (outchars (%bard-filter-list fn inchars)))
    (list->string outchars)))

(%add-primitive-method! bard:filter
                        (list <primitive> <string>)
                        %bard-filter-string
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <interpreted-method> <string>)
                        %bard-filter-string
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <function> <string>)
                        %bard-filter-string
                        debug-name: 'filter)

;;; <alist-table>

(define (%bard-filter-alist-table fn table)
  (let* ((inslots (alist-table-slots table))
         (outslots (%bard-filter-list fn inslots)))
    (%make-alist-table outslots)))

(%add-primitive-method! bard:filter
                        (list <primitive> <pair>)
                        %bard-filter-alist-table
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <interpreted-method> <pair>)
                        %bard-filter-alist-table
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <function> <pair>)
                        %bard-filter-alist-table
                        debug-name: 'filter)

;;; ---------------------------------------------------------------------
;;; first
;;; ---------------------------------------------------------------------

(define bard:first (make-function debug-name: 'first
                                  signatures: (list (signature (List) #f (Anything)))))

(%add-primitive-method! bard:first
                        (list <pair>)
                        car
                        debug-name: 'first)

(%add-primitive-method! bard:first
                        (list <string>)
                        (lambda (s)(string-ref s 0))
                        debug-name: 'first)

(%add-primitive-method! bard:first
                        (list <alist-table>)
                        (lambda (table)(car (alist-table-slots table)))
                        debug-name: 'first)

(%add-primitive-method! bard:first
                        (list <generator>)
                        (lambda (gen)
                          (if (null? (generator-results gen))
                              (next gen))
                          (list-ref (generator-results gen)
                                    (- (length (generator-results gen)) 1)))
                        debug-name: 'first)


;;; ---------------------------------------------------------------------
;;; last
;;; ---------------------------------------------------------------------

(define bard:last (make-function debug-name: 'last
                                 signatures: (list (signature (List) #f (Anything)))))

(define (%pair-last p)
  (if (null? p)
      '()
      (if (list? p)
          (list-ref p (- (length p) 1))
          (cdr p))))

(%add-primitive-method! bard:last
                        (list <pair>)
                        %pair-last
                        debug-name: 'last)

(%add-primitive-method! bard:last
                        (list <string>)
                        (lambda (string)(string-ref string (- (string-length string) 1)))
                        debug-name: 'last)

(%add-primitive-method! bard:last
                        (list <alist-table>)
                        (lambda (table)
                          (%pair-last (alist-table-slots table)))
                        debug-name: 'last)

;;; ---------------------------------------------------------------------
;;; length
;;; ---------------------------------------------------------------------

(define bard:length (make-function debug-name: 'length
                                   signatures: (list (signature (List) #f (Integer)))))

(%add-primitive-method! bard:length
                        (list <null>)
                        (constantly 0)
                        debug-name: 'length)

(%add-primitive-method! bard:length
                        (list <pair>)
                        length
                        debug-name: 'length)

(%add-primitive-method! bard:length
                        (list <string>)
                        string-length
                        debug-name: 'length)

(%add-primitive-method! bard:length
                        (list <alist-table>)
                        (lambda (table)(length (alist-table-slots table)))
                        debug-name: 'length)

;;; ---------------------------------------------------------------------
;;; next-last
;;; ---------------------------------------------------------------------

(define bard:next-last (make-function debug-name: 'next-last
                                      signatures: (list (signature (List) #f (Anything)))))

(%add-primitive-method! bard:next-last
                        (list <pair>)
                        next-last
                        debug-name: 'next-last)

(%add-primitive-method! bard:next-last
                        (list <string>)
                        string-next-last
                        debug-name: 'next-last)

(%add-primitive-method! bard:next-last
                        (list <alist-table>)
                        (lambda (table)(next-last (alist-table-slots table)))
                        debug-name: 'next-last)


;;; ---------------------------------------------------------------------
;;; reduce
;;; ---------------------------------------------------------------------

(define bard:reduce (make-function debug-name: 'reduce
                                   signatures: (list (signature (Applicable Anything List) #f (Anything)))))

(define (%bard-reduce fn init ls)
  (let loop ((items ls)
             (result init))
    (if (null? items)
        result
        (loop (cdr items)
              (%funcall fn result (car items))))))

;;; <null>

(%add-primitive-method! bard:reduce
                        (list <primitive> Anything <null>)
                        %bard-reduce
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <null>)
                        %bard-reduce
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <null>)
                        %bard-reduce
                        debug-name: 'reduce)

;;; <pair>

(%add-primitive-method! bard:reduce
                        (list <primitive> Anything <pair>)
                        %bard-reduce
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <pair>)
                        %bard-reduce
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <pair>)
                        %bard-reduce
                        debug-name: 'reduce)

;;; <string>

(%add-primitive-method! bard:reduce
                        (list <primitive> Anything <string>)
                        (lambda (fn init str)(%bard-reduce fn init (string->list str)))
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <string>)
                        (lambda (fn init str)(%bard-reduce fn init (string->list str)))
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <string>)
                        (lambda (fn init str)(%bard-reduce fn init (string->list str)))
                        debug-name: 'reduce)

;;; <alist-table>

(%add-primitive-method! bard:reduce
                        (list <primitive> Anything <alist-table>)
                        (lambda (fn init tbl)(%bard-reduce fn init (alist-table-slots tbl)))
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <alist-table>)
                        (lambda (fn init tbl)(%bard-reduce fn init (alist-table-slots tbl)))
                        debug-name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <alist-table>)
                        (lambda (fn init tbl)(%bard-reduce fn init (alist-table-slots tbl)))
                        debug-name: 'reduce)

;;; ---------------------------------------------------------------------
;;; rest 
;;; ---------------------------------------------------------------------

(define bard:rest (make-function debug-name: 'rest
                                 signatures: (list (signature (List) #f (List)))))

(%add-primitive-method! bard:rest
                        (list <null>)
                        (constantly '())
                        debug-name: 'rest)

(%add-primitive-method! bard:rest
                        (list <pair>)
                        cdr
                        debug-name: 'rest)

(%add-primitive-method! bard:rest
                        (list <string>)
                        (lambda (str)(substring str 1 (string-length str)))
                        debug-name: 'rest)

(%add-primitive-method! bard:rest
                        (list <alist-table>)
                        (lambda (tbl)(%make-alist-table (cdr (alist-table-slots tbl))))
                        debug-name: 'rest)

;;; ---------------------------------------------------------------------
;;; second
;;; ---------------------------------------------------------------------

(define bard:second (make-function debug-name: 'second
                                   signatures: (list (signature (List) #f (Anything)))))

(%add-primitive-method! bard:second
                        (list <pair>)
                        cadr
                        debug-name: 'second)

(%add-primitive-method! bard:second
                        (list <string>)
                        (lambda (s)(string-ref s 1))
                        debug-name: 'second)

(%add-primitive-method! bard:second
                        (list <alist-table>)
                        (lambda (tbl)(list-ref (alist-table-slots tbl) 1))
                        debug-name: 'second)

;;; ---------------------------------------------------------------------
;;; some?
;;; ---------------------------------------------------------------------

(define bard:some? (make-function debug-name: 'some?
                                  signatures: (list (signature (Applicable List) #f (Anything)))))

(define (%bard-some? test ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (%funcall test (car items))
            (car items)
            (loop (cdr items))))))

;;; <null>

(%add-primitive-method! bard:some?
                        (list Anything <null>)
                        (constantly '())
                        debug-name: 'some?)

;;; <pair>

(%add-primitive-method! bard:some?
                        (list <primitive> <pair>)
                        %bard-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <interpreted-method> <pair>)
                        %bard-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <function> <pair>)
                        %bard-some?
                        debug-name: 'some?)

;;; <string>

(define (%bard-string-some? fn str)
  (let ((ls (string->list str)))
    (%bard-some? fn ls)))

(%add-primitive-method! bard:some?
                        (list <primitive> <string>)
                        %bard-string-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <interpreted-method> <string>)
                        %bard-string-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <function> <string>)
                        %bard-string-some?
                        debug-name: 'some?)

;;; <alist-table>

(define (%bard-alist-table-some? fn tbl)
  (let ((ls (alist-table-slots tbl)))
    (%bard-some? fn ls)))

(%add-primitive-method! bard:some?
                        (list <primitive> <alist-table>)
                        %bard-alist-table-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <interpreted-method> <alist-table>)
                        %bard-alist-table-some?
                        debug-name: 'some?)

(%add-primitive-method! bard:some?
                        (list <function> <alist-table>)
                        %bard-alist-table-some?
                        debug-name: 'some?)

;;; ---------------------------------------------------------------------
;;; take
;;; ---------------------------------------------------------------------

(define bard:take (make-function debug-name: 'take
                                 signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:take
                        (list (%singleton 0)  <null>)
                        (constantly 0)
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <null>)
                        (lambda (n ls)(error (str "Can't take more items from the empty list")))
                        debug-name: 'take)

(define (%bard-list-take n ls)
  (let loop ((items ls)
             (i 0)
             (result '()))
    (if (>= i n)
        result
        (if (null? items)
            (error (string-append "Count out of bounds: " (%as-string n)))
            (loop (cdr items)(+ i 1)(append result (list (car items))))))))

(%add-primitive-method! bard:take
                        (list <fixnum>  <pair>)
                        %bard-list-take
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <string>)
                        (lambda (n str)(substring str 0 n))
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <alist-table>)
                        (lambda (n tbl)(%make-alist-table (%bard-list-take n (alist-table-slots tbl))))
                        debug-name: 'take)

(define (%bard-generator-take n gen)
  (let loop ((len (length (generator-results gen))))
    (if (< n len)
        (take n (reverse (generator-results gen)))
        (begin
          (next gen)
          (loop (+ len 1))))))

(%add-primitive-method! bard:take
                        (list <fixnum>  <generator>)
                        %bard-generator-take
                        debug-name: 'take)

;;; ---------------------------------------------------------------------
;;; take-by
;;; ---------------------------------------------------------------------

(define bard:take-by (make-function debug-name: 'take-by
                                    signatures: (list (signature (Integer Integer List) #f (List)))))

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <pair>)
                        take-by
                        debug-name: 'take-by)

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <string>)
                        (lambda (len advance s)(map (lambda (i)(list->string i))
                                                    (take-by len advance (string->list s))))
                        debug-name: 'take-by)

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <alist-table>)
                        (lambda (len advance tbl)
                          (let* ((inslots (alist-table-slots tbl))
                                 (outslots (take-by len advance inslots)))
                            (%make-alist-table outslots)))
                        debug-name: 'take-by)

;;; ---------------------------------------------------------------------
;;; take-one
;;; ---------------------------------------------------------------------

(define bard:take (make-function debug-name: 'take-one
                                 signatures: (list (signature (List) #f (List)))))

(%add-primitive-method! bard:take-one
                        (list <null>)
                        (lambda (ls)(error (str "Can't take more items from the empty list")))
                        debug-name: 'take-one)

(%add-primitive-method! bard:take-one
                        (list <pair>)
                        (lambda (ls)(list (car ls)))
                        debug-name: 'take)

(%add-primitive-method! bard:take-one
                        (list <string>)
                        (lambda (s)(list (string-ref s 0)))
                        debug-name: 'take)

(%add-primitive-method! bard:take-one
                        (list <fixnum>  <alist-table>)
                        (lambda (tbl)(%make-alist-table (list (car (alist-table-slots tbl)))))
                        debug-name: 'take)

(%add-primitive-method! bard:take-one
                        (list <fixnum>  <generator>)
                        (lambda (gen)(%bard-generator-take 1 gen))
                        debug-name: 'take-one)
