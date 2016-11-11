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

(declare (extended-bindings))
(##include "type-signature-macros.scm")
(##include "protocol-macros.scm")

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

(define-protocol-function Listing add-first
  signatures: (list (signature (Anything List) #f (List))))

(define-primitive-method add-first (Anything <null>) 
  cons)

(define-primitive-method add-first (Anything <pair>) 
  cons)

(define-primitive-method add-first (<character> <string>) 
  (lambda (ch str)(string-append (string ch) str)))

(define-primitive-method add-first (Anything <vector>) 
  (lambda (it vec)(vector-append (vector it) vec)))

(define-primitive-method add-first (<pair> <alist-table>) 
  (lambda (entry table)(%make-alist-table (cons entry (alist-table-slots table)))))

;;; ---------------------------------------------------------------------
;;; add-last
;;; ---------------------------------------------------------------------

(define-protocol-function Listing add-last
  signatures: (list (signature (List Anything) #f (List))))

(define-primitive-method add-last (<null> Anything) 
  (lambda (ls thing)(list thing)))

(define-primitive-method add-last (<pair> Anything) 
  (lambda (ls thing)
    (if (list? ls)
        (append ls (list thing))
        (error (str "Improper list: " ls)))))

(define-primitive-method add-last (<string> <character>) 
  (lambda (str ch)(string-append str (string ch))))

(define-primitive-method add-last (<vector> Anything) 
  (lambda (vec it)(vector-append vec (vector it))))

(define-primitive-method add-last (<alist-table> <pair>) 
  (lambda (table entry)
    (%make-alist-table (append (alist-table-slots table) (list entry)))))

;;; ---------------------------------------------------------------------
;;; any
;;; ---------------------------------------------------------------------

(define-protocol-function Listing any
  signatures: (list (signature (List) #f (Anything))))

(define-primitive-method any (<null>) 
  (constantly '()))

(define-primitive-method any (<pair>) 
  (lambda (ls)
    (if (list? ls)
        (list-ref ls (random-integer (length ls)))
        (error (str "Improper list: " ls)))))

(define-primitive-method any (<string>) 
  (lambda (str)(string-ref str (random-integer (string-length str)))))

(define-primitive-method any (<vector>) 
  (lambda (vec)(vector-ref vec (random-integer (vector-length vec)))))

(define-primitive-method any (<alist-table>) 
  (lambda (table)(alist-table-get table (car (any (alist-table-keys table))))))

(define-primitive-method any (<generator>) 
  (lambda (gen)
    (if (null? (generator-instance-results gen))
        (next gen))
    (if (null? (generator-instance-results gen))
        '()
        (list-ref (generator-instance-results gen)
                  (random-integer (length (generator-instance-results gen)))))))

;;; ---------------------------------------------------------------------
;;; append
;;; ---------------------------------------------------------------------

(define-protocol-function Listing append
  signatures: (list (signature (List List) #f (List))))

(define-primitive-method append (<null> <null>) 
  (constantly '()))

(define-primitive-method append (<null> <pair>)
  (lambda (n p) p))

(define-primitive-method append (<pair> <null>)
  (lambda (p n) p))

(define-primitive-method append (<pair> <pair>)
  append)

(define-primitive-method append (<string> <string>)
  string-append)

(define-primitive-method append (<vector> <vector>)
  vector-append)

(define-primitive-method append (<alist-table> <alist-table>)
  (lambda (a b) (%make-alist-table (append (alist-table-slots a)(alist-table-slots b)))))

;;; ---------------------------------------------------------------------
;;; by
;;; ---------------------------------------------------------------------

(define-protocol-function Listing by
  signatures: (list (signature (Integer List) #f (List))))

(define-primitive-method by (<fixnum> <null>)
  (constantly '()))

(define-primitive-method by (<fixnum> <pair>)
  by)

(define-primitive-method by (<fixnum> <string>)
  (lambda (n s)(map (lambda (i)(list->string i))
                    (by n (string->list s)))))

(define-primitive-method by (<fixnum> <vector>)
  (lambda (n s)
    (list->vector
     (map (lambda (i)(list->vector i))
          (by n (vector->list s))))))

(define-primitive-method by (<fixnum> <alist-table>)
  (lambda (n s)(by n (alist-table-slots s))))

(define-primitive-method by (<fixnum> <generator>)
  (lambda (n g)
    (%eval `(generate ()
                      (yield (take-n n g))
                      (resume))
           '())))

;;; ---------------------------------------------------------------------
;;; drop
;;; ---------------------------------------------------------------------

(define-protocol-function Listing drop
  signatures: (list (signature (Integer List) #f (List))))

(define-primitive-method drop (<fixnum> <null>)
  (lambda (n ls)
    (if (= n 0)
        ls
        (error (str "Can't drop more elements from the empty list")))))

(define-primitive-method drop (<fixnum> <pair>)
  (lambda (n ls)
    (let loop ((items ls)
               (i 0))
      (if (>= i n)
          items
          (if (null? items)
              (error (string-append "Count out of bounds: " (%as-string n)))
              (loop (cdr items)(+ i 1)))))))

(define-primitive-method drop (<fixnum> <string>)
  (lambda (n str)(substring str n (string-length str))))

(define-primitive-method drop (<fixnum> <vector>)
  (lambda (n vec)(subvector vec n (vector-length vec))))

(define-primitive-method drop (<fixnum> <alist-table>)
  (lambda (n s)(%make-alist-table (drop n (alist-table-slots s)))))

;;; ---------------------------------------------------------------------
;;; element
;;; ---------------------------------------------------------------------

(define-protocol-function Listing element
  signatures: (list (signature (List Integer) #f (Anything))))

(define-primitive-method element (<null> <fixnum>)
  (lambda (ls n)(error (str "element index out of range"))))

(define (%bard-list-element ls n)
  (if (= n 0)
      (car ls)
      (if (pair? (cdr ls))
          (%bard-list-element (cdr ls) (- n 1))
          (else: (if (= n 1)
                     (cdr ls)
                     (error (str "element index out of range")))))))

(define-primitive-method element (<pair> <fixnum>)
  %bard-list-element)

(define-primitive-method element (<string> <fixnum>)
  string-ref)

(define-primitive-method element (<vector> <fixnum>)
  vector-ref)

(define-primitive-method element (<alist-table> <fixnum>)
  (lambda (table n)
    (let* ((keys (alist-table-keys table))
           (key (list-ref keys n)))
      (alist-table-get table key))))

(define (%bard-generator-element gen n)
  (let loop ((len (length (generator-results gen))))
    (if (< n len)
        (list-ref (generator-results gen)
                  (- len (+ n 1)))
        (begin
          (next gen)
          (loop (+ len 1))))))

(define-primitive-method element (<generator> <fixnum>)
  %bard-generator-element)

;;; ---------------------------------------------------------------------
;;; empty?
;;; ---------------------------------------------------------------------

(define (%empty? x)
  (or (eq? x #!unbound)
      (null? x)
      (equal? x "")
      (and (alist-table-instance? x)
           (zero? (length (alist-table-slots x))))))

(define-protocol-function Listing empty?
  signatures: (list (signature (List) #f (Boolean))))

(define-primitive-method empty? (<null>)
  (constantly #t))

(define-primitive-method empty? (<pair>)
  (constantly #f))

(define-primitive-method empty? (<string>)
  (lambda (str)(<= (string-length str) 0)))

(define-primitive-method empty? (<vector>)
  (lambda (vec)(<= (vector-length vec) 0)))

(define-primitive-method empty? (<alist-table>)
  (lambda (table)(null? (alist-table-slots table))))

(define-primitive-method empty? (<generator>)
  (constantly #f))

;;; ---------------------------------------------------------------------
;;; filter
;;; ---------------------------------------------------------------------

(define-protocol-function Listing filter
  signatures: (list (signature (Applicable List) #f (List))))

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

(define-primitive-method filter (Anything <null>)
  (constantly '()))

;;; <pair>

(define-primitive-method filter (<primitive> <pair>)
  %bard-filter-list)

(define-primitive-method filter (<interpreted-method> <pair>)
  %bard-filter-list)

(define-primitive-method filter (<function> <pair>)
  %bard-filter-list)

;;; <string>

(define (%bard-filter-string fn str)
  (let* ((inchars (string->list str))
         (outchars (%bard-filter-list fn inchars)))
    (list->string outchars)))

(define-primitive-method filter (<primitive> <string>)
  %bard-filter-string)

(define-primitive-method filter (<interpreted-method> <string>)
  %bard-filter-string)

(define-primitive-method filter (<function> <string>)
  %bard-filter-string)

;;; <vector>

(define (%bard-filter-vector fn vec)
  (let* ((invals (vector->list vec))
         (outvals (%bard-filter-list fn invals)))
    (list->vector outvals)))

(define-primitive-method filter (<primitive> <vector>)
  %bard-filter-vector)

(define-primitive-method filter (<interpreted-method> <vector>)
  %bard-filter-vector)

(define-primitive-method filter (<function> <vector>)
  %bard-filter-vector)

;;; <alist-table>

(define (%bard-filter-alist-table fn table)
  (let* ((inslots (alist-table-slots table))
         (outslots (%bard-filter-list fn inslots)))
    (%make-alist-table outslots)))

(define-primitive-method filter (<primitive> <alist-table>)
  %bard-filter-alist-table)

(define-primitive-method filter (<interpreted-method> <alist-table>)
  %bard-filter-alist-table)

(define-primitive-method filter (<function> <alist-table>)
  %bard-filter-alist-table)

;;; <alist-table>

(define (%bard-filter-generator fn gen)
  (%eval `(generate ((f ,fn)
                     (g ,gen))
                    (loop again ((x (next g)))
                          (if (,fn x)
                              (begin
                                (yield x)
                                (resume f g))
                              (again (next g)))))
         '()))

(define-primitive-method filter (<primitive> <generator>)
  %bard-filter-generator)

(define-primitive-method filter (<interpreted-method> <generator>)
  %bard-filter-generator)

(define-primitive-method filter (<function> <generator>)
  %bard-filter-generator)


;;; ---------------------------------------------------------------------
;;; first
;;; ---------------------------------------------------------------------

(define-protocol-function Listing first
  signatures: (list (signature (List) #f (Anything))))

(define-primitive-method first (<pair>)
  car)

(define-primitive-method first (<string>)
  (lambda (s)(string-ref s 0)))

(define-primitive-method first (<vector>)
  (lambda (v)(vector-ref v 0)))

(define-primitive-method first (<alist-table>)
  (lambda (table)(car (alist-table-slots table))))

(define-primitive-method first (<generator>)
  (lambda (gen)
    (if (null? (generator-results gen))
        (next gen))
    (list-ref (generator-results gen)
              (- (length (generator-results gen)) 1))))


;;; ---------------------------------------------------------------------
;;; last
;;; ---------------------------------------------------------------------

(define-protocol-function Listing last
  signatures: (list (signature (List) #f (Anything))))

(define (%pair-last p)
  (if (null? p)
      '()
      (if (list? p)
          (list-ref p (- (length p) 1))
          (cdr p))))

(define-primitive-method last (<pair>)
  %pair-last)

(define-primitive-method last (<string>)
  (lambda (string)(string-ref string (- (string-length string) 1))))

(define-primitive-method last (<vector>)
  (lambda (vec)(vector-ref vec (- (vector-length vec) 1))))

(define-primitive-method last (<alist-table>)
  (lambda (table)
    (%pair-last (alist-table-slots table))))

;;; ---------------------------------------------------------------------
;;; length
;;; ---------------------------------------------------------------------

(define-protocol-function Listing length
  signatures: (list (signature (List) #f (Integer))))

(define-primitive-method length (<null>)
  (constantly 0))

(define-primitive-method length (<pair>)
  length)

(define-primitive-method length (<string>)
  string-length)

(define-primitive-method length (<vector>)
  vector-length)

(define-primitive-method length (<alist-table>)
  (lambda (table)(length (alist-table-slots table))))

;;; ---------------------------------------------------------------------
;;; member?
;;; ---------------------------------------------------------------------

(define-protocol-function Listing member?
  signatures: (list (signature (Anything List) #f (Boolean))
                    (signature (Function Protocol) #f (Boolean))
                    (signature (Schema Class) #f (Boolean))))

(define-primitive-method member? (Anything <null>)
  (constantly #f))

(define-primitive-method member? (Anything <pair>)
  (lambda (x ls)(and (member x ls) #t)))

(define-primitive-method member? (Anything <string>)
  (constantly #f))

(define-primitive-method member? (<character> <string>)
  (lambda (ch str)(and (string-char-position ch str) #t)))

(define-primitive-method member? (Anything <vector>)
  (lambda (it vec)(and (vector-position (lambda (e)(equal? it e))
                                        vec)
                       #t)))

(define-primitive-method member? (<pair> <alist-table>)
  (lambda (p tbl)
    (and (some? (lambda (x slot)(and (equal? (car x)(car slot))
                                     (equal? (cdr x)(cdr slot))))
                (alist-table-slots tbl))
         #t)))

(define-primitive-method member? (<function> <protocol>)
  (lambda (fn pro)
    (let* ((fname (function-instance-name fn))
           (found (and fname (%protocol-ref pro fname))))
      (and found
           (or (and (null? (function-signatures fn))
                    (null? (function-signatures found)))
               (signature-congruent? (car (function-signatures fn))
                                     (car (function-signatures found))))))))

;;; ---------------------------------------------------------------------
;;; next-last
;;; ---------------------------------------------------------------------

(define-protocol-function Listing next-last
  signatures: (list (signature (List) #f (Anything))))

(define-primitive-method next-last (<pair>)
  next-last)

(define-primitive-method next-last (<string>)
  string-next-last)

(define-primitive-method next-last (<vector>)
  vector-next-last)

(define-primitive-method next-last (<alist-table>)
  (lambda (table)(next-last (alist-table-slots table))))

;;; ---------------------------------------------------------------------
;;; position
;;; ---------------------------------------------------------------------

(define-protocol-function Listing position
  signatures: (list (signature (Anything List) #f (Anything))))

(define-primitive-method position (Anything <null>)
  (constantly '()))

(define-primitive-method position (Anything <pair>)
  (lambda (x p)
    (if (list? p)
        (or (position (lambda (i)(equal? x i)) p)
            '())
        (cond
         ((equal? x (car p)) 'left)
         ((equal? x (cdr p)) 'right)
         (else '())))))

(define-primitive-method position (Anything <string>)
  (constantly '()))

(define-primitive-method position (<character> <string>)
  (lambda (ch str)(or (string-char-position ch str) '())))

(define-primitive-method position (Anything <vector>)
  (lambda (it vec)(or (vector-position (lambda (e)(= e it)) vec) '())))

(define-primitive-method position (<pair> <alist-table>)
  (lambda (entry tbl)
    (or (position (lambda (e)(equal? (car entry)(car e)))
                  (alist-table-slots tbl))
        '())))

;;; ---------------------------------------------------------------------
;;; position-if
;;; ---------------------------------------------------------------------

(define-protocol-function Listing position-if
  signatures: (list (signature (Applicable List) #f (Anything))))

(define-primitive-method position-if (Anything <null>)
  (constantly '()))

(define-primitive-method position-if (Anything <pair>)
  (lambda (test p)
    (let ((test (lambda (x)(%funcall test x))))
      (if (list? p)
          (or (position test p) '())
          (cond
           ((test (car p)) 'left)
           ((test (cdr p)) 'right)
           (else '()))))))

(define-primitive-method position-if (Anything <string>)
  (lambda (test str)
    (let ((test (lambda (x)(%funcall test x))))
      (or (string-char-position-if test str) '()))))

(define-primitive-method position-if (Anything <vector>)
  (lambda (test vec)
    (let ((test (lambda (x)(%funcall test x))))
      (or (vector-position test vec) '()))))

(define-primitive-method position-if (<pair> <alist-table>)
  (lambda (test tbl)
    (let ((test (lambda (x)(%funcall test x))))
      (or (position (lambda (e)(test e))
                    (alist-table-slots tbl))
          '()))))

;;; ---------------------------------------------------------------------
;;; rest 
;;; ---------------------------------------------------------------------

(define-protocol-function Listing rest
  signatures: (list (signature (List) #f (List))))

(define-primitive-method rest (<null>)
  (constantly '()))

(define-primitive-method rest (<pair>)
  cdr)

(define-primitive-method rest (<string>)
  (lambda (str)(substring str 1 (string-length str))))

(define-primitive-method rest (<vector>)
  (lambda (vec)(subvector vec 1 (vector-length vec))))

(define-primitive-method rest (<alist-table>)
  (lambda (tbl)(%make-alist-table (cdr (alist-table-slots tbl)))))

(define-primitive-method rest (<generator>)
  (lambda (gen)
    (%eval `(generate ((first-time? true))
                      (if first-time?
                          (begin
                            (next ,gen)
                            (yield (next ,gen))
                            (resume false))
                          (begin
                            (yield (next ,gen))
                            (resume false))))
           '())))

;;; ---------------------------------------------------------------------
;;; reverse 
;;; ---------------------------------------------------------------------

(define-protocol-function Listing reverse
  signatures: (list (signature (List) #f (List))))

(define-primitive-method reverse (<null>)
  (constantly '()))

(define-primitive-method reverse (<pair>)
  reverse)

(define-primitive-method reverse (<string>)
  (lambda (str)(list->string (reverse (string->list str)))))

(define-primitive-method reverse (<vector>)
  (lambda (vec)(list->vector (reverse (vector->list vec)))))

;;; ---------------------------------------------------------------------
;;; second
;;; ---------------------------------------------------------------------

(define-protocol-function Listing second
  signatures: (list (signature (List) #f (Anything))))

(define-primitive-method second (<pair>)
  cadr)

(define-primitive-method second (<string>)
  (lambda (s)(string-ref s 1)))

(define-primitive-method second (<vector>)
  (lambda (v)(vector-ref v 1)))

(define-primitive-method second (<alist-table>)
  (lambda (tbl)(list-ref (alist-table-slots tbl) 1)))

(define-primitive-method second (<generator>)
  (lambda (gen)(%bard-generator-element gen 1)))

;;; ---------------------------------------------------------------------
;;; some?
;;; ---------------------------------------------------------------------

(define-protocol-function Listing some?
  signatures: (list (signature (Applicable List) #f (Anything))))

(define (%bard-some? test ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (%funcall test (car items))
            (car items)
            (loop (cdr items))))))

;;; <null>

(define-primitive-method some? (Anything <null>)
  (constantly '()))

;;; <pair>

(define-primitive-method some? (<primitive> <pair>)
  %bard-some?)

(define-primitive-method some? (<interpreted-method> <pair>)
  %bard-some?)

(define-primitive-method some? (<function> <pair>)
  %bard-some?)

;;; <string>

(define (%bard-string-some? fn str)
  (let ((ls (string->list str)))
    (%bard-some? fn ls)))

(define-primitive-method some? (<primitive> <string>)
  %bard-string-some?)

(define-primitive-method some? (<interpreted-method> <string>)
  %bard-string-some?)

(define-primitive-method some? (<function> <string>)
  %bard-string-some?)

;;; <vector>

(define (%bard-vector-some? fn vec)
  (let ((ls (vector->list vec)))
    (%bard-some? fn ls)))

(define-primitive-method some? (<primitive> <vector>)
  %bard-vector-some?)

(define-primitive-method some? (<interpreted-method> <vector>)
  %bard-vector-some?)

(define-primitive-method some? (<function> <vector>)
  %bard-vector-some?)

;;; <alist-table>

(define (%bard-alist-table-some? fn tbl)
  (let ((ls (alist-table-slots tbl)))
    (%bard-some? fn ls)))

(define-primitive-method some? (<primitive> <alist-table>)
  %bard-alist-table-some?)

(define-primitive-method some? (<interpreted-method> <alist-table>)
  %bard-alist-table-some?)

(define-primitive-method some? (<function> <alist-table>)
  %bard-alist-table-some?)

;;; ---------------------------------------------------------------------
;;; take
;;; ---------------------------------------------------------------------

(define-protocol-function Listing take
  signatures: (list (signature (Integer List) #f (List))))

(define-primitive-method take (list (%singleton 0)  <null>)
  (constantly 0))

(define-primitive-method take (<fixnum> <null>)
  (lambda (n ls)(error (str "Can't take more items from the empty list"))))

(define (%bard-list-take n ls)
  (let loop ((items ls)
             (i 0)
             (result '()))
    (if (>= i n)
        result
        (if (null? items)
            (error (string-append "Count out of bounds: " (%as-string n)))
            (loop (cdr items)(+ i 1)(append result (list (car items))))))))

(define-primitive-method take (<fixnum> <pair>)
  %bard-list-take)

(define-primitive-method take (<fixnum> <string>)
  (lambda (n str)(substring str 0 n)))

(define-primitive-method take (<fixnum> <vector>)
  (lambda (n vec)(subvector vec 0 n)))

(define-primitive-method take (<fixnum> <alist-table>)
  (lambda (n tbl)(%make-alist-table (%bard-list-take n (alist-table-slots tbl)))))

(define (%bard-generator-take n gen)
  (let loop ((len (length (generator-results gen))))
    (if (< n len)
        (take n (reverse (generator-results gen)))
        (begin
          (next gen)
          (loop (+ len 1))))))

(define-primitive-method take (<fixnum> <generator>)
  %bard-generator-take)

;;; ---------------------------------------------------------------------
;;; take-by
;;; ---------------------------------------------------------------------

(define-protocol-function Listing take-by
  signatures: (list (signature (Integer Integer List) #f (List))))

(define-primitive-method take-by (<fixnum> <fixnum> <pair>)
  take-by)

(define-primitive-method take-by (<fixnum> <fixnum> <string>)
  (lambda (len advance s)(map (lambda (i)(list->string i))
                              (take-by len advance (string->list s)))))

(define-primitive-method take-by (<fixnum> <fixnum> <vector>)
  (lambda (len advance v)(map (lambda (i)(list->vector i))
                              (take-by len advance (vector->list v)))))

(define-primitive-method take-by (<fixnum> <fixnum> <alist-table>)
  (lambda (len advance tbl)
    (let* ((inslots (alist-table-slots tbl))
           (outslots (take-by len advance inslots)))
      (%make-alist-table outslots))))

;;; ---------------------------------------------------------------------
;;; take-one
;;; ---------------------------------------------------------------------

(define-protocol-function Listing take-one
  signatures: (list (signature (List) #f (List))))

(define-primitive-method take-one (<null>)
  (lambda (ls)(error (str "Can't take more items from the empty list"))))

(define-primitive-method take-one (<pair>)
  (lambda (ls)(list (car ls))))

(define-primitive-method take-one (<string>)
  (lambda (s)(list (string-ref s 0))))

(define-primitive-method take-one (<vector>)
  (lambda (s)(vector (vector-ref s 0))))

(define-primitive-method take-one (<fixnum> <alist-table>)
  (lambda (tbl)(%make-alist-table (list (car (alist-table-slots tbl))))))

(define-primitive-method take-one (<fixnum> <generator>)
  (lambda (gen)(%bard-generator-take 1 gen)))
