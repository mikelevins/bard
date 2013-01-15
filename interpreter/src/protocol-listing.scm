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

;;; ---------------------------------------------------------------------
;;; by
;;; ---------------------------------------------------------------------

(define bard:by (make-function debug-name: 'by
                               signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:by
                        (list <fixnum> <pair>)
                        by
                        debug-name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <string>)
                        (lambda (n s)(map (lambda (i)(list->string i))
                                          (by n (string->list s))))
                        debug-name: 'by)

;;; ---------------------------------------------------------------------
;;; drop
;;; ---------------------------------------------------------------------

(define bard:drop (make-function debug-name: 'drop
                                 signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:drop
                        (list <fixnum>  <pair>)
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

;;; ---------------------------------------------------------------------
;;; element
;;; ---------------------------------------------------------------------

(define bard:element (make-function debug-name: 'element
                                    signatures: (list (signature (List Integer) #f (Anything)))))

(%add-primitive-method! bard:element
                        (list <pair> <fixnum>)
                        list-ref
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

;;; ---------------------------------------------------------------------
;;; filter
;;; ---------------------------------------------------------------------

(define bard:filter (make-function debug-name: 'filter
                                   signatures: (list (signature (Applicable List) #f (List)))))

(define (%bard-filter test ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (if (%funcall test (car items))
            (loop (cdr items)
                  (cons (car items) result))
            (loop (cdr items)
                  result)))))

(%add-primitive-method! bard:filter
                        (list Anything <null>)
                        (constantly '())
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <primitive> <pair>)
                        %bard-filter
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <interpreted-method> <pair>)
                        %bard-filter
                        debug-name: 'filter)

(%add-primitive-method! bard:filter
                        (list <function> <pair>)
                        %bard-filter
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
                        (list <generator>)
                        (lambda (gen)
                          (if (null? (generator-results gen))
                              (next gen))
                          (list-ref (generator-results gen)
                                    (- (length (generator-results gen)) 1)))
                        debug-name: 'first)

;;; ---------------------------------------------------------------------
;;; join
;;; ---------------------------------------------------------------------

(define (%bard-join-text cupola strs)
  (if (null? strs)
      ""
      (apply string-append 
             (cdr (apply append (map (lambda (s)(list cupola s))
                                     strs))))))

(define bard:join-text (make-function debug-name: 'join-text
                                      signatures: (list (signature (Text List) #f (Text)))))

(%add-primitive-method! bard:join-text
                        (list <string> <pair>)
                        %bard-join-text
                        debug-name: 'join-text)


;;; ---------------------------------------------------------------------
;;; last
;;; ---------------------------------------------------------------------

(define bard:last (make-function debug-name: 'last
                                 signatures: (list (signature (List) #f (Anything)))))

(define (%pair-last p)
  (if (null? (cdr p))
      (car p)
      (%pair-last (cdr p))))

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
                          (let loop ((slots (alist-table-slots table))
                                     (already '()))
                            (if (null? slots)
                                '()
                                (let ((slot (car slots))
                                      (more (cdr slots)))
                                  (if (member (car slot) already)
                                      (loop more already)
                                      (if (null? more)
                                          (car slot)
                                          (loop more (cons (car slot) already))))))))
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
;;; map
;;; ---------------------------------------------------------------------

(define bard:map (make-function debug-name: 'map
                                signatures: (list (signature (Applicable List) #f (List)))))

;;; <null>
;;; ---------------------------------------------------------------------

(%add-primitive-method! bard:map
                        (list <primitive> <null>)
                        (constantly '())
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <null>)
                        (constantly '())
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <null>)
                        (constantly '())
                        debug-name: 'map)

;;; <pair>
;;; ---------------------------------------------------------------------

(define (%bard-map-list fn ls)
  (let loop ((items ls)
             (result '()))
    (if (null? items)
        (reverse result)
        (loop (cdr items)
              (cons (%funcall fn (car items))
                    result)))))

(%add-primitive-method! bard:map
                        (list <primitive> <pair>)
                        %bard-map-list
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <pair>)
                        %bard-map-list
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <pair>)
                        %bard-map-list
                        debug-name: 'map)

;;; <string>
;;; ---------------------------------------------------------------------

(define (%bard-map-string fn str)
  (let ((len (string-length str)))
    (let loop ((i 0)
               (out '()))
      (if (>= i len)
          (let ((out (reverse out)))
            (if (every? char? out)
                (list->string out)
                out))
          (loop (+ i 1)
                (cons (%funcall fn (string-ref str i))
                      out))))))

(%add-primitive-method! bard:map
                        (list <primitive> <string>)
                        %bard-map-string
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <string>)
                        %bard-map-string
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <string>)
                        %bard-map-string
                        debug-name: 'map)

;;; <alist-table>
;;; ---------------------------------------------------------------------

(define (%bard-map-alist-table fn fr)
  (let loop ((ks (alist-table-keys fr))
             (out '()))
    (if (null? ks)
        (%make-alist-table (reverse out))
        (let* ((k (car ks))
               (v (alist-table-get fr k)))
          (loop (cdr ks)
                (cons (%funcall fn k v) out))))))

(%add-primitive-method! bard:map
                        (list <primitive> <alist-table>)
                        %bard-map-alist-table
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <alist-table>)
                        %bard-map-alist-table
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <alist-table>)
                        %bard-map-alist-table
                        debug-name: 'map)

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

;;; ---------------------------------------------------------------------
;;; partition
;;; ---------------------------------------------------------------------

(define bard:partition (make-function debug-name: 'parition
                                      signatures: (list (signature () 'functions (List &)))))

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

(%add-primitive-method! bard:some?
                        (list Anything <null>)
                        (constantly '())
                        debug-name: 'some?)

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

;;; ---------------------------------------------------------------------
;;; split
;;; ---------------------------------------------------------------------

(define (%bard-split-string str ch)
  (let ((len (string-length str)))
    (let loop ((i 0)
               (last-found #f)
               (chunks '()))
      (if (< i len)
          (let ((foundch (string-ref str i)))
            (if (char=? foundch ch)
                (if last-found
                    (let ((chunk (substring str (+ 1 last-found) i)))
                      (loop (+ i 1) i (cons chunk chunks)))
                    (let ((chunk (substring str 0 i)))
                      (loop (+ i 1) i (cons chunk chunks))))
                (loop (+ i 1) last-found chunks)))
          (if last-found
              (let ((chunk (substring str (+ 1 last-found) i)))
                (reverse (cons chunk chunks)))
              (let ((chunk (substring str 0 i)))
                (reverse (cons chunk chunks))))))))

(define bard:split-text (make-function debug-name: 'split-text
                                       signatures: (list (signature (Text Character) #f (List)))))

(%add-primitive-method! bard:split-text
                        (list <string>  <character>)
                        %bard-split-string
                        debug-name: 'split-text)

;;; ---------------------------------------------------------------------
;;; take
;;; ---------------------------------------------------------------------

(define bard:take (make-function debug-name: 'take
                                 signatures: (list (signature (Integer List) #f (List)))))

(%add-primitive-method! bard:take
                        (list <fixnum>  <pair>)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0)
                                     (result '()))
                            (if (>= i n)
                                result
                                (if (null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1)(append result (list (car items))))))))
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <string>)
                        (lambda (n str)(substring str 0 n))
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <generator>)
                        (lambda (n gen)
                          (let loop ((len (length (generator-results gen))))
                            (if (< n len)
                                (take n (reverse (generator-results gen)))
                                (begin
                                  (next gen)
                                  (loop (+ len 1))))))
                        debug-name: 'take)

;;; ---------------------------------------------------------------------
;;; take-by
;;; ---------------------------------------------------------------------

(define bard:take-by (make-function debug-name: 'take-by
                                    signatures: (list (signature (Integer Integer List) #f (List)))))

(%add-primitive-method! bard:take-by
                        (list <fixnum>  <fixnum> <pair>)
                        take-by
                        debug-name: 'take-by)

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <string>)
                        (lambda (len advance s)(map (lambda (i)(list->string i))
                                                    (take-by len advance (string->list s))))
                        debug-name: 'take-by)

