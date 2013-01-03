;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocols.scm
;;;; Project:       Bard
;;;; Purpose:       built-in protocols
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


;;; ---------------------------------------------------------------------
;;; Common utilities
;;; ---------------------------------------------------------------------

(define %yes (constantly #t))
(define %no (constantly #f))

;;; ---------------------------------------------------------------------
;;; Applicable
;;; ---------------------------------------------------------------------

(define (%applicable? thing)
  (or 
   (%keyed-collection? thing)
   (%primitive-method? thing)
   (%interpreted-method? thing)
   (%function? thing)
   (procedure? thing)))

(define bard:applicable?
  (make-primitive 
   procedure: %applicable?
   debug-name: 'applicable?
   required-count: 1
   restarg: #f))

(define bard:apply
  (make-primitive
   procedure: %apply
   debug-name: 'apply
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; As
;;; ---------------------------------------------------------------------

(define bard:as (make-function debug-name: 'as))

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <symbol>) 
                        (lambda (type thing)(symbol->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <symbol>) <string>)
                        (lambda (type thing)(string->symbol thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <keyword>)
                        (lambda (type thing)(keyword->string thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <keyword>) <string>)
                        (lambda (type thing)(string->keyword thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <pair>) <string>)
                        (lambda (type thing)(string->list thing))
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <null>)
                        (constantly "")
                        debug-name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <pair>)
                        (lambda (type thing)(list->string (%bard-list->cons thing)))
                        debug-name: 'as)

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(define bard:boolean?
  (make-primitive 
   procedure: boolean?
   debug-name: 'boolean?
   required-count: 1
   restarg: #f))

(define (%false? x)
  (or (eqv? x #f)
      (null? x)))

(define bard:false?
  (make-primitive 
   procedure: %false?
   debug-name: 'false?
   required-count: 1
   restarg: #f))

(define (%true? x)
  (and (not (%false? x))
       (not (eqv? x #!unbound))))

(define bard:true?
  (make-primitive 
   procedure: %true?
   debug-name: 'true?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

(define bard:char?
  (make-primitive
   procedure: char?
   debug-name: 'char?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Equal
;;; ---------------------------------------------------------------------

(define bard:= (make-function debug-name: '=))

(%add-primitive-method! bard:=
                        (list Anything Anything)
                        equal?
                        debug-name: '=)

;;; ---------------------------------------------------------------------
;;; Float
;;; ---------------------------------------------------------------------

(define bard:float?
  (make-primitive
   procedure: flonum?
   debug-name: 'float?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; ForeignValue
;;; ---------------------------------------------------------------------

(define bard:foreign-value?
  (make-primitive
   procedure: ##foreign?
   debug-name: 'foreign-value?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Function
;;; ---------------------------------------------------------------------

(define bard:function?
  (make-primitive
   procedure: function?
   debug-name: 'function?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; IOStream
;;; ---------------------------------------------------------------------

(define bard:current-input
  (make-primitive
   procedure: current-input-port
   debug-name: 'current-input
   required-count: 0
   restarg: #f))

(define bard:current-output
  (make-primitive
   procedure: current-output-port
   debug-name: 'current-output
   required-count: 0
   restarg: #f))

(define bard:display
  (make-primitive
   procedure: display
   debug-name: 'display
   required-count: 1
   restarg: #f))

(define bard:input-stream?
  (make-primitive
   procedure: input-port?
   debug-name: 'input-stream?
   required-count: 1
   restarg: #f))

(define bard:iostream?
  (make-primitive
   procedure: (lambda (x)(or (input-port? x)(output-port? x)))
   debug-name: 'iostream?
   required-count: 1
   restarg: #f))

(define bard:output-stream?
  (make-primitive
   procedure: output-port?
   debug-name: 'output-stream?
   required-count: 1
   restarg: #f))

(define bard:read-line
  (make-primitive
   procedure: read-line
   debug-name: 'read-line
   required-count: 1
   restarg: #f))

(define (%bard-read-lines in)
  (cond
   ((input-port? in)(read-all in read-line))
   ((string? in)(call-with-input-string in (lambda (stream)(read-all stream read-line))))
   (else (error (string-append "Invalid argument to read-lines: "
                               (object->string in))))))

(define bard:read-lines
  (make-primitive
   procedure: %bard-read-lines
   debug-name: 'read-lines
   required-count: 1
   restarg: #f))

(define (%bard-write data out)
  (let ((data (if (string? data)
                  data
                  (%as-string data))))
    (cond
     ((output-port? out)(write-substring data 0 (string-length data) out))
     ((string? out)(call-with-output-file out 
                     (lambda (out)
                       (write-substring data 0 (string-length data) out))))
     (else (error (string-append "Invalid output argument to write: "
                                 (object->string out)))))))

(define bard:write
  (make-primitive
   procedure: %bard-write
   debug-name: 'write
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Integer
;;; ---------------------------------------------------------------------

(define bard:integer?
  (make-primitive
   procedure: integer?
   debug-name: 'integer?
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

;;; add-first

(define bard:add-first (make-function debug-name: 'add-first))

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

;;; add-last

(define bard:add-last (make-function debug-name: 'add-last))

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

;;; any

(define bard:any (make-function debug-name: 'any))

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

;;; append

(define bard:append (make-function debug-name: 'append))

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

;;; next-last

(define bard:next-last (make-function debug-name: 'next-last))

(%add-primitive-method! bard:next-last
                        (list <pair>)
                        next-last
                        debug-name: 'next-last)

(%add-primitive-method! bard:next-last
                        (list <string>)
                        string-next-last
                        debug-name: 'next-last)

;;; by

(define bard:by (make-function debug-name: 'by))

(%add-primitive-method! bard:by
                        (list <fixnum> <pair>)
                        by
                        debug-name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <string>)
                        (lambda (n s)(map (lambda (i)(list->string i))
                                          (by n (string->list s))))
                        debug-name: 'by)

;;; drop

(define bard:drop (make-function debug-name: 'drop))

(%add-primitive-method! bard:drop
                        (list <fixnum>  <pair>)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0))
                            (if (>= i n)
                                items
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1))))))
                        debug-name: 'drop)

(%add-primitive-method! bard:drop
                        (list <fixnum>  <string>)
                        (lambda (n str)(substring str n (string-length str)))
                        debug-name: 'drop)

;;; element

(define bard:element (make-function debug-name: 'element))

(%add-primitive-method! bard:element
                        (list <pair> <fixnum>)
                        list-ref
                        debug-name: 'element)

(%add-primitive-method! bard:element
                        (list <string> <fixnum>)
                        string-ref
                        debug-name: 'element)

;;; empty?

(define bard:empty? (make-function debug-name: 'empty?))

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

;;; filter

(define bard:filter (make-function debug-name: 'filter))

(define (%bard-filter test ls)
  (let loop ((items ls)
             (result '()))
    (if (%null? items)
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

;;; first

(define bard:first (make-function debug-name: 'first))

(%add-primitive-method! bard:first
                        (list <pair>)
                        car
                        debug-name: 'first)

(%add-primitive-method! bard:first
                        (list <string>)
                        (lambda (s)(string-ref s 0))
                        debug-name: 'first)

;;; join

(define (%bard-join-strings cupola strs)
  (if (null? strs)
      ""
      (apply string-append 
             (cdr (apply append (map (lambda (s)(list cupola s))
                                     strs))))))

(define bard:join-strings (make-function debug-name: 'join-strings))

(%add-primitive-method! bard:join-strings
                        (list <string>  <pair>)
                        %bard-join-strings
                        debug-name: 'join-strings)


;;; last

(define bard:last (make-function debug-name: 'last))

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

;;; length

(define bard:length (make-function debug-name: 'length))

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

;;; list?

(define bard:list? (make-function debug-name: 'list?))

(%add-primitive-method! bard:list?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <pair>)
                        (lambda (ls)(list? ls))
                        debug-name: 'list?)

(%add-primitive-method! bard:list?
                        (list <string>)
                        (constantly #t)
                        debug-name: 'list?)

;;; map

(define bard:map (make-function debug-name: 'map))

;;; <null>

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

;;; <list>

(define (%bard-map-list fn ls)
  (let loop ((items ls)
             (result '()))
    (if (%null? items)
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

(define (%bard-map-table fn fr)
  (let loop ((ks (%table-keys fr))
             (out '()))
    (if (%null? ks)
        (%maybe-slot-list->table (reverse out))
        (let* ((k (car ks))
               (v (%table-get fr k)))
          (loop (cdr ks)
                (cons (%funcall fn k v) out))))))

(%add-primitive-method! bard:map
                        (list <primitive> <alist-table>)
                        %bard-map-table
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <alist-table>)
                        %bard-map-table
                        debug-name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <alist-table>)
                        %bard-map-table
                        debug-name: 'map)

;;; reduce

(define bard:reduce (make-function debug-name: 'reduce))

(define (%bard-reduce fn init ls)
  (let loop ((items ls)
             (result init))
    (if (%null? items)
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
;;; rest 

(define bard:rest (make-function debug-name: 'rest))

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

;;; second

(define bard:second (make-function debug-name: 'second))

(%add-primitive-method! bard:second
                        (list <pair>)
                        cadr
                        debug-name: 'second)

(%add-primitive-method! bard:second
                        (list <string>)
                        (lambda (s)(string-ref s 1))
                        debug-name: 'second)

;;; some?

(define bard:some? (make-function debug-name: 'some?))

(define (%bard-some? test ls)
  (let loop ((items ls))
    (if (%null? items)
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

;;; split

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

(define bard:split-string (make-function debug-name: 'split-string))

(%add-primitive-method! bard:split-string
                        (list <string>  <character>)
                        %bard-split-string
                        debug-name: 'split-string)
;;; take

(define bard:take (make-function debug-name: 'take))

(%add-primitive-method! bard:take
                        (list <fixnum>  <pair>)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0)
                                     (result '()))
                            (if (>= i n)
                                result
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1)(append result (list (car items))))))))
                        debug-name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <string>)
                        (lambda (n str)(substring str 0 n))
                        debug-name: 'take)


;;; take-by

(define bard:take-by (make-function debug-name: 'take-by))

(%add-primitive-method! bard:take-by
                        (list <fixnum>  <fixnum> <pair>)
                        take-by
                        debug-name: 'take-by)

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <string>)
                        (lambda (len advance s)(map (lambda (i)(list->string i))
                                                    (take-by len advance (string->list s))))
                        debug-name: 'take-by)


;;; ---------------------------------------------------------------------
;;; Name
;;; ---------------------------------------------------------------------

(define bard:symbol? (make-function debug-name: 'symbol?))

(%add-primitive-method! bard:symbol?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'symbol?)

(%add-primitive-method! bard:symbol?
                        (list <symbol>)
                        (constantly #t)
                        debug-name: 'symbol?)

;;; ---------------------------------------------------------------------
;;; Null
;;; ---------------------------------------------------------------------

(define bard:nothing? (make-function debug-name: 'nothing?))

(%add-primitive-method! bard:nothing?
                        (list Anything)
                        (constantly #f)
                        debug-name: 'nothing?)

(%add-primitive-method! bard:nothing?
                        (list <null>)
                        (constantly #t)
                        debug-name: 'nothing?)

(define bard:something? (make-function debug-name: 'something?))

(%add-primitive-method! bard:something?
                        (list Anything)
                        (constantly #t)
                        debug-name: 'something?)

(%add-primitive-method! bard:something?
                        (list <null>)
                        (constantly #f)
                        debug-name: 'something?)

;;; ---------------------------------------------------------------------
;;; Ordered
;;; ---------------------------------------------------------------------

(define bard:< (make-function debug-name: '<))

(%add-primitive-method! bard:<
                        (list <fixnum> <fixnum>)
                        <
                        debug-name: '<)

(define bard:> (make-function debug-name: '>))

(%add-primitive-method! bard:>
                        (list <fixnum> <fixnum>)
                        >
                        debug-name: '>)

(define bard:<= (make-function debug-name: '<=))

(%add-primitive-method! bard:<=
                        (list <fixnum> <fixnum>)
                        <=
                        debug-name: '<=)

(define bard:>= (make-function debug-name: '>=))

(%add-primitive-method! bard:>=
                        (list <fixnum> <fixnum>)
                        >=
                        debug-name: '>=)

;;; ---------------------------------------------------------------------
;;; Pair
;;; ---------------------------------------------------------------------

;;; pair

(define bard:pair (make-function debug-name: 'pair))

(%add-primitive-method! bard:pair
                        (list Anything Anything)
                        cons
                        debug-name: 'pair)

;;; left

(define bard:left (make-function debug-name: 'left))

(%add-primitive-method! bard:left
                        (list <pair>)
                        car
                        debug-name: 'left)

;;; right

(define bard:right (make-function debug-name: 'right))

(%add-primitive-method! bard:right
                        (list <pair>)
                        cdr
                        debug-name: 'right)

;;; ---------------------------------------------------------------------
;;; Table
;;; ---------------------------------------------------------------------

;;; table?

(define bard:table?
  (make-primitive
   procedure: alist-table-instance?
   debug-name: 'table?
   required-count: 1
   restarg: #f))

;;; get

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

;;; keys

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

;;; vals

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

;;; put

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

;;; merge

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
;;; Table
;;; ---------------------------------------------------------------------

(define bard:text? (make-function debug-name: 'text?))

(%add-primitive-method! bard:text?
                        (list Anything) 
                        (constantly #f)
                        debug-name: 'text?)

(%add-primitive-method! bard:text?
                        (list <string>) 
                        (constantly #t)
                        debug-name: 'text?)


