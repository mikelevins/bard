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

(define %yes (constantly (%true)))
(define %no (constantly (%false)))

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
  (%make-primitive-method %applicable?
   name: 'applicable?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

(define bard:apply
  (%make-primitive-method %apply
   name: 'apply
   parameters: (list 'op 'args)
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; As
;;; ---------------------------------------------------------------------

(define bard:as (%make-function name: 'as))

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <symbol>)
                        (list 'type 'thing)
                        (lambda (type thing)(symbol->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <symbol>) <string>)
                        (list 'type 'thing)
                        (lambda (type thing)(string->symbol thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <keyword>)
                        (list 'type 'thing)
                        (lambda (type thing)(keyword->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <keyword>) <string>)
                        (list 'type 'thing)
                        (lambda (type thing)(string->keyword thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <pair>) <string>)
                        (list 'type 'thing)
                        (lambda (type thing)(string->list thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <null>)
                        (list 'type 'thing)
                        (constantly "")
                        name: 'as)

(%add-primitive-method! bard:as
                        (list (%singleton <string>) <pair>)
                        (list 'type 'thing)
                        (lambda (type thing)(list->string (%bard-list->cons thing)))
                        name: 'as)

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(define bard:boolean?
  (%make-primitive-method %boolean?
   name: 'boolean?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

(define bard:false?
  (%make-primitive-method %false?
   name: 'false?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

(define bard:true?
  (%make-primitive-method %true?
   name: 'true?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

(define bard:character?
  (%make-primitive-method %character?
   name: 'character?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Equal
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))

(%add-primitive-method! bard:=
                        (list Anything Anything)
                        (list 'apple 'orange)
                        equal?
                        name: '=)

;;; ---------------------------------------------------------------------
;;; Float
;;; ---------------------------------------------------------------------

(define bard:float?
  (%make-primitive-method flonum?
   name: 'float?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; ForeignValue
;;; ---------------------------------------------------------------------

(define bard:foreign-value?
  (%make-primitive-method ##foreign?
   name: 'foreign-value?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Function
;;; ---------------------------------------------------------------------

(define bard:function?
  (%make-primitive-method %function?
   name: 'function?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; IOStream
;;; ---------------------------------------------------------------------

(define bard:current-input
  (%make-primitive-method current-input-port
   name: 'current-input
   parameters: '()
   required-count: 0
   restarg: #f))

(define bard:current-output
  (%make-primitive-method current-output-port
   name: 'current-output
   parameters: '()
   required-count: 0
   restarg: #f))

(define bard:display
  (%make-primitive-method display
   name: 'display
   parameters: (list 'x)
   required-count: 1
   restarg: #f))

(define bard:input-stream?
  (%make-primitive-method input-port?
   name: 'input-stream?
   parameters: (list 'x)
   required-count: 1
   restarg: #f))

(define bard:iostream?
  (%make-primitive-method 
   (lambda (x)(or (input-port? x)(output-port? x)))
   name: 'iostream?
   parameters: (list 'x)
   required-count: 1
   restarg: #f))

(define bard:output-stream?
  (%make-primitive-method output-port?
   name: 'output-stream?
   parameters: (list 'x)
   required-count: 1
   restarg: #f))

(define bard:read-line
  (%make-primitive-method 
   read-line
   name: 'read-line
   parameters: (list 'stream)
   required-count: 1
   restarg: #f))

(define (%bard-read-lines in)
  (cond
   ((input-port? in)(read-all in read-line))
   ((string? in)(call-with-input-string in (lambda (stream)(read-all stream read-line))))
   (else (error (string-append "Invalid argument to read-lines: "
                               (object->string in))))))

(define bard:read-lines
  (%make-primitive-method %bard-read-lines
   name: 'read-lines
   parameters: (list 'stream)
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
  (%make-primitive-method %bard-write
   name: 'write
   parameters: (list 'data 'stream)
   required-count: 2
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Integer
;;; ---------------------------------------------------------------------

(define bard:integer?
  (%make-primitive-method integer?
   name: 'integer?
   parameters: (list 'x)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

;;; add-first

(define bard:add-first (%make-function name: 'add-first))

(%add-primitive-method! bard:add-first
                        (list Anything <null>)
                        (list 'thing 'ls)
                        %cons
                        name: 'add-first)

(%add-primitive-method! bard:add-first
                        (list Anything <pair>)
                        (list 'thing 'ls)
                        %cons
                        name: 'add-first)

(%add-primitive-method! bard:add-first
                        (list <character> <string>)
                        (list 'ch 'str)
                        (lambda (ch str)(string-append (string ch) str))
                        name: 'add-first)

;;; add-last

(define bard:add-last (%make-function name: 'add-last))

(%add-primitive-method! bard:add-last
                        (list <null> Anything)
                        (list 'ls 'thing)
                        (lambda (ls thing)(list thing))
                        name: 'add-last)

(%add-primitive-method! bard:add-last
                        (list <pair> Anything)
                        (list 'ls 'thing)
                        (lambda (ls thing)
                          (if (list? ls)
                              (append ls (list thing))
                              (error (str "Improper list: " ls))))
                        name: 'add-last)

(%add-primitive-method! bard:add-last
                        (list <string> <character>)
                        (list 'str 'ch)
                        (lambda (str ch)(string-append str (string ch)))
                        name: 'add-last)

;;; any

(define bard:any (%make-function name: 'any))

(%add-primitive-method! bard:any
                        (list <null>)
                        (list 'ls)
                        (constantly (%nothing))
                        name: 'any)

(%add-primitive-method! bard:any
                        (list <pair>)
                        (list 'ls)
                        (lambda (ls)
                          (if (list? ls)
                              (list-ref ls (random-integer (%length ls)))
                              (error (str "Improper list: " ls))))
                        name: 'any)

(%add-primitive-method! bard:any
                        (list <string>)
                        (list 'ls)
                        (lambda (ls)(string-ref ls (random-integer (string-length ls))))
                        name: 'any)

;;; append

(define bard:append (%make-function name: 'append))

(%add-primitive-method! bard:append
                        (list <null>  <null>)
                        (list 'ls1 'ls2)
                        (constantly (%nothing))
                        name: 'append)

(%add-primitive-method! bard:append
                        (list <null>  <pair>)
                        (list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls2)
                        name: 'append)

(%add-primitive-method! bard:append
                        (list <pair>  <null>)
                        (list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls1)
                        name: 'append)

(%add-primitive-method! bard:append
                        (list <pair>  <pair>)
                        (list 'ls1 'ls2)
                        append
                        name: 'append)

(%add-primitive-method! bard:append
                        (list <string>  <string>)
                        (list 'str1 'str2)
                        string-append
                        name: 'append)

;;; next-last

(define bard:next-last (%make-function name: 'next-last))

(%add-primitive-method! bard:next-last
                        (list <pair>)
                        (list 'ls)
                        next-last
                        name: 'next-last)

(%add-primitive-method! bard:next-last
                        (list <string>)
                        (list 'string)
                        string-next-last
                        name: 'next-last)

;;; by

(define bard:by (%make-function name: 'by))

(%add-primitive-method! bard:by
                        (list <fixnum> <pair>)
                        (list 'n 'ls)
                        by
                        name: 'by)

(%add-primitive-method! bard:by
                        (list <fixnum> <string>)
                        (list 'n 's)
                        (lambda (n s)(map (lambda (i)(list->string i))
                                          (by n (string->list s))))
                        name: 'by)

;;; drop

(define bard:drop (%make-function name: 'drop))

(%add-primitive-method! bard:drop
                        (list <fixnum>  <pair>)
                        (list 'n 'ls)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0))
                            (if (>= i n)
                                items
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1))))))
                        name: 'drop)

(%add-primitive-method! bard:drop
                        (list <fixnum>  <string>)
                        (list 'n 'str)
                        (lambda (n str)(substring str n (string-length str)))
                        name: 'drop)

;;; element

(define bard:element (%make-function name: 'element))

(%add-primitive-method! bard:element
                        (list <pair> <fixnum>)
                        (list 'ls 'n)
                        list-ref
                        name: 'element)

(%add-primitive-method! bard:element
                        (list <string> <fixnum>)
                        (list 'str 'n)
                        string-ref
                        name: 'element)

;;; empty?

(define bard:empty? (%make-function name: 'empty?))

(%add-primitive-method! bard:empty?
                        (list <null>)
                        (list 'ls)
                        (constantly (%true))
                        name: 'empty?)

(%add-primitive-method! bard:empty?
                        (list <pair>)
                        (list 'ls)
                        (constantly (%false))
                        name: 'empty?)

(%add-primitive-method! bard:empty?
                        (list <string>)
                        (list 'str)
                        (lambda (str)(<= (string-length str) 0))
                        name: 'empty?)

;;; filter

(define bard:filter (%make-function name: 'filter))

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
                        (list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (list <primitive-method> <pair>)
                        (list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (list <interpreted-method> <pair>)
                        (list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (list <function> <pair>)
                        (list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

;;; first

(define bard:first (%make-function name: 'first))

(%add-primitive-method! bard:first
                        (list <pair>)
                        (list 'ls)
                        car
                        name: 'first)

(%add-primitive-method! bard:first
                        (list <string>)
                        (list 's)
                        (lambda (s)(string-ref s 0))
                        name: 'first)

;;; join

(define (%bard-join-strings cupola strs)
  (if (null? strs)
      ""
      (apply string-append 
             (cdr (apply append (map (lambda (s)(list cupola s))
                                     strs))))))

(define bard:join-strings (%make-function name: 'join-strings))

(%add-primitive-method! bard:join-strings
                        (list <string>  <pair>)
                        (list 'str 'strs)
                        %bard-join-strings
                        name: 'join-strings)


;;; last

(define bard:last (%make-function name: 'last))

(%add-primitive-method! bard:last
                        (list <pair>)
                        (list 'ls)
                        %last
                        name: 'last)

(%add-primitive-method! bard:last
                        (list <string>)
                        (list 'string)
                        (lambda (string)(string-ref string (- (string-length string) 1)))
                        name: 'last)

(%add-primitive-method! bard:last
                        (list <table>)
                        (list 'table)
                        (lambda (table)
                          (let* ((keys (%table-keys table))
                                 (key (list-ref keys (- (%length keys) 1))))
                            (%table key (%table-get table key (%nothing)))))
                        name: 'last)

;;; length

(define bard:length (%make-function name: 'length))

(%add-primitive-method! bard:length
                        (list <null>)
                        (list 'ls)
                        (constantly 0)
                        name: 'length)

(%add-primitive-method! bard:length
                        (list <pair>)
                        (list 'ls)
                        %length
                        name: 'length)

(%add-primitive-method! bard:length
                        (list <string>)
                        (list 'string)
                        string-length
                        name: 'length)

(%add-primitive-method! bard:length
                        (list <table>)
                        (list 'table)
                        (lambda (table)(%length (%table-keys table)))
                        name: 'length)

;;; list?

(define bard:list? (%make-function name: 'list?))

(%add-primitive-method! bard:list?
                        (list Anything)
                        (list 'ls)
                        (constantly (%false))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (list <null>)
                        (list 'ls)
                        (constantly (%true))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (list <pair>)
                        (list 'ls)
                        (lambda (ls)(list? ls))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (list <string>)
                        (list 'ls)
                        (constantly (%true))
                        name: 'list?)

;;; map

(define bard:map (%make-function name: 'map))

;;; <null>

(%add-primitive-method! bard:map
                        (list <primitive-method> <null>)
                        (list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <null>)
                        (list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <null>)
                        (list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

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
                        (list <primitive-method> <pair>)
                        (list 'fn 'ls)
                        %bard-map-list
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <pair>)
                        (list 'fn 'ls)
                        %bard-map-list
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <pair>)
                        (list 'fn 'ls)
                        %bard-map-list
                        name: 'map)

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
                        (list <primitive-method> <string>)
                        (list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <string>)
                        (list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <string>)
                        (list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

;;; <table>

(define (%bard-map-table fn fr)
  (let loop ((ks (%table-keys fr))
             (out '()))
    (if (%null? ks)
        (%maybe-slot-list->table (reverse out))
        (let* ((k (car ks))
               (v (%table-get fr k)))
          (loop (cdr ks)
                (%cons (%funcall fn k v) out))))))

(%add-primitive-method! bard:map
                        (list <primitive-method> <table>)
                        (list 'fn 'ls)
                        %bard-map-table
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <interpreted-method> <table>)
                        (list 'fn 'ls)
                        %bard-map-table
                        name: 'map)

(%add-primitive-method! bard:map
                        (list <function> <table>)
                        (list 'fn 'ls)
                        %bard-map-table
                        name: 'map)

;;; reduce

(define bard:reduce (%make-function name: 'reduce))

(define (%bard-reduce fn init ls)
  (let loop ((items ls)
             (result init))
    (if (%null? items)
        result
        (loop (cdr items)
              (%funcall fn result (car items))))))

(%add-primitive-method! bard:reduce
                        (list <primitive-method> Anything <null>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <null>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <null>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <primitive-method> Anything <pair>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <interpreted-method> Anything <pair>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (list <function> Anything <pair>)
                        (list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)
;;; rest 

(define bard:rest (%make-function name: 'rest))

(%add-primitive-method! bard:rest
                        (list <null>)
                        (list 'ls)
                        (constantly (%nothing))
                        name: 'rest)

(%add-primitive-method! bard:rest
                        (list <pair>)
                        (list 'ls)
                        cdr
                        name: 'rest)

(%add-primitive-method! bard:rest
                        (list <string>)
                        (list 'str)
                        (lambda (str)(substring str 1 (string-length str)))
                        name: 'rest)

;;; second

(define bard:second (%make-function name: 'second))

(%add-primitive-method! bard:second
                        (list <pair>)
                        (list 'ls)
                        cadr
                        name: 'second)

(%add-primitive-method! bard:second
                        (list <string>)
                        (list 's)
                        (lambda (s)(string-ref s 1))
                        name: 'second)

;;; some?

(define bard:some? (%make-function name: 'some?))

(define (%bard-some? test ls)
  (let loop ((items ls))
    (if (%null? items)
        (%nothing)
        (if (%funcall test (car items))
            (car items)
            (loop (cdr items))))))

(%add-primitive-method! bard:some?
                        (list Anything <null>)
                        (list 'test 'ls)
                        (constantly (%nothing))
                        name: 'some?)

(%add-primitive-method! bard:some?
                        (list <primitive-method> <pair>)
                        (list 'test 's)
                        %bard-some?
                        name: 'some?)

(%add-primitive-method! bard:some?
                        (list <interpreted-method> <pair>)
                        (list 'test 'ls)
                        %bard-some?
                        name: 'some?)

(%add-primitive-method! bard:some?
                        (list <function> <pair>)
                        (list 'test 'ls)
                        %bard-some?
                        name: 'some?)

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

(define bard:split-string (%make-function name: 'split-string))

(%add-primitive-method! bard:split-string
                        (list <string>  <character>)
                        (list 'str 'ch)
                        %bard-split-string
                        name: 'split-string)
;;; take

(define bard:take (%make-function name: 'take))

(%add-primitive-method! bard:take
                        (list <fixnum>  <pair>)
                        (list 'n 'ls)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0)
                                     (result '()))
                            (if (>= i n)
                                result
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (cdr items)(+ i 1)(append result (list (car items))))))))
                        name: 'take)

(%add-primitive-method! bard:take
                        (list <fixnum>  <string>)
                        (list 'n 'str)
                        (lambda (n str)(substring str 0 n))
                        name: 'take)


;;; take-by

(define bard:take-by (%make-function name: 'take-by))

(%add-primitive-method! bard:take-by
                        (list <fixnum>  <fixnum> <pair>)
                        (list 'len 'advance 'ls)
                        take-by
                        name: 'take-by)

(%add-primitive-method! bard:take-by
                        (list <fixnum> <fixnum> <string>)
                        (list 'len 'advance 's)
                        (lambda (len advance s)(map (lambda (i)(list->string i))
                                                    (take-by len advance (string->list s))))
                        name: 'take-by)


;;; ---------------------------------------------------------------------
;;; Name
;;; ---------------------------------------------------------------------

(define bard:symbol? (%make-function name: 'symbol?))

(%add-primitive-method! bard:symbol?
                        (list Anything)
                        (list 'x)
                        (constantly (%false))
                        name: 'symbol?)

(%add-primitive-method! bard:symbol?
                        (list <symbol>)
                        (list 'x)
                        (constantly (%true))
                        name: 'symbol?)

;;; ---------------------------------------------------------------------
;;; Null
;;; ---------------------------------------------------------------------

(define bard:nothing? (%make-function name: 'nothing?))

(%add-primitive-method! bard:nothing?
                        (list Anything)
                        (list 'x)
                        (constantly (%false))
                        name: 'nothing?)

(%add-primitive-method! bard:nothing?
                        (list <null>)
                        (list 'x)
                        (constantly (%true))
                        name: 'nothing?)

(define bard:something? (%make-function name: 'something?))

(%add-primitive-method! bard:something?
                        (list Anything)
                        (list 'x)
                        (constantly (%true))
                        name: 'something?)

(%add-primitive-method! bard:something?
                        (list <null>)
                        (list 'x)
                        (constantly (%false))
                        name: 'something?)

;;; ---------------------------------------------------------------------
;;; Ordered
;;; ---------------------------------------------------------------------

(define bard:< (%make-function name: '<))

(%add-primitive-method! bard:<
                        (list <fixnum> <fixnum>)
                        (list 'n1 'n2)
                        <
                        name: '<)

(define bard:> (%make-function name: '>))

(%add-primitive-method! bard:>
                        (list <fixnum> <fixnum>)
                        (list 'n1 'n2)
                        >
                        name: '>)

(define bard:<= (%make-function name: '<=))

(%add-primitive-method! bard:<=
                        (list <fixnum> <fixnum>)
                        (list 'n1 'n2)
                        <=
                        name: '<=)

(define bard:>= (%make-function name: '>=))

(%add-primitive-method! bard:>=
                        (list <fixnum> <fixnum>)
                        (list 'n1 'n2)
                        >=
                        name: '>=)

;;; ---------------------------------------------------------------------
;;; Pair
;;; ---------------------------------------------------------------------

;;; pair

(define bard:pair (%make-function name: 'pair))

(%add-primitive-method! bard:pair
                        (list Anything Anything)
                        (list 'l 'r)
                        %cons
                        name: 'pair)

;;; left

(define bard:left (%make-function name: 'left))

(%add-primitive-method! bard:left
                        (list <pair>)
                        (list 'p)
                        car
                        name: 'left)

;;; right

(define bard:right (%make-function name: 'right))

(%add-primitive-method! bard:right
                        (list <pair>)
                        (list 'p)
                        cdr
                        name: 'right)

;;; ---------------------------------------------------------------------
;;; Table
;;; ---------------------------------------------------------------------

;;; table?

(define bard:table?
  (%make-primitive-method %table?
   name: 'table?
   parameters: (list 'thing)
   required-count: 1
   restarg: #f))

;;; get

(define (%bard-get fr k)
  (cond
   ((%table? fr)(%table-get fr k))
   ((list? fr)(if (integer? k)
                   (list-ref fr k)
                   (getf k fr (%nothing))))
   ((string? fr)(string-ref fr k))
   (else (%nothing))))

(define bard:get
  (%make-primitive-method %bard-get
   name: 'get
   parameters: (list 'table 'key)
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
   (else (%false))))

(define bard:contains-key?
  (%make-primitive-method %bard-contains-key?
   name: 'contains-key?
   parameters: (list 'table 'key)
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
   (else (%false))))

(define bard:contains-value?
  (%make-primitive-method %bard-contains-value?
   name: 'contains-value?
   parameters: (list 'table 'val)
   required-count: 2
   restarg: #f))

;;; keys

(define (%bard-keys fr)
  (cond
   ((%table? fr)(%table-keys fr))
   ((list? fr)(iota (%length fr)))
   ((string? fr)(iota (string-length fr)))
   (else (%nothing))))

(define bard:keys
  (%make-primitive-method %bard-keys
   name: 'keys
   parameters: (list 'table)
   required-count: 1
   restarg: #f))

;;; vals

(define (%bard-vals fr)
  (cond
   ((%table? fr)(%table-vals fr))
   ((list? fr) fr)
   ((string? fr) fr)
   (else (%nothing))))

(define bard:vals
  (%make-primitive-method %bard-vals
   name: 'vals
   parameters: (list 'table)
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
  (%make-primitive-method %bard-put
   name: 'put
   parameters: (list 'table 'key 'value)
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
  (%make-primitive-method %merge
   name: 'merge
   parameters: (list 't1 't2)
   required-count: 2
   restarg: #f))
