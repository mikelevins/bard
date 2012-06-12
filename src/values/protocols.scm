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
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:apply
  (%make-primitive-method %apply
   name: 'apply
   parameters: (%list 'op 'args)
   required-count: 2
   restarg: #f))


;;; ---------------------------------------------------------------------
;;; As
;;; ---------------------------------------------------------------------

(define bard:as (%make-function name: 'as))

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <symbol>)
                        (%list 'type 'thing)
                        (lambda (type thing)(symbol->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <symbol>) <string>)
                        (%list 'type 'thing)
                        (lambda (type thing)(string->symbol thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <keyword>)
                        (%list 'type 'thing)
                        (lambda (type thing)(keyword->string thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <keyword>) <string>)
                        (%list 'type 'thing)
                        (lambda (type thing)(string->keyword thing))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <list>) <string>)
                        (%list 'type 'thing)
                        (lambda (type thing)(%cons->bard-list (string->list thing)))
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <null>)
                        (%list 'type 'thing)
                        (constantly "")
                        name: 'as)

(%add-primitive-method! bard:as
                        (%list (%singleton <string>) <list>)
                        (%list 'type 'thing)
                        (lambda (type thing)(list->string (%bard-list->cons thing)))
                        name: 'as)

;;; ---------------------------------------------------------------------
;;; Boolean
;;; ---------------------------------------------------------------------

(define bard:boolean?
  (%make-primitive-method %boolean?
   name: 'boolean?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:false?
  (%make-primitive-method %false?
   name: 'false?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

(define bard:true?
  (%make-primitive-method %true?
   name: 'true?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Character
;;; ---------------------------------------------------------------------

(define bard:character?
  (%make-primitive-method %character?
   name: 'character?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Equal
;;; ---------------------------------------------------------------------

(define bard:= (%make-function name: '=))

(%add-primitive-method! bard:=
                        (%list Anything Anything)
                        (%list 'apple 'orange)
                        equal?
                        name: '=)

;;; ---------------------------------------------------------------------
;;; Float
;;; ---------------------------------------------------------------------

(define bard:float?
  (%make-primitive-method flonum?
   name: 'float?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; ForeignValue?
;;; ---------------------------------------------------------------------

(define bard:foreign-value?
  (%make-primitive-method ##foreign?
   name: 'foreign-value?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; Frame
;;; ---------------------------------------------------------------------

;;; frame?

(define bard:frame?
  (%make-primitive-method %frame?
   name: 'frame?
   parameters: (%list 'thing)
   required-count: 1
   restarg: #f))

;;; get

(define bard:get (%make-function name: 'get))

(%add-primitive-method! bard:get
                        (%list <frame> Anything)
                        (%list 'frame 'key)
                        (lambda (frame key)(%frame-get frame key (%nothing)))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <null> <fixnum>)
                        (%list 'ls 'n)
                        (lambda (ls n)(error (string-append "Index out of range: " (object->string n))))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <list> <fixnum>)
                        (%list 'ls 'n)
                        (lambda (ls n)(%list-ref ls n))
                        name: 'get)

(%add-primitive-method! bard:get
                        (%list <string> <fixnum>)
                        (%list 'str 'n)
                        (lambda (str n)(string-ref str n))
                        name: 'get)

;;; keys

(define bard:keys (%make-function name: 'keys))

(%add-primitive-method! bard:keys
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)(%frame-keys frame))
                        name: 'keys)
;;; put

(define bard:put (%make-function name: 'put))

(%add-primitive-method! bard:put
                        (%list <frame> Anything Anything)
                        (%list 'frame 'key 'val)
                        %frame-put
                        name: 'put)

;;; ---------------------------------------------------------------------
;;; IOStream
;;; ---------------------------------------------------------------------

(define bard:current-input
  (%make-primitive-method current-input-port
   name: 'current-input
   parameters: %nil
   required-count: 0
   restarg: #f))

(define bard:current-output
  (%make-primitive-method current-output-port
   name: 'current-output
   parameters: %nil
   required-count: 0
   restarg: #f))

(define bard:read-line
  (%make-primitive-method 
   read-line
   name: 'read-line
   parameters: (%list 'stream)
   required-count: 1
   restarg: #f))

(define bard:read-lines
  (%make-primitive-method 
   (lambda (stream)(read-all stream read-line))
   name: 'read-lines
   parameters: (%list 'stream)
   required-count: 1
   restarg: #f))

;;; ---------------------------------------------------------------------
;;; List
;;; ---------------------------------------------------------------------

;;; add-first

(define bard:add-first (%make-function name: 'add-first))

(%add-primitive-method! bard:add-first
                        (%list Anything <null>)
                        (%list 'thing 'ls)
                        %cons
                        name: 'add-first)

(%add-primitive-method! bard:add-first
                        (%list Anything <list>)
                        (%list 'thing 'ls)
                        %cons
                        name: 'add-first)

(%add-primitive-method! bard:add-first
                        (%list <character> <string>)
                        (%list 'ch 'str)
                        (lambda (ch str)(string-append (string ch) str))
                        name: 'add-first)

;;; add-last

(define bard:add-last (%make-function name: 'add-last))

(%add-primitive-method! bard:add-last
                        (%list <null> Anything)
                        (%list 'ls 'thing)
                        (lambda (ls thing)(%list thing))
                        name: 'add-last)

(%add-primitive-method! bard:add-last
                        (%list <list> Anything)
                        (%list 'ls 'thing)
                        (lambda (ls thing)(%append ls (%list thing)))
                        name: 'add-last)

(%add-primitive-method! bard:add-last
                        (%list <string> <character>)
                        (%list 'str 'ch)
                        (lambda (str ch)(string-append str (string ch)))
                        name: 'add-last)

;;; any

(define bard:any (%make-function name: 'any))

(%add-primitive-method! bard:any
                        (%list <list>)
                        (%list 'ls)
                        (lambda (ls)(%list-ref ls (random-integer (%length ls))))
                        name: 'any)

(%add-primitive-method! bard:any
                        (%list <string>)
                        (%list 'ls)
                        (lambda (ls)(string-ref ls (random-integer (string-length ls))))
                        name: 'any)

;;; append

(define bard:append (%make-function name: 'append))

(%add-primitive-method! bard:append
                        (%list <null>  <null>)
                        (%list 'ls1 'ls2)
                        (constantly (%nothing))
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <null>  <list>)
                        (%list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls2)
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <list>  <null>)
                        (%list 'ls1 'ls2)
                        (lambda (ls1 ls2) ls1)
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <list>  <list>)
                        (%list 'ls1 'ls2)
                        %append
                        name: 'append)

(%add-primitive-method! bard:append
                        (%list <string>  <string>)
                        (%list 'str1 'str2)
                        string-append
                        name: 'append)

;;; drop

(define bard:drop (%make-function name: 'drop))

(%add-primitive-method! bard:drop
                        (%list <fixnum>  <list>)
                        (%list 'n 'ls)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0))
                            (if (>= i n)
                                items
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (%cdr items)(+ i 1))))))
                        name: 'drop)

(%add-primitive-method! bard:drop
                        (%list <fixnum>  <string>)
                        (%list 'n 'str)
                        (lambda (n str)(substring str n (string-length str)))
                        name: 'drop)

;;; element

(define bard:element (%make-function name: 'element))

(%add-primitive-method! bard:element
                        (%list <list> <fixnum>)
                        (%list 'ls 'n)
                        %list-ref
                        name: 'element)

(%add-primitive-method! bard:element
                        (%list <string> <fixnum>)
                        (%list 'str 'n)
                        string-ref
                        name: 'element)

;;; empty?

(define bard:empty? (%make-function name: 'empty?))

(%add-primitive-method! bard:empty?
                        (%list <null>)
                        (%list 'ls)
                        (constantly (%true))
                        name: 'empty?)

(%add-primitive-method! bard:empty?
                        (%list <list>)
                        (%list 'ls)
                        (constantly (%false))
                        name: 'empty?)

(%add-primitive-method! bard:empty?
                        (%list <string>)
                        (%list 'str)
                        (lambda (str)(<= (string-length str) 0))
                        name: 'empty?)

;;; filter

(define bard:filter (%make-function name: 'filter))

(define (%bard-filter test ls)
  (let loop ((items ls)
             (result '()))
    (if (%null? items)
        (%cons->bard-list (reverse result))
        (if (%funcall test (%car items))
            (loop (%cdr items)
                  (cons (%car items) result))
            (loop (%cdr items)
                  result)))))

(%add-primitive-method! bard:filter
                        (%list Anything <null>)
                        (%list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (%list <primitive-method> <list>)
                        (%list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (%list <interpreted-method> <list>)
                        (%list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

(%add-primitive-method! bard:filter
                        (%list <function> <list>)
                        (%list 'fn 'ls)
                        %bard-filter
                        name: 'filter)

;;; first

(define bard:first (%make-function name: 'first))

(%add-primitive-method! bard:first
                        (%list <list>)
                        (%list 'ls)
                        %car
                        name: 'first)

;;; last

(define bard:last (%make-function name: 'last))

(%add-primitive-method! bard:last
                        (%list <list>)
                        (%list 'ls)
                        %last
                        name: 'last)

(%add-primitive-method! bard:last
                        (%list <string>)
                        (%list 'string)
                        (lambda (string)(string-ref string (- (string-length string) 1)))
                        name: 'last)

(%add-primitive-method! bard:last
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)
                          (let* ((keys (%frame-keys frame))
                                 (key (%list-ref keys (- (%length keys) 1))))
                            (%frame key (%frame-get frame key (%nothing)))))
                        name: 'last)

;;; length

(define bard:length (%make-function name: 'length))

(%add-primitive-method! bard:length
                        (%list <null>)
                        (%list 'ls)
                        (constantly 0)
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <list>)
                        (%list 'ls)
                        %length
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <string>)
                        (%list 'string)
                        string-length
                        name: 'length)

(%add-primitive-method! bard:length
                        (%list <frame>)
                        (%list 'frame)
                        (lambda (frame)(%length (%frame-keys frame)))
                        name: 'length)

;;; list?

(define bard:list? (%make-function name: 'list?))

(%add-primitive-method! bard:list?
                        (%list Anything)
                        (%list 'ls)
                        (constantly (%false))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (%list <null>)
                        (%list 'ls)
                        (constantly (%true))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (%list <list>)
                        (%list 'ls)
                        (constantly (%true))
                        name: 'list?)

(%add-primitive-method! bard:list?
                        (%list <string>)
                        (%list 'ls)
                        (constantly (%true))
                        name: 'list?)

;;; map

(define bard:map (%make-function name: 'map))

;;; <null>

(%add-primitive-method! bard:map
                        (%list <primitive-method> <null>)
                        (%list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <interpreted-method> <null>)
                        (%list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <function> <null>)
                        (%list 'fn 'ls)
                        (constantly (%nothing))
                        name: 'map)

;;; <list>

(define (%bard-map-list fn ls)
  (let loop ((items ls)
             (result '()))
    (if (%null? items)
        (%cons->bard-list (reverse result))
        (loop (%cdr items)
              (cons (%funcall fn (%car items))
                    result)))))

(%add-primitive-method! bard:map
                        (%list <primitive-method> <list>)
                        (%list 'fn 'ls)
                        %bard-map-list
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <interpreted-method> <list>)
                        (%list 'fn 'ls)
                        %bard-map-list
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <function> <list>)
                        (%list 'fn 'ls)
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
                        (%list <primitive-method> <string>)
                        (%list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <interpreted-method> <string>)
                        (%list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <function> <string>)
                        (%list 'fn 'ls)
                        %bard-map-string
                        name: 'map)

;;; <frame>

(define (%bard-map-frame fn fr)
  (let loop ((ks (%frame-keys fr))
             (out '()))
    (if (%null? ks)
        (%maybe-slot-list->frame (reverse out))
        (let* ((k (%car ks))
               (v (%frame-get fr k)))
          (loop (%cdr ks)
                (%cons (%funcall fn k v) out))))))

(%add-primitive-method! bard:map
                        (%list <primitive-method> <frame>)
                        (%list 'fn 'ls)
                        %bard-map-frame
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <interpreted-method> <frame>)
                        (%list 'fn 'ls)
                        %bard-map-frame
                        name: 'map)

(%add-primitive-method! bard:map
                        (%list <function> <frame>)
                        (%list 'fn 'ls)
                        %bard-map-frame
                        name: 'map)

;;; reduce

(define bard:reduce (%make-function name: 'reduce))

(define (%bard-reduce fn init ls)
  (let loop ((items ls)
             (result init))
    (if (%null? items)
        result
        (loop (%cdr items)
              (%funcall fn result (%car items))))))

(%add-primitive-method! bard:reduce
                        (%list <primitive-method> Anything <list>)
                        (%list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (%list <interpreted-method> Anything <list>)
                        (%list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

(%add-primitive-method! bard:reduce
                        (%list <function> Anything <list>)
                        (%list 'fn 'init 'ls)
                        %bard-reduce
                        name: 'reduce)

;;; rest 

(define bard:rest (%make-function name: 'rest))

(%add-primitive-method! bard:rest
                        (%list <null>)
                        (%list 'ls)
                        (constantly (%nothing))
                        name: 'rest)

(%add-primitive-method! bard:rest
                        (%list <list>)
                        (%list 'ls)
                        %cdr
                        name: 'rest)

(%add-primitive-method! bard:rest
                        (%list <string>)
                        (%list 'str)
                        (lambda (str)(substring str 1 (string-length str)))
                        name: 'rest)


;;; take

(define bard:take (%make-function name: 'take))

(%add-primitive-method! bard:take
                        (%list <fixnum>  <list>)
                        (%list 'n 'ls)
                        (lambda (n ls)
                          (let loop ((items ls)
                                     (i 0)
                                     (result %nil))
                            (if (>= i n)
                                result
                                (if (%null? items)
                                    (error (string-append "Count out of bounds: " (%as-string n)))
                                    (loop (%cdr items)(+ i 1)(%append result (%list (%car items))))))))
                        name: 'take)

(%add-primitive-method! bard:take
                        (%list <fixnum>  <string>)
                        (%list 'n 'str)
                        (lambda (n str)(substring str 0 n))
                        name: 'take)

;;; ---------------------------------------------------------------------
;;; Null
;;; ---------------------------------------------------------------------

(define bard:nothing? (%make-function name: 'nothing?))

(%add-primitive-method! bard:nothing?
                        (%list Anything)
                        (%list 'x)
                        (constantly (%false))
                        name: 'nothing?)

(%add-primitive-method! bard:nothing?
                        (%list <null>)
                        (%list 'x)
                        (constantly (%true))
                        name: 'nothing?)

(define bard:something? (%make-function name: 'something?))

(%add-primitive-method! bard:something?
                        (%list Anything)
                        (%list 'x)
                        (constantly (%true))
                        name: 'something?)

(%add-primitive-method! bard:something?
                        (%list <null>)
                        (%list 'x)
                        (constantly (%false))
                        name: 'something?)

;;; ---------------------------------------------------------------------
;;; Ordered
;;; ---------------------------------------------------------------------

(define bard:< (%make-function name: '<))

(%add-primitive-method! bard:<
                        (%list <fixnum> <fixnum>)
                        (%list 'n1 'n2)
                        <
                        name: '<)

(define bard:> (%make-function name: '>))

(%add-primitive-method! bard:>
                        (%list <fixnum> <fixnum>)
                        (%list 'n1 'n2)
                        >
                        name: '>)

(define bard:<= (%make-function name: '<=))

(%add-primitive-method! bard:<=
                        (%list <fixnum> <fixnum>)
                        (%list 'n1 'n2)
                        <=
                        name: '<=)

(define bard:>= (%make-function name: '>=))

(%add-primitive-method! bard:>=
                        (%list <fixnum> <fixnum>)
                        (%list 'n1 'n2)
                        >=
                        name: '>=)

