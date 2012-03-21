;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.scm
;;;; Project:       bard
;;;; Purpose:       printing Bard values
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define print:$object-printers (make-table))

(define (print:type-for-printing obj)
  (cond
   ((eqv? obj #!eof) 'eof)
   ((eqv? obj #!void) 'undefined)
   ((null? obj) 'nothing)
   ((eqv? obj #t) 'true)
   ((eqv? obj #f) 'false)
   ((integer? obj) 'integer)
   ((flonum? obj) 'float)
   ((##ratnum? obj) 'ratio)
   ((char? obj) 'character)
   ((bard:cell? obj) 'cell)
   ((bard:slot? obj) 'slot)
   ((bard:sequence? obj) 'sequence)
   ((bard:frame? obj) 'frame)
   (else (error "Unrecognized object type when printing" obj))))

(define (bard:defprinter print-type printfn)
  (table-set! print:$object-printers print-type printfn))

(define (print:lookup-printer obj)
  (let ((print-type (print:type-for-printing obj)))
    (table-ref print:$object-printers print-type #f)))

(define (bard:%format-sequence obj)
  (let loop ((outstr "(")
             (items obj))
    (if (null? items)
        (string-append outstr ")")
        (let ((i (car items)))
          (loop (string-append outstr
                               " " (bard:object->string i))
                (cdr items))))))

(define (bard:syntax-object? obj)
  (and (bard:frame? obj)
       (eq? 'bard:syntax (frame:get obj built-in-type:))))

(define (bard:format-syntax-object obj)
  (let* ((prologue "#<syntax ")
         (epilogue " >")
         (syntax-type (frame:get obj type:))
         (syntax-value (frame:get obj value:))
         (value-string (case syntax-type
                         ((undefined) "")
                         ((nothing) "")
                         ((boolean)(if syntax-value
                                       " value: true"
                                       " value: false"))
                         ((integer)(string-append " value: " (object->string syntax-value)))
                         ((flonum)(string-append " value: " (object->string syntax-value)))
                         ((ratnum)(string-append " value: " (object->string syntax-value)))
                         ((character)(string-append " value: \\" (string syntax-value)))
                         ((name)(let ((mname (frame:get obj module-name:))
                                      (vname syntax-value))
                                  (if mname
                                      (if (eq? mname 'bard.keyword)
                                          (string-append " value: :" (object->string vname))
                                          (string-append " value: "
                                                         (object->string mname)
                                                         ":"
                                                         (object->string vname)))
                                      (string-append " value: " (object->string vname)))))
                         ((text)(string-append " value: \"" syntax-value "\""))
                         ((sequence)(let ((items (map bard:object->string (frame:get obj value:))))
                                      (let loop ((outstr "[")
                                                 (items items))
                                        (if (null? items)
                                            (string-append " value: " outstr "]")
                                            (loop (string-append outstr " " (car items))
                                                  (cdr items))))))
                         ((application) (let ((items (map bard:object->string (frame:get obj value:))))
                                          (let loop ((outstr "(")
                                                     (items items))
                                            (if (null? items)
                                                (string-append " value: " outstr ")")
                                                (loop (string-append outstr " " (car items))
                                                      (cdr items))))))
                         ((frame)(let ((items (map bard:object->string (frame:get obj value:))))
                                      (let loop ((outstr "{")
                                                 (items items))
                                        (if (null? items)
                                            (string-append " value: " outstr "}")
                                            (loop (string-append outstr " " (car items))
                                                  (cdr items))))))
                         (else (error "Unknown type of syntax object" obj)))))
    (string-append prologue "type: " (object->string syntax-type) value-string epilogue)))

(define (bard:%format-frame-value obj)
  (let* ((fkeys (frame:keys obj))
         (fvals (map (lambda (f)(frame:get obj f)) fkeys)))
    (let loop ((outstr "{")
               (fkeys fkeys)
               (fvals fvals))
      (if (null? fkeys)
          (string-append outstr "}")
          (let ((k (car fkeys))
                (v (car fvals)))
            (loop (string-append outstr
                                 " " (bard:object->string k)
                                 " " (bard:object->string v))
                  (cdr fkeys)
                  (cdr fvals)))))))

(define (bard:%format-frame obj)
  (if (bard:syntax-object? obj)
      (bard:format-syntax-object obj)
      (bard:%format-frame-value obj)))

(bard:defprinter 'undefined (lambda (x) "undefined"))
(bard:defprinter 'nothing (lambda (x) "nothing"))
(bard:defprinter 'true (lambda (x) "true"))
(bard:defprinter 'false (lambda (x) "false"))
(bard:defprinter 'integer (lambda (x) (number->string x)))
(bard:defprinter 'float (lambda (x) (number->string x)))
(bard:defprinter 'ratio (lambda (x) (number->string x)))
(bard:defprinter 'character (lambda (x) (string-append "\\" (string x))))
(bard:defprinter 'sequence (lambda (x)(bard:%format-sequence x)))
(bard:defprinter 'frame (lambda (x)(bard:%format-frame x)))

(define (bard:object->string obj)
  (let* ((printer (print:lookup-printer obj)))
    (printer obj)))

(define (bard:print-object obj #!optional (port #f))
  (let* ((port (or port (current-output-port)))
         (object-string (bard:object->string obj)))
    (display object-string port)))
