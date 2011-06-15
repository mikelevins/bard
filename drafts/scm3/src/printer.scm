;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          printer.scm
;;;; Project:       bard
;;;; Purpose:       bard's printer
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define print:$object-printers (make-table))

(define (print:type-for-printing obj)
  (cond
   ((eqv? obj #!void) 'undefined)
   ((null? obj) 'nothing)
   ((eqv? obj #t) 'true)
   ((eqv? obj #f) 'false)
   ((integer? obj) 'integer)
   ((flonum? obj) 'float)
   ((##ratnum? obj) 'ratio)
   ((char? obj) 'character)
   ((bard:syntax-atom? obj) 'syntax-atom)
   ((bard:syntax-sequence? obj) 'syntax-sequence)
   (else (error "Unrecognized object type when printing" obj))))

(define (bard:defprinter print-type printfn)
  (table-set! print:$object-printers print-type printfn))

(define (print:lookup-printer obj)
  (let ((print-type (print:type-for-printing obj)))
    (table-ref print:$object-printers print-type #f)))

(define (bard:%format-syntax-atom x)
  (let* ((type (bard:%syntax-atom-type x))
         (valstr (case type
                   ((bard:undefined) "undefined")
                   ((bard:nothing) "nothing")
                   ((bard:boolean) (if (bard:%syntax-atom-value x) "true" "false"))
                   ((bard:integer) (number->string (bard:%syntax-atom-value x)))
                   ((bard:float) (number->string (bard:%syntax-atom-value x)))
                   ((bard:ratio) (number->string (bard:%syntax-atom-value x)))
                   ((bard:character) (string (bard:%syntax-atom-value x)))
                   ((bard:name)(let* ((val (bard:%syntax-atom-value x))
                                      (mname (car val))
                                      (vname (cdr val)))
                                 (if mname
                                     (if (eq? mname 'bard.keyword)
                                         (string-append ":" (symbol->string vname))
                                         (string-append (symbol->string mname) ":" (symbol->string vname)))
                                     (symbol->string vname))))
                   ((bard:text)(string-append "\""(bard:%syntax-atom-value x) "\""))
                   (else (error "Unrecognized syntax atom" x)))))
    (string-append "#<syntax [" (symbol->string type) "]: " valstr ">")))

(define (bard:%format-syntax-sequence)
  (error "Unimplemented syntax sequence printer"))

(bard:defprinter 'undefined (lambda (x) "undefined"))
(bard:defprinter 'nothing (lambda (x) "nothing"))
(bard:defprinter 'true (lambda (x) "true"))
(bard:defprinter 'false (lambda (x) "false"))
(bard:defprinter 'integer (lambda (x) (number->string x)))
(bard:defprinter 'float (lambda (x) (number->string x)))
(bard:defprinter 'ratio (lambda (x) (number->string x)))
(bard:defprinter 'character (lambda (x) (string-append "\\" (string x))))
(bard:defprinter 'syntax-atom (lambda (x)(bard:%format-syntax-atom x)))
(bard:defprinter 'syntax-sequence (lambda (x)(bard:%format-syntax-sequence x)))

(define (bard:print-object obj #!optional (port #f))
  (let* ((printer (print:lookup-printer obj))
         (port (or port (current-output-port)))
         (object-string (printer obj)))
    (display object-string port)))