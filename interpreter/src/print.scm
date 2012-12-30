;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          print.scm
;;;; Project:       Bard
;;;; Purpose:       printing Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(define $bard-printers (make-table test: eqv?))

(define (%defprinter type printfn)
  (table-set! $bard-printers type printfn))

(%defprinter <undefined> (constantly "undefined"))
(%defprinter <null> (constantly "nothing"))
(%defprinter <boolean> (lambda (b)(if b "true" "false")))
(%defprinter <character> object->string)
(%defprinter <fixnum> object->string)
(%defprinter <bignum> object->string)
(%defprinter <flonum> object->string)
(%defprinter <ratnum> object->string)
(%defprinter <string> object->string)
(%defprinter <symbol> object->string)
(%defprinter <keyword> object->string)
(%defprinter <primitive-procedure> 
             (lambda (x)(string-append "#<primitive-procedure " (object->string (object->serial-number x)) ">")))

(%defprinter <table> 
             (lambda (fr)
               (with-output-to-string '() 
                                      (lambda () 
                                        (display "{")
                                        (let* ((space (lambda ()(display " ")))
                                               (item (lambda (x)
                                                       (display (%as-string x))
                                                       (space)
                                                       (display (%as-string (%table-get fr x)))))
                                               (keys (%table-keys fr)))
                                          (if (not (%null? keys))
                                              (begin
                                                (item (%car keys))
                                                (let loop ((keys (%cdr keys)))
                                                  (if (not (%null? keys))
                                                      (begin
                                                        (space)
                                                        (item (%car keys))
                                                        (loop (%cdr keys))))))))
                                        (display "}")))))

(%defprinter <record> 
             (lambda (sc)
               (with-output-to-string '() 
                                      (lambda () 
                                        (let ((schema-name (%schema-name sc))
                                              (slot-names (%schema-slot-names sc)))
                                          (display "#<")
                                          (if schema-name
                                              (begin (display "schema ")(display schema-name))
                                              (display "an anonymous schema"))
                                          (display " ( ")
                                          (for-each (lambda (sn)(display sn)(display " ")) slot-names)
                                          (display ") ")
                                          (display ">"))))))


(%defprinter <pair> 
             (lambda (ls)
               (with-output-to-string '() 
                                      (lambda () 
                                        (display "(")
                                        (let ((space (lambda ()(display " ")))
                                              (dot (lambda ()(display ".")))
                                              (item (lambda (x)(display (%as-string x)))))
                                          (if (not (%null? ls))
                                              (begin
                                                (item (%car ls))
                                                (let loop ((items (%cdr ls)))
                                                  (if (not (%null? items))
                                                      (if (list? items)
                                                          (begin
                                                            (space)
                                                            (item (%car items))
                                                            (loop (%cdr items)))
                                                          (begin
                                                            (space)
                                                            (dot)
                                                            (space)
                                                            (item items))))))))
                                        (display ")")))))

(%defprinter <foreign-value> 
             (lambda (x)
               (string-append "#<foreign value "
                              (object->string (object->serial-number x))
                              ">")))

(%defprinter <iostream> object->string)

(%defprinter Type 
             (lambda (x)
               (if (%singleton? x)
                   (string-append "(singleton " (%as-string (%singleton-value x)) ")")
                   (object->string (%type-name x)))))

(%defprinter <function> 
             (lambda (x)
               (let ((nm (%debug-name x)))
                 (if  nm
                      (string-append "#<function " (object->string nm) ">")
                      (string-append "#<anonymous function " (object->string (object->serial-number x)) ">")))))

(%defprinter <primitive-method> 
             (lambda (x)
               (let ((nm (%debug-name x)))
                 (if  nm
                      (string-append "#<primitive-method " (object->string nm) ">")
                      (string-append "#<anonymous primitive method " (object->string (object->serial-number x)) ">")))))

(%defprinter <interpreted-method> 
             (lambda (x)
               (let ((nm (or (and (%debug-name x)
                                  (%as-string (%debug-name x)))
                             ""))
                     (formals (%method-formals x))
                     (body (%method-body x)))
                 (with-output-to-string '() 
                                        (lambda () 
                                          (display "(method ")
                                          (display nm)
                                          (if (> (string-length nm) 0) (display " "))
                                          (display (interpose " " (%bard-list->cons formals)))
                                          (if (> (%length body) 0) (display " "))
                                          (display (%as-string body))
                                          (display ")"))))))

(define (%as-string x)
  (let ((printer (table-ref $bard-printers (%object->bard-type x) #f)))
    (if printer
        (printer x)
        (let ((xtype (%object->bard-type x)))
          (if (%schema? xtype)
              (let ((printer (table-ref $bard-printers <table> #f)))
                (if printer
                    (printer x)
                    (error (string-append "No Bard printer defined for value: " (object->string x))))))))))


(define (show x)
  (newline)
  (display (%as-string x))
  (newline))

(define (bard:show x)
  (%as-string x))

(define (bard:print x #!optional (port (current-output-port)))
  (display (%as-string x) port)
  x)



