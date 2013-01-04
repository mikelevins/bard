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

(define +printer-functions+ (make-table test: eqv?))

(define (define-printer-function tag fn)
  (table-set! +printer-functions+ tag fn))

(define (get-printer-function tag)
  (table-ref +printer-functions+ tag #f))

(define (%as-string x)
  (let ((printer (get-printer-function (%tag x))))
    (printer x)))

(define (%show x)(%as-string x))

(define (%print x #!optional (port (current-output-port)))
  (display (%as-string x) port)
  x)

;;; ---------------------------------------------------------------------
;;; printer definitions
;;; ---------------------------------------------------------------------

(define-printer-function (%tag #!void) (constantly ""))
(define-printer-function (schema-tag <undefined>) (constantly "undefined"))
(define-printer-function (schema-tag <null>) (constantly "nothing"))
(define-printer-function (schema-tag <boolean>) (lambda (b)(if b "true" "false")))
(define-printer-function (schema-tag <character>) object->string)
(define-printer-function (schema-tag <fixnum>) object->string)
(define-printer-function (schema-tag <bignum>) object->string)
(define-printer-function (schema-tag <flonum>) object->string)
(define-printer-function (schema-tag <ratnum>) object->string)
(define-printer-function (schema-tag <string>) object->string)
(define-printer-function (schema-tag <symbol>) object->string)
(define-printer-function (schema-tag <keyword>) object->string)
(define-printer-function (schema-tag <primitive-procedure>) 
  (lambda (x)(string-append "#<primitive-procedure " (object->string (object->serial-number x)) ">")))

(define-printer-function (schema-tag <alist-table>) 
  (lambda (tbl)
    (with-output-to-string 
      '() 
      (lambda () 
        (display "{")
        (let loop ((slots (alist-table-slots tbl))
                   (already '()))
          (if (not (null? slots))
              (let ((slot (car slots))
                    (more (cdr slots)))
                (if (member (car slot) already)
                    (loop more (cons (car slot) already))
                    (begin 
                      (if (not (null? already))(display " "))
                      (display (%as-string (car slot)))
                      (display " ")
                      (display (%as-string (cdr slot)))
                      (if (not (null? more))
                          (loop more (cons (car slot) already))))))))
        (display "}")))))

(define-printer-function (schema-tag <pair>) 
  (lambda (ls)
    (with-output-to-string
      '() 
      (lambda () 
        (display "(")
        (let loop ((items ls)
                   (already? #f))
          (if (not (null? items))
              (let ((item (car items))
                    (more (cdr items)))
                (if already? (display " "))
                (display (%as-string item))
                (if (pair? more)
                    (loop more #t)
                    (if (not (null? more))
                        (begin
                          (display " . ")
                          (display (%as-string more))))))))
        (display ")")))))

(define-printer-function (schema-tag <function>) 
  (lambda (fn)
    (let ((nm (function-name fn)))
      (if  nm
           (string-append "#<function " (object->string nm) ">")
           (string-append "#<an-anonymous-function " (object->string (object->serial-number fn)) ">")))))

(define-printer-function (schema-tag <interpreted-method>) 
  (lambda (m)
    (with-output-to-string
      '() 
      (lambda () 
        (let* ((name (interpreted-method-name m))
               (formals (interpreted-method-formals m))
               (restarg (interpreted-method-restarg m))
               (params (if restarg
                           (append formals (list '& restarg))
                           formals))
               (env (interpreted-method-environment m))
               (body (interpreted-method-body m)))
          (display "#:<interpreted-method>")
          (if (null? env)
              (begin
                (display "(method ")
                (display params)
                (display body))
              (let ((bindings (map (lambda (b)(list (car b)(cdr b)))
                                   env)))
                (display "(let ")
                (display bindings)
                (begin
                  (display "(method ")
                  (display params)
                  (display body))
                (display ")"))))))))

(define-printer-function (schema-tag <primitive>) 
  (lambda (pr)
    (let ((nm (primitive-name pr)))
      (if  nm
           (string-append "#<primitive " (object->string nm) ">")
           (string-append "#<an-anonymous-primitive " (object->string (object->serial-number pr)) ">")))))

(define-printer-function (schema-tag <class>) 
  (lambda (class)(object->string (class-name class))))

(define-printer-function (schema-tag <protocol>) 
  (lambda (p)(object->string (protocol-name p))))
