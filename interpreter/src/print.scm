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

;;; ---------------------------------------------------------------------
;;; printer registry
;;; ---------------------------------------------------------------------

(define +printer-functions+ (make-table test: eqv?))

(define (define-printer-function tag fn)
  (table-set! +printer-functions+ tag fn))

(define (get-printer-function tag)
  (table-ref +printer-functions+ tag #f))

;;; ---------------------------------------------------------------------
;;; printing utils
;;; ---------------------------------------------------------------------

(define (alist-slots->string alist)
  (with-output-to-string 
    '() 
    (lambda () 
      (let loop ((slots alist)
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
                        (loop more (cons (car slot) already)))))))))))

;;; ---------------------------------------------------------------------
;;; specialized printing
;;; ---------------------------------------------------------------------

(define (%record->string x)
  (let* ((schema (instance-schema x))
         (schema-name (schema-name schema)))
    (str "#" schema-name "{" (alist-slots->string (record-instance-slots x)) "}")))

(define (%tuple->string x)
  (with-output-to-string
    '() 
    (lambda ()
      (let* ((schema (instance-schema x))
             (schema-name (schema-name schema)))
        (display "#")
        (display schema-name)
        (display "(")
        (let* ((slots (tuple-instance-slots x))
               (slot-count (vector-length slots)))
          (let loop ((i 0))
            (if (< i slot-count)
                (let ((item (vector-ref slots i)))
                  (if (not (zero? i)) (display " "))
                  (display (%as-string item))
                  (loop (+ i 1))))))
        (display ")")))))

(define (%schema->string x)
  (str (schema-name x)))

;;; ---------------------------------------------------------------------
;;; general printing
;;; ---------------------------------------------------------------------

(define (%as-string x)
  (cond
   ((schema? x)(%schema->string x))
   ((record-instance? x)(%record->string x))
   ((tuple-instance? x)(%tuple->string x))
   (else: (let ((printer (get-printer-function (%tag x))))
            (if printer
                (printer x)
                (str "#<Unprintable value " (object->string x) " >"))))))

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
    (str "{" (alist-slots->string (alist-table-slots tbl)) "}")))

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

(define (function->string fn  #!key (name #f))
  (let* ((fname (or name ""))
         (sigs (function-signatures fn))
         (sigstr (string-join
                  (str (string #\newline) "  ")
                  (map (lambda (s)(signature->string s name: fname))
                       sigs))))
    (string-join "" `("(function " ,sigstr ")"))))

(define-printer-function (schema-tag <function>) 
  (lambda (fn)(function->string fn name: (function-instance-name fn))))

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

(define-printer-function (schema-tag <generator>) 
  (lambda (gen)(string-append "#<generator " (object->string (object->serial-number gen)) ">")))

(define-printer-function (schema-tag <class>) 
  (lambda (class)(object->string (class-name class))))

(define-printer-function (schema-tag <protocol>) 
  (lambda (p)
    (with-output-to-string
      '()
      (lambda ()
        (display "(protocol ")
        (display (if (protocol-instance-name p)
                     (object->string (protocol-instance-name p))
                     ""))
        (table-for-each (lambda (fname fn)
                          (newline)(display " ")
                          (display (function->string fn name: fname)))
                        (protocol-instance-functions p))
        (display ")")))))

(define-printer-function (schema-tag <singleton>) 
  (lambda (s)
    (with-output-to-string
        '()
        (lambda ()(display (str "(singleton " (singleton-value s) ")"))))))

(define-printer-function (schema-tag <record>) 
  (lambda (rec)(object->string (schema-name rec))))

(define-printer-function (schema-tag <iostream>) 
  (lambda (s)
    (cond
     ((and (input-port? s)(output-port? s)) (str "#<iostream " (object->string s) ">"))
     ((input-port? s) (str "#<input-stream " (object->string (object->serial-number s)) ">"))
     ((output-port? s) (str "#<output-stream " (object->string (object->serial-number s)) ">")))))

(define-printer-function (schema-tag <url>) 
  (lambda (url)
    (str "#<url>\""
         (url-scheme url) "://"
         (if (url-username url)
             (url-username url)
             "")
         (if (and (url-username url)
                  (url-password url))
             (str ":" (url-password url))
             "")
         (if (url-username url)
             "@"
             "")
         (url-domain url)
         (if (url-port url)
             (str ":" (url-port url))
             "")
         (url-path url)
         (if (%empty? (url-query url))
             ""
             (str "?" (url-query url)))
         "\"")))
