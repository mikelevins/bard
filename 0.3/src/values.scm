;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          values.scm
;;;; Project:       Bard
;;;; Purpose:       representation of basic Bard values
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (unit bard-values))
(declare (uses bard-utils))

;;; ----------------------------------------------------------------------
;;; Chicken Scheme prerequisites
;;; ----------------------------------------------------------------------

(require-extension coops)
(require-extension coops-primitive-objects)

;;; ----------------------------------------------------------------------
;;; undefined
;;; ----------------------------------------------------------------------

(define-class <undefined> ()())
(define-method (undefined? (x <standard-object>)) #f)
(define-method (undefined? (x <primitive-object>)) #f)
(define-method (undefined? (x <undefined>)) #t)
(define (defined? x) (not (undefined? x)))

(define +undefined+ (make <undefined>))
(define (undefined) +undefined+)

(define-method (print-object (x <undefined>)(out <port>))
  (display "undefined" out))

;;; ----------------------------------------------------------------------
;;; nothing
;;; ----------------------------------------------------------------------

(define-method (nothing? (x <standard-object>)) #f)
(define-method (nothing? (x <primitive-object>)) #f)
(define-method (nothing? (x <null>)) #t)
(define (something? x) (not (nothing? x)))

(define +nothing+ '())
(define (nothing) +nothing+)

(define-method (print-object (x <null>)(out <port>))
  (display "nothing" out))

;;; ----------------------------------------------------------------------
;;; Booleans
;;; ----------------------------------------------------------------------

(define-primitive-class <boolean> () boolean?)
(define (true? x) (eqv? x #t))
(define-primitive-class <true> (<boolean>) true?)
(define (false? x) (eqv? x #f))
(define-primitive-class <false> (<boolean>) false?)

(define-method (print-object (x <true>)(out <port>))
  (display "true" out))

(define-method (print-object (x <false>)(out <port>))
  (display "false" out))

;;; ----------------------------------------------------------------------
;;; characters
;;; ----------------------------------------------------------------------

(define-method (character? (x <char>)) #f)
(define-method (character? (x <char>)) #f)
(define-method (character? (x <char>)) #t)
(define <character> <char>)

(define-method (print-object (x <character>)(out <port>))
  (write x out))

;;; ----------------------------------------------------------------------
;;; symbols
;;; ----------------------------------------------------------------------

(define-method (print-object (x <symbol>)(out <port>))
  (display (symbol->string x) out))

;;; ----------------------------------------------------------------------
;;; keywords
;;; ----------------------------------------------------------------------

(define-method (print-object (x <keyword>)(out <port>))
  (display (symbol->string x) out)
  (display ":" out))

;;; ----------------------------------------------------------------------
;;; numbers
;;; ----------------------------------------------------------------------

(define-method (print-object (x <number>)(out <port>))
  (display (number->string x) out))

;;; ----------------------------------------------------------------------
;;; lists
;;; ----------------------------------------------------------------------

(define-method (print-object (x <pair>)(out <port>))
  (if (null? x)
      (display "[]" out)
      (let ((hd (car x))
            (tl (cdr x)))
        (if (null? tl)
            (begin
              (display "[" out)
              (print-object hd out)
              (display "]" out))
            (begin
              (display "[" out)
              (print-object hd out)
              (let loop ((more tl))
                (if (not (null? more))
                    (begin
                      (display " " out)
                      (print-object (car more) out)
                      (loop (cdr more)))))
              (display "]" out))))))

;;; ----------------------------------------------------------------------
;;; text
;;; ----------------------------------------------------------------------

(define-primitive-class <text> () string?)
(define-method (text? (x <standard-object>)) #f)
(define-method (text? (x <primitive-object>)) #f)
(define-method (text? (x <text>)) #t)

(define-method (print-object (x <text>)(out <port>))
  (display "\"" out)
  (display x out)
  (display "\"" out))

;;; ----------------------------------------------------------------------
;;; tables
;;; ----------------------------------------------------------------------
;;; this implementation of tables wraps alists. using alists keeps
;;; the implementation simple, while enabling us to preserve the
;;; table API requirement that keys appear in the order they were
;;; added to the table, which in turn preserves stable behavior
;;; under the List protocol. However, access is O(n), so
;;; we may wish to add other table representations that use
;;; more complicated data structures. 

(define-class <simple-table> ()
  ((entries reader: table-entries initform: '())))

(define-method (table? (x <standard-object>)) #f)
(define-method (table? (x <primitive-object>)) #f)
(define-method (table? (x <simple-table>)) #t)

(define-method (print-object (x <simple-table>)(out <port>))
  (let ((entries (table-entries x)))
    (if (null? entries)
        (display "{}" out)
        (let ((hd (car entries))
              (tl (cdr entries))
              (print-entry (lambda (k v)
                             (print-object k out)
                             (display " " out)
                             (print-object v out))))
          (if (null? tl)
              (begin
                (display "{" out)
                (print-entry (car hd) (cdr hd))
                (display "}" out))
              (begin
                (display "{" out)
                (print-entry (car hd) (cdr hd))
                (let loop ((more tl))
                  (if (not (null? more))
                      (begin
                        (display " " out)
                        (print-entry (car (car more)) (cdr (car more)))
                        (loop (cdr more)))))
                (display "}" out)))))))

