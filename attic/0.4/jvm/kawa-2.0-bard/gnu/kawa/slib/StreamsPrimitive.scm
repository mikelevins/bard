;; Based upon the SRFI-41 reference implementation which is:
;; Copyright (C) Philip L. Bewig (2007). All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Translation to Kawa: Copyright (C) Jamison Hope (2010).
;; Re-written to build on SRFI-45-compatible promises.

;; This Kawa module exports SRFI-41's (streams primitive) library.

(module-compile-options warn-undefined-variable: #t
                        warn-unknown-member: #t)

(module-export stream-null stream-cons stream? stream-null?
               stream? stream-type
               stream-pair? stream-car stream-cdr stream-lambda)

(require 'srfi-41-streams-type)

(define (stream? value)
  (instance? value Stream))

(define stream-null
  (stream-delay stream-null-1))

(define (stream-pair? obj) :: boolean
  (or (instance? obj StreamPair)
      (and (instance? obj StreamPromise)
           (stream-pair? (force obj)))))

(define (stream-null? obj) :: boolean
  (or (eq? obj stream-null-1)
      (and (gnu.mapping.Lazy? obj)
           (let ((v ((as gnu.mapping.Lazy obj):getValue)))
             (and (not (eq? v obj)) (stream-null? v))))))

(define-syntax stream-cons
  (syntax-rules ()
    ((_ obj strm)
     (StreamPair (stream-delay obj) (stream-lazy strm)))))

(define (stream-car strm)
  (cond ((stream-null? strm) (error 'stream-car "null stream"))
        (else (force (car strm)))))

(define (stream-cdr strm)
  (cond ((stream-null? strm) (error 'stream-cdr "null stream"))
        (else (cdr strm))))

(define-syntax stream-lambda
  (syntax-rules ()
    ((_ formals body0 body1 ...)
     (lambda formals (stream-lazy (let () body0 body1 ...))))))
