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

;; This Kawa module exports SRFI-41's (streams derived) library.

;(import (srfi :41 streams primitive))
(require 'srfi-41-streams-type)
(require 'srfi-41-streams-primitive)

(module-compile-options warn-undefined-variable: #t
                        warn-unknown-member: #t)

(module-export stream-null stream-cons stream? stream-null?
               stream-pair? stream-car stream-cdr stream-lambda
               define-stream list->stream port->stream stream
               stream->list stream-append stream-concat
               stream-constant stream-drop stream-drop-while
               stream-filter stream-fold stream-for-each stream-from
               stream-iterate stream-length stream-let stream-map
               stream-match stream-of stream-range stream-ref
               stream-reverse stream-scan stream-take
               stream-take-while stream-unfold stream-unfolds
               stream-zip)

(provide 'srfi-41-streams-derived)

;; R6RS's #'exists is equivalent to SRFI-1's #'any, at least in a
;; boolean context.
(require 'srfi-1)
(define exists any)

(define-syntax define-stream
  (syntax-rules ()
    ((_ (name . formal) body0 body1 ...)
     (define name (stream-lambda formal body0 body1 ...)))))

(define (list->stream (objs :: list)) :: stream-type
  (define list->stream
    (stream-lambda (objs :: list)
      (if (null? objs)
          stream-null
          (stream-cons (car objs) (list->stream (cdr objs))))))
  (list->stream objs))

(define (port->stream #!optional (p :: input-port (current-input-port))) :: stream-type
  (define port->stream
    (stream-lambda (p :: input-port)
      (let ((c :: character (read-char p)))
        (if (eof-object? c)
            stream-null
            (stream-cons c (port->stream p))))))
  (port->stream p))

(define-syntax stream
  (syntax-rules ()
    ((_) stream-null)
    ((_ x y ...) (stream-cons x (stream y ...)))))

(define (stream->list . args) :: list
  (let ((n (if (= 1 (length args)) #f (car args)))
        (strm :: stream-type (if (= 1 (length args)) (car args) (cadr args))))
    (cond ((and n (not (integer? n))) (error 'stream->list "non-integer count"))
          ((and n (negative? n)) (error 'stream->list "negative count"))
          (else (let loop ((n :: integer (if n n -1)) (strm :: stream-type strm))
                  (if (or (zero? n) (stream-null? strm))
                      '()
                      (cons (stream-car strm)
                            (loop (- n 1) (stream-cdr strm)))))))))

(define (stream-append . strms) :: stream-type
  (define stream-append
    (stream-lambda (strms :: list)
      (cond ((null? (cdr strms)) (car strms))
            ((stream-null? (car strms)) (stream-append (cdr strms)))
            (else (stream-cons (stream-car (car strms))
                               (stream-append (cons (stream-cdr (car strms)) (cdr strms))))))))
  (cond ((null? strms) stream-null)
        ((exists (lambda (x) (not (stream? x))) strms)
         (error 'stream-append "non-stream argument"))
        (else (stream-append strms))))

(define (stream-concat (strms :: stream-type)) :: stream-type
  (define stream-concat
    (stream-lambda (strms :: stream-type)
      (cond ((stream-null? strms) stream-null)
            ((not (stream? (stream-car strms)))
             (error 'stream-concat "non-stream object in input stream"))
            ((stream-null? (stream-car strms))
             (stream-concat (stream-cdr strms)))
            (else (stream-cons
                   (stream-car (stream-car strms))
                   (stream-concat
                    (stream-cons (stream-cdr (stream-car strms)) (stream-cdr strms))))))))
  (stream-concat strms))

(define stream-constant
  (stream-lambda objs
    (cond ((null? objs) stream-null)
          ((null? (cdr objs)) (stream-cons (car objs) (stream-constant (car objs))))
          (else (stream-cons (car objs)
                             (apply stream-constant (append (cdr objs) (list (car objs)))))))))

(define (stream-drop (n :: integer) (strm :: stream-type)) :: stream-type
  (define stream-drop
    (stream-lambda ((n :: integer) (strm :: stream-type))
      (if (or (zero? n) (stream-null? strm))
          strm
          (stream-drop (- n 1) (stream-cdr strm)))))
  (cond ((negative? n) (error 'stream-drop "negative argument"))
        (else (stream-drop n strm))))

(define (stream-drop-while (pred? :: procedure) (strm :: stream-type)) :: stream-type
  (define stream-drop-while
    (stream-lambda (strm :: stream-type)
      (if (and (stream-pair? strm) (pred? (stream-car strm)))
          (stream-drop-while (stream-cdr strm))
          strm)))
  (stream-drop-while strm))

(define (stream-filter (pred? :: procedure) (strm :: stream-type)) :: stream-type
  (define stream-filter
    (stream-lambda (strm :: stream-type)
      (cond ((stream-null? strm) stream-null)
            ((pred? (stream-car strm))
             (stream-cons (stream-car strm) (stream-filter (stream-cdr strm))))
            (else (stream-filter (stream-cdr strm))))))
  (stream-filter strm))

(define (stream-fold (proc :: procedure) base (strm :: stream-type))
  (let loop ((base base) (strm :: stream-type strm))
    (if (stream-null? strm)
        base
        (loop (proc base (stream-car strm)) (stream-cdr strm)))))

(define (stream-for-each (proc :: procedure) . strms)
  (define (stream-for-each (strms :: list))
    (if (not (exists stream-null? strms))
        (begin (apply proc (map stream-car strms))
               (stream-for-each (map stream-cdr strms)))))
  (cond ((null? strms) (error 'stream-for-each "no stream arguments"))
        ((exists (lambda (x) (not (stream? x))) strms)
         (error 'stream-for-each "non-stream argument"))
        (else (stream-for-each strms))))

(define (stream-from (first :: number) #!optional (delta :: number 1)) :: stream-type
  (define stream-from
    (stream-lambda ((first :: number) (delta :: number))
      (stream-cons first (stream-from (+ first delta) delta))))
  (stream-from first delta))

(define (stream-iterate (proc :: procedure) base) :: stream-type
  (define stream-iterate
    (stream-lambda (base)
      (stream-cons base (stream-iterate (proc base)))))
  (stream-iterate base))

(define (stream-length (strm :: stream-type)) :: integer
  (let loop ((len :: integer 0) (strm :: stream-type strm))
    (if (stream-null? strm)
        len
        (loop (+ len 1) (stream-cdr strm)))))

(define-syntax stream-let
  (syntax-rules ()
    ((_ tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (stream-lambda (name ...) body1 body2 ...))) tag) val ...))))

(define (stream-map (proc :: procedure) . strms) :: stream-type
  (define stream-map
    (stream-lambda (strms :: list)
      (if (exists stream-null? strms)
          stream-null
          (stream-cons (apply proc (map stream-car strms))
                       (stream-map (map stream-cdr strms))))))
  (cond ((null? strms) (error 'stream-map "no stream arguments"))
        ((exists (lambda (x) (not (stream? x))) strms)
         (error 'stream-map "non-stream argument"))
        (else (stream-map strms))))

(define-syntax stream-match
  (syntax-rules ()
    ((_ strm-expr clause ...)
     (let ((strm strm-expr))
       (cond
        ((not (stream? strm)) (error 'stream-match "non-stream argument"))
        ((stream-match-test strm clause) => car) ...
        (else (error 'stream-match "pattern failure")))))))

(define-syntax stream-match-test
  (syntax-rules ()
    ((_ strm (pattern fender expr))
     (stream-match-pattern strm pattern () (and fender (list expr))))
    ((_ strm (pattern expr))
     (stream-match-pattern strm pattern () (list expr)))))

(define-syntax stream-match-pattern 
  (lambda (x)
    (define (wildcard? x)
      (and (identifier? x)
           (free-identifier=? x (syntax _))))
    (syntax-case x () 
      ((_ strm () (binding ...) body)
       (syntax (and (stream-null? strm) (let (binding ...) body))))
      ((_ strm (w? . rest) (binding ...) body)
       (wildcard? #'w?) 
       (syntax (and (stream-pair? strm)
                    (let ((strm (stream-cdr strm)))
                      (stream-match-pattern strm rest (binding ...) body)))))
      ((_ strm (var . rest) (binding ...) body)
       (syntax (and (stream-pair? strm)
                    (let ((temp (stream-car strm)) (strm (stream-cdr strm))) 
                      (stream-match-pattern strm rest ((var temp) binding ...) body)))))
      ((_ strm w? (binding ...) body)
       (wildcard? #'w?)
       (syntax (let (binding ...) body)))
      ((_ strm var (binding ...) body) 
       (syntax (let ((var strm) binding ...) body))))))

(define-syntax stream-of
  (syntax-rules ()
    ((_ expr rest ...)
     (stream-of-aux expr stream-null rest ...))))

(define-syntax stream-of-aux
  (syntax-rules (in is)
    ((_ expr base)
     (stream-cons expr base))
    ((_ expr base (var in stream) rest ...)
     (stream-let loop ((strm stream))
       (if (stream-null? strm)
           base
           (let ((var (stream-car strm)))
             (stream-of-aux expr (loop (stream-cdr strm)) rest ...)))))
    ((_ expr base (var is exp) rest ...)
     (let ((var exp)) (stream-of-aux expr base rest ...)))
    ((_ expr base pred? rest ...)
     (if pred? (stream-of-aux expr base rest ...) base))))

(define (stream-range (first :: number) (past :: number) . step) :: stream-type
  (define stream-range
    (stream-lambda ((first :: number) (past :: number)
                    (delta :: number) (lt? :: procedure))
      (if (lt? first past)
          (stream-cons first (stream-range (+ first delta) past delta lt?))
          stream-null)))
  (let ((delta :: number (cond ((pair? step) (car step)) ((< first past) 1) (else -1))))
    (let ((lt? (if (< 0 delta) < >)))
      (stream-range first past delta lt?))))

(define (stream-ref (strm :: stream-type) (n :: integer))
  (cond ((negative? n) (error 'stream-ref "negative argument"))
        (else (let loop ((strm :: stream-type strm) (n :: integer n))
                (cond ((stream-null? strm) (error 'stream-ref "beyond end of stream"))
                      ((zero? n) (stream-car strm))
                      (else (loop (stream-cdr strm) (- n 1))))))))

(define (stream-reverse (strm :: stream-type)) :: stream-type
  (define stream-reverse
    (stream-lambda ((strm :: stream-type) (rev :: stream-type))
      (if (stream-null? strm)
          rev
          (stream-reverse (stream-cdr strm) (stream-cons (stream-car strm) rev)))))
  (stream-reverse strm stream-null))

(define (stream-scan (proc :: procedure) base (strm :: stream-type)) :: stream-type
  (define stream-scan
    (stream-lambda (base (strm :: stream-type))
      (if (stream-null? strm)
          (stream base)
          (stream-cons base (stream-scan (proc base (stream-car strm)) (stream-cdr strm))))))
  (stream-scan base strm))

(define (stream-take (n :: integer) (strm :: stream-type)) :: stream-type
  (define stream-take
    (stream-lambda ((n :: integer) (strm :: stream-type))
      (if (or (stream-null? strm) (zero? n))
          stream-null
          (stream-cons (stream-car strm) (stream-take (- n 1) (stream-cdr strm))))))
  (cond ((negative? n) (error 'stream-take "negative argument"))
        (else (stream-take n strm))))

(define (stream-take-while (pred? :: procedure) (strm :: stream-type)) :: stream-type
  (define stream-take-while
    (stream-lambda (strm :: stream-type)
      (cond ((stream-null? strm) stream-null)
            ((pred? (stream-car strm))
             (stream-cons (stream-car strm) (stream-take-while (stream-cdr strm))))
            (else stream-null))))
  (stream-take-while strm))

(define (stream-unfold (mapper :: procedure) (pred? :: procedure)
                       (generator :: procedure) base) :: stream-type
  (define stream-unfold
    (stream-lambda (base)
      (if (pred? base)
          (stream-cons (mapper base) (stream-unfold (generator base)))
          stream-null)))
  (stream-unfold base))

(define (stream-unfolds (gen :: procedure) seed)
  (define (len-values gen seed)
    (call-with-values
        (lambda () (gen seed))
      (lambda vs (- (length vs) 1))))
  (define unfold-result-stream
    (stream-lambda ((gen :: procedure) seed)
      (call-with-values
          (lambda () (gen seed))
        (lambda (next . results)
          (stream-cons results (unfold-result-stream gen next))))))
  (define result-stream->output-stream
    (stream-lambda ((result-stream :: stream-type) (i :: integer))
      (let ((result (list-ref (stream-car result-stream) (- i 1))))
        (cond ((pair? result)
               (stream-cons
                (car result)
                (result-stream->output-stream (stream-cdr result-stream) i)))
              ((not result)
               (result-stream->output-stream (stream-cdr result-stream) i))
              ((null? result) stream-null)
              (else (error 'stream-unfolds "can't happen"))))))
  (define (result-stream->output-streams result-stream)
    (let loop ((i :: integer (len-values gen seed)) (outputs :: list '()))
      (if (zero? i)
          (apply values outputs)
          (loop (- i 1) (cons (result-stream->output-stream result-stream i) outputs)))))
  (result-stream->output-streams (unfold-result-stream gen seed)))

(define (stream-zip . strms) :: stream-type
  (define stream-zip
    (stream-lambda (strms :: list)
      (if (exists stream-null? strms)
          stream-null
          (stream-cons (map stream-car strms) (stream-zip (map stream-cdr strms))))))
  (cond ((null? strms) (error 'stream-zip "no stream arguments"))
        ((exists (lambda (x) (not (stream? x))) strms)
         (error 'stream-zip "non-stream argument"))
        (else (stream-zip strms))))
