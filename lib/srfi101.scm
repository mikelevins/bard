;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          srfi101.scm
;;;; Project:       bard
;;;; Purpose:       srfi101 random-access (purely functional) lists
;;;; Author:        adapted from David Van Horn's srfi101 reference
;;;;                implementation by mikel evins
;;;;
;;;; ***********************************************************************

;; SRFI 101: Purely Functional Random-Access Pairs and Lists
;; Copyright (c) David Van Horn 2009.  All Rights Reserved.

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without restriction,
;; including without limitation the rights to use, copy, modify, merge,
;; publish, distribute, sublicense, and/or sell copies of the Software,
;; and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. REMEMBER, THERE IS NO SCHEME UNDERGROUND. IN NO EVENT
;; SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
;; DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
;; OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
;; THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-type ra:kons
  id: 00080B15-5B9B-4626-A4C4-A61CFFF52D9E
  constructor: ra:make-kons
  (size ra:kons-size)
  (tree ra:kons-tree)
  (rest ra:kons-rest))

(define-type ra:node 
  id: 988F81FB-5A88-46B8-9BE3-51B9C3361FA3
  constructor: ra:make-node
  (val ra:node-val)
  (left ra:node-left)
  (right ra:node-right)) 

;; Nat -> Nat
(define (sub1 n) (- n 1))
(define (add1 n) (+ n 1))

;; [Tree X] -> X
(define (tree-val t)
  (if (ra:node? t) 
      (ra:node-val t)
      t))

;; [X -> Y] [Tree X] -> [Tree Y]
(define (tree-map f t)
  (if (ra:node? t)
      (ra:make-node (f (ra:node-val t))
                 (tree-map f (ra:node-left t))
                 (tree-map f (ra:node-right t)))
      (f t)))

;; [X -> Y] [Tree X] -> unspecified
(define (tree-for-each f t)
  (if (ra:node? t)
      (begin (f (ra:node-val t))
             (tree-for-each f (ra:node-left t))
             (tree-for-each f (ra:node-right t)))
      (f t)))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> [Tree R]
(define (tree-map/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (ra:node? (car ts)))
        (ra:make-node (apply f (map ra:node-val ts))
                   (recr (map ra:node-left ts))
                   (recr (map ra:node-right ts)))
        (apply f ts))))

;; [X Y Z ... -> R] [List [Tree X] [Tree Y] [Tree Z] ...] -> unspecified
(define (tree-for-each/n f ts)
  (let recr ((ts ts))
    (if (and (pair? ts)
             (ra:node? (car ts)))
        (begin (apply f (map ra:node-val ts))
               (recr (map ra:node-left ts))
               (recr (map ra:node-right ts)))
        (apply f ts))))

;; Nat [Nat -> X] -> [Tree X]
;; like build-list, but for complete binary trees
(define (build-tree i f) ;; i = 2^j-1
  (let rec ((i i) (o 0))
    (if (= 1 i) 
        (f o)
        (let ((i/2 (half i)))
          (ra:make-node (f o)
                     (rec i/2 (add1 o))
                     (rec i/2 (+ 1 o i/2)))))))

;; Consumes n = 2^i-1 and produces 2^(i-1)-1.
;; Nat -> Nat
(define (half n)
  (arithmetic-shift n -1))

;; Nat X -> [Tree X]
(define (tr:make-tree i x) ;; i = 2^j-1
  (let recr ((i i))
    (if (= 1 i) 
        x
        (let ((n (recr (half i))))
          (ra:make-node x n n)))))

;; Nat [Tree X] Nat [X -> X] -> X [Tree X]
#| modified 2012-05-25 by mikel evins
   replaced let-values with receive for compatibility with Gambit
   replaced make-ra:node with ra:make-node
(define (tree-ref/update mid t i f)
  (cond ((zero? i)
         (if (ra:node? t) 
             (values (ra:node-val t)
                     (ra:make-node (f (ra:node-val t))
                                (ra:node-left t)
                                (ra:node-right t)))
             (values t (f t))))
        ((<= i mid)
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (ra:node-left t) 
                                                (sub1 i) 
                                                f)))
           (values v* (make-ra:node (ra:node-val t) t* (ra:node-right t)))))
        (else           
         (let-values (((v* t*) (tree-ref/update (half (sub1 mid)) 
                                                (ra:node-right t) 
                                                (sub1 (- i mid)) 
                                                f)))
           (values v* (ra:make-node (ra:node-val t) (ra:node-left t) t*))))))
|#

(define (tree-ref/update mid t i f)
  (cond ((zero? i)
         (if (ra:node? t) 
             (values (ra:node-val t)
                     (ra:make-node (f (ra:node-val t))
                                (ra:node-left t)
                                (ra:node-right t)))
             (values t (f t))))
        ((<= i mid)
         (receive (v* t*)
                  (tree-ref/update (half (sub1 mid)) 
                                   (ra:node-left t) 
                                   (sub1 i) 
                                   f)
                  (values v* (ra:make-node (ra:node-val t) t* (ra:node-right t)))))
        (else           
         (receive (v* t*)
                  (tree-ref/update (half (sub1 mid)) 
                                   (ra:node-right t) 
                                   (sub1 (- i mid)) 
                                   f)
                  (values v* (ra:make-node (ra:node-val t) (ra:node-left t) t*))))))

;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
;; [Tree X] Nat Nat -> X
;; invariant: (= mid (half (sub1 (tree-count t))))
(define (tree-ref/a t i mid) 
  (cond ((zero? i) (tree-val t))
        ((<= i mid) 
         (tree-ref/a (ra:node-left t) 
                     (sub1 i) 
                     (half (sub1 mid))))
        (else 
         (tree-ref/a (ra:node-right t) 
                     (sub1 (- i mid)) 
                     (half (sub1 mid))))))

;; Nat [Tree X] Nat -> X
;; invariant: (= size (tree-count t))
(define (tree-ref size t i)
  (if (zero? i)
      (tree-val t)
      (tree-ref/a t i (half (sub1 size)))))

;; Nat [Tree X] Nat [X -> X] -> [Tree X]
(define (tree-update size t i f)
  (let recr ((mid (half (sub1 size))) (t t) (i i))
    (cond ((zero? i)
           (if (ra:node? t)
               (ra:make-node (f (ra:node-val t))
                          (ra:node-left t)
                          (ra:node-right t))
               (f t)))
          ((<= i mid)
           (ra:make-node (ra:node-val t) 
                      (recr (half (sub1 mid))
                            (ra:node-left t) 
                            (sub1 i)) 
                      (ra:node-right t)))
          (else
           (ra:make-node (ra:node-val t) 
                      (ra:node-left t) 
                      (recr (half (sub1 mid))
                            (ra:node-right t) 
                            (sub1 (- i mid))))))))

;; ------------------------
;; Random access lists

;; [RaListof X]
(define ra:null (quote ()))

;; [Any -> Boolean]
(define ra:pair? ra:kons?)

;; [Any -> Boolean]
(define ra:null? null?)

;; X [RaListof X] -> [RaListof X]  /\
;; X Y -> [RaPair X Y]
(define (ra:cons x ls)
  (if (ra:kons? ls)
      (let ((s (ra:kons-size ls)))
        (if (and (ra:kons? (ra:kons-rest ls))
                 (= (ra:kons-size (ra:kons-rest ls))
                    s))
            (ra:make-kons (+ 1 s s) 
                       (ra:make-node x 
                                  (ra:kons-tree ls)
                                  (ra:kons-tree (ra:kons-rest ls)))
                       (ra:kons-rest (ra:kons-rest ls)))
            (ra:make-kons 1 x ls)))
      (ra:make-kons 1 x ls)))


;; [RaPair X Y] -> X Y
(define ra:car+cdr 
  (lambda (p)
    (if (ra:node? (ra:kons-tree p))
        (let ((s* (half (ra:kons-size p))))
          (values (tree-val (ra:kons-tree p))
                  (ra:make-kons s* 
                             (ra:node-left (ra:kons-tree p))
                             (ra:make-kons s*
                                           (ra:node-right (ra:kons-tree p))
                                           (ra:kons-rest p)))))
        (values (ra:kons-tree p) (ra:kons-rest p)))))

;; [RaPair X Y] -> X
(define (ra:car p)
  (call-with-values (lambda () (ra:car+cdr p))
    (lambda (kar kdr) kar)))

;; [RaPair X Y] -> Y
(define (ra:cdr p)
  (call-with-values (lambda () (ra:car+cdr p))
    (lambda (kar kdr) kdr)))

;; [RaListof X] Nat [X -> X] -> X [RaListof X]
(define (ra:list-ref/update ls i f)
  (let recr ((xs ls) (j i))
    (if (< j (ra:kons-size xs))
        (receive (v* t*)(tree-ref/update (half (sub1 (ra:kons-size xs))) 
                                         (ra:kons-tree xs) j f)
                 (values v* (ra:make-kons (ra:kons-size xs) 
                                          t* 
                                          (ra:kons-rest xs))))
        (receive (v* r*)(recr (ra:kons-rest xs) 
                                 (- j (ra:kons-size xs)))
                    (values v* (ra:make-kons (ra:kons-size xs) 
                                             (ra:kons-tree xs) 
                                             r*))))))

;; [RaListof X] Nat [X -> X] -> [RaListof X]
(define (ra:list-update ls i f)
  (let recr ((xs ls) (j i))
    (let ((s (ra:kons-size xs)))
      (if (< j s) 
          (ra:make-kons s (tree-update s (ra:kons-tree xs) j f) (ra:kons-rest xs))
          (ra:make-kons s (ra:kons-tree xs) (recr (ra:kons-rest xs) (- j s)))))))

;; [RaListof X] Nat X -> (values X [RaListof X])
(define (ra:list-ref/set ls i v)
  (ra:list-ref/update ls i (lambda (_) v)))

;; X ... -> [RaListof X]
(define (ra:list . xs)
  (fold-right ra:cons ra:null xs))

;; Nat X -> [RaListof X]
(define (ra:make-list k . rest)
  (if (null? rest)
      (ra:make-list k 0)
      (let ((obj (car rest)))
        (let loop ((n k) 
                   (a ra:null))
          (cond ((zero? n) a)
                (else 
                 (let ((t (largest-skew-binary n)))
                   (loop (- n t)
                         (ra:make-kons t (tr:make-tree t obj) a)))))))))

;; A Skew is a Nat 2^k-1 with k > 0.

;; Skew -> Skew
(define (skew-succ t) (add1 (arithmetic-shift t 1)))

;; Computes the largest skew binary term t <= n.
;; Nat -> Skew
(define (largest-skew-binary n)
  (if (= 1 n) 
      1
      (let* ((t (largest-skew-binary (half n)))
             (s (skew-succ t)))
        (if (> s n) t s))))  

;; [Any -> Boolean]
;; Is x a PROPER list?
(define (ra:list? x)
  (or (ra:null? x)
      (and (ra:kons? x)
           (ra:list? (ra:kons-rest x)))))

(define ra:caar (lambda (ls) (ra:car (ra:car ls))))
(define ra:cadr (lambda (ls) (ra:car (ra:cdr ls))))
(define ra:cddr (lambda (ls) (ra:cdr (ra:cdr ls))))
(define ra:cdar (lambda (ls) (ra:cdr (ra:car ls))))

(define ra:caaar (lambda (ls) (ra:car (ra:car (ra:car ls)))))
(define ra:caadr (lambda (ls) (ra:car (ra:car (ra:cdr ls)))))
(define ra:caddr (lambda (ls) (ra:car (ra:cdr (ra:cdr ls)))))
(define ra:cadar (lambda (ls) (ra:car (ra:cdr (ra:car ls)))))
(define ra:cdaar (lambda (ls) (ra:cdr (ra:car (ra:car ls)))))
(define ra:cdadr (lambda (ls) (ra:cdr (ra:car (ra:cdr ls)))))
(define ra:cdddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr ls)))))
(define ra:cddar (lambda (ls) (ra:cdr (ra:cdr (ra:car ls)))))

(define ra:caaaar (lambda (ls) (ra:car (ra:car (ra:car (ra:car ls))))))
(define ra:caaadr (lambda (ls) (ra:car (ra:car (ra:car (ra:cdr ls))))))
(define ra:caaddr (lambda (ls) (ra:car (ra:car (ra:cdr (ra:cdr ls))))))
(define ra:caadar (lambda (ls) (ra:car (ra:car (ra:cdr (ra:car ls))))))
(define ra:cadaar (lambda (ls) (ra:car (ra:cdr (ra:car (ra:car ls))))))
(define ra:cadadr (lambda (ls) (ra:car (ra:cdr (ra:car (ra:cdr ls))))))
(define ra:cadddr (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:cdr ls))))))
(define ra:caddar (lambda (ls) (ra:car (ra:cdr (ra:cdr (ra:car ls))))))
(define ra:cdaaar (lambda (ls) (ra:cdr (ra:car (ra:car (ra:car ls))))))
(define ra:cdaadr (lambda (ls) (ra:cdr (ra:car (ra:car (ra:cdr ls))))))
(define ra:cdaddr (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:cdr ls))))))
(define ra:cdadar (lambda (ls) (ra:cdr (ra:car (ra:cdr (ra:car ls))))))
(define ra:cddaar (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:car ls))))))
(define ra:cddadr (lambda (ls) (ra:cdr (ra:cdr (ra:car (ra:cdr ls))))))
(define ra:cddddr (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:cdr ls))))))
(define ra:cdddar (lambda (ls) (ra:cdr (ra:cdr (ra:cdr (ra:car ls))))))

;; [RaList X] -> Nat
(define (ra:length ls)
  (let recr ((ls ls))
    (if (ra:kons? ls)
        (+ (ra:kons-size ls) (recr (ra:kons-rest ls)))
        0)))

(define (make-foldl empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (f cons
                       (cons (first ls) empty) 
                       (rest ls))))))
    f))

(define fold-left (make-foldl null? car cdr))

(define (make-foldr empty? first rest)
  (letrec ((f (lambda (cons empty ls)
                (if (empty? ls) 
                    empty
                    (cons (first ls)
                          (f cons empty (rest ls)))))))
    f))

(define fold-right (make-foldr null? car cdr))

;; [X Y -> Y] Y [RaListof X] -> Y
(define ra:foldl/1 (make-foldl ra:null? ra:car ra:cdr))
(define ra:foldr/1 (make-foldr ra:null? ra:car ra:cdr))

;; [RaListof X] ... -> [RaListof X]
(define (ra:append . lss)
  (cond ((null? lss) ra:null)
        (else (let recr ((lss lss))
                (cond ((null? (cdr lss)) (car lss))
                      (else (ra:foldr/1 ra:cons
                                        (recr (cdr lss))
                                        (car lss))))))))

;; [RaListof X] -> [RaListof X]
(define (ra:reverse ls)
  (ra:foldl/1 ra:cons ra:null ls))

;; [RaListof X] Nat -> [RaListof X]
(define (ra:list-tail ls i)
  (let loop ((xs ls) (j i))
    (cond ((zero? j) xs)
          (else (loop (ra:cdr xs) (sub1 j))))))

;; [RaListof X] Nat -> X
;; Special-cased above to avoid logarathmic amount of cons'ing
;; and any multi-values overhead.  Operates in constant space.
(define (ra:list-ref ls i)
  (let loop ((xs ls) (j i))
    (if (< j (ra:kons-size xs))
        (tree-ref (ra:kons-size xs) (ra:kons-tree xs) j)
        (loop (ra:kons-rest xs) (- j (ra:kons-size xs))))))

;; [RaListof X] Nat X -> [RaListof X]
(define (ra:list-set ls i v)
  (receive (_ l*) (ra:list-ref/set ls i v)
           l*))

;; [X ... -> y] [RaListof X] ... -> [RaListof Y]
;; Takes advantage of the fact that map produces a list of equal size.
(define (ra:map f . lss)
  (if (= 1 (length lss))
      (let ((ls (car lss)))
        (let recr ((ls ls))
          (if (ra:kons? ls)
              (ra:make-kons (ra:kons-size ls) 
                         (tree-map f (ra:kons-tree ls)) 
                         (recr (ra:kons-rest ls)))
              ra:null)))
      (let recr ((lss lss))
        (cond ((ra:null? (car lss)) ra:null)
              (else
               ;; IMPROVE ME: make one pass over lss.
               (ra:make-kons (ra:kons-size (car lss))
                          (tree-map/n f (map ra:kons-tree lss))
                          (recr (map ra:kons-rest lss))))))))


;; [X ... -> Y] [RaListof X] ... -> unspecified
(define (ra:for-each f . lss)
  (if (= 1 (length lss))
      (let ((ls (car lss)))
        (if (ra:kons? ls)
            (begin
              (tree-for-each f (ra:kons-tree ls))
              (ra:for-each f (ra:kons-rest ls)))))
      (let recr ((lss lss))
        (if (ra:pair? (car lss))
            (begin
              (tree-map/n f (map ra:kons-tree lss))
              (recr (map ra:kons-rest lss)))))))

;; [RaListof X] -> [Listof X]
(define (ra:random-access-list->linear-access-list x)
  (ra:foldr/1 cons '() x))

;; [Listof X] -> [RaListof X]
(define (ra:linear-access-list->random-access-list x)
  (fold-right ra:cons '() x))

;; This code based on code written by Abdulaziz Ghuloum
;; http://ikarus-scheme.org/pipermail/ikarus-users/2009-September/000595.html
;; (define get-cached
;;   (let ((h (make-table)))
;;     (lambda (x)
;;       (define (f x)
;;         (cond
;;          ((pair? x) (ra:cons (f (car x)) (f (cdr x))))
;;          ((vector? x) (vector-map f x))
;;          (else x)))
;;       (cond
;;        ((not (or (pair? x) (vector? x))) x)
;;        ((table-ref h x #f))
;;        (else
;;         (let ((v (f x)))
;;           (hashtable-set! h x v)
;;           v))))))


;;; ra:null
;;; (ra:null? ra:null)
;;; (define $s1 (ra:cons 'a ra:null))
;;; (ra:pair? $s1)
;;; (ra:null? $s1)
;;; (define $s2 (ra:cons 'b $s1))
;;; (ra:pair? $s2)
;;; (ra:car $s2)
;;; (ra:cadr $s2)
;;; (define $s3 (ra:list 0 1 2 3 4))
;;; (ra:car $s3)
;;; (ra:cadr $s3)
;;; (ra:caddr $s3)
;;; (ra:cadddr $s3)
;;; (ra:car (ra:cdr (ra:cdr (ra:cdr (ra:cdr $s3)))))
;;; (ra:cdr (ra:cdr (ra:cdr (ra:cdr (ra:cdr $s3)))))
;;; (define $s4 (ra:make-list 2))
;;; (ra:car $s4)
;;; (ra:cadr $s4)
;;; (define $s4 (ra:make-list 2 'a))
;;; (ra:car $s4)
;;; (ra:cadr $s4)
;;; (ra:list? $s4)
;;; (ra:list? '(0 1 2 3))
;;; (ra:length $s4)
;;; (define $r1 (ra:list 0 1 2))
;;; (define $r2 (ra:list 3 4 5))
;;; (define $r3 (ra:append $r1 $r2))
;;; (ra:car $r3)
;;; (ra:length $r3)
;;; (ra:list-ref $r3 5)
;;; (define $r4 (ra:reverse $r3))
;;; (ra:car $r4)
;;; (ra:length $r4)
;;; (ra:list-ref $r4 5)
;;; (define $r5 (ra:list-tail $r3 1))
;;; (ra:car $r5)
;;; (ra:length $r5)
;;; (ra:list-ref $r5 4)
;;; (define $q1 (ra:list 0 1 2 3 4))
;;; (define $q2 (ra:map (lambda (x)(+ x 1)) $q1))
;;; (ra:for-each (lambda (x)(display x)(newline)) $q2)
;;; (ra:random-access-list->linear-access-list $q1)
;;; (define $p1 (ra:linear-access-list->random-access-list '(4 3 2 1 0)))
;;; (ra:for-each (lambda (x)(display x)(newline)) $p1)
;;; (define $o1 (ra:list 0 1 2 3 4))
;;; (define $o2 (ra:list-update $o1 2 (lambda (x) 'two)))
;;; (ra:random-access-list->linear-access-list $o2)
;;; (ra:random-access-list->linear-access-list $o1)
