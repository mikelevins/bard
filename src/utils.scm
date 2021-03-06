;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          utils.scm
;;;; Project:       Bard
;;;; Purpose:       uncategorized utilities
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(declare (standard-bindings)
         (extended-bindings)
         (block))

;;; ---------------------------------------------------------------------
;;; file utils
;;; ---------------------------------------------------------------------

(define (read-file path)
  (call-with-input-file path
    (lambda (in)
      (apply string-append (interpose (string #\newline) (read-all in read-line))))))

;;; ---------------------------------------------------------------------
;;; system utils
;;; ---------------------------------------------------------------------

(define (assert test msg)
  (if test #t (error msg)))

(define (bound? name)
  (not (##unbound? (##global-var-ref (##make-global-var name)))))

(define (not-yet-implemented . args)
  (error "Not yet implemented" args))

(define (slib:error . args)
  (error (apply string-append (interpose " " (map object->string args)))))

;;; ---------------------------------------------------------------------
;;; functional utils
;;; ---------------------------------------------------------------------

(define (complement fn . args)
  (lambda more-args
    (not (apply fn `(,@args ,@more-args)))))

(define (constantly x) (lambda args x))

(define (current-continuation)
  (call/cc (lambda (cc) (cc cc))))

(define (identity x) x)

(define (flip fn)(lambda (x y)(fn y x)))

(define (iota n)
  (let loop ((i (- n 1))
             (result '()))
    (if (<= i 0)
        (cons i result)
        (loop (- i 1)
              (cons i result)))))

(define (partial fn . args)
  (lambda more-args
    (apply fn `(,@args ,@more-args))))

;;; ---------------------------------------------------------------------
;;; list utils
;;; ---------------------------------------------------------------------

(define (any ls)
  (list-ref ls (random-integer (length ls))))

(define (by n ls)
  (cond
   ((<= n 0) '())
   ((= n 1) (map list ls))
   (else (let ((_drop (lambda (c l)
                        (let loop ((l l)
                                   (c c))
                          (if (<= c 0)
                              l
                              (if (null? l)
                                  l
                                  (loop (cdr l)
                                        (- c 1)))))))
               (_take (lambda (c l)
                        (let loop ((l l)
                                   (c c))
                          (if (<= c 0)
                              '()
                              (if (null? l)
                                  l
                                  (cons (car l)
                                        (loop (cdr l)
                                              (- c 1)))))))))
           (let loop ((ls ls)
                      (result '()))
             (if (null? ls)
                 (reverse result)
                 (loop (_drop n ls)
                       (cons (_take n ls)
                             result))))))))


(define (copy-tree ls)
  (if (null? ls)
      '()
      (let ((head (car ls))
            (tail (cdr ls)))
        (cons (if (pair? head)
                  (copy-tree head)
                  head)
              (if (pair? tail)
                  (copy-tree tail)
                  tail)))))

(define (drop n ls)
  (list-tail ls n))

(define (every? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #t
        (if (fn (car items))
            (loop (cdr items))
            #f))))

(define (filter test ls)
  (if (null? ls)
      '()
      (if (test (car ls))
          (cons (car ls)
                (filter test (cdr ls)))
          (filter test (cdr ls)))))

(define (interpose item ls)
  (if (or (null? ls)
          (null? (cdr ls)))
      ls
      (cons (car ls)
            (cons item
                  (interpose item (cdr ls))))))

(define (make-list n #!optional (initial-element '()))
  (let loop ((i n)
             (result '()))
    (if (<= i 0)
        result
        (loop (- i 1)
              (cons initial-element result)))))

(define (next-last ls)
  (if (null? ls)
      (error "next-last: out of range")
      (if (null? (cdr ls))
          (error "next-last: out of range")
          (if (null? (cddr ls))
              (car ls)
              (next-last (cdr ls))))))

(define (nub ls #!key (test eq?))
  (let loop ((in ls)
             (out '()))
    (if (null? in)
        (reverse out)
        (let ((i (car in)))
          (if (some? (lambda (o)(test i o)) out)
              (loop (cdr in) out)
              (loop (cdr in)(cons i out)))))))

(define (position test ls #!key (start 0)(end #f))
  (let ((end (or end (length ls))))
    (let loop ((items (drop start ls))
               (i start))
      (if (or (null? items)
              (>= i end))
          #f
          (let ((item (car items))
                (more (cdr items)))
            (if (test item)
                i
                (loop more (+ 1 i))))))))

(define (reduce fn init args)
  (let loop ((items args)
             (result init))
    (if (null? items)
        result
        (loop (cdr items)
              (fn result (car items))))))

(define (remove-if test ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (test (car items))
            (remove-if test (cdr items))
            (cons (car items)
                  (remove-if test (cdr items)))))))

(define (remv item ls)
  (let loop ((items ls))
    (if (null? items)
        '()
        (if (eqv? item (car items))
            (remv item (cdr items))
            (cons (car items)
                  (remv item (cdr items)))))))

(define (some? fn ls)
  (let loop ((items ls))
    (if (null? items)
        #f
        (if (fn (car items))
            (car items)
            (loop (cdr items))))))

(define (take n ls)
  (let loop ((ls ls)
             (n n))
    (if (<= n 0)
        '()
        (if (null? ls)
            (error "count out of range")
            (cons (car ls)
                  (loop (cdr ls)
                        (- n 1)))))))

(define (take-by len advance ls)
  (cond
   ((<= len 0) '())
   (else (let ((_drop (lambda (c l)
                        (let loop ((l l)
                                   (c c))
                          (if (<= c 0)
                              l
                              (if (null? l)
                                  l
                                  (loop (cdr l)
                                        (- c 1)))))))
               (_take (lambda (c l)
                        (let loop ((l l)
                                   (c c))
                          (if (<= c 0)
                              '()
                              (if (null? l)
                                  l
                                  (cons (car l)
                                        (loop (cdr l)
                                              (- c 1)))))))))
           (let loop ((ls ls)
                      (result '()))
             (if (null? ls)
                 (reverse result)
                 (loop (_drop advance ls)
                       (cons (_take len ls)
                             result))))))))

(define (zip l1 l2)(map cons l1 l2))

;;; ---------------------------------------------------------------------
;;; alist utils
;;; ---------------------------------------------------------------------

(define (alist-get alist key #!key (default #f)(test equal?))
  (let loop ((alist alist))
    (if (null? alist)
        default
        (if (test key (car (car alist)))
            (car alist)
            (loop (cdr alist))))))

(define (alist-ref alist key #!key (default #f)(test equal?))
  (let ((slot (alist-get alist key default: default test: test)))
    (if (test slot default)
        default
        (cdr slot))))

;;; works only if the key is found in the alist
(define (alist-set! alist key val #!key (test equal?))
  (let ((slot (alist-get alist key test: test)))
    (if slot
        (set-cdr! slot val)
        (error (str "Key not found: " key)))))

(define (alist-put alist key val #!key (test equal?))
  (cons (cons key val)
        (alist-remove alist key test: test)))

(define (alist-remove alist key #!key (test equal?))
  (let loop ((in alist)
             (out '()))
    (if (null? in)
        (reverse out)
        (let ((entry (car in)))
          (if (test key (car entry))
              (loop (cdr in) out)
              (loop (cdr in)(cons entry out)))))))

(define (alist-keys alist)(map car alist))
(define (alist-vals alist)(map cdr alist))

(define (merge-alists a1 a2 #!optional (test equal?))
  (if (null? a1)
      a2
      (if (null? a2)
          a1
          (let loop1 ((slots1 a1)
                      (out1 '()))
            (if (null? slots1)
                (let loop2 ((slots2 a2)
                            (out2 out1))
                  (if (null? slots2)
                      (reverse out2)
                      (let* ((slot2 (car slots2))
                             (k (car slot2))
                             (out-slot (alist-get out2 k test: test)))
                        (if out-slot
                            (loop2 (cdr slots2) out2)
                            (loop2 (cdr slots2) (cons slot2 out2))))))
                (let* ((slot1 (car slots1))
                       (k (car slot1))
                       (slot2 (alist-get a2 k test: test)))
                  (if slot2
                      (loop1 (cdr slots1)(cons slot2 out1))
                      (loop1 (cdr slots1)(cons slot1 out1)))))))))

(define (plist->alist plist)
  (let loop ((kvs plist)
             (result '()))
    (if (null? kvs)
        (reverse result)
        (if (null? (cdr kvs))
            (error (string-append "Malformed plist: " (object->string plist)))
            (loop (cddr kvs)
                  (cons (cons (car kvs)(cadr kvs))
                        result))))))

;;; ---------------------------------------------------------------------
;;; plist utils
;;; ---------------------------------------------------------------------

(define (getf key plist #!key (default #f) (test equal?))
  (let loop ((items plist))
    (if (null? items)
        default
        (if (null? (cdr items))
            (error (str "malformed plist: " plist))
            (if (test key (car items))
                (cadr items)
                (loop (cddr items)))))))

;;; ---------------------------------------------------------------------
;;; string utils
;;; ---------------------------------------------------------------------

(define (comment-line? str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (let ((ch (string-ref str i)))
            (if (char-whitespace? ch)
                (loop (+ i 1))
                (if (char=? #\; ch)
                    #t
                    #f)))
          #f))))

(define (empty-string? str)
  (> 1 (string-length str)))

(define (left-trim str test)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (let ((ch (string-ref str i)))
            (if (test ch)
                (loop (+ i 1))
                (substring str i len)))
          ""))))

(define (nonempty-source-line? str)
  (and (string? str)
       (not (comment-line? str))
       (not (empty-string? (trim-whitespace str)))))


(define (right-trim str test)
  (let loop ((i (- (string-length str) 1)))
    (if (< i 0)
        ""
        (let ((ch (string-ref str i)))
          (if (test ch)
              (loop (- i 1))
              (substring str 0 (+ i 1)))))))

(define (string-char-position ch str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (if (char=? ch (string-ref str i))
              i
              (loop (+ 1 i)))
          #f))))

(define (string-char-position-if test str)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (< i len)
          (if (test (string-ref str i))
              i
              (loop (+ 1 i)))
          #f))))

(define (string-split-on ch str)
  (let loop ((instr str)
             (result '()))
    (if (< (string-length instr) 1)
        (reverse result)
        (let ((chpos (string-char-position ch instr)))
          (if chpos
              (let ((seg (substring instr 0 chpos))
                    (outstr (substring instr (+ 1 chpos) (string-length instr))))
                (loop outstr (cons seg result)))
              (loop "" (cons instr result)))))))

(define (str . args)
  (if (null? args)
      ""
      (let ((s (if (string? (car args))
                   (car args)
                   (if (char? (car args))
                       (string (car args))
                       (object->string (car args))))))
        (if (null? (cdr args))
            s
            (string-append s (apply str (cdr args)))))))

(define (string-join cupola strings)
  (apply string-append (interpose cupola strings)))

(define (string-next-last str)
  (string-ref str (- (string-length str) 2)))


(define (trim-whitespace str)
  (right-trim (left-trim str char-whitespace?) char-whitespace?))

;;; ---------------------------------------------------------------------
;;; vector utils
;;; ---------------------------------------------------------------------

(define (vector-every? fn vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0))
      (if (>= i len)
          #t
          (if (fn (vector-ref vec i))
              (loop (+ i 1))
              #f)))))

(define (vector-position test vec #!key (start 0)(end #f))
  (let ((start start)
        (end (or end (vector-length vec))))
    (let loop ((i start))
      (if (< i end)
          (if (test (vector-ref vec i))
              i
              (loop (+ 1 i)))
          #f))))

(define (vector-next-last vec)
  (vector-ref vec (- (vector-length vec) 2)))
