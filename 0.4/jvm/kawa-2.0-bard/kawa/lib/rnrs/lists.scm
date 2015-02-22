;; The (rnrs lists (6)) library. Contains some code from Kawa's SRFI-1
;; implementation (i.e. the reference implementation, which is
;; Copyright (c) 1998, 1999 by Olin Shivers). Documentation strings
;; are adapted from the text of the R6RS List utilities library
;; specification. The rest is Copyright (c) 2011 by Jamison Hope.

(module-name kawa.lib.rnrs.lists)
(module-export find for-all exists filter partition fold-left
               fold-right remp remove remv remq memp member memv memq
               assp assoc assv assq cons*)
(require kawa.lib.lists "../lists.scm")

;;; Helper Functions

(define (complement (proc ::procedure)) ::procedure
  (lambda (x) (not (proc x))))

;; car+cdr, %cars+cdrs, %cars+cdrs/pair, %cdrs, %cars+ taken from
;; srfi1.scm
(define (car+cdr (pair ::pair)) (values (car pair) (cdr pair)))

(define (%cars+cdrs (lists ::list))
  "LISTS is a (not very long) non-empty list of lists. Return two
lists: the cars & the cdrs of the lists. However, if any of the lists
is empty, just abort and return [() ()]."
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
           (receive (list other-lists) (car+cdr lists)
             (if (null? list) (abort '() '()) ; LIST is empty -- bail out
                 (receive (a d) (car+cdr list)
                   (receive (cars cdrs) (recur other-lists)
                     (values (cons a cars) (cons d cdrs))))))
           (values '() '()))))))

(define (%cars+cdrs/pair (lists ::list)) ::pair
  "Return the `%cars+cdrs' result as a pair instead of a multiple
value return.  Kawa finds it easier to optimize a tail recursive loop
when the `%cars+cdrs' logic is called this way."
  (let-values (((cars cdrs) (%cars+cdrs lists)))
    (cons cars cdrs)))

(define (%cdrs lists)
  "Return (map cdr lists). However, if any element of LISTS is empty,
just abort and return '()."
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
           (let ((lis (car lists)))
             (if (null? lis) (abort '())
                 (cons (cdr lis) (recur (cdr lists)))))
           '())))))

(define (%cars+ (lists ::list) last-elt) ::list
  ; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists)))
        (list last-elt))))

;;; Exported Functions

;;; find
(define (find (proc ::procedure) (lst ::list))
  "PROC should accept one argument and return a single value. PROC
should not mutate LST. The `find' procedure applies PROC to the
elements of LST in order. If PROC returns a true value for an
element, `find' immediately returns that element. If PROC returns #f
for all elements of the list, `find' returns #f. PROC is always called
in the same dynamic environment as `find' itself."
  (let loop ((list lst))
    (if (null? list) #f
        (let ((x (car list)))
          (if (proc x) x
              (loop (cdr list)))))))

;;; for-all, exists (based upon SRFI-1 every, any)
(define (for-all (proc ::procedure) (list1 ::list) . lists)
  "The LISTs should all have the same length, and PROC should accept N
arguments and return a single value. PROC should not mutate the LIST
arguments.

For natural numbers I=0,1,..., the `for-all' procedure successively
applies PROC to arguments X_I^1 ... X_I^N, where X_I^J is the Ith
element of LIST_J, until #f is returned. If PROC returns true values
for all but the last element of LIST_1, `for-all' performs a tail call
of PROC on the Kth elements, where K is the length of LIST_1. If PROC
returns #f on any set of elements, `for-all' returns #f after the
first such application of PROC. If the LISTs are all empty, `for-all'
returns #t.

PROC is always called in the same dynamic environment as `for-all'
itself."
  (if (pair? lists)
      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons list1 lists))
        (or (not (pair? heads))
            (let lp ((heads heads) (tails tails))
              (receive (next-heads next-tails) (%cars+cdrs tails)
                (if (pair? next-heads)
                    (and (apply proc heads) (lp next-heads next-tails))
                    (apply proc heads)))))) ; Last PROC app is tail call.
      ;; Fast path
      (or (null? list1)
          (let lp ((head (car list1)) (tail (cdr list1)))
            (if (null? tail)
                (proc head)             ; Last PROC app is tail call.
                (and (proc head) (lp (car tail) (cdr tail))))))))

(define (exists (proc ::procedure) (list1 ::list) . lists)
  "The LISTs should all have the same length, and PROC should accept N
arguments and return a single value. PROC should not mutate the LIST
arguments.

For natural numbers I=0,1,..., the `exists' procedure applies PROC
successively to arguments X_I^1 ... X_I^N, where X_I^N is the Ith
element of LIST_J, until a true value is returned. If PROC returns #f
for all but the last elements of the LISTs, `exists' performs a tail
call of PROC on the Kth elements, where K is the length of LIST_1. If
PROC returns a true value on any set of elements, `exists' returns
that value after the first such application of PROC. If the LISTs are
all empty, `exists' returns #f.

PROC is always called in the same dynamic environment as `exists'
itself."
  (if (pair? lists)
      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons list1 lists))
        (and (pair? heads)
             (let lp ((heads heads) (tails tails))
               (let* ((split (%cars+cdrs/pair tails))
                      (next-heads (car split))
                      (next-tails (cdr split)))
                 (if (pair? next-heads)
                     (or (apply proc heads) (lp next-heads next-tails))
                     (apply proc heads)))))) ; Last PROC app is tail call.
      ;; Fast path
      (and (not (null? list1))
           (let lp ((head (car list1)) (tail (cdr list1)))
             (if (null? tail)
                 (proc head)            ; Last PROC app is tail call.
                 (or (proc head) (lp (car tail) (cdr tail))))))))

;;; filter, partition
(define (filter (proc ::procedure) (lst ::list))
  "PROC should accept one argument and return a single value. PROC
should not mutate LST.

The `filter' procedure applies PROC to each element of LST and
returns a list of the elements of LST for which PROC returned a true
value. The elements of the result list are in the same order as they
appear in the input list. PROC is always called in the same dynamic
environment as `filter' itself. If multiple returns occur from
`filter', the return values returned by earlier returns are not
mutated."
  (let recur ((list lst) (res '()))
    (if (null? list)
        (reverse! res)
        (let ((head (car list))
              (tail (cdr list)))
          (if (proc head)
              (recur tail (cons head res))
              (recur tail res))))))

(define (partition (proc ::procedure) (lst ::list))
  "PROC should accept one argument and return a single value. PROC
should not mutate LST.

The `partition' procedure applies PROC to each element of LST, and
returns two values, the first one a list of the elements of LST for
which PROC returned a true value, and the second a list of the
elements of LST for which PROC returned #f. The elements of the
result lists are in the same order as they appear in the input
list. PROC is always called in the same dynamic environment as
`partition' itself. If multiple returns occur from `partition', the
return values returned by earlier returns are not mutated."
  (let loop ((list lst) (in '()) (out '()))
    (if (null? list)
        (values (reverse! in) (reverse! out))
        (let ((head (car list))
              (tail (cdr list)))
          (if (proc head)
              (loop tail (cons head in) out)
              (loop tail in (cons head out)))))))

;;; fold-left
(define (fold-left (combine ::procedure) nil (list1 ::list) . lists)
  "The LISTs should all have the same length. COMBINE must be a
procedure. It should accept one more argument than there are LISTs and
return a single value. It should not mutate the LIST arguments. The
`fold-left' procedure iterates the COMBINE procedure over an
accumulator value and the elements of the LISTs from left to right,
starting with an accumulator value of NIL. More specifically,
`fold-left' returns NIL if the LISTs are empty. If they are not empty,
COMBINE is first applied to NIL and the respective first elements of
the LISTs in order. The result becomes the new accumulator value, and
COMBINE is applied to the new accumulator value and the respective
next elements of the LISTs. This step is repeated until the end of the
list is reached; then the accumulator value is returned. COMBINE is
always called in the same dynamic environment as `fold-left' itself."
  (if (pair? lists)
      ;; N-ary case
      (let lp ((lists (cons list1 lists)) (ans nil))
        (receive (cars cdrs) (%cars+cdrs lists)
          (if (null? cars) ans ; Done.
              (lp cdrs (apply combine ans cars)))))
      ;; Fast path
      (let lp ((list list1) (ans nil))
        (if (null? list) ans
            (lp (cdr list) (combine ans (car list)))))))

;;; fold-right
(define (fold-right (combine ::procedure) nil (list1 ::list) . lists)
  "The LISTs should all have the same length. COMBINE must be a
procedure. It should accept one more argument than there are LISTs and
return a single value. COMBINE should not mutate the LIST
arguments. The `fold-right' procedure iterates the COMBINE procedure
over the elements of the LISTs from right to left and an accumulator
value, starting with an accumulator value of NIL. More specifically,
`fold-right' returns NIL if the LISTs are empty. If they are not
empty, COMBINE is first applied to the respective last elements of the
LISTs in order and NIL. The result becomes the new accumulator value,
and COMBINE is applied to the respective previous elements of the
LISTs and the new accumulator value. This step is repeated until the
beginning of the list is reached; then the accumulator value is
returned. PROC is always called in the same dynamic environment as
`fold-right' itself."
  (if (pair? lists)
      ;; N-ary case
      (let recur ((lists (cons list1 lists)))
        (let ((cdrs (%cdrs lists)))
          (if (null? cdrs) nil
              (apply combine (%cars+ lists (recur cdrs))))))
      ;; Fast path
      (let recur ((list list1))
        (if (null? list) nil
            (let ((head (car list)))
              (combine head (recur (cdr list))))))))

;;; remp, remove, remv, remq
(define (remp (proc ::procedure) (lst ::list)) ::list
  "PROC should accept one argument and return a single value. PROC
should not mutate LIST.

The `remp' procedure applies PROC to each element of LIST and returns
a list of the elements of LIST for which PROC returned #f. PROC is
always called in the same dynamic environment as `remp' itself. The
elements of the result list are in the same order as they appear in
the input list. If multiple returns occur from `remp', the return
values returned by earlier returns are not mutated."
  (filter (complement proc) lst))

(define (remove obj (lst ::list)) ::list
  "The `remove' procedure returns a list of the elements that are not
OBJ. `remove' uses `equal?' to compare OBJ with the elements of
LST. The elements of the result list are in the same order as they
appear in the input list."
  (filter (lambda (o) (not (equal? o obj))) lst))

(define (remv obj (lst ::list)) ::list
  "The `remv' procedure returns a list of the elements that are not
OBJ. `remv' uses `eqv?' to compare OBJ with the elements of LST. The
elements of the result list are in the same order as they appear in
the input list."
  (filter (lambda (o) (not (eqv? o obj))) lst))

(define (remq obj (lst ::list)) ::list
  "The `remq' procedure returns a list of the elements that are not
OBJ. `remq' uses `eq?' to compare OBJ with the elements of LIST. The
elements of the result list are in the same order as they appear in
the input list."
  (filter (lambda (o) (not (eq? o obj))) lst))

;;; memp, member, memv, memq
(define (memp (proc ::procedure) (lst ::list))
  "PROC should accept one argument and return a single value. PROC
should not mutate LST.

`memp' returns the first sublist of LST whose car satisfies a given
condition, where the sublists of LST are the lists returned by
(`list-tail' LST K) for K less than the length of LST. The `memp'
procedure applies PROC to the cars of the sublists of LST until it
finds one for which PROC returns a true value. PROC is always called
in the same dynamic environment as `memp' itself. If LST does not
contain an element satisfying the condition, then #f (not the empty
list) is returned."
  (let recur ((list lst))
    (cond ((null? list) #f)
          ((proc (car list)) list)
          (else (recur (cdr list))))))

;; member, memv, and memq are defined in kawa.lib.lists
;; (define (member obj (lst ::list)))
;; (define (memv obj (lst ::list)))
;; (define (memq obj (lst ::list)))

;;; assp, assoc, assv, assq
(define (assp (proc ::procedure) (alist ::list))
  "ALIST (for \"association list\") should be a list of pairs. PROC
should accept one argument and return a single value. PROC should not
mutate ALIST.

The `assp' procedure finds the first pair in ALIST whose car field
satisfies a given condition, and returns that pair without traversing
ALIST further. If no pair in ALIST satisfies the condition, then #f is
returned. The `assp' procedure successively applies PROC to the car
fields of ALIST and looks for a pair for which it returns a true
value. PROC is always called in the same dynamic environment as `assp'
itself."
  (let recur ((alist alist))
    (cond ((null? alist) #f)
          ((proc (caar alist)) (car alist))
          (else (recur (cdr alist))))))

;; assoc, assv, and assq are defined in kawa.lib.lists
;; (define (assoc obj (alist ::list)))
;; (define (assv obj (alist ::list)))
;; (define (assq obj (alist ::list)))

;;; cons*
(define (cons* #!rest (args :: object[]))
  "If called with at least two arguments, `cons*' returns a freshly
allocated chain of pairs whose cars are OBJ1,...,OBJN, and whose last
cdr is OBJ. If called with only one argument, `cons*' returns that
argument."
  (gnu.lists.LList:consX args))
