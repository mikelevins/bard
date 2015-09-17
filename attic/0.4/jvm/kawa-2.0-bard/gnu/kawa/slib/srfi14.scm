;; char-set implementation for Kawa, backed by Unicode inversion lists
;; Copyright (C) 2011 by Jamison Hope. The implementations of
;; char-set-unfold and char-set-unfold!, and all documentation strings
;; were taken from the text of the SRFI-14 document, which  is
;; copyright (C) Olin Shivers (1998, 1999, 2000). All Rights
;; Reserved.

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

;;; INVERSION LISTS
;;;
;;; char-sets are backed by a data structure called an inversion
;;; list. These inversion lists work in Unicode code points (which
;;; are, helpfully, the values returned by char->integer). An
;;; inversion list is a sort of run-length encoding of an ordered
;;; set. Here's a description of inversion lists from
;;; http://www.ibm.com/developerworks/linux/library/l-cpinv.html :
;;;
;;; \begin{quotation}
;;; So what are inversion lists? Inversion lists are best described as
;;; a condensed summary of a bit string. They are similar to a simple
;;; run-length encoding of data, though there are some differences.
;;;
;;; Let's look at an illustrative example. Suppose you want to encode
;;; the bit string "1110011." An inversion list would store a list of
;;; three numbers: 0, 3, 5. All we store is the start position of the
;;; 1s, then the start position of the 0s, then the position of 1s
;;; again, and so on until the bit string is over.
;;;
;;; If the run begins with a 1, we start the list with the number 0,
;;; meaning the 1s start the bit string. This rule can be inverted to
;;; store a 0 if the run begins with a 0 bit, but the effect is the
;;; same as long as both the encoder and the decoder of the inversion
;;; list agree on this detail. In fact, this rule is the only tricky
;;; thing about inversion lists!
;;;
;;; If we are building an inversion list for searching only, we do not
;;; need to store the position of the last bit. If, however, we want
;;; to construct the full original data, we need to know where to stop
;;; adding bits.
;;;
;;; Making an inversion list, then, is nothing more than counting
;;; bits. For specific applications such as Unicode character ranges,
;;; inversion lists can save you a lot of time and effort.
;;; \end{quotation}
;;;
;;; Here, we do not explicitly include the position of the last bit in
;;; each list; instead we just assume that the list ends with
;;; *highest-code-point* (#\u10FFFF).
;;;
;;; Moreover, the lists are stored in arrays of ints, and they are
;;; stored *backwards*. The list length is stored separately, so that
;;; reallocation isn't required on every update. In particular,
;;; because an inversion list can be complemented by adding or
;;; removing a leading 0, we can complement the array representation
;;; with {arr[length++] = 0;} or {length--;}.
;;;
;;; EXAMPLES
;;;
;;; {#\a} => [98 97 ...]
;;; {#\a #\b #\d} => [101 100 99 97 ...]
;;; {#\a #\b #\c ... #\z} => [123 97 ...]
;;; {} => [...]
;;; {#\u0000 #\u0001 #\u0002 ... #\u10FFFF} => [0 ...]
;;;
;;; Additionally, the built-in char-sets are marked immutable, so that
;;; any attempts to mutate them via a linear-update function will
;;; signal an error (and not instead silently corrupt later
;;; computations which expect, for example, that char-set:empty is
;;; truly empty).

(module-compile-options warn-undefined-variable: #t
                        warn-invoke-unknown-method: #t)
(module-export
 ;; Predicates & comparison
 ;; char-set?
 char-set= char-set<= char-set-hash
 ;; Iterating over character sets
 char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
 char-set-fold char-set-unfold char-set-unfold! char-set-for-each
 char-set-map
 ;; Creating character sets
 char-set-copy char-set list->char-set string->char-set
 list->char-set! string->char-set! char-set-filter ucs-range->char-set
 char-set-filter! ucs-range->char-set! ->char-set
 ;; Querying character sets
 char-set->list char-set->string char-set-size char-set-count
 char-set-contains? char-set-every char-set-any
 ;; Character-set algebra
 char-set-adjoin char-set-delete char-set-adjoin! char-set-delete!
 char-set-complement char-set-union char-set-intersection
 char-set-complement! char-set-union! char-set-intersection!
 char-set-difference char-set-xor char-set-diff+intersection
 char-set-difference! char-set-xor! char-set-diff+intersection!
 ;; Standard character sets
 ;; char-set:lower-case  char-set:upper-case  char-set:title-case
 ;; char-set:letter      char-set:digit       char-set:letter+digit
 ;; char-set:graphic     char-set:printing    char-set:whitespace
 ;; char-set:iso-control char-set:punctuation char-set:symbol
 ;; char-set:hex-digit   char-set:blank       char-set:ascii
 ;; char-set:empty       char-set:full
)

;;; char-set? and char-set:* are implicitly provided by the char-set
;;; class.

(define-alias reflectArray java.lang.reflect.Array)
(define-alias Arrays java.util.Arrays)
(define-alias Character java.lang.Character)

(include-relative "char-tables.iscm")

(define (%make-builtin nm arr::int[])
  (char-set inversion-list: arr
            inversion-list-size: arr:length name: nm
            immutable?: #t))

(define-syntax array-copy
  (syntax-rules ()
    ((_ orig len)
     (cond-expand
      (java-6 (Arrays:copy-of orig len))
      (else
       (let* ((cls ::java.lang.Class
                   (*:get-component-type (*:get-class orig)))
              (arr (reflectArray:new-instance cls len))
              (cp-len (min len (reflectArray:get-length orig))))
         (java.lang.System:arraycopy orig 0 arr 0 cp-len)
         arr))))))

;;; The largest valid Unicode code point.
(define *highest-code-point* ::int #x10FFFF)

(define (char-set= #!rest (csets ::char-set[])) ::boolean
  "Are the character sets equal?

Boundary cases:
  (char-set=) => true
  (char-set= cs) => true"
  (or (< csets:length 2)
      (let loop ((i ::int 1))
        (or (= i csets:length)
            (and (equal? (csets 0) (csets i))
                 (loop (+ 1 i)))))))

(define (char-set<= #!rest (csets ::char-set[])) ::boolean
  "Returns #t if every character set cs_i is a subset of character set
cs_{i+1}.

Boundary cases:
  (char-set<=) => true
  (char-set<= cs) => true"
  (or (< csets:length 2)
      (let loop ((i ::int 1))
        (or (= i csets:length)
            (and ((csets (- i 1)):subset-of? (csets i))
                 (loop (+ 1 i)))))))

(define (char-set-hash (cs ::char-set)
                       #!optional (bound ::int 0))
  ::int
  "Compute a hash value for the character set CS. BOUND is a
non-negative exact integer specifying the range of the hash
function. A positive value restricts the return value to the range
[0,BOUND-1].

If BOUND is either zero or not given, the default range will be the
size of an int, which will maintain compatibility with Java hash
codes."
  (let ((natural-hash ::int (*:hash-code cs)))
    (when (< natural-hash 0)
          (set! natural-hash
                (modulo natural-hash java.lang.Integer:MAX_VALUE)))
    (if (or (= 0 bound) (< natural-hash bound))
        natural-hash
        (remainder natural-hash bound))))

(define (char-set-cursor (cset ::char-set)) ::int
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. `char-set-cursor' produces a new cursor for a given set."
  (*:get-cursor cset))

(define (char-set-ref (cset ::char-set) (cursor ::int)) ::character
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. The set element indexed by the cursor is fetched with
`char-set-ref'.
"
  (integer->char cursor))

(define (char-set-cursor-next (cset ::char-set) (cursor ::int)) ::int
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. A cursor index is incremented with `char-set-cursor-next'; in
this way, code can step through every character in a char set."
  (*:cursor-next cset cursor))

(define (end-of-char-set? (cursor ::int)) ::boolean
  "Cursors are a low-level facility for iterating over the characters
in a set. A cursor is a value that indexes a character in a char
set. Stepping a cursor \"past the end\" of a char set produces a
cursor that answers true to `end-of-char-set?'. It is an error to pass
such a cursor to `char-set-ref' or to `char-set-cursor-next'."
  (> cursor *highest-code-point*))

(define (char-set-fold (kons ::procedure) knil (cs ::char-set))
  "This is the fundamental iterator for character sets. Applies the
function KONS across the character set CS using initial state value
KNIL. That is, if CS is the empty set, the procedure returns
KNIL. Otherwise, some element C of CS is chosen; let CS' be the
remaining, unchosen characters. The procedure returns
  (char-set-fold KONS (KONS C KNIL) CS')"
  (let loop ((cursor (char-set-cursor cs)) (answer knil))
    (if (end-of-char-set? cursor) answer
        (loop (char-set-cursor-next cs cursor)
              (kons (char-set-ref cs cursor) answer)))))

(define (char-set-unfold (p ::procedure) (f ::procedure)
                         (g ::procedure) seed
                         #!optional
                         (base-cs ::char-set char-set:empty))
  ::char-set
  "This is a fundamental constructor for char-sets. G is used to
generate a series of \"seed\" values from the initial seed: SEED,
(g SEED), (g (g SEED)), .... P tells us when to stop -- when it
returns true when applied to one of these seed values. F maps each
seed value to a character. These characters are added to the base
character set BASE-CS to form the result; BASE-CS defaults to the
empty set."
  ;; this implementation copied from SRFI text
  (char-set-unfold! p f g seed (char-set-copy base-cs)))

(define (char-set-unfold! (p ::procedure) (f ::procedure)
                          (g ::procedure) seed (base-cs ::char-set))
  ::char-set
  "`char-set-unfold!' adds the characters to BASE-CS in a
linear-update -- it is allowed, but not required, to side-effect and
use BASE-CS's storage to construct the result."
  ;; this implementation copied from SRFI text
  (let loop ((seed seed) (cs ::char-set base-cs))
    (if (p seed) cs
        (loop (g seed) (char-set-adjoin! cs (f seed))))))

(define (char-set-for-each (proc ::procedure) (cs ::char-set))
  "Apply procedure PROC to each character in the character set
CS. Note that the order in which PROC is applied to the characters in
the set is not specified, and may even change from one procedure
application to another."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor)))
      ((end-of-char-set? cursor) 'done) ; a single unspecified return
                                        ; value is required (due to
                                        ; R5RS language)
    (proc (char-set-ref cs cursor))))

(define (char-set-map (proc ::procedure) (cs ::char-set)) ::char-set
  "PROC is a char->char procedure. Apply it to all the characters in
the char-set CS, and collect the results into a new character set.

Essentially lifts PROC from a char->char procedure to a
char-set->char-set procedure."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
       (result-cs (char-set-copy char-set:empty)
                  (char-set-adjoin! result-cs
                                    (proc (char-set-ref cs cursor)))))
      ((end-of-char-set? cursor) result-cs)))

(define (char-set-copy (cs ::char-set)) ::char-set
  "Returns a copy of the character set CS. \"Copy\" means that if
either the input parameter or the result valoue of this procedure is
passed to one of the linear-update procedures described below, the
other character set is guaranteed not to be altered."
  (*:clone cs))

;;; char-set, char-set? and the built-in char-set:* sets are all
;;; provided by the char-set class. Several of the built-ins were
;;; originally generated dynamically, but have been inlined to speed
;;; up load time. Refer to the description of inversion lists below
;;; for the meanings of the lists of numbers.

(define-simple-class char-set (java.lang.Cloneable)
  class-name: ".CharSet"

  (empty                                ; contains no characters
   ::char-set allocation: 'static init:
   (char-set name: "char-set:empty" immutable?: #t))

  (full                                 ; contains all characters
   ::char-set allocation: 'static init:
   (char-set inversion-list: [0] inversion-list-size: 1
             name: "char-set:full" immutable?: #t))

  (ascii                                ; contains 0-127
   ::char-set allocation: 'static init:
   (char-set inversion-list: [128 0] inversion-list-size: 2
             name: "char-set:ascii" immutable?: #t))

  (title-case
   ;; The set of title-case characters was taken directly from the
   ;; SRFI document.
   ::char-set allocation: 'static
   init: (%make-builtin "char-set:title-case" %title-case))

  (whitespace
   ;; The set of whitespace characters is all the characters in
   ;; Unicode categories Zs, Zl or Zp, along with points 9-13.
   ::char-set allocation: 'static
   init: (%make-builtin "char-set:whitespace" %whitespace))

  (blank
   ;; The set of blank characters is defined to be the union of U+0009
   ;; and Unicode category Zs.
   ::char-set allocation: 'static
   init: (%make-builtin "char-set:blank" %blank))

  (lower-case
   ;; Characters for which Character:lower-case? returns #t
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:lower-case" %lower-case))
 
  (upper-case
   ;; Characters for which Character:upper-case? returns #t
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:upper-case" %upper-case))

  (letter
   ;; Characters for which Character:letter? returns #t
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:letter" %letter))

   (digit
   ;; Characters for which Character:digit? returns #t
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:digit" %digit))

  (punctuation
   ;; Characters whose Unicode type is one of:
   ;; Character:CONNECTOR_PUNCTUATION, Character:DASH_PUNCTUATION,
   ;; Character:START_PUNCTUATION, Character:END_PUNCTUATION,
   ;; Character:INITIAL_QUOTE_PUNCTUATION,
   ;; Character:FINAL_QUOTE_PUNCTUATION, or
   ;; Character:OTHER_PUNCTUATION
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:punctuation" %punctuation))
 
  (symbol
   ;; Characters whose Unicode type is one of:
   ;; Character:MATH_SYMBOL, Character:CURRENCY_SYMBOL,
   ;; Character:MODIFIER_SYMBOL, or Character:OTHER_SYMBOL
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:symbol" %symbol))
 
  (letter+digit
   ;; The union of char-set:letter and char-set:digit
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:letter+digit" %letter+digit))
 
  (graphic
   ;; The union of char-set:letter+digit, char-set:punctuation, and
   ;; char-set:symbol.
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:graphic" %graphic))

  (printing
   ;; The union of char-set:graphic and char-set:whitespace
   ::char-set allocation: 'static
    init: (%make-builtin "char-set:printing" %printing))

  (hex-digit                            ; [0-9A-Fa-f]
   ::char-set allocation: 'static init:
   (char-set inversion-list: [103 97 71 65 58 48 0] inversion-list-size: 6
             name: "char-set:hex-digit" immutable?: #t))

  (iso-control
   ;; Characters for which Character:isISOControl returns #t.
   ::char-set allocation: 'static init:
   (char-set inversion-list: [160 127 32 0] inversion-list-size: 4
             name: "char-set:iso-control" immutable?: #t))

  ;; instance fields
  (inversion-list ::int[] [0])          ; list stored as int array
  (inversion-list-size ::int 0)         ; list length <= array length
  (immutable? ::boolean #f)             ; locks a set (used for
                                        ; built-ins)
  (name ::String #!null)

  ;; constructor
  ((*init* #!rest (characters ::character[]))
   "Return a character set containing the given characters."
   (if (> characters:length 0)
       (let ((chars ::character[]
                    (as character[]
                        (array-copy (as int[] characters) characters:length))))
         (Arrays:sort (as int[] chars))
         (let ((first-pt ::int (char->integer (chars 0))))
           (let loop ((index ::int 1)
                      (pt ::int (+ 1 first-pt))
                      (inv-ls ::list `(,first-pt)))
             (cond ((= index chars:length)
                    (unless (= pt *highest-code-point*)
                            (set! inv-ls (cons pt inv-ls)))
                    (let ((len (length inv-ls)))
                      (set! inversion-list (int[] length: (+ 1 len)))
                      (set! inversion-list-size len)
                      (do ((i ::int 0 (+ i 1))
                           (inv-ls ::list inv-ls (cdr inv-ls)))
                          ((= i len))
                        (set! (inversion-list i) (car inv-ls)))))
                   (else
                    (let ((next-char-pt ::int (char->integer (chars index))))
                      (cond ((< pt next-char-pt)
                             (loop (+ 1 index) (+ 1 next-char-pt)
                                   (cons next-char-pt
                                         (cons pt inv-ls))))
                            ((= pt next-char-pt)
                             (loop (+ 1 index) (+ 1 pt) inv-ls))
                            ((> pt next-char-pt)
                             (loop (+ 1 index) pt inv-ls)))))))))))

  ;; clone
  ((clone) ::char-set
   (let ((copy ::char-set (invoke-special object (this) 'clone)))
     (set! copy:inversion-list
           (array-copy inversion-list inversion-list-size))
     (set! copy:immutable? #f)
     (set! copy:name #!null)
     copy))

  ;; hash code
  ((hash-code) (@java.lang.Override) ::int
   (let loop ((i ::int (- inversion-list-size 1)) (hash ::int 1))
     (if (= -1 i) hash
         (loop (- i 1) (+ (* 31 hash) (inversion-list i))))))

  ;; to string
  ((to-string) (@java.lang.Override) ::String
   (let ((s ::String (invoke-special object (this) 'toString)))
     (if (eq? #!null name)
         s
         (string-append s ": (" name ")"))))

  ;; equality, subset, and membership tests
  ((equals o) (@java.lang.Override) ::boolean
   (and (char-set? o)
        (let ((other ::char-set (as char-set o)))
          (and (= other:inversion-list-size inversion-list-size)
               (let loop ((i ::int 0))
                 (or (= i inversion-list-size)
                     (and (= (other:inversion-list i)
                             (inversion-list i))
                          (loop (+ i 1)))))))))

  ((subset-of? (cs ::char-set)) ::boolean
   (let loop ((ai ::int (- inversion-list-size 1))
              (bi ::int (- cs:inversion-list-size 1)))
     (cond ((= ai -1) #t)
           ((= bi -1) #f)
           ((< (inversion-list ai)
               (cs:inversion-list bi)) #f)
           ((= bi 0) #t)
           ((= ai 0) #f)
           ((< (inversion-list (- ai 1))
               (cs:inversion-list (- bi 1)))
            (loop (- ai 2) bi))
           ((= (inversion-list (- ai 1))
               (cs:inversion-list (- bi 1)))
            (loop (- ai 2) (- bi 2)))
           ((> (inversion-list ai)
               (cs:inversion-list (- bi 1)))
            (loop ai (- bi 2)))
           (else #f))))

  ((contains? (char ::character)) ::boolean
   (and (not (= 0 inversion-list-size))
        (begin
          (define charnum ::int (char->integer char))
          (let loop ((low ::int 0) (high ::int inversion-list-size))
            (let ((mid ::int (ash (+ low high) -1)))
              (cond ((= low high) #f)
                    ((and (< charnum (inversion-list mid))
                          (< mid (- inversion-list-size 1)))
                     (loop mid high))
                    ((and (> mid 0)
                          (>= charnum (inversion-list (- mid 1))))
                     (loop low mid))
                    ((and (= high inversion-list-size)
                          (= mid (- inversion-list-size 1)))
                     (>= charnum (inversion-list mid)))
                    (else (odd? (- inversion-list-size mid)))))))))

  ((size) ::int
   (let loop ((i ::int (- inversion-list-size 1))
              (num ::int 0))
     (cond ((= i -1) num)
           ((= 0 i)
            (+ num (- *highest-code-point* (inversion-list i)) 1))
           (else (loop (- i 2)
                       (+ num (- (inversion-list (- i 1))
                                 (inversion-list i))))))))

  ((to-list) ::list
   (char-set-fold cons '() (this)))

  ;; iteration
  ((get-cursor) ::int
   (if (= 0 inversion-list-size)
       (+ *highest-code-point* 1)
       (inversion-list (- inversion-list-size 1))))

  ((cursor-next (cursor ::int)) ::int
   (if (or (= 0 inversion-list-size)
           (and (even? inversion-list-size)
                (>= (+ cursor 1) (inversion-list 0))))
       (+ *highest-code-point* 1)
       (let ((cursor ::int (+ cursor 1)))
         (let loop ((low ::int 0) (high ::int inversion-list-size))
           (let ((mid ::int (ash (+ low high) -1)))
             (cond ((= low high) (inversion-list low))
                   ((< cursor (inversion-list mid))
                    (loop mid high))
                   ((and (> mid 0)
                         (>= cursor (inversion-list (- mid 1))))
                    (loop low mid))
                   ((odd? (- inversion-list-size mid)) cursor)
                   (else (inversion-list (- mid 1)))))))))

  ;; set logic mutators (complement!, intersection!, union!, xor!)
  ((complement!) ::char-set
   (when immutable?
         (error "attempted to modify an immutable char-set" (this)))
   (cond ((and (> inversion-list-size 0)
               (= 0 (inversion-list (- inversion-list-size 1))))
          (set! inversion-list-size (- inversion-list-size 1)))
         ((< inversion-list-size inversion-list:length)
          (set! (inversion-list inversion-list-size) 0)
          (set! inversion-list-size (+ 1 inversion-list-size)))
         (else                          ; must realloc
          (set! inversion-list
                (array-copy inversion-list
                            (+ 1  (* inversion-list-size 2))))
          (set! inversion-list-size (+ 1 inversion-list-size))))
   (this))

  ((adjoin! (c ::character))
   ::char-set
   (let ((i ::int (char->integer c)))
     (*:union! (this) (int[] (+ i 1) i) 2)))

  ((delete! (c ::character))
   ::char-set
   (let ((i ::int (char->integer c)))
     (*:intersection! (this) (int[] (+ i 1) i 0) 3)))

  ((combine! (arr ::int[]) (arr-size ::int) (proc ::procedure))
   ::char-set
   access: 'private
   (when immutable?
         (error "attempted to modify an immutable char-set" (this)))
   (let* ((l1 ::list (%make-boundary-pairs
                      inversion-list inversion-list-size))
          (l2 ::list (%make-boundary-pairs arr arr-size))
          (combo-pairs ::list (proc l1 l2))
          (new-length ::int (%boundary-pairs-length combo-pairs)))
     (when (or (> new-length inversion-list:length)
               (< new-length (/ inversion-list:length 2)))
           (set! inversion-list (int[] length: (* new-length 2))))
     (%write-inversion-list inversion-list combo-pairs new-length)
     (set! inversion-list-size new-length))
   (this))

  ((intersection! (cs ::char-set)) ::char-set
   (*:intersection! (this) cs:inversion-list cs:inversion-list-size))

  ((intersection! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-intersection))

  ((union! (cs ::char-set)) ::char-set
   (*:union! (this) cs:inversion-list cs:inversion-list-size))

  ((union! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-union))

  ((xor! (cs ::char-set)) ::char-set
   (*:xor! (this) cs:inversion-list cs:inversion-list-size))

  ((xor! (arr ::int[]) (arr-size ::int)) ::char-set
   (*:combine! (this) arr arr-size %boundary-pairs-xor)))

(define (list->char-set (char-list ::list)
                        #!optional
                        (base-cs ::char-set char-set:empty))
  ::char-set
  "Return a character set containing the characters in the list of
characters CHAR-LIST. If character set BASE-CS is provided, the
characters from CHAR-LIST are added to it. `list->char-set!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `list->char-set' produces a fresh character set."
  (let ((res-cs ::char-set (char-set @char-list)))
    (char-set-union! res-cs base-cs)))

(define (list->char-set! (char-list ::list) (base-cs ::char-set))
  ::char-set
  "Return a character set containing the characters in the list of
characters CHAR-LIST. If character set BASE-CS is provided, the
characters from CHAR-LIST are added to it. `list->char-set!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `list->char-set' produces a fresh character set."
  (char-set-adjoin! base-cs @char-list))

(define (string->char-set (s ::String)
                          #!optional
                          (base-cs ::char-set char-set:empty))
  ::char-set
  "Return a character set containing the characters in the string
S. If character set BASE-CS is provided, the characters from S are
added to it. `string->char-set!' is allowed, but not required, to
side-effect and reuse the storage in BASE-CS; `string->char-set'
produces a fresh character set."
  (list->char-set (string->list s) base-cs))

(define (string->char-set! (s ::String) (base-cs ::char-set))
  ::char-set
  "Return a character set containing the characters in the string
S. If character set BASE-CS is provided, the characters from S are
added to it. `string->char-set!' is allowed, but not required, to
side-effect and reuse the storage in BASE-CS; `string->char-set'
produces a fresh character set."
  (list->char-set! (string->list s) base-cs))

(define (char-set-filter (pred ::procedure) (cs ::char-set)
                         #!optional
                         (base-cs ::char-set char-set:empty))
  ::char-set
  "Returns a character set containing every character C in CS such
that (PRED C) returns true. If character set BASE-CS is provided, the
characters specified by PRED are added to it. `char-set-filter!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `char-set-filter' produces a fresh character set."
  (do ((cursor (char-set-cursor cs) (char-set-cursor-next cs cursor))
       (result-cs (char-set-copy base-cs)
                  (let ((c ::character (char-set-ref cs cursor)))
                    (if (pred c) (char-set-adjoin! result-cs c)
                        result-cs))))
      ((end-of-char-set? cursor) result-cs)))

(define (char-set-filter! (pred ::procedure) (cs ::char-set)
                          (base-cs ::char-set))
  ::char-set
  "Returns a character set containing every character C in CS such
that (PRED C) returns true. If character set BASE-CS is provided, the
characters specified by PRED are added to it. `char-set-filter!' is
allowed, but not required, to side-effect and reuse the storage in
BASE-CS; `char-set-filter' produces a fresh character set."
  (let loop ((cursor (char-set-cursor cs)) (base-cs base-cs))
    (if (end-of-char-set? cursor) base-cs
        (let ((c ::character (char-set-ref cs cursor)))
          (if (pred c)
              (loop (char-set-cursor-next cs cursor)
                    (char-set-adjoin! base-cs c))
              (loop (char-set-cursor-next cs cursor) base-cs))))))

(define (ucs-range->char-set (lower ::int) (upper ::int)
                             #!optional
                             (error? ::boolean #f)
                             (base-cs ::char-set char-set:empty))
  ::char-set
  "LOWER and UPPER are exact non-negative integers; LOWER < UPPER.

Returns a character set containing every character whose UCS-4 code
lies in the half-open range [lower,upper).
If character set BASE-CS is provided, the characters specified by the
range are added to it. `ucs-range->char-set!' is allowed, but not
required, to side-effect and reuse the storage in BASE-CS;
`ucs-range->char-set' produces a fresh character set.

ERROR?, which is only meaningful for Scheme implementations which do
not natively use Unicode characters, is ignored."
  (let ((res-cs ::char-set (char-set inversion-list: [upper lower]
                                     inversion-list-size: 2)))
    (char-set-union! res-cs base-cs)))

(define (ucs-range->char-set! (lower ::int) (upper ::int)
                              (error? ::boolean)
                              (base-cs ::char-set))
  ::char-set
  "LOWER and UPPER are exact non-negative integers; LOWER < UPPER.

Returns a character set containing every character whose UCS-4 code
lies in the half-open range [lower,upper).
If character set BASE-CS is provided, the characters specified by the
range are added to it. `ucs-range->char-set!' is allowed, but not
required, to side-effect and reuse the storage in BASE-CS;
`ucs-range->char-set' produces a fresh character set.

ERROR?, which is only meaningful for Scheme implementations which do
not natively use Unicode characters, is ignored."
  (*:union! base-cs (int[] upper lower) 2))

(define (->char-set x) ::char-set
  "Coerces X into a char-set. X may be a string, character, or
char-set. A string is converted to the set of its constituent
characters; a character is converted to a singleton set; a char-set is
returned as-is. This procedure is intended for use by other procedures
that want to provide \"user-friendly,\" wide-spectrum interfaces to
their clients."
  (cond ((string? x) (string->char-set x))
        ((character? x) (char-set x))
        ((char-set? x) x)
        (else (primitive-throw (java.lang.ClassCastException "not converible to char-set")))))

(define (char-set-size (cs ::char-set)) ::int
  "Returns the number of elements in character set CS."
  (*:size cs))

(define (char-set-count (pred ::procedure) (cs ::char-set)) ::int
  "Apply PRED to the chars of character set CS, and return the number
of chars that caused the predicate to return true."
  (char-set-fold
   (lambda (x sum)
     (if (pred x) (+ sum 1) sum)) 0 cs))

(define (char-set->list (cs ::char-set)) ::list
  "This procedure returns a list of the members of character set
CS. The order in which CS's characters appear in the list is not
defined, and may be different from one call to another."
  (*:to-list cs))

(define (char-set->string (cs ::char-set)) ::String
  "This procedure returns a string containing the members of character
set CS. The order in which CS's characters appear in the string is not
defined, and may be different from one call to another."
  (list->string (char-set->list cs)))

(define (char-set-contains? (cs ::char-set) (char ::character))
  ::boolean
  "This procedure tests CHAR for membership in character set CS."
  (*:contains? cs char))

(define (char-set-every (pred ::procedure) (cs ::char-set)) ::boolean
  "The `char-set-every' procedure returns true if predicate PRED
returns true of every character in the character set CS. The order in
which this procedure sequences through the elements of CS is not
specified."
  (let loop ((cursor (char-set-cursor cs)))
    (or (end-of-char-set? cursor)
        (and (pred (char-set-ref cs cursor))
             (loop (char-set-cursor-next cs cursor))))))

(define (char-set-any (pred ::procedure) (cs ::char-set))
  "`char-set-any' applies PRED to every character in character set CS,
and returns the first true value it finds. If no character produces a
true value, it returns false. The order in which this procedure
sequences through the elements of CS is not specified.

Note that if you need to determine the actual character on which a
predicate returns true, arrange for the predicate to return the
character parameter as its true value, e.g.
  (char-set-any (lambda (c) (and (char-upper-case? c) c)) cs)"
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor) #f)
          ((pred (char-set-ref cs cursor)) => values)
          (else (loop (char-set-cursor-next cs cursor))))))

(define (char-set-adjoin (cs ::char-set) #!rest (chars ::character[]))
  ::char-set
  "Add the CHAR_i characters to character set CS."
  (char-set-adjoin! (char-set-copy cs) chars))

(define (char-set-delete (cs ::char-set) #!rest (chars ::character[]))
  ::char-set
  "Delete the CHAR_i characters from character set CS."
  (char-set-delete! (char-set-copy cs) chars))

(define (char-set-adjoin! (cs ::char-set)
                          #!rest (chars ::character[]))
  ::char-set
  "Add the CHAR_i characters to character set CS. This is the
linear-update variant of `char-set-adjoin', which is allowed, but not
required, to side-effect its first parameter."
  (case chars:length
    ((0) cs)
    ((1) (*:adjoin! cs (chars 0)))
    (else (*:union! cs (char-set @chars)))))

(define (char-set-delete! (cs ::char-set)
                          #!rest (chars ::character[]))
  ::char-set
  "Delete the CHAR_i characters from character set CS. This is the
linear-update variant of `char-set-delete', which is allowed, but not
required, to side-effect its first parameter."
  (case chars:length
    ((0) cs)
    ((1) (*:delete! cs (chars 0)))
    (else (let ((to-remove ::char-set (char-set @chars)))
            (*:intersection! cs (*:complement! to-remove))))))


(define (char-set-complement (cs ::char-set)) ::char-set
  "Set complement for character sets."
  (char-set-complement! (char-set-copy cs)))

(define (char-set-union #!rest (csets ::char-set[])) ::char-set
  "Set union for character sets. `char-set-union' is n-ary; its
boundary case (when n=0) is:
  (char-set-union) => char-set:empty"
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:union! cs (csets i))))))

(define (char-set-intersection #!rest (csets ::char-set[])) ::char-set
  "Set intersection for character sets. `char-set-intersection' is
n-ary; its boundary case (when n=0) is:
  (char-set-intersection) => char-set:full"
  (case csets:length
    ((0) char-set:full)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:intersection! cs (csets i))))))

(define (char-set-difference (cs1 ::char-set)
                             #!rest (csets ::char-set[]))
  ::char-set
  "Set difference for character sets. `char-set-difference' is n-ary,
associates to the left (that is, it computes the difference between
its first argument and the union of all the other arguments), and
requires at least one argument. Its boundary case is:
  (char-set-difference cs) => cs"
  (if (= 0 csets:length) cs1
      (let ((rest (char-set-union csets)))
        (char-set-intersection cs1 (char-set-complement rest)))))

(define (char-set-xor #!rest (csets ::char-set[])) ::char-set
  "Set exclusive-or for character sets. `char-set-xor' is n-ary; its
boundary case (when n=0) is:
  (char-set-xor) => char-set:empty"
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((cs ::char-set (char-set-copy (csets 0)))
          (i ::int 1 (+ i 1)))
         ((= i csets:length) cs)
       (*:xor! cs (csets i))))))

(define (char-set-diff+intersection (cs1 ::char-set) (cs2 ::char-set)
                                    #!rest (csets ::char-set[]))
  "`char-set-diff+intersection' returns both the difference and the
intersection of the arguments -- it partitions its first parameter."
  (let ((union ::char-set (char-set-union cs2 @csets)))
    (values (char-set-intersection cs1 (char-set-complement union))
            (char-set-intersection cs1 union))))

(define (char-set-complement! (cs ::char-set)) ::char-set
  "Set complement for character sets, linear-update variant. It is
allowed, but not required, to side-effect its argument."
  (*:complement! cs))

(define (char-set-union! #!rest (csets ::char-set[])) ::char-set
  "Set union for character sets, linear-update variant. It is allowed,
but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:union! (csets 0) (csets i))))))

(define (char-set-intersection! #!rest (csets ::char-set[]))
  ::char-set
  "Set intersection for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:full)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:intersection! (csets 0) (csets i))))))

(define (char-set-difference! (cs1 ::char-set)
                              #!rest (csets ::char-set[]))
  ::char-set
  "Set difference for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (if (= 0 csets:length) cs1
      (let ((rest (char-set-union csets)))
        (char-set-intersection! cs1 (char-set-complement rest)))))

(define (char-set-xor! #!rest (csets ::char-set[])) ::char-set
  "Set exclusive-or for character sets, linear-update variant. It is
allowed, but not required, to side-effect its first argument."
  (case csets:length
    ((0) char-set:empty)
    ((1) (csets 0))
    (else
     (do ((i ::int 1 (+ i 1)))
         ((= i csets:length) (csets 0))
       (*:xor! (csets 0) (csets i))))))

;;; char-set-diff+intersection! is allowed to side-effect both of its
;;; first two arguments, but it doesn't.
(define char-set-diff+intersection! char-set-diff+intersection)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; HELPER FUNCTIONS ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A helper function which converts an inversion list into a list of
;;; boundary pairs -- cons cells representing groups of contiguous
;;; *included* characters, in which the car is the lower code point
;;; and the cdr is the higher code point.
(define (%make-boundary-pairs (arr ::int[]) (len ::int))
  (if (= 0 len) '()
      (begin
        (define (make-pairs (i ::int)) ::list
          (if (= i len) '()
              (cons (cons (arr (+ 1 i)) (- (arr i) 1))
                    (make-pairs (+ i 2)))))
        (if (even? len)
            (make-pairs 0)
            (cons (cons (arr 0) *highest-code-point*)
                  (make-pairs 1))))))

;;; A helper function which computes the required array size for an
;;; inversion list representing the given list of boundary pairs.
(define (%boundary-pairs-length (l ::list)) ::int
  (let loop ((l ::list l) (size ::int 0))
    (cond ((null? l) size)
          ((= *highest-code-point* (cdar l))
           (loop (cdr l) (+ size 1)))
          (else (loop (cdr l) (+ size 2))))))

;;; A helper function which writes the inversion list representation
;;; of the given boundary pairs list to the given array.
(define (%write-inversion-list (arr ::int[]) (l ::list) (len ::int))
  (when (> len 0)
        (define (write-pairs (i ::int) (l ::list))
          (when (< i len)
                (set! (arr i) (+ 1 (cdar l)))
                (set! (arr (+ i 1)) (caar l))
                (write-pairs (+ i 2) (cdr l))))
        (cond ((= *highest-code-point* (cdar l))
               (set! (arr 0) (caar l))
               (write-pairs 1 (cdr l)))
              (else (write-pairs 0 l)))))

;;; A helper function which computes the intersection of two boundary
;;; pairs lists.
(define (%boundary-pairs-intersection (l1 ::list) (l2 ::list)) ::list
  (cond ((or (null? l1) (null? l2)) '()) ; no further overlaps
        ((> (caar l1) (cdar l2)) ; (car l1) does not overlap with l2
         (%boundary-pairs-intersection (cdr l1) l2))
        ((> (caar l2) (cdar l1)) ; (car l2) does not overlap with l1
         (%boundary-pairs-intersection l1 (cdr l2)))
        (else
         (let ((l1a ::int (caar l1))
               (l1b ::int (cdar l1))
               (l2a ::int (caar l2))
               (l2b ::int (cdar l2)))
           (cond ((<= l1a l2a l1b l2b)  ; (l2a . l1b) is the overlap
                  `((,l2a . ,l1b)       ; with (car l1)
                    ,@(%boundary-pairs-intersection
                       `((,l1a . ,(- l2a 1)) ,@(cdr l1)) (cdr l2))))
                 ((<= l1a l2a l2b l1b)  ; (car l2) is entirely
                  `(,(car l2)           ; contained in (car l1)
                    ,@(%boundary-pairs-intersection
                       `((,l1a . ,(- l2a 1)) ,@(cdr l1)) (cdr l2))))
                 (else (%boundary-pairs-intersection l2 l1)))))))

;;; A helper function which computes the union of two boundary pairs
;;; lists.
(define (%boundary-pairs-union (l1 ::list) (l2 ::list)) ::list
  (cond ((null? l1) l2)
        ((null? l2) l1)
        ((or (> (cdar l2) (cdar l1))        ; swap arguments if
             (and (= (cdar l2) (cdar l1))   ; necessary to ensure
                  (< (caar l2) (cdar l1))   ; (cdar l1) is highest
                  (> (caar l2) (caar l1))))
         (%boundary-pairs-union l2 l1))
        (else
         (let ((ending ::int (cdar l1)))
           (let find-start ((l1 ::list l1) (l2 ::list l2))
             ;; Walk both lists as long as there is overlap, to find
             ;; the start of the contiguous set. Then recurse.
             (cond ((null? l2)
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) l2)))
                   ((>= (cdar l2) (- (caar l1) 1) (- (caar l2) 1))
                    (find-start l2 (cdr l1)))
                   ((>= (cdar l2) (- (caar l1) 1))
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) (cdr l2))))
                   (else
                    `((,(caar l1) . ,ending)
                      ,@(%boundary-pairs-union (cdr l1) l2)))))))))

;;; A helper function which computes the xor of two boundary pairs
;;; lists.
(define (%boundary-pairs-xor (l1 ::list) (l2 ::list)) ::list
  (cond ((null? l1) l2)                 ; (xor () x) => x
        ((null? l2) l1)                 ; (xor x ()) => x
        ((> (caar l1) (+ 1 (cdar l2)))
         ;; (car l1) is beyond (car l2)
         `(,(car l1) ,@(%boundary-pairs-xor (cdr l1) l2)))
        ((> (caar l2) (+ 1 (cdar l1)))
         ;; (car l2) is beyond (car l1)
         `(,(car l2) ,@(%boundary-pairs-xor l1 (cdr l2))))
        ((= (caar l1) (+ 1 (cdar l2)))
         ;; (car l1) is adjacent to (car l2) -- merge them
         (%boundary-pairs-xor
          (cons (cons (caar l2) (cdar l1)) (cdr l1)) (cdr l2)))
        ((= (caar l2) (+ 1 (cdar l1)))
         ;; (car l2) is adjacent to (car l1) -- merge them
         (%boundary-pairs-xor
          (cdr l1) (cons (cons (caar l1) (cdar l2)) (cdr l2))))
        ;; the rest of these handle overlapping regions...
        ((> (cdar l1) (cdar l2)) ; (car l1) extends beyond l2
         `((,(+ 1 (cdar l2)) . ,(cdar l1))
           ,@(%boundary-pairs-xor
              `((,(caar l1) . ,(cdar l2)) ,@(cdr l1)) l2)))
        ((> (cdar l2) (cdar l1)) ; (car l2) extends beyond l1
         `((,(+ 1 (cdar l1)) . ,(cdar l2))
           ,@(%boundary-pairs-xor
              `((,(caar l2) . ,(cdar l1)) ,@(cdr l2)) l1)))
        ;; otherwise, they end at the same point
        ((> (caar l1) (caar l2)) ; (car l1) is a subset of (car l2)
         (%boundary-pairs-xor
          (cdr l1) `((,(caar l2) . ,(- (caar l1) 1)) ,@(cdr l2))))
        ((> (caar l2) (caar l1)) ; (car l2) is a subset of (car l1)
         (%boundary-pairs-xor
          (cdr l2) `((,(caar l1) . ,(- (caar l2) 1)) ,@(cdr l1))))
        ;; otherwise (car l1) == (car l2). (xor x x) => ()
        (else (%boundary-pairs-xor (cdr l1) (cdr l2)))))
