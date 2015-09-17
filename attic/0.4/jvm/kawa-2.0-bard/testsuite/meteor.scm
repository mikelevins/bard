#| The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   Contributed by Jamison Hope. Based on the "Java 6 steady state #2"
   version by Amir K and Isaac Gouy.
|#

(module-static #t)
(define-alias SB java.lang.StringBuilder)

;; Some helpful macros

(define-syntax while
  (syntax-rules ()
    ((_ pred e ...)
     (let loop ()
       (when pred
             e ...
             (loop))))))

(define-syntax ++!
  (syntax-rules ()
    ((_ var) (++! var 1))
    ((_ var amt)
     (begin
       (set! var (+ var amt)) var))))

(define-syntax !++
  (syntax-rules ()
    ((_ var) (!++ var 1))
    ((_ var amt)
     (let ((ret var)) (set! var (+ var amt)) ret))))

(define-syntax --!
  (syntax-rules ()
    ((_ var) (--! var 1))
    ((_ var amt)
     (begin
       (set! var (- var amt)) var))))

(define-syntax !--
  (syntax-rules ()
    ((_ var) (!-- var 1))
    ((_ var amt)
     (let ((ret var)) (set! var (- var amt)) ret))))

(define-syntax set-<<!
  (syntax-rules ()
    ((_ var amt)
     (set! var (bitwise-arithmetic-shift-left var amt)))))

(define-syntax set->>!
  (syntax-rules ()
    ((_ var amt)
     (set! var (bitwise-arithmetic-shift-right var amt)))))

(define-syntax set-ior!
  (syntax-rules ()
    ((_ var e ...)
     (set! var (bitwise-ior var e ...)))))

(define-syntax set-xor!
  (syntax-rules ()
    ((_ var e ...)
     (set! var (bitwise-xor var e ...)))))

(define-syntax set-and!
  (syntax-rules ()
    ((_ var e ...)
     (set! var (bitwise-and var e ...)))))


;; Constants
(define-constant X :: int 0)
(define-constant Y :: int 1)
(define-constant N-DIM :: int 2)

(define-constant EVEN :: int 0)
(define-constant ODD :: int 1)
(define-constant N-PARITY :: int 2)

(define-constant GOOD :: int 0)
(define-constant BAD :: int 1)
(define-constant ALWAYS-BAD :: int 2)

(define-constant OPEN :: int 0)
(define-constant CLOSED :: int 1)
(define-constant N-FIXED :: int 2)

(define-constant MAX-ISLAND-OFFSET :: int 1024)
(define-constant N-COL :: int 5)
(define-constant N-ROW :: int 10)
(define-constant N-CELL :: int (* N-COL N-ROW))
(define-constant N-PIECE-TYPE :: int 10)
(define-constant N-ORIENT :: int 12)

;; Board constants
(define-constant TOP-ROW :: int (- (bitwise-arithmetic-shift-left 1 N-COL) 1))
(define-constant L-EDGE-MASK :: int
  (bitwise-ior (bitwise-arithmetic-shift-left 1 0)
               (bitwise-arithmetic-shift-left 1 5)
               (bitwise-arithmetic-shift-left 1 10)
               (bitwise-arithmetic-shift-left 1 15)
               (bitwise-arithmetic-shift-left 1 20)
               (bitwise-arithmetic-shift-left 1 25)
               (bitwise-arithmetic-shift-left 1 30)))
(define-constant R-EDGE-MASK :: int
  (bitwise-arithmetic-shift-left L-EDGE-MASK 4))
(define-constant ROW-0-MASK :: int
  (bitwise-ior TOP-ROW
               (bitwise-arithmetic-shift-left TOP-ROW 10)
               (bitwise-arithmetic-shift-left TOP-ROW 20)
               (bitwise-arithmetic-shift-left TOP-ROW 30)))
(define-constant ROW-1-MASK :: int
  (bitwise-arithmetic-shift-left ROW-0-MASK 5))
(define-constant BOARD-MASK :: int
  (- (bitwise-arithmetic-shift-left 1 30) 1))

;; Piece constants
(define-constant N-ELEM :: int 5)
(define-constant ALL-PIECE-MASK :: int
  (- (bitwise-arithmetic-shift-left 1 N-PIECE-TYPE) 1))
(define-constant SKIP-PIECE :: int 5)
(define-constant BaseVecs :: int[]
  (int[] #x10f #x0cb #x1087 #x427 #x465 #x0c7 #x8423 #x0a7 #x187 #x08f))

;; (do-decrementing COUNTER START . BODY)
;; Do BODY with COUNTER set top START-1 down to 0.
;; Equivalent to Java's: for (int COUNTER=START; --COUNTER>=0; ) { BODY }
(define-syntax do-decrementing
  (syntax-rules ()
    ((_ counter start . body)
     (let ((counter ::int start))
       (let loop ()
	 (set! counter (- counter 1))
	 (if (>= counter 0)
	     (begin
	       (begin . body)
	       (loop))))))))

(define s-base-pieces ::Piece[] (Piece[] length: (* N-PIECE-TYPE N-ORIENT)))
(do-decrementing i (* N-PIECE-TYPE N-ORIENT)
		 (set! (s-base-pieces i) (Piece)))
(define-syntax s-base-piece
  (syntax-rules ()
    ((_ ipiece iorient) (s-base-pieces (+ (* N-ORIENT ipiece) iorient)))))

;; Global variables
(define g-island-info :: IslandInfo[]
  (IslandInfo[] length: MAX-ISLAND-OFFSET))

(define g-n-island-info :: int 0)

(define g-ok-pieces :: OkPieces[] (OkPieces[] length: N-CELL))

(define-constant g-first-region :: int[]
  (int[]
      #x00 #x01 #x02 #x03 #x04 #x01 #x06 #x07
      #x08 #x01 #x02 #x03 #x0c #x01 #x0e #x0f
      #x10 #x01 #x02 #x03 #x04 #x01 #x06 #x07
      #x18 #x01 #x02 #x03 #x1c #x01 #x1e #x1f))

(define-constant g-flip :: int[]
  (int[]
      #x00 #x10 #x08 #x18 #x04 #x14 #x0c #x1c
      #x02 #x12 #x0a #x1a #x06 #x16 #x0e #x1e
      #x01 #x11 #x09 #x19 #x05 #x15 #x0d #x1d
      #x03 #x13 #x0b #x1b #x07 #x17 #x0f #x1f))

(define-constant s-first-one :: int[]
  (int[]
      0 0 1 0 2 0 1 0
      3 0 1 0 2 0 1 0
      4 0 1 0 2 0 1 0
      3 0 1 0 2 0 1 0))

(define-syntax get-mask
  (syntax-rules ()
    ((_ i-pos)
     (bitwise-arithmetic-shift-left 1 i-pos))))

(define-syntax my-floor
  (syntax-rules ()
    ((_ top bot)
     (let ((to-zero :: int (quotient top bot)))
       (if (and (not (= top (* to-zero bot)))
                (not (eq? (< top 0) (<= bot 0))))
           (- to-zero 1)
           to-zero)))))

(define (get-first-one (v :: int)) :: int
  (if (= 0 v) 0
      (let ((start-pos :: int 0)
            (i-pos :: int 0)
            (mask :: int #xff))
        (while (= 0 (bitwise-and mask v))
               (set-<<! mask 8)
               (++! i-pos 8))
        (let* ((result :: int
                       (bitwise-arithmetic-shift-right
                        (bitwise-and mask v) i-pos))
               (result-low :: int (bitwise-and result #x0f)))
          (if (= 0 result-low)
              (+ i-pos 4 (s-first-one (bitwise-arithmetic-shift-right
                                       result 4)))
              (+ i-pos (s-first-one result-low)))))))

;; (define count-ones bitwise-bit-count)
(define (count-ones (v :: int)) :: int
  (let ((n :: int 0))
    (while (not (= 0 v))
           (++! n)
           (set-and! v (- v 1)))
    n))

(define (flip-two-rows (bits :: int)) :: int
  (let ((flipped :: int
         (bitwise-arithmetic-shift-left
          (g-flip (bitwise-arithmetic-shift-right bits N-COL))
          N-COL)))
    (bitwise-ior flipped (g-flip (bitwise-and bits TOP-ROW)))))

(define (mark-bad (info :: IslandInfo)
                  (mask :: int)
                  (eo :: int)
                  (always :: boolean))
  :: void
  (set-ior! (info:has-bad (+ (* eo N-PARITY) OPEN)) mask)
  (set-ior! (info:has-bad (+ (* eo N-PARITY) CLOSED)) mask)
  (when always
        (set-ior! (info:always-bad eo) mask)))

(define (init-globals) :: void
  (do ((i :: int 0 (+ i 1)))
      ((= i MAX-ISLAND-OFFSET))
    (set! (g-island-info i) (IslandInfo)))

  (do-decrementing yx N-CELL
		   (set! (g-ok-pieces yx) (OkPieces))))

;; OkPieces
(define-simple-class OkPieces ()
  (n-pieces :: byte[] init: (byte[] length: N-PIECE-TYPE))
  (piece-vec :: int[] init: (int[] length: (* N-PIECE-TYPE N-ORIENT))))

;; IslandInfo
(define-simple-class IslandInfo ()
  (has-bad :: int[] init: (int[] length: (* N-FIXED N-PARITY)))
  (is-known :: int[] init: (int[] length: (* N-FIXED N-PARITY)))
  (always-bad :: int[] init: (int[] length: N-PARITY)))

;; SPiece
(define-simple-class SPiece ()
  (vec :: int)
  (i-piece :: short)
  (row :: short)
  ((*init*)
   (set! vec 0)
   (set! i-piece 0)
   (set! row 0))
  ((*init* (other :: SPiece))
   (set! vec other:vec)
   (set! i-piece other:i-piece)
   (set! row other:row)))

(define-syntax m-cell
  (syntax-rules ()
    ((_ obj r c) (obj:m-cells (get-index c r)))))
(define-syntax p-cell
  (syntax-rules ()
    ((_ pts x y) (pts (+ (* N-ELEM y) x)))))

;; Soln
(define-simple-class Soln ()
  (NO-PIECE :: int allocation: 'static init: -1)
  (m-pieces :: SPiece[] (SPiece[] length: N-PIECE-TYPE))
  (m-n-piece :: int)
  (m-cells :: byte[] init: (byte[] length: N-CELL))
  (m-synched :: boolean)

  ((is-empty) :: boolean (= 0 m-n-piece))

  ((pop-piece) :: void
   (--! m-n-piece)
   (set! m-synched #f))

  ((push-piece (vec :: int) (i-piece :: int) (row :: int)) :: void
   (let ((p :: SPiece (m-pieces (!++ m-n-piece))))
     (set! p:vec vec)
     (set! p:i-piece i-piece)
     (set! p:row row)))

  ((*init*)
   (set! m-synched #f)
   (set! m-n-piece 0)
   (*:init (this)))

  ((init)
   (do ((i :: int 0 (+ i 1)))
       ((= i N-PIECE-TYPE))
     (set! (m-pieces i) (SPiece))))

  ((*init* (fill-val :: int))
   (*:init (this))
   (set! m-n-piece 0)
   (*:fill (this) fill-val))

  ((clone2) :: Soln
   (let ((s :: Soln (Soln)))
     (do ((i :: int 0 (+ i 1)))
         ((= i m-pieces:length))
       (set! (s:m-pieces i) (SPiece (m-pieces i))))
     (set! s:m-n-piece m-n-piece)
     (do-decrementing ij N-CELL
		      (set! (s:m-cells ij) (m-cells ij)))
     (set! s:m-synched m-synched)
     s))

  ((fill (val :: int)) :: void
   (set! m-synched #f)
   (do-decrementing ij N-CELL
		    (set! (m-cells ij) val)))

  ((to-string) :: String
   (let ((result :: SB (SB)))
     (do ((y :: int 0 (+ y 1)))
         ((= y N-ROW) (result:to-string))
       (do ((x :: int 0 (+ x 1)))
           ((= x N-COL) (result:append
                         (constant-fold
                          list->string '(#\newline))))
         (let ((val :: int (m-cell (this) y x)))
           (result:append val))
         (result:append " "))
       (when (even? y) (result:append " ")))))

  ((set-cells) :: void
   (unless m-synched
           (do ((i-piece :: int 0 (+ i-piece 1)))
               ((= i-piece m-n-piece))
             (let* ((p :: SPiece (m-pieces i-piece))
                    (vec :: int p:vec)
                    (p-id :: byte p:i-piece)
                    (row-offset :: int p:row)
                    (n-new-cells :: int 0))
               (call-with-current-continuation
                (lambda (break)
                  (do ((y :: int row-offset (+ y 1)))
                      ((= y N-ROW))
                    (do ((x :: int 0 (+ x 1)))
                        ((= x N-COL))
                      (when (not (= 0 (bitwise-and vec 1)))
                            (set! (m-cell (this) y x) p-id)
                            (++! n-new-cells))
                      (set->>! vec 1))
                    (when (= n-new-cells N-ELEM)
                          (break)))))))
           (set! m-synched #t)))

  ((less-than (r :: Soln)) :: boolean
   (cond ((not (= (m-pieces 0):i-piece (r:m-pieces 0):i-piece))
          (< (m-pieces 0):i-piece (r:m-pieces 0):i-piece))
         (else
          (*:set-cells (this))
          (*:set-cells r)

          (call-with-current-continuation
           (lambda (return)
             (do ((y :: int 0 (+ y 1)))
                 ((= y N-ROW) (return #f))
               (do ((x :: int 0 (+ x 1)))
                   ((= x N-COL))
                 (let ((lval :: int (m-cell (this) y x))
                       (rval :: int (m-cell r y x)))
                   (when (not (= lval rval))
                         (return (< lval rval)))))))))))

  ((spin (spun :: Soln)) :: void
   (*:set-cells (this))

   (do-decrementing yx N-CELL
		    (set! (spun:m-cells yx)
			  (m-cells (- N-CELL 1 yx))))

   (set! (spun:m-pieces 0):i-piece (m-pieces (- N-PIECE-TYPE 1)):i-piece)
   (set! spun:m-synched #t)))

(define-syntax get-index
  (syntax-rules ()
    ((_ x y)
     (+ (* y N-COL) x))))

;; Board
(define-simple-class Board ()
  (m-cur-soln :: Soln init: (Soln Soln:NO-PIECE))
  (m-min-soln :: Soln init: (Soln N-PIECE-TYPE))
  (m-max-soln :: Soln init: (Soln Soln:NO-PIECE))
  (m-n-soln :: int init: 0)

  ((bad-region (to-fill ::int) (r-new ::int)) ::int
   allocation: 'static
   ;; grow empty region until it doesn't change anymore
   (let loop ((r-new :: int r-new))
     (let ((region :: int r-new))
       ;; simple grow up/down
       (set-ior! r-new (bitwise-arithmetic-shift-right region N-COL))
       (set-ior! r-new (bitwise-arithmetic-shift-left region N-COL))
       ;; grow right/left
       (set-ior! r-new (bitwise-arithmetic-shift-right
                        (bitwise-and region (bitwise-not L-EDGE-MASK)) 1))
       (set-ior! r-new (bitwise-arithmetic-shift-left
                        (bitwise-and region (bitwise-not R-EDGE-MASK)) 1))

       ;; tricky growth
       (let ((even-region
	      :: int
              (bitwise-and
               region (bitwise-and ROW-0-MASK (bitwise-not L-EDGE-MASK)))))
         (set-ior! r-new
                   (bitwise-arithmetic-shift-right even-region (+ N-COL 1)))
         (set-ior! r-new
                   (bitwise-arithmetic-shift-left even-region (- N-COL 1))))
       (let ((odd-region
              :: int
              (bitwise-and
               region (bitwise-and ROW-1-MASK (bitwise-not R-EDGE-MASK)))))
         (set-ior! r-new
                   (bitwise-arithmetic-shift-right odd-region (- N-COL 1)))
         (set-ior! r-new
                   (bitwise-arithmetic-shift-left odd-region (+ N-COL 1))))

       ;; clamp against existing pieces
       (set-and! r-new to-fill)

       (cond ((and (not (= r-new to-fill))
                   (not (= r-new region)))
              (loop r-new))
             (else
              (bitwise-xor to-fill r-new))))))

  ((has-bad-islands (board-vec :: int) (row :: int)) :: int
   allocation: 'static
   (while (= TOP-ROW (bitwise-and board-vec TOP-ROW))
          (set->>! board-vec N-COL)
          (!++ row))
   (let* ((i-info
           :: int
           (bitwise-and
            board-vec
            (- (bitwise-arithmetic-shift-left 1 (* 2 N-COL)) 1)))
          (info :: IslandInfo (g-island-info i-info))
          (last-row
           :: int
           (bitwise-and
            (bitwise-arithmetic-shift-right board-vec (* 2 N-COL))
            TOP-ROW))
          (mask :: int (get-mask last-row))
          (is-odd :: int (bitwise-and row 1))
          (is-closed :: int (if (> row 6) 1 0))
	  (odd-closed-index (+ (* is-odd N-PARITY) is-closed)))
     (cond ((not (= 0 (bitwise-and mask (info:always-bad is-odd))))
            BAD)
           ((not (= 0 (bitwise-and board-vec
                                   (bitwise-arithmetic-shift-left
                                    TOP-ROW (* N-COL 3)))))
            (Board:calc-bad-islands board-vec row))
           ((not (= 0 (bitwise-and mask
                                   (info:is-known odd-closed-index))))
            (bitwise-and mask (info:has-bad odd-closed-index)))
           ((= 0 board-vec) GOOD)
           (else
            (let ((has-bad :: int (Board:calc-bad-islands board-vec row)))
              (set-ior! (info:is-known odd-closed-index) mask)
              (when (not (= 0 has-bad))
                    (set-ior! (info:has-bad odd-closed-index) mask))
              has-bad)))))

  ((calc-bad-islands (board-vec :: int) (row :: int)) :: int
   allocation: 'static
   (let ((to-fill :: int (bitwise-not board-vec))
         (board-mask :: int BOARD-MASK)
         (bottom :: int (bitwise-arithmetic-shift-left TOP-ROW (* 5 N-COL)))
         (start-region :: int 0))
     (when (not (= 0 (bitwise-and row 1)))
           (!-- row)
           (set-<<! to-fill N-COL))
     (when (> row 4)
           (let ((board-mask-shift :: int (* (- row 4) N-COL)))
             (set->>! board-mask board-mask-shift)))
     (set-and! to-fill board-mask)

     (let ((filled :: boolean (= bottom (bitwise-and bottom to-fill))))
       (while (= bottom (bitwise-and bottom to-fill))
              (set-xor! to-fill bottom)
              (set->>! bottom N-COL))

       (if (or filled (< row 4))
           (set! start-region (bitwise-and bottom to-fill))
           (begin
             (set! start-region
                   (g-first-region (bitwise-and to-fill TOP-ROW)))
             (when (= 0 start-region)
                   (set! start-region
                         (bitwise-and (bitwise-arithmetic-shift-right
                                       to-fill N-COL) TOP-ROW))
                   (set! start-region
                         (g-first-region start-region))
                   (set-<<! start-region N-COL))
             (set-ior! start-region
                       (bitwise-and
                        (bitwise-arithmetic-shift-left start-region N-COL)
                        to-fill))))

       (call-with-current-continuation
        (lambda (return)
          (while (not (= 0 to-fill))
                 (set! to-fill (bad-region to-fill start-region))
                 (when (> (remainder (count-ones to-fill) N-ELEM) 0)
                       (return (if (not (= 0 to-fill)) ALWAYS-BAD BAD)))
                 (set! start-region (get-mask (get-first-one to-fill))))
          (return GOOD))))))

  ((calc-always-bad) :: void allocation: 'static
   (do ((i-word :: int 1 (+ i-word 1)))
       ((= i-word MAX-ISLAND-OFFSET))
     (let ((isle-info :: IslandInfo (g-island-info i-word))
           (flipped :: IslandInfo (g-island-info (flip-two-rows i-word))))
       (let loop ((i :: int 0) (mask :: int 1))
         (cond ((= i 32))
               ((not (= 0 (bitwise-and mask (isle-info:is-known OPEN))))
		(loop (+ i 1) (bitwise-arithmetic-shift-left mask 1)))
               (else
                (let* ((board-vec
                       :: int
                       (bitwise-ior
                        (bitwise-arithmetic-shift-left i (* 2 N-COL))
                        i-word))
                       (has-bad :: int
                                (Board:calc-bad-islands board-vec 0)))
                  (when (not (= has-bad GOOD))
                        (let ((always :: boolean (= has-bad
                                                    ALWAYS-BAD)))
                          (mark-bad isle-info mask EVEN always)
                          (let ((flip-mask :: int (get-mask (g-flip i))))
                            (mark-bad flipped flip-mask ODD always))))
                  (loop (+ i 1) (bitwise-arithmetic-shift-left mask 1))))))

       (set! (flipped:is-known (+ N-PARITY OPEN)) -1)
       (set! (isle-info:is-known OPEN) -1))))

  ((has-bad-islands-single (board-vec :: int) (row :: int)) :: boolean
   allocation: 'static
   (let ((to-fill :: int (bitwise-not board-vec))
         (is-odd :: boolean (not (= 0 (bitwise-and row 1))))
         (start-region :: int TOP-ROW)
         (last-row :: int (bitwise-arithmetic-shift-left
                           TOP-ROW (* 5 N-COL)))
         (board-mask :: int BOARD-MASK))
     (when is-odd
           (!-- row)
           (set-<<! to-fill N-COL)
           (set-ior! to-fill TOP-ROW))
     (cond ((>= row 4)
            (set->>! board-mask (* (- row 4) N-COL)))
           ((or is-odd (= row 0))
            (set! start-region last-row)))

     (set-and! to-fill board-mask)
     (set-and! start-region to-fill)

     (call-with-current-continuation
      (lambda (return)
        (while (not (= 0 to-fill))
	       (set! to-fill (bad-region to-fill start-region))
               (when (> (remainder (count-ones to-fill) N-ELEM) 0)
                     (return #t))
               (set! start-region
                     (get-mask (get-first-one to-fill))))
        (return #f)))))

  ((gen-all-solutions (board-vec :: int)
                      (placed-pieces :: int)
                      (row :: int))
   :: void
   (while (= TOP-ROW (bitwise-and board-vec TOP-ROW))
          (set->>! board-vec N-COL)
          (!++ row))

   (let* ((i-next-fill :: int (s-first-one
                               (bitwise-and TOP-ROW (bitwise-not board-vec))))
          (allowed :: OkPieces (g-ok-pieces (+ (* row N-COL) i-next-fill)))
          (i-piece :: int (get-first-one (bitwise-not placed-pieces)))
          (piece-mask :: int (get-mask i-piece)))

     (do ((i-piece :: int i-piece (+ i-piece 1))
          (piece-mask :: int piece-mask (bitwise-arithmetic-shift-left
                                         piece-mask 1)))
         ((= i-piece N-PIECE-TYPE))
       (call-with-current-continuation
        (lambda (continue-outer)
          (when (not (= 0 (bitwise-and piece-mask placed-pieces)))
                (continue-outer))
          (set-ior! placed-pieces piece-mask)
          (do ((i-orient :: int 0 (+ i-orient 1)))
              ((= i-orient (allowed:n-pieces i-piece)))
            (call-with-current-continuation
             (lambda (continue-inner)
               (let ((piece-vec :: int (allowed:piece-vec (+ (* i-piece N-ORIENT) i-orient))))
                 (when (not (= 0 (bitwise-and piece-vec board-vec)))
                       (continue-inner))
                 (set-ior! board-vec piece-vec)
                 (when (not (= 0 (Board:has-bad-islands board-vec row)))
                       (set-xor! board-vec piece-vec)
                       (continue-inner))
                 (m-cur-soln:push-piece piece-vec i-piece row)
                 ;; recur or record solution
                 (if (< placed-pieces ALL-PIECE-MASK)
                     (*:gen-all-solutions (this) board-vec
                                          placed-pieces row)
                     (*:record-solution (this) m-cur-soln))

                 (set-xor! board-vec piece-vec)
                 (m-cur-soln:pop-piece)))))
          (set-xor! placed-pieces piece-mask))))))

  ((record-solution (s :: Soln)) :: void
   (!++ m-n-soln 2)

   (if (m-min-soln:is-empty)
       (begin (set! m-min-soln (s:clone2))
              (set! m-max-soln m-min-soln))
       (begin
         (cond ((s:less-than m-min-soln)
                (set! m-min-soln (s:clone2)))
               ((m-max-soln:less-than s)
                (set! m-max-soln (s:clone2))))
         (let ((spun :: Soln (Soln)))
           (s:spin spun)
           (cond ((spun:less-than m-min-soln)
                  (set! m-min-soln spun))
                 ((m-max-soln:less-than spun)
                  (set! m-max-soln spun))))))))


;; Instance
(define-simple-class Instance ()
  (m-allowed :: long)
  (m-vec :: int)
  (m-offset :: int))

;; Piece
(define-simple-class Piece ()

  (m-instance :: Instance[] init: (Instance[] length: N-PARITY))
  (init: (do ((i :: int 0 (+ i 1)))
             ((= i N-PARITY))
           (set! (m-instance i) (Instance))))

  ((set-coord-list (vec :: int) (pts :: int[])) :: void
   allocation: 'static

   (let ((i-pt :: int 0)
         (mask :: int 1))
     (do ((y :: int 0 (+ y 1)))
         ((= y N-ROW))
       (do ((x :: int 0 (+ x 1)))
           ((= x N-COL))
         (when (not (= 0 (bitwise-and mask vec)))
               (set! (p-cell pts i-pt X) x)
               (set! (p-cell pts i-pt Y) y)
               (!++ i-pt))
         (set-<<! mask 1)))))

  ((to-bit-vector (pts :: int[])) :: int allocation: 'static
   (let ((result :: int 0))
     (do ((i-pt :: int 0 (+ i-pt 1)))
         ((= i-pt N-ELEM) result)
       (set-ior! result
                 (bitwise-arithmetic-shift-left
                  1 (get-index (p-cell pts i-pt X) (p-cell pts i-pt Y)))))))

  ((shift-up-lines (pts :: int[]) (shift :: int)) :: void
   allocation: 'static

   (do ((i-pt :: int 0 (+ i-pt 1)))
       ((= i-pt N-ELEM))
     (when (not (= 0 (bitwise-and (p-cell pts i-pt Y) shift #x1)))
           (++! (p-cell pts i-pt X)))
     (--! (p-cell pts i-pt Y) shift)))

  ((shift-to-x0 (pts :: int[])
                (instance :: Instance)
                (offset-row :: int))
   :: int allocation: 'static

   (let* ((x-min :: int (p-cell pts 0 X))
          (x-max :: int x-min))
     (do ((i-pt :: int 1 (+ i-pt 1)))
         ((= i-pt N-ELEM))
       (let ((x :: int (p-cell pts i-pt X)))
         (cond ((< x x-min) (set! x-min x))
               ((> x x-max) (set! x-max x)))))

     (let ((offset :: int N-ELEM))
       (do ((i-pt :: int 0 (+ i-pt 1)))
           ((= i-pt N-ELEM))
         (--! (p-cell pts i-pt X) x-min)
         (when (and (= (p-cell pts i-pt Y) offset-row)
                    (< (p-cell pts i-pt X) offset))
               (set! offset (p-cell pts i-pt X))))

       (set! instance:m-offset offset)
       (set! instance:m-vec (Piece:to-bit-vector pts))
       (- x-max x-min))))

  ((set-ok-pos (is-odd :: int) (w :: int) (h :: int)) :: void
   (let ((p :: Instance (m-instance is-odd))
         (pos-mask :: long (bitwise-arithmetic-shift-left
                            1 (* is-odd N-COL))))
     (set! p:m-allowed (as long 0))

     (do ((y :: int is-odd (+ y 2))
          (pos-mask :: long pos-mask (bitwise-arithmetic-shift-left
                                      pos-mask N-COL)))
         ((>= y (- N-ROW h)))

       (when (not (= 0 p:m-offset))
             (set-<<! pos-mask p:m-offset))

       (do ((x-pos :: int 0 (+ x-pos 1)))
           ((= x-pos (- N-COL p:m-offset)))

         (when (< x-pos (- N-COL w))
               (let ((piece-vec :: int (bitwise-arithmetic-shift-left
                                        p:m-vec x-pos)))
                 (unless (Board:has-bad-islands-single piece-vec y)
                         (set-ior! p:m-allowed pos-mask))))

         (set-<<! pos-mask 1)))))

  ((gen-orientation (vec :: int) (i-orient :: int) (target :: Piece))
   :: void allocation: 'static
   (let ((pts :: int[] (int[] length: (* N-ELEM N-DIM))))
     (Piece:set-coord-list vec pts)

     (let* ((rot :: int (remainder i-orient 6))
            (flip :: boolean (>= i-orient 6)))
       (when flip
             (do-decrementing i-pt N-ELEM
			      (set! (p-cell pts i-pt Y) (- (p-cell pts i-pt Y)))))

       (while (not (= 0 (!-- rot)))
              (do ((i-pt :: int 0 (+ i-pt 1)))
                  ((= i-pt N-ELEM))
                (let ((x :: int (p-cell pts i-pt X))
                      (y :: int (p-cell pts i-pt Y)))
                  (let ((x-new
                         :: int
                         (my-floor (+ (* 2 x) (* -3 y) 1) 4))
                        (y-new
                         :: int
                         (my-floor (+ (* 2 x) y 1) 2)))
                    (set! (p-cell pts i-pt X) x-new)
                    (set! (p-cell pts i-pt Y) y-new)))))

       (let* ((y-min :: int (p-cell pts 0 Y))
              (y-max :: int y-min))
         (do ((i-pt :: int 1 (+ i-pt 1)))
             ((= i-pt N-ELEM))
           (let ((y :: int (p-cell pts i-pt Y)))
             (cond ((< y y-min) (set! y-min y))
                   ((> y y-max) (set! y-max y)))))

         (let ((h :: int (- y-max y-min))
               (even :: Instance (target:m-instance EVEN))
               (odd :: Instance (target:m-instance ODD)))
           (Piece:shift-up-lines pts y-min)
           (let ((w :: int (Piece:shift-to-x0 pts even 0)))
             (target:set-ok-pos EVEN w h)
             (set->>! even:m-vec even:m-offset)
             (Piece:shift-up-lines pts -1)
             (set! w (Piece:shift-to-x0 pts odd 1))
             (set->>! odd:m-vec N-COL)
             (target:set-ok-pos ODD w h)
             (set->>! odd:m-vec odd:m-offset)))))))

  ((gen-all-orientations) :: void allocation: 'static
   (do ((i-piece :: int 0 (+ i-piece 1)))
       ((= i-piece N-PIECE-TYPE))
     (let ((ref-piece :: int (BaseVecs i-piece)))
       (do ((i-orient :: int 0 (+ i-orient 1)))
           ((= i-orient N-ORIENT))
         (let ((p :: Piece (s-base-piece i-piece i-orient)))
           (Piece:gen-orientation ref-piece i-orient p)
           (when (and (= i-piece SKIP-PIECE) ;; 5
                      (not (= 0 (bitwise-and 1 (quotient i-orient 3)))))
                 (set! (p:m-instance 0):m-allowed 0)
                 (set! (p:m-instance 1):m-allowed 0))))))
   (do ((i-piece :: int 0 (+ i-piece 1)))
       ((= i-piece N-PIECE-TYPE))
     (do ((i-orient :: int 0 (+ i-orient 1)))
         ((= i-orient N-ORIENT))
       (let ((mask :: long 1))
         (do ((i-row :: int 0 (+ i-row 1)))
             ((= i-row N-ROW))
           (let ((p :: Instance (Piece:get-piece i-piece i-orient
                                                 (bitwise-and i-row 1))))
             (do ((i-col :: int 0 (+ i-col 1)))
                 ((= i-col N-COL))
               (when (not (= 0 (bitwise-and p:m-allowed mask)))
                     (let* ((allowed :: OkPieces
                                     (g-ok-pieces (+ (* i-row N-COL) i-col)))
                            (val :: int (bitwise-arithmetic-shift-left
                                         p:m-vec i-col))
                            (i2 :: int (allowed:n-pieces i-piece)))
                       (set! (allowed:piece-vec (+ (* i-piece N-ORIENT) i2)) val)
                       (++! (allowed:n-pieces i-piece))))
               (set-<<! mask 1))))))))

  ((get-piece (i-piece :: int) (i-orient :: int) (i-parity :: int))
   :: Instance allocation: 'static
   ((s-base-piece i-piece i-orient):m-instance i-parity)))


;;;; main
(define (program-main (args :: list) (is-warm :: boolean))
  (when (> (length args) 2) (exit -1))

  (init-globals)
  (let ((b :: Board (Board)))
    (Piece:gen-all-orientations)
    (Board:calc-always-bad)
    (b:gen-all-solutions 0 0 0)

    (when is-warm
          (format #t "~A solutions found~2%" b:m-n-soln)
          (format #t "~A~%~A~%" b:m-min-soln b:m-max-soln))))

#|
(do ((i :: int 0 (+ i 1)))
    ((= i 65))
  (program-main (cdr (command-line)) #f))
|#

(program-main (cdr (command-line)) #t)
