#| The Computer Language Benchmarks Game
   http://shootout.alioth.debian.org/
   Contributed by Jamison Hope. Based on the "Java 6 -server" version
   by Michael Barker.
|#

(define-alias ArrayBlockingQueue java.util.concurrent.ArrayBlockingQueue)
(define-alias BlockingQueue java.util.concurrent.BlockingQueue)
(define-alias CountDownLatch java.util.concurrent.CountDownLatch)
(define-alias AtomicInt java.util.concurrent.atomic.AtomicInteger)
(define-alias AtomicRef java.util.concurrent.atomic.AtomicReference)
(define-alias StringBuilder java.lang.StringBuilder)
(define-alias Thread java.lang.Thread)


(define-enum Color (blue red yellow))

(define (complement (c1 :: Color) (c2 :: Color)) :: Color
  (cond ((eq? c1 Color:blue)
         (cond ((eq? c2 Color:blue) Color:blue)
               ((eq? c2 Color:red) Color:yellow)
               (else Color:red)))
        ((eq? c1 Color:red)
         (cond ((eq? c2 Color:blue) Color:yellow)
               ((eq? c2 Color:red) Color:red)
               (else Color:blue)))
        (else (cond ((eq? c2 Color:blue) Color:red)
                    ((eq? c2 Color:red) Color:blue)
                    (else Color:yellow)))))

;;; MeetingPlace
(define-simple-class MeetingPlace ()
  (meetings-left :: AtomicInt)
  (creature-ref :: AtomicRef (AtomicRef))
  ((*init* (meetings :: int))
   (set! meetings-left (AtomicInt meetings)))

  ((meet (incoming :: Creature)) :: void
   (let ((new-color :: Color #!null)
         (first :: Creature #!null)
         (next :: Creature #!null))
     (let loop ()
       (set! first (creature-ref:get))
       (set! next incoming)
       (when (not (eq? #!null first))
             (set! new-color (complement incoming:color first:color))
             (set! next #!null))
       (when (not (creature-ref:compare-and-set first next))
             (loop)))

     (when (not (eq? #!null first))
           (let ((meetings :: int
                           (meetings-left:decrement-and-get)))
             (cond ((>= meetings 0)
                    (first:set-color incoming:id new-color)
                    (incoming:set-color first:id new-color))
                   (else
                    (first:complete)
                    (incoming:complete))))))))

;;; Dispatcher
(define-simple-class Dispatcher (java.lang.Runnable)
  (q :: BlockingQueue)
  ((run)
   (try-catch
    (let loop ()
      ((as Creature (q:take)):run)
      (loop))
    (ex java.lang.InterruptedException #!void))))

;;; Creature
(define-simple-class Creature ()
  (id :: int)
  (place :: MeetingPlace)
  (q :: BlockingQueue)
  (latch :: CountDownLatch)
  (count :: int 0)
  (same-count :: int 0)
  (color :: Color)

  ((*init*)
   (set! id (java.lang.System:identity-hash-code (this))))

  ((complete)
   (latch:count-down))

  ((set-color (other-id :: int) (new-color :: Color))
   (set! color new-color)
   (set! count (+ count 1))
   (let* ((d :: int (- id other-id))
          (a :: int (my-abs d))
          (s :: int (java.lang.Integer:signum a)))
     (set! same-count (+ same-count (- 1 s))))
   (q:add (this)))

  ((run)
   (place:meet (this)))

  ((to-string) :: String
   (string-append (java.lang.Integer:to-string count)
                  (get-number same-count))))

(define (my-abs (x :: int)) :: int
  (let ((y :: int (bitwise-arithmetic-shift-right x 31)))
    (- (bitwise-xor x y) y)))

(define (run (n :: int) #!rest (colors :: Color[]))
  (let* ((len :: int colors:length)
         (place :: MeetingPlace (MeetingPlace n))
         (creatures :: Creature[] (Creature[] length: len))
         (q :: BlockingQueue (ArrayBlockingQueue len))
         (latch :: CountDownLatch (CountDownLatch (- len 1)))
         (ts :: java.lang.Thread[] (java.lang.Thread[] length: len))
         (total :: int 0))
    (do ((i :: int 0 (+ i 1)))
        ((= i len))
      (format #t " ~A" (colors i))
      (set! (creatures i) (Creature place: place
                                    color: (colors i)
                                    q: q
                                    latch: latch)))
    (newline)

    (do ((i :: int 0 (+ i 1)))
        ((= i len))
      (set! (ts i) (Thread (Dispatcher q: q)))
      (set! (ts i):daemon #t)
      ((ts i):start))

    (do ((i :: int 0 (+ i 1)))
        ((= i len))
      (q:add (creatures i)))

    (try-catch
     (begin (latch:await)
            (do ((i :: int 0 (+ i 1)))
                ((= i len))
              ((ts i):interrupt))
            (do ((i :: int 0 (+ i 1)))
                ((= i len))
              ((ts i):join)))
     (ex java.lang.InterruptedException
         (format (current-error-port)
                 "Existing with error: ~A~%" (ex:to-string))))

    (do ((i :: int 0 (+ i 1)))
        ((= i len))
      (format #t "~A~%" (creatures i))
      (set! total (+ total (creatures i):count)))
    (format #t "~A~2%" (get-number total))))

(define (get-number (n :: int)) :: String
  (let ((sb :: StringBuilder (StringBuilder))
        (nStr :: String (java.lang.Integer:to-string n)))
    (do ((i :: int 0 (+ i 1)))
        ((= i (nStr:length))
         (sb:to-string))
      (let* ((c :: char (nStr:char-at i))
             (i :: int (java.lang.Character:get-numeric-value c)))
        (sb:append (format #f " ~R" i))))))

(define (print-all-colors) :: void
  (print-colors Color:blue Color:blue)
  (print-colors Color:blue Color:red)
  (print-colors Color:blue Color:yellow)
  (print-colors Color:red Color:blue)
  (print-colors Color:red Color:red)
  (print-colors Color:red Color:yellow)
  (print-colors Color:yellow Color:blue)
  (print-colors Color:yellow Color:red)
  (print-colors Color:yellow Color:yellow))

(define (print-colors (c1 :: Color) (c2 :: Color)) :: void
  (format #t "~A + ~A -> ~A~%" c1 c2 (complement c1 c2)))

(define (main (n::int)) ::void
    (print-all-colors)
    (newline)

    (run n Color:blue Color:red Color:yellow)
    (run n Color:blue Color:red Color:yellow Color:red Color:yellow
         Color:blue Color:red Color:yellow Color:red Color:blue))

(main (try-catch
       (java.lang.Integer:parse-int (cadr (command-line)))
       (ex java.lang.Exception 600)))
