(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)

(define (bytevector? x) ::boolean
  (instance? x bytevector))

(define (make-bytevector (n ::int) #!optional (init ::int 0)) :: bytevector
  (make gnu.lists.U8Vector n init))

;; Needed to suppress warning/error if an argument is >= 128.
;; FUTURE: (define ($make$bytevector$ #!rest args ::ubyte[]) ::bytevector
(define ($make$bytevector$ #!rest args ::int[]) ::bytevector
  (let* ((n ::int args:length)
         (arr ::byte[] (byte[] length: n)))
    (do ((i :: <int> 0 (+ i 1)))
	((>= i n)  (make gnu.lists.U8Vector arr))
      (set! (arr i) (args i)))))

(define (bytevector-length (v :: bytevector)) ::int
  (invoke v 'size))

(define (bytevector-u8-ref (v ::bytevector) (i ::int)) ::int
  (invoke v 'intAt i))

(define (bytevector-u8-set! (v ::bytevector) (i ::int) (x ::int)) ::void
  (invoke v 'setByteAt i x))

(define (bytevector-copy v::bytevector
                         #!optional (start ::int 0) (end ::int (v:size)))
  ::bytevector
  (gnu.lists.U8Vector v start (- end start)))

(define (bytevector-copy! (to ::bytevector)
                          (at ::int)
                          (from ::bytevector)
                          #!optional
                          (start ::int 0)
                          (end ::int (from:size)))
  (to:copyFrom at from start end))

(define (bytevector-append #!rest (bvs ::bytevector[]))
  (define nbvs ::int bvs:length)
  (define size ::int
    (let loop ((i ::int 0) (sz ::int 0))
      (if (< i nbvs)
          (loop (+ i 1) (+ sz ((bvs i):size)))
          sz)))
  (define result ::bytevector (gnu.lists.U8Vector size))
  (let loop ((i ::int 0) (off ::int 0))
    (if (< i nbvs)
        (let* ((bv (bvs i))
               (bvlength (bv:size)))
          (result:copyFrom off bv 0 bvlength)
          (loop (+ i 1) (+ off bvlength)))
        result)))

(define (utf8->string (v ::bytevector)
                      #!optional
                      (start ::int 0)
                      (end ::int (v:size)))
  ::string
  (v:toUtf8 start (- end start)))

(define (string->utf8
         (v ::string)
         #!optional
         (start ::int 0)
         (end ::int (v:length)))
  ::bytevector
  (gnu.lists.U8Vector
   (((v:toString):substring start end):getBytes
    (cond-expand (java-7 java.nio.charset.StandardCharsets:UTF_8)
                 (else "UTF-8")))))
