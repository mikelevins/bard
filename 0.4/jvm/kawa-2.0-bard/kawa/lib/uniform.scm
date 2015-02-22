;; Uniform vectors, as specified by SRFI-4.

(require <kawa.lib.prim_syntax>)
(require <kawa.lib.std_syntax>)
(require <kawa.lib.syntax>)

(define (s8vector? x) :: <boolean>
  (instance? x <s8vector>))

(define (make-s8vector (n :: <int>) #!optional (init :: <int> 0)) :: <s8vector>
  (make <s8vector> n init))

(define (s8vector . values) :: <s8vector>
  (list->s8vector values))

(define (s8vector-length (v :: <s8vector>)) :: <int>
  (invoke v 'size))

(define (s8vector-ref (v :: <s8vector>) (i :: <int>)) :: <int>
  (invoke v 'intAt i))

(define (s8vector-set! (v :: <s8vector>) (i :: <int>) (x :: <int>)) :: <void>
  (invoke v 'setByteAt i x))

(define (s8vector->list (v :: <s8vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->s8vector (l :: <list>)) :: <s8vector>
  (make <s8vector> l))

(define (u8vector? x) :: <boolean>
  (instance? x <u8vector>))

(define (make-u8vector (n :: <int>) #!optional (init :: <int> 0)) :: <u8vector>
  (make <u8vector> n init))

(define (u8vector . values) :: <u8vector>
  (list->u8vector values))

(define (u8vector-length (v :: <u8vector>)) :: <int>
  (invoke v 'size))

(define (u8vector-ref (v :: <u8vector>) (i :: <int>)) :: <int>
  (invoke v 'intAt i))

(define (u8vector-set! (v :: <u8vector>) (i :: <int>) (x :: <int>)) :: <void>
  (invoke v 'setByteAt i x))

(define (u8vector->list (v :: <u8vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->u8vector (l :: <list>)) :: <u8vector>
  (make <u8vector> l))

(define (s16vector? x) :: <boolean>
  (instance? x <s16vector>))

(define (make-s16vector (n :: <int>) #!optional (init :: <int> 0)) :: <s16vector>
  (make <s16vector> n init))

(define (s16vector . values) :: <s16vector>
  (list->s16vector values))

(define (s16vector-length (v :: <s16vector>)) :: <int>
  (invoke v 'size))

(define (s16vector-ref (v :: <s16vector>) (i :: <int>)) :: <int>
  (invoke v 'intAt i))

(define (s16vector-set! (v :: <s16vector>) (i :: <int>) (x :: <int>)) :: <void>
  (invoke v 'setShortAt i x))

(define (s16vector->list (v :: <s16vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->s16vector (l :: <list>)) :: <s16vector>
  (make <s16vector> l))

(define (u16vector? x) :: <boolean>
  (instance? x <u16vector>))

(define (make-u16vector (n :: <int>) #!optional (init :: <int> 0)) :: <u16vector>
  (make <u16vector> n init))

(define (u16vector . values) :: <u16vector>
  (list->u16vector values))

(define (u16vector-length (v :: <u16vector>)) :: <int>
  (invoke v 'size))

(define (u16vector-ref (v :: <u16vector>) (i :: <int>)) :: <int>
  (invoke v 'intAt i))

(define (u16vector-set! (v :: <u16vector>) (i :: <int>) (x :: <int>)) :: <void>
  (invoke v 'setShortAt i x))

(define (u16vector->list (v :: <u16vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->u16vector (l :: <list>)) :: <u16vector>
  (make <u16vector> l))

(define (s32vector? x) :: <boolean>
  (instance? x <s32vector>))

(define (make-s32vector (n :: <int>) #!optional (init :: <int> 0)) :: <s32vector>
  (make <s32vector> n init))

(define (s32vector . values) :: <s32vector>
  (list->s32vector values))

(define (s32vector-length (v :: <s32vector>)) :: <int>
  (invoke v 'size))

(define (s32vector-ref (v :: <s32vector>) (i :: <int>)) :: <int>
  (invoke v 'intAt i))

(define (s32vector-set! (v :: <s32vector>) (i :: <int>) (x :: <int>)) :: <void>
  (invoke v 'setIntAt i x))

(define (s32vector->list (v :: <s32vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->s32vector (l :: <list>)) :: <s32vector>
  (make <s32vector> l))

(define (u32vector? x) :: <boolean>
  (instance? x <u32vector>))

(define (make-u32vector (n :: <int>) #!optional (init :: <long> 0)) :: <u32vector>
  (make <u32vector> n init))

(define (u32vector . values) :: <u32vector>
  (list->u32vector values))

(define (u32vector-length (v :: <u32vector>)) :: <int>
  (invoke v 'size))

(define (u32vector-ref (v :: <u32vector>) (i :: <int>)) :: <long>
  (invoke v 'get i))

(define (u32vector-set! (v :: <u32vector>) (i :: <int>) (x :: <long>)) :: <void>
  (invoke v 'setIntAt i x))

(define (u32vector->list (v :: <u32vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->u32vector (l :: <list>)) :: <u32vector>
  (make <u32vector> l))

(define (s64vector? x) :: <boolean>
  (instance? x <s64vector>))

(define (make-s64vector (n :: <int>) #!optional (init :: <long> 0)) :: <s64vector>
  (make <s64vector> n init))

(define (s64vector . values) :: <s64vector>
  (list->s64vector values))

(define (s64vector-length (v :: <s64vector>)) :: <int>
  (invoke v 'size))

(define (s64vector-ref (v :: <s64vector>) (i :: <int>)) :: <long>
  (invoke v 'longAt i))

(define (s64vector-set! (v :: <s64vector>) (i :: <int>) (x :: <long>)) :: <void>
  (invoke v 'setLongAt i x))

(define (s64vector->list (v :: <s64vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->s64vector (l :: <list>)) :: <s64vector>
  (make <s64vector> l))

(define (u64vector? x) :: <boolean>
  (instance? x <u64vector>))

(define (make-u64vector (n :: <int>) #!optional (init :: <integer> 0)) :: <u64vector>
  (make <u64vector> n init))

(define (u64vector . values) :: <u64vector>
  (list->u64vector values))

(define (u64vector-length (v :: <u64vector>)) :: <int>
  (invoke v 'size))

(define (u64vector-ref (v :: <u64vector>) (i :: <int>)) :: <integer>
  (invoke v 'get i))

(define (u64vector-set! (v :: <u64vector>) (i :: <int>) (x :: <integer>)) :: <void>
  (invoke v 'setLongAt i x))

(define (u64vector->list (v :: <u64vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->u64vector (l :: <list>)) :: <u64vector>
  (make <u64vector> l))

(define (f32vector? x) :: <boolean>
  (instance? x <f32vector>))

(define (make-f32vector (n :: <int>) #!optional (init :: <float> 0.0)) :: <f32vector>
  (make <f32vector> n init))

(define (f32vector . values) :: <f32vector>
  (list->f32vector values))

(define (f32vector-length (v :: <f32vector>)) :: <int>
  (invoke v 'size))

(define (f32vector-ref (v :: <f32vector>) (i :: <int>)) :: <float>
  (invoke v 'floatAt i))

(define (f32vector-set! (v :: <f32vector>) (i :: <int>) (x :: <float>)) :: <void>
  (invoke v 'setFloatAt i x))

(define (f32vector->list (v :: <f32vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->f32vector (l :: <list>)) :: <f32vector>
  (make <f32vector> l))

(define (f64vector? x) :: <boolean>
  (instance? x <f64vector>))

(define (make-f64vector (n :: <int>) #!optional (init :: <double> 0.0)) :: <f64vector>
  (make <f64vector> n init))

(define (f64vector . values) :: <f64vector>
  (list->f64vector values))

(define (f64vector-length (v :: <f64vector>)) :: <int>
  (invoke v 'size))

(define (f64vector-ref (v :: <f64vector>) (i :: <int>)) :: <double>
  (invoke v 'doubleAt i))

(define (f64vector-set! (v :: <f64vector>) (i :: <int>) (x :: <double>)) :: <void>
  (invoke v 'setDoubleAt i x))

(define (f64vector->list (v :: <f64vector>)) :: <list>
  (invoke-static <list> 'makeList v))

(define (list->f64vector (l :: <list>)) :: <f64vector>
  (make <f64vector> l))
