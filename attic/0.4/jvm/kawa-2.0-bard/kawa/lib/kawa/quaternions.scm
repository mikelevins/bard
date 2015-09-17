;; Copyright (c) 2014 Jamison Hope
;; This is free software;  for terms and warranty disclaimer see ./COPYING.

(module-name (kawa quaternions))
(export quaternion quaternion?
        real-part imag-part jmag-part kmag-part complex-part
        vector-part unit-quaternion unit-vector
        magnitude angle colatitude longitude
        make-rectangular make-polar
        + - * /
        dot-product cross-product conjugate
        exp log expt sqrt
        sin cos tan asin acos atan)
(require <kawa.lib.prim_syntax>)
(import (only (gnu kawa functions AddOp) + -))
(import (only (gnu kawa functions DivideOp) /))
(import (only (gnu kawa functions MultiplyOp) *))
(import (only (kawa standard expt) expt))
(import (only (kawa lib numbers)
              quaternion quaternion? complex?
              real-part imag-part jmag-part kmag-part
              unit-vector
              magnitude angle
              make-rectangular make-polar
              exp log sqrt
              sin cos tan asin acos atan))

(define (complex-part x::java.lang.Number) ::complex
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):complexPart)
      x))

(define (vector-part x::java.lang.Number) ::quaternion
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):vectorPart)
      0))

(define (colatitude x::java.lang.Number) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):colatitude)
      0))

(define (longitude x::java.lang.Number) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):longitude)
      0))

(define (unit-quaternion x::java.lang.Number) ::java.lang.Number
  (cond ((gnu.math.Quaternion? x)
         ((->gnu.math.Quaternion x):unitQuaternion))
        ((gnu.math.Quantity? x)
         (quantity:make (unit-quaternion ((->quantity x):number))
                        ((->quantity x):unit)))
        ((zero? x) 0)
        ((negative? x) -1)
        ((positive? x) 1)
        (else +nan.0)))

(define (dot-product x::java.lang.Number y::java.lang.Number) ::real
  (if (not (and (= (real-part x) 0) (= (real-part y) 0)))
      (error 'dot-product "arguments must be vector quaternions")
      ;; inline expansion of (- (real-part (* x y)))
      (+ (* (imag-part x) (imag-part y))
         (* (jmag-part x) (jmag-part y))
         (* (kmag-part x) (kmag-part y)))))

(define (cross-product x::java.lang.Number y::java.lang.Number) ::quaternion
  (if (not (and (= (real-part x) 0) (= (real-part y) 0)))
      (error 'cross-product "arguments must be vector quaternions")
      (vector-part (* x y))))

(define (conjugate x::java.lang.Number) ::java.lang.Number
  (if (gnu.math.Quaternion? x)
      ((->gnu.math.Quaternion x):conjugate)
      x))
