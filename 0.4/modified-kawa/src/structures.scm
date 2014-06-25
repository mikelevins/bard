;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          structures.scm
;;;; Project:       Bard
;;;; Purpose:       the structures API
;;;; Author:        mikel evins
;;;; Copyright:     2014 by mikel evins
;;;;
;;;; ***********************************************************************

(define-private-alias Structure net.bardcode.Structure)

(define (structure x)
  (net.bardcode.Structure:structureOf x))

(define (undefined? x)
  (net.bardcode.Structure:isUndefined x))

(define (defined? x)
  (not (net.bardcode.Structure:isUndefined x)))

(define (nothing? x)
  (net.bardcode.Structure:isNothing x))

(define (something? x)
  (not (net.bardcode.Structure:isNothing x)))

(define (boolean? x)
  (net.bardcode.Structure:isBoolean x))

(define (character? x)
  (net.bardcode.Structure:isCharacter x))

(define (float? x)
  (net.bardcode.Structure:isFloat x))

(define (integer? x)
  (net.bardcode.Structure:isInteger x))

(define (keyword? x)
  (net.bardcode.Structure:isKeyword x))

(define (symbol? x)
  (net.bardcode.Structure:isSymbol x))

(define (string? x)
  (net.bardcode.Structure:isString x))

(define (cons? x)
  (net.bardcode.Structure:isCons x))

(define (vector? x)
  (net.bardcode.Structure:isVector x))

(define (uri? x)
  (net.bardcode.Structure:isURI x))

(define (method? x)
  (net.bardcode.Structure:isMethod x))

(define (box? x)
  (net.bardcode.Structure:isBox x))

(define (class? x)
  (net.bardcode.Structure:isClass x))

(define (map? x)
  (net.bardcode.Structure:isMap x))

(define (seq? x)
  (net.bardcode.Structure:isSeq x))

(define (function? x)
  (net.bardcode.Structure:isFunction x))

(define (protocol? x)
  (net.bardcode.Structure:isProtocol x))

