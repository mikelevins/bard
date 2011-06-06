;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.scm
;;;; Project:       bard
;;;; Purpose:       abstract syntax of bard programs
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define-type bard:syntax-atom
  id: 72EE33C1-805C-417F-B9DE-3E045C0F97F0
  constructor: bard:%make-syntax-atom
  (type bard:%syntax-atom-type)
  (value bard:%syntax-atom-value))

(define-type bard:syntax-sequence
  id: 589E52CA-E294-4BA7-BBA7-25BD1927A6D6
  constructor: bard:%make-syntax-sequence
  (hd bard:%syntax-sequence-hd)
  (tl bard:%syntax-sequence-tl))

(define (bard:%syntax-undefined)
  (bard:%make-syntax-atom 'bard:undefined #!void))

(define (bard:%syntax-nothing)
  (bard:%make-syntax-atom 'bard:nothing '()))

(define (bard:%syntax-true)
  (bard:%make-syntax-atom 'bard:boolean #t))

(define (bard:%syntax-false)
  (bard:%make-syntax-atom 'bard:boolean #f))

(define (bard:%syntax-integer val)
  (bard:%make-syntax-atom 'bard:integer val))

(define (bard:%syntax-float val)
  (bard:%make-syntax-atom 'bard:float val))

(define (bard:%syntax-ratio val)
  (bard:%make-syntax-atom 'bard:ratio val))

(define (bard:%syntax-character val)
  (bard:%make-syntax-atom 'bard:character val))
