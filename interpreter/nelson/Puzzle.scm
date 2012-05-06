;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          Puzzle.scm
;;;; Project:       Bard
;;;; Purpose:       implementation of the Nelson Puzzle reader
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************

(##include "../src/values/type-macros.scm")
(##include "../src/values/function-macros.scm")

;;; ---------------------------------------------------------------------
;;; Puzzle state
;;; ---------------------------------------------------------------------

(define *puzzle-version* (list 1 0 0))
(%defglobal '*puzzle-version* *puzzle-version*)

(%defglobal '*puzzles* (%make-frame '()))


;;; ---------------------------------------------------------------------
;;; The Protocol
;;; ---------------------------------------------------------------------

(%define-protocol Puzzle)

;;; puzzle
;;; ---------------------------------------------------------------------

(define bard:puzzle
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame
                                  (list (list kind: 'puzzle)
                                        (list version: *puzzle-version*))))))

(%defglobal 'puzzle bard:puzzle)

;;; define-puzzle
;;; ---------------------------------------------------------------------

(define bard:define-puzzle
  (%primitive-method (pname pz)
                     (%defglobal '*puzzles*
                                 (%frame-put (%eval '*puzzles* '())
                                             pname pz))))

(%defglobal 'define-puzzle bard:define-puzzle)

;;; find-puzzle
;;; ---------------------------------------------------------------------

(define bard:find-puzzle
  (%primitive-method (pname)(%frame-get (%eval '*puzzles* '()) pname)))

(%defglobal 'find-puzzle bard:find-puzzle)

;;; list-puzzles
;;; ---------------------------------------------------------------------

(define bard:list-puzzles
  (%primitive-method ()(%keys (%eval '*puzzles* '()))))

(%defglobal 'list-puzzles bard:list-puzzles)

;;; sprite
;;; ---------------------------------------------------------------------

(define bard:sprite
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame
                                  (list (list kind: 'sprite)
                                        (list version: *puzzle-version*))))))

(%defglobal 'sprite bard:sprite)

;;; variant
;;; ---------------------------------------------------------------------

(define bard:variant
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame
                                  (list (list kind: 'variant)
                                        (list version: *puzzle-version*))))))

(%defglobal 'variant bard:variant)

;;; point
;;; ---------------------------------------------------------------------

(define bard:point
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame
                                  (list (list kind: 'point)
                                        (list version: *puzzle-version*))))))

(%defglobal 'point bard:point)

;;; rectangle
;;; ---------------------------------------------------------------------

(define bard:rectangle
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame
                                  (list (list kind: 'rectangle)
                                        (list version: *puzzle-version*))))))

(%defglobal 'rectangle bard:rectangle)

;;; label
;;; ---------------------------------------------------------------------

(define bard:label
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame (list (list kind: 'label)
                                                     (list version: *puzzle-version*))))))

(%defglobal 'label bard:label)

;;; button
;;; ---------------------------------------------------------------------

(define bard:button
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                 (%list->frame (list (list kind: 'button)
                                                     (list version: *puzzle-version*))))))

(%defglobal 'button bard:button)

;;; timer
;;; ---------------------------------------------------------------------

(define bard:timer
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                   (%list->frame
                                    (list (list kind: 'timer)
                                          (list version: *puzzle-version*))))))

(%defglobal 'timer bard:timer)

;;; actor
;;; ---------------------------------------------------------------------

(define bard:actor
  (%primitive-method (& args)
                     (%frame-merge (%make-frame args)
                                   (%list->frame
                                    (list (list kind: 'actor)
                                          (list version: *puzzle-version*))))))

(%defglobal 'actor bard:actor)

;;; any-of
;;; ---------------------------------------------------------------------

(define bard:any-of
  (%primitive-method (& args)
                     (%list->frame (list
                                    (list kind: 'any-of)
                                    (list version: *puzzle-version*)
                                    (list values: args)))))

(%defglobal 'any-of bard:any-of)
