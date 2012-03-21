;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          syntax.scm
;;;; Project:       bard
;;;; Purpose:       abstract syntax of bard programs
;;;; Author:        mikel evins
;;;;
;;;; ***********************************************************************

(define <syntax>
  (frame:plist->frame 
   `(built-in-type: bard:syntax)))

(define (bard:syntax type value)
  (frame:%merge <syntax>
                (make-frame type: type
                            value: value)))

