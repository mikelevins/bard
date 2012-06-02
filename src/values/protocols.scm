;;;; ***********************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          protocols.scm
;;;; Project:       Bard
;;;; Purpose:       built-in protocols
;;;; Author:        mikel evins
;;;; Copyright:     2012 by mikel evins
;;;;
;;;; ***********************************************************************


;;; ---------------------------------------------------------------------
;;; Frame
;;; ---------------------------------------------------------------------

(define bard:get (%make-function name: 'get))

(%add-primitive-method! bard:get
                        (%list <frame> Anything)
                        (%list 'frame 'key)
                        (lambda (frame key)(%frame-get frame key (%nothing)))
                        name: 'get)


(define bard:put (%make-function name: 'put))

(%add-primitive-method! bard:put
                        (%list <frame> Anything Anything)
                        (%list 'frame 'key 'val)
                        %frame-put
                        name: 'put)
